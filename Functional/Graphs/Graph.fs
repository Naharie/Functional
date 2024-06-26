[<RequireQualifiedAccess>]
module Functional.Graphs.Graph

open System
open Functional

let empty<'t when 't : comparison> = Graph (Set.empty, Set.empty): Graph<'t>

let isDirected (Graph (_, edges)) =
    edges
    |> Set.forall (fun (Edge (direction, _, _)) -> direction = Directed)

let fromEdges (edges: #seq<Edge<'t>>) =
    let edges = Seq.toArray edges
    
    let vertices =
        edges
        |> Array.collect (fun (Edge (_, a, b)) -> [| Vertex a; Vertex b |])
        |> Set.ofSeq

    Graph (vertices, Set.ofArray edges)

/// Returns the edges of the specified graph.
let getEdges (Graph (_, edges)) = edges
/// Returns the vertices of the specified graph.
let getVertices (Graph (vertices, _)) = vertices

/// Converts an undirected graph to a directed graph.
let toDirect (Graph (vertices, edges): Graph<'t>) =
    let newEdges =
        edges
        |> Set.map (
            function
            | Edge (Directed, a, b) -> Edge(Directed, a, b)
            | Edge (Undirected, a, b) -> Edge(Directed, b, a)
        )
        |> Set.union edges

    Graph (vertices, newEdges)

/// Returns a mapping of nodes to a set of nodes that connect to the key node.
let getIncoming (Graph (vertices, edges): Graph<'t>) =
    let connections =
        edges
        |> Set.toArray
        |> Array.fold (fun (mapping: Map<'t, Set<'t>>) (Edge (direction, a, b)) ->
            let addLink a b mapping =
                mapping
                |> Map.tryFind b
                |> Option.defaultValue Set.empty
                |> Set.add a
                |> applyACB Map.add b mapping
            
            match direction with
            | Directed ->
                addLink a b mapping
                
            | Undirected ->
                mapping
                |> addLink a b
                |> addLink b a
        ) Map.empty

    vertices
    |> Set.fold (fun map (Vertex vertex) ->
        map
        |> Map.tryAdd vertex Set.empty
    ) connections

/// Returns a mapping of nodes to their neighbors.
let getOutgoing (Graph (vertices, edges): Graph<'t>) =
    let connections =
        edges
        |> Set.toArray
        |> Array.fold (fun (mapping: Map<'t, Set<'t>>) (Edge (direction, a, b)) ->
            let addLink a b mapping =
                mapping
                |> Map.tryFind a
                |> Option.defaultValue Set.empty
                |> Set.add b
                |> applyACB Map.add a mapping
            
            match direction with
            | Directed ->
                addLink a b mapping
                
            | Undirected ->
                mapping
                |> addLink a b
                |> addLink b a
        ) Map.empty

    vertices
    |> Set.fold (fun map (Vertex vertex) ->
        map
        |> Map.tryAdd vertex Set.empty
    ) connections

/// Returns a list of all vertices that can be reached from the specified vertex.
let getAdjacentVertices(Graph (_, edges), (Vertex vertex)) =
    edges
    |> Set.toList
    |> List.choose (fun (Edge (direction, a, b)) ->
        match direction with
        | Directed when a = vertex -> Some b
        | Directed -> None
        
        | Undirected when a = vertex -> Some b
        | Undirected when b = vertex -> Some a
        | Undirected -> None
    )

/// Returns a topological sort of the specified graph.
/// If the graph is cyclic then the function returns an empty list.
let topologicalSort (Graph (vertices, _) as graph : Graph<'t>) =
    let connections = getIncoming graph
   
    let rec gather (sort: 't[] list) gathered (vertices: 't[]) =
        if Array.isEmpty vertices then
            sort
        else
            // The set of nodes that only reference nodes we have already gathered.
            let next, remaining =
                vertices
                |> Array.partition (fun vertex ->
                    let links =
                        connections
                        |> Map.tryFind vertex
                        |> Option.defaultValue Set.empty

                    Set.isSubset links gathered
                )

            if Array.isEmpty next && not (Array.isEmpty remaining) then
                []
            else
                gather (next :: sort) (Set.union gathered (Set.ofArray next)) remaining

    /// The root nodes; nodes which are not linked to by other nodes.
    /// In other words, those nodes that have no dependencies.
    let roots, vertices =
        vertices
        |> Set.toArray
        |> Array.partition (fun (Vertex vertex) ->
            connections
            |> Map.containsKey vertex
            |> not
        )

    if roots.Length = 0 then
        [||]
    else
        let roots = Array.map Vertex.value roots
        let vertices = Array.map Vertex.value vertices

        gather [ roots ] (Set.ofArray roots) vertices
        |> List.toArray
        |> Array.rev
        |> Array.collect id

// Shamelessly stolen and ported from wikipedia.
/// Computes the shortest possible path from one vertex in the graph to another vertex in the graph.
let a_star heuristic (weight: Vertex<'t> -> Vertex<'t> -> int) start goal (graph: Graph<'t>) =
    let neighborMap = getOutgoing graph

    let reconstructPath cameFrom current =
        let rec reconstruct path current =
            match cameFrom |> Map.tryFind current with
            | Some previous ->
                reconstruct (previous :: path) previous

            | None -> path

        reconstruct [ current ] current

    let rec search (openHeap: BinaryHeap<Vertex<'t>>) (openSet: Set<Vertex<'t>>) (cameFrom: Map<Vertex<'t>, Vertex<'t>>) (known: Map<Vertex<'t>, int>) (guess: Map<Vertex<'t>, int>) =
        if BinaryHeap.isEmpty openHeap then
            ValueNone
        else
            let current = BinaryHeap.minOrMax openHeap

            if current = goal then
                ValueSome (reconstructPath cameFrom current)
            else
                let openHeap = BinaryHeap.removeMinOrMax openHeap
                let openSet = Set.remove current openSet

                let neighbors = neighborMap |> Map.find (Vertex.value current)

                let openHeap, openSet, cameFrom, known, guess =
                    neighbors
                    |> Set.fold (fun (openHeap, openSet, cameFrom, known, guess) neighbor ->
                        let neighbor = Vertex neighbor

                        match known |> Map.tryFind current with
                        | Some currentScore ->
                            let tentativeScore = currentScore + weight current neighbor
                            
                            let isBetter =
                                match known |> Map.tryFind neighbor with
                                | Some neighborScore -> tentativeScore < neighborScore
                                | None -> true

                            if isBetter then
                                let cameFrom = cameFrom |> Map.add neighbor current
                                let known = known |> Map.add neighbor tentativeScore
                                let guess = guess |> Map.add neighbor (tentativeScore + heuristic neighbor)

                                let openHeap, openSet =
                                    if not (Set.contains neighbor openSet) then
                                        (BinaryHeap.insert (tentativeScore, neighbor) openHeap),
                                        (Set.add neighbor openSet)
                                    else
                                        openHeap, openSet

                                openHeap, openSet, cameFrom, known, guess
                            else
                                openHeap, openSet, cameFrom, known, guess
                        | None ->
                            openHeap, openSet, cameFrom, known, guess
                    ) (openHeap, openSet, cameFrom, known, guess)

                search openHeap openSet cameFrom known guess

    search
        (BinaryHeap.singleton MinHeap (0, start)) Set.empty
        Map.empty 
        (Map.ofArray [| (start, 0) |])
        (Map.ofArray [| (start, heuristic start) |])