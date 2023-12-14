[<RequireQualifiedAccess>]
module Functional.Graphs.Graph

open System
open Functional

#nowarn "44"

[<Obsolete>]
module GraphInline =
    type IsDirected = | IsDirected
    with
        static member inline ($) (IsDirected, _: Graph<_, 'k>) =
            typeof<'k>.FullName = typeof<Directed>.FullName
        static member inline ($) (IsDirected, _: Graph<_, Directed>) = true
        static member inline ($) (IsDirected, _: Graph<_, Undirected>) = false

let inline empty<'t, 'kind when 't : comparison and 'kind :> GraphType> = Graph (Set.empty, Set.empty) : Graph<'t, 'kind>

let inline isDirected (graph : Graph<'t, 'k>) =
    GraphInline.IsDirected $ graph

let fromEdges (edges: #seq<Edge<'t, 'kind>>) =
    let edges = Seq.toArray edges
    
    let vertices =
        edges
        |> Array.collect (fun (Edge (a, b)) -> [| Vertex a; Vertex b |])
        |> Set.ofSeq

    Graph (vertices, Set.ofArray edges)

let fromArray (array: 't[][]) =
    array
    |> Array.collecti (fun rowIndex row ->
        row
        |> Array.collecti (fun columnIndex item ->
            [|
                if rowIndex > 0 then
                    if columnIndex > 0 then
                        yield Edge (array[rowIndex - 1][columnIndex - 1], item)
                    
                    yield Edge (array[rowIndex - 1][columnIndex], item)
                    
                    if columnIndex + 1 < row.Length then
                        yield Edge (array[rowIndex - 1][columnIndex + 1], item)
                        
                if columnIndex > 0 then
                    yield Edge (row[columnIndex - 1], item)
                if columnIndex + 1 < row.Length then
                    yield Edge(row[columnIndex + 1], item)
                    
                if rowIndex + 1 < array.Length then
                    if columnIndex > 0 then
                        yield Edge (array[rowIndex + 1][columnIndex - 1], item)
                    
                    yield Edge (array[rowIndex + 1][columnIndex], item)
                    
                    if columnIndex + 1 < row.Length then
                        yield Edge (array[rowIndex + 1][columnIndex + 1], item)
            |]
        )
    )
    |> fromEdges

/// Returns the edges of the specified graph.
let getEdges (Graph (_, edges)) = edges
/// Returns the vertices of the specified graph.
let getVertices (Graph (vertices, _)) = vertices

/// Converts an undirected graph to a directed graph.
/// The type is left open and generic so that the function can be used as a way of ensuring
/// that an unknown graph is safely cast as a directed graph.
let toDirect (Graph (vertices, edges): Graph<'t, _>) =
    let newEdges =
        edges
        |> Set.map (fun (Edge (a, b)) -> Edge (b, a))
        |> Set.union edges

    Graph (vertices, newEdges)

/// Returns a mapping of nodes to a set of nodes that connect to the key node.
let getIncoming (Graph (vertices, edges): Graph<'t, Directed>) =
    let connections =
        edges
        |> Set.toArray
        |> Array.groupBy Edge.second
        |> Array.map (fun (key, group) ->
            let group =
                group
                |> Array.map Edge.first
                |> Set.ofArray
        
            Vertex key, group
        )
        |> Map.ofArray

    vertices
    |> Set.fold (fun map vertex ->
        map
        |> Map.tryAdd vertex Set.empty
    ) connections

/// Returns a mapping of nodes to their neighbors.
let getOutgoing (Graph (vertices, edges): Graph<'t, Directed>) =
    let connections =
        edges
        |> Set.toArray
        |> Array.groupBy Edge.first
        |> Array.map (fun (key, group) ->
            let group =
                group
                |> Array.map Edge.second
                |> Set.ofArray
        
            Vertex key, group
        )
        |> Map.ofArray

    vertices
    |> Set.fold (fun map vertex ->
        map
        |> Map.tryAdd vertex Set.empty
    ) connections

/// Returns a mapping of nodes to a set of nodes that connect to the key node.
let getConnections (Graph (vertices, edges): Graph<'t, Undirected>) =
    let connections =
        edges
        |> Set.toArray
        |> Array.groupBy Edge.first
        |> Array.map (fun (key, group) ->
            let group =
                group
                |> Array.map Edge.second
                |> Set.ofArray
        
            Vertex key, group
        )
        |> Map.ofArray

    vertices
    |> Set.fold (fun map vertex ->
        map
        |> Map.tryAdd vertex Set.empty
    ) connections


/// Returns a topological sort of the specified graph.
/// If the graph is cyclic then the function returns an empty list.
let topologicalSort (Graph (vertices, _) as graph : Graph<'t, Directed>) =
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
                        |> Map.tryFind (Vertex vertex)
                        |> Option.defaultValue Set.empty

                    Set.isSubset links gathered
                )

            if Array.isEmpty next && not (Array.isEmpty remaining) then
                []
            else
                gather (next :: sort) (Set.union gathered (Set.ofArray next)) remaining

    /// The nodes which are not linked to by other nodes.
    /// In other words, those nodes that have no dependencies.
    let roots, vertices =
        vertices
        |> Set.toArray
        |> Array.partition (fun vertex ->
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
/// Computes the shortest possible path from vertex in the graph to another vertex in the graph.
let a_star heuristic (weight: Vertex<'t> -> Vertex<'t> -> int) start goal (graph: Graph<'t, Directed>) =
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
                let openHeap = BinaryHeap.removeMin openHeap
                let openSet = Set.remove current openSet

                let neighbors = neighborMap |> Map.find current

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