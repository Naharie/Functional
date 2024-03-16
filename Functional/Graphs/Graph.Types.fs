namespace Functional.Graphs

/// Represents a vertex in a graph.
type Vertex<'t> = Vertex of 't

/// Whether an edge is one directional or bidirectional.
[<Struct>]
type EdgeKind = Directed | Undirected

/// Represents an edge in a graph.
type Edge<'t> = Edge of EdgeKind * 't * 't

/// Represents a graph as a set of edges.
type Graph<'t when 't : comparison> = private Graph of Set<Vertex<'t>> * Set<Edge<'t>>

[<RequireQualifiedAccess>]
module Vertex =
    /// Creates a vertex from the given value.
    let inline from value = Vertex value

    /// Returns the value of the given vertex.
    let inline value (Vertex value) = value

[<RequireQualifiedAccess>]
module Edge =
    /// Creates an undirected edge from the specified two vertices.
    let inline undirected (Vertex first) (Vertex second) = Edge (Undirected, first, second)
    /// Creates a directed edge from the specified two vertices.
    let inline directed (Vertex first) (Vertex second) = Edge (Directed, first, second)

    let inline isDirected (Edge (direction, _, _)) = direction = Directed

    /// Returns the first element in the edge.
    let inline first (Edge (_, value, _)) = value
    /// Returns the second element in the edge.
    let inline second (Edge (_, _, value)) = value

[<AutoOpen>]
module Prelude =    
    let graph (edges: Edge<'t> list) =
        let vertices =
            edges
            |> List.collect (fun (Edge (_, a, b)) -> [ Vertex a; Vertex b ])
            |> Set.ofList
        
        Graph (vertices, Set.ofList edges)
        
    /// Constructs an undirected edge for a graph.
    let inline (<->) (a: 't) (b: 't): Edge<'t> = Edge(Undirected, a, b)
        
    /// Constructs an directed edge for a graph
    let inline (-->) (a: 't) (b: 't): Edge<'t> = Edge(Directed, a, b)