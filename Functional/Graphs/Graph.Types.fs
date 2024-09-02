namespace Functional.Graphs

/// Whether an edge is one directional or bidirectional.
[<Struct>]
type EdgeKind = Directed | Undirected

/// Represents an edge in a graph.
type Edge<'t> = Edge of EdgeKind * 't * 't

/// Represents a graph as a set of edges.
type Graph<'t when 't : comparison> = private Graph of Set<Edge<'t>>


[<RequireQualifiedAccess>]
module Edge =
    /// Creates an undirected edge from the specified two vertices.
    let inline undirected first second = Edge (Undirected, first, second)
    /// Creates a directed edge from the specified two vertices.
    let inline directed first second = Edge (Directed, first, second)

    let inline isDirected (Edge (direction, _, _)) = direction = Directed

    /// Returns the first element in the edge.
    let inline first (Edge (_, value, _)) = value
    /// Returns the second element in the edge.
    let inline second (Edge (_, _, value)) = value

[<AutoOpen>]
module Prelude =    
    let graph (edges: Edge<'t> list) =
        Graph (Set.ofList edges)
        
    /// Constructs an undirected edge for a graph.
    let inline (<->) (a: 't) (b: 't): Edge<'t> = Edge(Undirected, a, b)
        
    /// Constructs an directed edge for a graph
    let inline (-->) (a: 't) (b: 't): Edge<'t> = Edge(Directed, a, b)