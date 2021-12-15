namespace Functional.Graphs

open System

#nowarn "44"

// Marker interface
type GraphType = interface end
/// Marks a graph as being an undirected graph.
type Undirected = inherit GraphType
/// Marks a graph as being a directed graph.
type Directed = inherit GraphType

/// Represents a vertex in a graph.
[<Struct>]
type Vertex<'t> = Vertex of 't
/// Represents an edge in a graph.
[<Struct>]
type Edge<'t, 'kind when 'kind :> GraphType> = Edge of 't * 't

/// Represents a graph as a set of edges.
type Graph<'t, 'kind when 'kind :> GraphType and 't : comparison> = Graph of Set<Vertex<'t>> * Set<Edge<'t, 'kind>>

[<RequireQualifiedAccess>]
module Vertex =
    /// Creates a vertex from the given value.
    let inline from value = Vertex value

    /// Returns the value of the given vertex.
    let inline value (Vertex value) = value

[<RequireQualifiedAccess>]
module Edge =
    [<Obsolete>]
    module EdgeInternal =
        type IsDirected = IsDirected with
            static member inline ($) (IsDirected, _: Edge<_, 'k>) =
                typeof<'k>.FullName = typeof<Directed>.FullName
            static member inline ($) (IsDirected, _: Edge<_, Directed>) = true
            static member inline ($) (IsDirected, _: Edge<_, Undirected>) = false

    /// Creates an undirected edge from the specified two vertices.
    let inline undirected (Vertex first) (Vertex second) : Edge<'t, Undirected> = Edge (first, second)
    /// Creates a directed edge from the specified two vertices.
    let inline directed (Vertex first) (Vertex second) : Edge<'t, Directed> = Edge (first, second)

    let inline isDirected (edge: Edge<_, 'kind>) = EdgeInternal.IsDirected $ edge

    /// Returns the first element in the edge.
    let inline first (Edge (value, _)) = value
    /// Returns the second element in the edge.
    let inline second (Edge (_, value)) = value

[<AutoOpen>]
module Prelude =
    let graph (edges: Edge<'t, 'kind> list) =
        let vertices =
            edges
            |> List.collect (fun (Edge (a, b)) -> [ Vertex a; Vertex b ])
            |> Set.ofList

        Graph (vertices, Set.ofList edges)