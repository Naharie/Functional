namespace Functional

[<Struct>]
type FoldStatus = Done | Continue

[<Struct>]
type CollectionSplitOptions =
    | DoNotIncludeSeparator
    | IncludeSeparatorAsFirstElement
    | IncludeSeparatorAsLastElement
    | IncludeSeparatorAsOwnGroup