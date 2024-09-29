namespace Functional

type FoldStatus<'t> =
    | Done of 't
    | Continue of 't

[<Struct>]
type CollectionSplitOptions =
    | DoNotIncludeSeparator
    | IncludeSeparatorAsFirstElement
    | IncludeSeparatorAsLastElement
    | IncludeSeparatorAsOwnGroup