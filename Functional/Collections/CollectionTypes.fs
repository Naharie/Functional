namespace Functional

type FoldStatus<'t> =
    | Done of 't
    | Continue of 't