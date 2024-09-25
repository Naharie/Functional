module Functional.Purity

/// <summary>
/// Allows the result of an expression to be used inside that expression,
/// so long as the value isn't accessed before initialization. In other words,
/// the expression must finish execution at least once before the value is accessed.
/// </summary>
/// <param name="handler">A function that produces a value with a lazy reference to its own result.</param>
let inline withRecursive handler =
    let mutable value = Unchecked.defaultof<'t>
    let mutable initialized = false
    
    value <- handler (lazy (
        if not initialized then
            invalidOp "Tried to access the recursive value before initialization."

        value
    ))
    initialized <- true
    value