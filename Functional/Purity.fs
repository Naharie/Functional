module Functional.Purity

/// Allows the result of an expression to be used inside that expression,
/// so long as the value isn't accessed before initialization. In other words,
/// the expression must finish execution at least once before the value is accessed.
let inline withRecursive handler =
    let mutable value = Unchecked.defaultof<'t>
    let mutable initialized = false
    
    value <- handler (lazy (
        if not initialized then
            invalidOp "Tried to access the recursive value for initialization."
            
        value
    ))
    initialized <- true
    value