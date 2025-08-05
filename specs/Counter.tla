---- MODULE Counter ----
\* Simple counter specification for demonstration
\* Shows basic TLA+ concepts: variables, init, next, invariants

VARIABLE count

\* Initial state: counter starts at 0
Init == count = 0

\* Actions: increment or decrement counter
Increment == count' = count + 1
Decrement == count' = count - 1

\* Next state relation: can increment or decrement
Next == Increment \/ Decrement

\* Type invariant: counter is always a natural number
TypeInvariant == count \in Nat

\* Safety property: counter never goes negative
SafetyInvariant == count >= 0

\* Specification: initial state and next-state relation
Spec == Init /\ [][Next]_count

====