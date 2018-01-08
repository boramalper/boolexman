boolexman
=========
*__boolean expression manipulator__ for educational purposes*


quick manual:
    tabulate (P and Q and R or S implies T)

    detabulateDNF [P, Q, R] {true, false, false, true, true, false, false, false}
    detabulateCNF [P, Q, R] {true, false, false, true, true, false, false, false}

    subexpressions ((P and Q and R) or (S implies T))

    symbols ((P and Q and R) or (S implies T))

    eval [P, Q] [R, S, T]    ((P and Q and R) or (S implies T))

    toDNF ((P and Q and R) or (S implies T))
    toCNF ((P and Q and R) or (S implies T))

    elininateITE (A ? B : C)
    eliminateIFF (A <=> B)
    eliminateIMP (A => B)
    eliminateXORdnf (A + B + C)

    resolve (((P and Q and R) or (S implies T)))
    resolveDNF {P, Q, false} {!P, !Q, !S} ...

    prove (((P and Q and R) or (S implies T)))  -- gentzen

    entail ((A implies (B and Q)) and (B implies C)) (A implies C)  -- gentzen

syntax:

    command: <small letter>*
    symbol: <capital letter><small letter>*
    symbols: [symbol <, symbol>*]
    expression: (...)

    True and False are reserved symbols

ITE
IFF
IMP

operators (in order):

    not !
    and ^
    xor +
    or  v
    implies =>
    iff <=>
    if X then Y else Z  (X ? Y : Z)

    e.g. A ^ B <=> C v D => E + F ? C : D  is unambigous!

    A iff B <=> C

    e.g. A + B <=> C => D ^ (A v B) ? (A ? B : C) : B
         A+B<=>C=>D^(AvB)?(A?B:C):B
         if A xor B implies (C or D) and (A or B) then (if A then B else C) else B

regexes:

    (eval) \[([A-Z][a-zA-Z]*(?: *, *[A-Z][a-zA-Z]*)*)\] \[([A-Z][a-zA-Z]*(?: *, *[A-Z][a-zA-Z]*)*)\] \((.*)\)
