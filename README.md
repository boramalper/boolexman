# boolexman
*__bool__ean __ex__pression __man__ipulator for educational purposes*

__boolexman__ is a boolean expression manipulator (program) to aid teaching
or studying propositional logic, primarily aimed for the [*Informatics 1 -
Computation and Logic*](http://www.inf.ed.ac.uk/teaching/courses/inf1/cl/)
at [The University of Edinburgh](https://www.ed.ac.uk/)
[School of Informatics](http://www.inf.ed.ac.uk/).

__boolexman__ offers various commands for working with boolean expressions,
from those that can transform any given expression into Disjunctive Normal Form
(*i.e.* an OR of ANDs) or Conjunctive Normal Form (*i.e.* an AND of ORs), to
those (functions) for fully-automated resolution, entailment, and
partial-evaluation. All commands shows *their working* step-by-step, with
detailed explanations of each rule that was used.

## Quick Manual
Each time you run __boolexman__, you will be greeted with a screen like follows:

```
boolexman - boolean expression manipulator | v0.1.0.0

   1>
```

`   1> ` is called *the prompt*, and it indicates that __boolexman__ is ready to
accept your command. The number before the greater-than symbol shows the
*command number*, which is provided only as a convenience to the user and has no
importance to the program at all. __boolexman__ uses Haskeline library to
provide a GNU Readline-like rich line-editing functionality to its users,
including moving backwards/forwards in the command history to Emacs/vi specific
key bindings, whose full list can be found on
[Haskeline Wiki](https://github.com/judah/haskeline/wiki/KeyBindings).

### Command Verbs
Every command to the __boolexman__ must start with a *command verb*, followed by
zero or more space-separated arguments. Every command verb starts with a letter,
followed by optionally some more alphanumeric characters. Command verbs are
case-insensitive!

- __quit__

  Quits the program. Takes no arguments.

- __subexpressions__


```
quit

help

tabulate (P and Q and R or S implies T)

subexpressions ((P and Q and R) or (S implies T))

symbols ((P and Q and R) or (S implies T))

eval [P, Q] [R, S, T] ((P and Q and R) or (S implies T))

toDNF ((P and Q and R) or (S implies T))
toCNF ((P and Q and R) or (S implies T))

resolve (((P and Q and R) or (S implies T)))

entail ((A implies (B and Q)) and (B implies C)) (A implies C)  -- gentzen
```

### Syntax

* __Command:__

## License
The ISC License, see [LICENSE](./LICENSE) for details.

Copyright (c) 2017 Mert Bora ALPER <bora@boramalper.org>

## Acknowledgements



__TODO:__
* entail (!True) (True => Q) fails!
* write quickchecks for every single command!
* Write manual!
* `prop_toCNF` and `prop_toDNF` are running very slowly, probably because of
  `toCNF` and `toDNF` are slow! Fix that.

quick manual:
    DONE  quit

          help

          tabulate (P and Q and R or S implies T)

    DONE  subexpressions ((P and Q and R) or (S implies T))

    DONE  symbols ((P and Q and R) or (S implies T))

    DONE  eval [P, Q] [R, S, T] ((P and Q and R) or (S implies T))

    DONE  toDNF ((P and Q and R) or (S implies T))
    DONE  toCNF ((P and Q and R) or (S implies T))

    DONE  resolve (((P and Q and R) or (S implies T)))

    DONE  entail ((A implies (B and Q)) and (B implies C)) (A implies C)  -- gentzen

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
