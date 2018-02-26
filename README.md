# boolexman
_**bool**ean **ex**pression **man**ipulator for educational purposes_

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

## Installation
1. Clone the repository
   ```
   $ git clone https://github.com/boramalper/boolexman
   $ cd boolexman
   ```

2. Update the `cabal` package cache
   ```
   $ cabal update
   ```

3. Install the dependencies of __boolexman__:
   ```
   $ cabal install --only-dependencies --enable-tests
   ```

4. Test if __boolexman__ works:
   ```
   $ cabal run
   ```

5. (Optional) Install it to your system:
   ```
   $ cabal install
   ```

   * If you wish not to install, you can always navigate to the directory you
     cloned __boolexman__ to, and execute `cabal run`.

### Running the Tests
Navigate to the directory you cloned __boolexman__ to, and execute `cabal test`.

Beware that some of the tests might (very likely indeed) timeout -due to some
performance problems I intend to address in an unforeseeable future- __none__ of
the tests should fail.

If you intend to contribute to __boolexman__, contact me at
<bora@boramalper.org>.

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

Every command consists of a verb (*i.e.* command verb), and arguments of some
format, which depends on the verb but is consistent throughout the
__boolexman__. Whilst any errors are printed on the prompt screen, the output of
the commands are piped to the `less`, which allows you to scroll in all four
directions (which is a crucial functionality since some commands produce *large*
outputs [*i.e.* entailment trees]). You can also save the output of your
commands to the disk, which are explained below.

### Command Verbs
Every command to the __boolexman__ must start with a *command verb*, followed by
zero or more space-separated arguments. Every command verb starts with a letter,
followed by optionally some more alphanumeric characters. Command verbs are
case-insensitive!

- __`quit`__

  Quits the program. Takes no arguments.

- __`symbols`__ `expression :: Expression`

  Extracts all the symbols of `expression`.

  __Example:__

  ```
     1> symbols (if A iff not B then C implies D xor E else True and F or not D)
  ```

  ```
      1 symbols ((A <=> !B) ? (C => (D + E)) : ((True ^ F) v !D))
      2 ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
      3
      4   • A
      5   • B
      6   • C
      7   • D
      8   • E
      9   • F
  ```

- __`subexpressions`__ `expression :: Expression`

  Finds all the subexpressions of `expression`, including the `expression`
  itself.

  __Example:__

  ```
     1> subexpressions (if A iff not B then C implies D xor E else True and F or not D)
  ```

  ```
      1 subexpressions ((A <=> !B) ? (C => (D + E)) : ((True ^ F) v !D))
      2 ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
      3
      4 Sub-Expression Tree:
      5   ((A <=> !B) ? (C => (D + E)) : ((True ^ F) v !D))
      6   ├─ (A <=> !B)
      7   │  ├─ A
      8   │  ├─ !B
      9   │  │  ├─ B
     10   ├─ (C => (D + E))
     11   │  ├─ C
     12   │  ├─ (D + E)
     13   │  │  ├─ D
     14   │  │  ├─ E
     15   ├─ ((True ^ F) v !D)
     16   │  ├─ (True ^ F)
     17   │  │  ├─ True
     18   │  │  ├─ F
     19   │  ├─ !D
     20   │  │  ├─ D
     21
     22 Sub-Expression List:
     23   • True
     24   • A
     25   • B
     26   • C
     27   • D
     28   • E
     29   • F
     30   • !B
     31   • !D
     32   • (D + E)
     33   • (True ^ F)
     34   • (A <=> !B)
     35   • (C => (D + E))
     36   • ((True ^ F) v !D)
     37   • ((A <=> !B) ? (C => (D + E)) : ((True ^ F) v !D))
  ```

- __`tabulate`__ `expression :: Expression`

  Constructs a truth table with subexpressions as columns, and possible
  evaluations as rows of the truth table.

  __Example:__

  ```
     1> tabulate (C implies D xor E <=> True and F or not D)
  ```

  ```
      1 tabulate ((C => (D + E)) <=> ((True ^ F) v !D))
      2 ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
      3
      4 ╔════╤═╤═╤═╤═╤══╤═══════╤══════════╤══════════════╤═════════════════╤══════════════════════════════════════╗
      5 ║True│C│D│E│F│!D│(D + E)│(True ^ F)│(C => (D + E))│((True ^ F) v !D)│((C => (D + E)) <=> ((True ^ F) v !D))║
      6 ╟────┼─┼─┼─┼─┼──┼───────┼──────────┼──────────────┼─────────────────┼──────────────────────────────────────╢
      7 ║ ⊤  │⊥│⊥│⊥│⊥│⊤ │   ⊥   │    ⊥     │      ⊤       │        ⊤        │                  ⊤                   ║
      8 ║ ⊤  │⊥│⊥│⊥│⊤│⊤ │   ⊥   │    ⊤     │      ⊤       │        ⊤        │                  ⊤                   ║
      9 ║ ⊤  │⊥│⊥│⊤│⊥│⊤ │   ⊤   │    ⊥     │      ⊤       │        ⊤        │                  ⊤                   ║
     10 ║ ⊤  │⊥│⊥│⊤│⊤│⊤ │   ⊤   │    ⊤     │      ⊤       │        ⊤        │                  ⊤                   ║
     11 ║ ⊤  │⊥│⊤│⊥│⊥│⊥ │   ⊤   │    ⊥     │      ⊤       │        ⊥        │                  ⊥                   ║
     12 ║ ⊤  │⊥│⊤│⊥│⊤│⊥ │   ⊤   │    ⊤     │      ⊤       │        ⊤        │                  ⊤                   ║
     13 ║ ⊤  │⊥│⊤│⊤│⊥│⊥ │   ⊥   │    ⊥     │      ⊤       │        ⊥        │                  ⊥                   ║
     14 ║ ⊤  │⊥│⊤│⊤│⊤│⊥ │   ⊥   │    ⊤     │      ⊤       │        ⊤        │                  ⊤                   ║
     15 ║ ⊤  │⊤│⊥│⊥│⊥│⊤ │   ⊥   │    ⊥     │      ⊥       │        ⊤        │                  ⊥                   ║
     16 ║ ⊤  │⊤│⊥│⊥│⊤│⊤ │   ⊥   │    ⊤     │      ⊥       │        ⊤        │                  ⊥                   ║
     17 ║ ⊤  │⊤│⊥│⊤│⊥│⊤ │   ⊤   │    ⊥     │      ⊤       │        ⊤        │                  ⊤                   ║
     18 ║ ⊤  │⊤│⊥│⊤│⊤│⊤ │   ⊤   │    ⊤     │      ⊤       │        ⊤        │                  ⊤                   ║
     19 ║ ⊤  │⊤│⊤│⊥│⊥│⊥ │   ⊤   │    ⊥     │      ⊤       │        ⊥        │                  ⊥                   ║
     20 ║ ⊤  │⊤│⊤│⊥│⊤│⊥ │   ⊤   │    ⊤     │      ⊤       │        ⊤        │                  ⊤                   ║
     21 ║ ⊤  │⊤│⊤│⊤│⊥│⊥ │   ⊥   │    ⊥     │      ⊥       │        ⊥        │                  ⊤                   ║
     22 ║ ⊤  │⊤│⊤│⊤│⊤│⊥ │   ⊥   │    ⊤     │      ⊥       │        ⊤        │                  ⊥                   ║
     23 ╚════╧═╧═╧═╧═╧══╧═══════╧══════════╧══════════════╧═════════════════╧══════════════════════════════════════╝
  ```

- __`eval`__ `symbols that are true :: List of Symbols` `symbols that are false :: List of Symbols` `expression :: Expression`

  Evaluates the `expression` given a set of `symbols that are true` and `symbols
  that are false`. If not every symbol in the `expression` appears in at least
  one of the sets, then the `expression` will be partially evaluated and the
  result will be in terms of those symbols that do not exists in neither set, in
  Disjunctive Normal Form.

  If some symbols that do not appear in the `expression` appear in one of the
  list of symbols, __boolexman__ will display a warning at the top of its
  output, but otherwise will work as intended.

  If some symbols appear in *both* lists of symbols, then __boolexman__ will
  display an error.

  __Example:__

  ```
     1> eval [A, D] [E,Z] (if A iff not B then C implies D xor E else True and F or not D)
  ```

  ```
      1 eval [A,D] [E,Z] ((A <=> !B) ? (C => (D + E)) : ((True ^ F) v !D))
      2 ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
      3
      4 WARNING: Some of the true/false symbols have not been found in the expression!
      5
      6   Redundant False Symbols: [Z]
      7
      8 1. Transform into Conjunctive Normal Form:
      9   • {F, !D, !A, !B}
     10   • {F, !D, B, A}
     11   • {A, !B, !C, D, E}
     12   • {A, !B, !C, !D, !E}
     13   • {!A, B, !C, D, E}
     14   • {!A, B, !C, !D, !E}
     15   • {F, !D, !C, !E}
     16
     17 2. Eliminate all maxterms which constains a true symbol:
     18   • {F, !D, B, A}
     19     is eliminated because A is true.
     20   • {A, !B, !C, D, E}
     21     is eliminated because A is true.
     22   • {A, !B, !C, !D, !E}
     23     is eliminated because A is true.
     24   • {!A, B, !C, D, E}
     25     is eliminated because D is true.
     26   • {!A, B, !C, !D, !E}
     27     is eliminated because !E is true.
     28   • {F, !D, !C, !E}
     29     is eliminated because !E is true.
     30
     31 Remaining maxterms:
     32   • {F, !D, !A, !B}
     33
     34 3. Transform into Disjunctive Normal Form:
     35   • {F, !D, !A, !B}
     36
     37 4. Eliminate all minterms which constains a false symbol:
     38   • {F, !D, !A, !B}
     39     is eliminated because !A is false.
     40
     41 Remaining minterms:
     42   • {False}
     43
     44 Resultant expression:
     45   False
  ```

- __`toCNF/toDNF`__ `expression :: Expression`

  Tranforms the `expression` into Disjunctive/Conjunctive Normal Form.

  __Example:__

  ```
     1> toCNF (if A iff not B then C implies D else True and F or not D)
  ```

  ```
      1 toCNF ((A <=> !B) ? (C => D) : ((True ^ F) v !D))
      2 ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
      3
      4 1. Transform all if-then-else (ITE) expressions:
      5   • ((A <=> !B) ? (C => D) : (F v !D))
      6     is transformed into
      7     (((A <=> !B) ^ (C => D)) v (!(A <=> !B) ^ (F v !D)))
      8
      9 After all ITE expressions are transformed:
     10   (((A <=> !B) ^ (C => D)) v (!(A <=> !B) ^ (F v !D)))
     11
     12 2. Transform all if-and-only-if (IFF) expressions:
     13   • (A <=> !B)
     14     is transformed into
     15     !(A + !B)
     16
     17 After all IFF expressions are transformed:
     18   ((!(A + !B) ^ (C => D)) v (!!(A + !B) ^ (F v !D)))
     19
     20 3. Tranform all implications:
     21   • (C => D)
     22     is transformed into
     23     (!C v D)
     24
     25 After all implications are transformed:
     26   ((!(A + !B) ^ (!C v D)) v (!!(A + !B) ^ (F v !D)))
     27
     28 4. Tranform all exclusive-or (XOR) expressions:
     29   • (A + !B)
     30     is transformed into
     31     ((A v !B) ^ (!A v !!B))
     32
     33 After all XOR expressions are transformed:
     34   ((!((A v !B) ^ (!A v !!B)) ^ (!C v D)) v (!!((A v !B) ^ (!A v !!B)) ^ (F v !D)))
     35
     36 5. Distribute NOTs:
     37   • !!B
     38     is transformed into
     39     B
     40   • !((A v !B) ^ (!A v B))
     41     is transformed into
     42     (!(A v !B) v !(!A v B))
     43   • !(A v !B)
     44     is transformed into
     45     (!A ^ !!B)
     46   • !(!A v B)
     47     is transformed into
     48     (!!A ^ !B)
     49   • !!A
     50     is transformed into
     51     A
     52   • !((!A ^ B) v (A ^ !B))
     53     is transformed into
     54     (!(!A ^ B) ^ !(A ^ !B))
     55   • !(!A ^ B)
     56     is transformed into
     57     (!!A v !B)
     58   • !(A ^ !B)
     59     is transformed into
     60     (!A v !!B)
     61
     62 After all NOTs are distributed:
     63   ((((!A ^ B) v (A ^ !B)) ^ (!C v D)) v ((A v !B) ^ (!A v B) ^ (F v !D)))
     64
     65 6. Distribute ORs over ANDs:
     66   • ((!A ^ B) v (A ^ !B))
     67     is transformed into
     68     ((!A v !B) ^ (B v A))
     69   • (((!A v !B) ^ (B v A) ^ (!C v D)) v ((A v !B) ^ (!A v B) ^ (F v !D)))
     70     is transformed into
     71     ((((!A v !B) ^ (B v A)) v A v !B) ^ (((!A v !B) ^ (B v A)) v !A v B) ^ (((!A v !B) ^ (B v A)) v F v !D) ^ (!C v D v A v !B) ^ (!C v D v !A v B))
     72   • (((!A v !B) ^ (B v A)) v A v !B)
     73     is transformed into
     74     True
     75   • (((!A v !B) ^ (B v A)) v !A v B)
     76     is transformed into
     77     True
     78   • (((!A v !B) ^ (B v A)) v F v !D)
     79     is transformed into
     80     ((F v !D v !A v !B) ^ (F v !D v B v A))
     81
     82 Resultant expression:
     83   ((F v !D v !A v !B) ^ (F v !D v B v A) ^ (!C v D v A v !B) ^ (!C v D v !A v B))
  ```

- __`resolve`__ `expression :: Expression`

  Iteratively applies the resolution rule to automatically resolve an expression
  until either an empty clause is found or there are no more symbols to resolve
  on left.

  __Example:__

  ```
     1> resolve (if A iff not B then C implies D xor E else True and F or not D)
  ```

  ```
      1 resolve ((A <=> !B) ? (C => (D + E)) : ((True ^ F) v !D))
      2 ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
      3
      4       • B[F,!D,!A,!B]
      5       • B[F,!D,B,A]
      6       • B[A,!B,!C,D,E]
      7       • B[A,!B,!C,!D,!E]
      8       • B[!A,B,!C,D,E]
      9       • B[!A,B,!C,!D,!E]
     10       • [F,!D,!C,!E]
     11     
     12   • ──┤ B ├────────────
     13       • ~[F,!D,A,!A]~
     14       • ~[F,!D,A,!C,D,E]~
     15       • A[F,!D,A,!C,!E]
     16       • ~[!A,!C,D,E,F,!D]~
     17       • ~[!A,!C,D,E,A]~
     18       • ~[!A,!C,D,E,A,!D,!E]~
     19       • A[!A,!C,!D,!E,F]
     20       • ~[!A,!C,!D,!E,A,D,E]~
     21       • ~[!A,!C,!D,!E,A]~
     22     
     23   • ──┤ A ├────────────
     24       • [F,!D,!C,!E]
     25
  ```

- __`entail`__ `antecedent :: Expression` `consequent :: Expression`

  Automatically constructs a (Gentzen-style) sequent calculus tree that branches
  bottom-up.

  __Bugs:__

  - __boolexman__ requires both antecedent and consequent to be non empty
    (*i.e.* non `()`) expressions. As a workaround, you can supply an expression
    that consists of a single symbol that does not appear in the other
    expression.

  __Example:__

  ```
     1> entail (Q) (if A iff not B then C implies D xor E else F or not D)
  ```

  ```
      entail Q ((A <=> !B) ? (C => (D + E)) : (F v !D))
      ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

      1(a). Transform all if-then-else (ITE) expressions in the condition:
        No ITE expressions are found in the condition!

      1(b). Transform all if-then-else (ITE) expressions in the consequence:
        • ((A <=> !B) ? (C => (D + E)) : (F v !D))
          is transformed into
          (((A <=> !B) ^ (C => (D + E))) v (!(A <=> !B) ^ (F v !D)))

      After all ITE expressions in the entailment are transformed:
        Q |- (((A <=> !B) ^ (C => (D + E))) v (!(A <=> !B) ^ (F v !D)))

      2(a). Transform all if-and-only-if (IFF) expressions in the condition:
        No IFF expressions are found in the condition!

      2(b). Transform all if-and-only-if (IFF) expressions in the consequence:
        • (A <=> !B)
          is transformed into
          !(A + !B)
        • (A <=> !B)
          is transformed into
          !(A + !B)

      After all IFF expressions in the entailment are transformed:
        Q |- ((!(A + !B) ^ (C => (D + E))) v (!!(A + !B) ^ (F v !D)))

      3(a). Transform all exclusive-or (XOR) expressions in the condition:
        No XOR expressions are found in the condition!

      3(b). Transform all exclusive-or (XOR) expressions in the consequence:
        • (A + !B)
          is transformed into
          ((A v !B) ^ (!A v !!B))
        • (D + E)
          is transformed into
          ((D v E) ^ (!D v !E))
        • (A + !B)
          is transformed into
          ((A v !B) ^ (!A v !!B))

      After all XOR expressions in the entailment are transformed:
        Q |- ((!((A v !B) ^ (!A v !!B)) ^ (C => ((D v E) ^ (!D v !E)))) v (!!((A v !B) ^ (!A v !!B)) ^ (F v !D)))

                                                                                                                                                                                                                                                                                                                                                                                                 ────────────────── (F)                                                                                                                                                               
                                                                                                                                                                                                                                                                                                                                                                                                 A, E, D, C, Q |- B                                                                                                                                                                   
                                                                                                                                                                                                                                                                                                                                                                                                 ────────────────────── (!L)                                                                                                                                                          
                                                                                                                                                                                                                                                                                                                                                                                                 !B, A, E, D, C, Q |-                                                                                                                                                                 
                                                                                                                                                                                                                                                                                   ────────────────── (F)                                           ────────────────── (F)                       ─────────────────────────── (!R)                                                                                                                                                     
                                                                                                                                                                                                                                                                                   A, C, Q |- B, D, E                                               B, E, D, C, Q |- A                              A, E, D, C, Q |- !!B                                                                                                                                                              
                                                                                                                                                                                                                                                                                   ────────────────────── (!L)                                      ────────────────────── (!R)                  ──────────────────────────────── (!R)                                                                                                                                                
                                                                                                                                                                                                                                                                                    !B, A, C, Q |- D, E                                              E, D, C, Q |- A, !B                              E, D, C, Q |- !A, !!B                                                                                                                                                           
                                                                                                     ─────────────── (F)                       ─────────────── (F)                                                                              ────────────────── (F)             ─────────────────────────── (!R)                                 ─────────────────────────── (!R)             ───────────────────────────────────── (!R)                                                                                                                                           
                                                                                                     D, B, A, Q |- F                           D, Q |- B, A, F                                                                                  B, C, Q |- D, E, A                    A, C, Q |- D, E, !!B                                             D, C, Q |- !E, A, !B                             D, C, Q |- !E, !A, !!B                                                                                                                                                        
                                                                                                     ─────────────────── (!R)                  ─────────────────── (!R)                                                                         ────────────────────── (!R)        ──────────────────────────────── (!R)                            ──────────────────────────────── (!R)        ────────────────────────────────────────── (!R)                                                                                                                                      
                                                                                                      B, A, Q |- F, !D                          Q |- B, A, F, !D                                                                                 C, Q |- D, E, A, !B                    C, Q |- D, E, !A, !!B                                            C, Q |- !D, !E, A, !B                            C, Q |- !D, !E, !A, !!B                                                                                                                                                     
                                                                         ──────────────── (I)        ──────────────────────── (!R)             ──────────────────────── (!L)        ────────────────── (I)                                      ─────────────────────────── (vR)   ───────────────────────────────────── (vR)                       ───────────────────────────────────── (vR)   ─────────────────────────────────────────────── (vR)                                                                     ─────────────── (F)                                         
                                                                         A, Q |- A, F, !D               A, Q |- !B, F, !D                         !B, Q |- A, F, !D                 !B, Q |- !B, F, !D                                            C, Q |- (A v !B), D, E                 C, Q |- (!A v !!B), D, E                                         C, Q |- (A v !B), !D, !E                         C, Q |- (!A v !!B), !D, !E                                                                                     E, D, C, Q |- F                                             
                                                                         ──────────────────── (!L)   ───────────────────────────── (!L)        ───────────────────────────── (!L)   ────────────────────── (!L)                                 ───────────────────────────────────────────────────────────────────────────── (^R)                  ───────────────────────────────────────────────────────────────────────────────────────────────── (^R)                             ────────────────── (I)             ─────────────────── (!R)                                    
                                                                          !A, A, Q |- F, !D               !!B, A, Q |- F, !D                        !A, !B, Q |- F, !D               !!B, !B, Q |- F, !D                                                            C, Q |- ((A v !B) ^ (!A v !!B)), D, E                                                                        C, Q |- ((A v !B) ^ (!A v !!B)), !D, !E                                                               D, C, Q |- F, D, E                  D, C, Q |- F, !E                                           
                                                                         ────────────────────────────────────────────────────────────── (vL)   ──────────────────────────────────────────────────────────────── (vL)                            ────────────────────────────────────────────────────────────────────────────────── (!L)             ────────────────────────────────────────────────────────────────────────────────────────────────────── (!L)                        ────────────────────── (!R)        ──────────────────────── (!R)                               
                                                                                           A, Q, (!A v !!B) |- F, !D                                              !B, Q, (!A v !!B) |- F, !D                                                                          !((A v !B) ^ (!A v !!B)), C, Q |- D, E                                                                       !((A v !B) ^ (!A v !!B)), C, Q |- !D, !E                                                             C, Q |- F, !D, D, E                  C, Q |- F, !D, !E                                        
                                                                         ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── (vL)                       ─────────────────────────────────────────────────────────────────────────────────────── (!R)        ─────────────────────────────────────────────────────────────────────────────────────────────────────────── (!R)                   ─────────────────────────── (vR)   ───────────────────────────── (vR)                          
                                                                                                                              Q, (A v !B), (!A v !!B) |- F, !D                                                                                                          C, Q |- !!((A v !B) ^ (!A v !!B)), D, E                                                                      C, Q |- !!((A v !B) ^ (!A v !!B)), !D, !E                                                           C, Q |- (D v E), F, !D             C, Q |- (!D v !E), F, !D                                  
                                                                         ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── (^L)                  ──────────────────────────────────────────────────────────────────────────────────────────── (vR)   ──────────────────────────────────────────────────────────────────────────────────────────────────────────────── (vR)              ───────────────────────────────────────────────────────────────────── (^R)                     
                                                                                                                               ((A v !B) ^ (!A v !!B)), Q |- F, !D                                                                                                       C, Q |- (D v E), !!((A v !B) ^ (!A v !!B))                                                                   C, Q |- (!D v !E), !!((A v !B) ^ (!A v !!B))                                                                     C, Q |- ((D v E) ^ (!D v !E)), F, !D                                           
      ─────────────────────────────────────────────────────── (I)        ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── (!R)             ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── (^R)         ────────────────────────────────────────────────────────────────────────── (=>R)               
      !((A v !B) ^ (!A v !!B)), Q |- !((A v !B) ^ (!A v !!B))                                                                    Q |- !((A v !B) ^ (!A v !!B)), F, !D                                                                                                                                                           C, Q |- ((D v E) ^ (!D v !E)), !!((A v !B) ^ (!A v !!B))                                                                                                                Q |- (C => ((D v E) ^ (!D v !E))), F, !D                                      
      ─────────────────────────────────────────────────────────── (!R)   ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── (vR)        ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── (=>R)   ──────────────────────────────────────────────────────────────────────────────── (vR)          
       Q |- !!((A v !B) ^ (!A v !!B)), !((A v !B) ^ (!A v !!B))                                                                   Q |- (F v !D), !((A v !B) ^ (!A v !!B))                                                                                                                                                        Q |- !!((A v !B) ^ (!A v !!B)), (C => ((D v E) ^ (!D v !E)))                                                                                                            Q |- (F v !D), (C => ((D v E) ^ (!D v !E)))                                  
      ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── (^R)   ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── (^R)     
                                                                                    Q |- !((A v !B) ^ (!A v !!B)), (!!((A v !B) ^ (!A v !!B)) ^ (F v !D))                                                                                                                                                                                                                Q |- (C => ((D v E) ^ (!D v !E))), (!!((A v !B) ^ (!A v !!B)) ^ (F v !D))                                                                                                                                    
      ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── (^R)
                                                                                                                                                                                                                                        Q |- (!((A v !B) ^ (!A v !!B)) ^ (C => ((D v E) ^ (!D v !E)))), (!!((A v !B) ^ (!A v !!B)) ^ (F v !D))                                                                                                                                                                                                                                        
      ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── (vR)
                                                                                                                                                                                                                                         Q |- ((!((A v !B) ^ (!A v !!B)) ^ (C => ((D v E) ^ (!D v !E)))) v (!!((A v !B) ^ (!A v !!B)) ^ (F v !D)))
  ```



### Operator Precedence & Associativity
In order of precedence:

| Operator | Opeartor (symbolic) | Associativity |
|:---------|:-------------------:|:--------------|
| not | ! | N/A (prefix operator) |
| and | ^ | Associative |
| xor | + | Associative |
| or  | v | Associative |
| implies | => | Right-Associative |
| iff | <=> | Associative |
| if-then-else | ( ? : ) | N/A (must always be enclosed in parantheses)

For instance

```
subexpressions (A ? (B ? C : D) : E xor not F and G implies H or I iff J)
```

would be interpreted as

```
subexpressions (A ? (B ? C : D) : (((E xor (not F and G)) implies (H or I)) iff J))
```

### Saving Outputs
As mentioned previously, __boolexman__ pipes its output through `less` to
provide a more convenient interface to its users, which in turn, also allows
them to save the output of the commands they execute.

To save the output of a command:

* Execute your command as usual.
* Press `s` (small-case!) while viewing the output of your command.
  * You should see a prompt at the bottom of your terminal `log file: `, if not,
    check the documentation of your operating system (your version of `less`
    might not have this functionality).
  * To cancel, press *BACKSPACE*; the prompt at the bottom of your terminal
    should turn back into `:` or `(END)`.
* Enter the name of the file you would like to save into, and press *ENTER*
  (*RETURN*).
  * If the file(name) you just entered already exists, you'll see the warning
    `Warning: "FILENAME" exists; Overwrite, Append or Don't log? ` at the prompt
    of `less`. Pressing `O` will overwrite, and pressing `D` will cancel the
    operation. You can also press `A` to append to the file, but that is
    probably not what you want.
* Done! The prompt at the bottom of your terminal should turn back into `:` or
  `(END)`.

### Command Syntax
In its most general form:

```
<VERB> <VERB SPECIFIC ARGUMENTS...>
```

The definitions below should be decipherable to many, although beware that none
of them are formal definitions (especially with respect to the spaces).

* `<VERB>`

  ```
  [A-Za-z]+
  ```

* `<SYMBOL>`

  ```
  [A-Z][a-z]*
  ```

  `True` and `False` are reserved symbols, meaning *true* and *false*
  respectively.

* `<LIST OF SYMBOLS>`

  ```
  \[<SYMBOL>(, <SYMBOL>)*\]
  ```

* `<PREFIX OPERATOR>`

  ```
  (!  |  not)
  ```

* `<INFIX OPERATOR>`

  ```
  (and  |  ^  |  xor  |  \+  |  or  |  v  |  implies  |  =>  |  iff  |  <=>)
  ```

* `<ITE EXPRESSION>`

  ```
  \((if <EXPRESSION> then <EXPRESSION> else <EXPRESSION>  |  <EXPRESSION> \? <EXPRESSION> : <EXPRESSION>)\)
  ```

* `<EXPRESSION>`

  ```
  (<SYMBOL>  |  <PREFIX OPERATOR> <EXPRESSION>  |  <EXPRESSION> <INFIX OPERATOR> <EXPRESSION>  |  <ITE EXPRESSION>)
  ```

## License
The ISC License, see [LICENSE](./LICENSE) for details.

Copyright (c) 2018 Mert Bora ALPER <bora@boramalper.org>

### Notice
Beware that __boolexman__ used to use GNU Readline library prior to the commit
[c93cb4350823832fc42883661f152c523d767bd4](https://github.com/boramalper/boolexman/commit/c93cb4350823832fc42883661f152c523d767bd4),
but mistakenly said to be licensed under The ISC License where it should have
been The General Public License (due to the Copyleft clause).

## Acknowledgements
In no specific order, I am grateful

* to __Prof. Michael Fourman__ for teaching us the principles of logic, which
  I relied on extensively in developing this program, and of computation.
* to __Prof. Don Sannella__ for teaching us functional programming, which
  enabled me to complete this program in less than two months thanks to the
  expressivity (and type system) of Haskell.
* and to __both__ of them, because [Curry–Howard correspondence](https://en.wikipedia.org/wiki/Curry%E2%80%93Howard_correspondence)! =)
