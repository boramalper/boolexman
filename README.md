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
performance problems I intend to address in an unforeseeable future- __NONE__ of
the tests should fail.

If you intend to contribute to __boolexman__, contact me -Bora M. Alper- at
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

- __`subexpressions`__ `expression :: Expression`

  Finds all the subexpressions of `expression`, including the `expression`
  itself.

  __Example:__

  ```
     1> subexpressions (if A iff B then C implies D xor E else F and G or not D)
  ```

  ```
      1 Sub-Expression Tree:
      2   ((A <=> B) ? (C => (D + E)) : ((F ^ G) v !D))
      3   ├─ (A <=> B)
      4   │  ├─ A
      5   │  ├─ B
      6   ├─ (C => (D + E))
      7   │  ├─ C
      8   │  ├─ (D + E)
      9   │  │  ├─ D
     10   │  │  ├─ E
     11   ├─ ((F ^ G) v !D)
     12   │  ├─ (F ^ G)
     13   │  │  ├─ F
     14   │  │  ├─ G
     15   │  ├─ !D
     16   │  │  ├─ D
     17
     18
     19 Sub-Expression List:
     20   • ((A <=> B) ? (C => (D + E)) : ((F ^ G) v !D))
     21   • (A <=> B)
     22   • A
     23   • B
     24   • (C => (D + E))
     25   • C   
     26   • (D + E)
     27   • D
     28   • E
     29   • ((F ^ G) v !D)
     30   • (F ^ G)
     31   • F
     32   • G
     33   • !D
  ```

- __`symbols`__ `expression :: Expression`

  Extracts all the symbols of `expression`.

  __Example:__

  ```
     1> symbols (if A iff B then C implies D xor E else F and G or not D)
  ```

  ```
     1 Symbols:
     2   • A
     3   • B
     4   • C
     5   • D
     6   • E
     7   • F
     8   • G
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
     1> eval [A, D] [E] (if A iff B then C implies D xor E else F and G or not D)
  ```

  TODO: Update Output!
  ```
      1 First transform into CNF:
      2 ((A v !B v !D v F) ^ (A v !B v !D v G) ^ (B v !A v !D v F) ^ (B v !A v !D v G) ^ (!A v !B v !C v !D v !E) ^ (!A v !B v !C v D v E) ^ (A v B v !C v !D v !E) ^ (A v B v !C v D v E) ^ (!C v !D v !E v F) ^ (
      3
      4 Eliminate all maxterms which constains a true symbol:
      5   • [A,!B,!D,F]
      6     is eliminated because A is true.
      7   • [A,!B,!D,G]
      8     is eliminated because A is true.
      9   • [!A,!B,!C,!D,!E]
     10     is eliminated because !E is true.
     11   • [!A,!B,!C,D,E]
     12     is eliminated because D is true.
     13   • [A,B,!C,!D,!E]
     14     is eliminated because A is true.
     15   • [A,B,!C,D,E]
     16     is eliminated because A is true.
     17   • [!C,!D,!E,F]
     18     is eliminated because !E is true.
     19   • [!C,!D,!E,G]
     20     is eliminated because !E is true.
     21
     22
     23 After all:
     24 ((B v !A v !D v F) ^ (B v !A v !D v G))
     25
     26 Transform into DNF:
     27 (B v (B ^ !A) v (B ^ !D) v (B ^ G) v (!A ^ B) v !A v (!A ^ !D) v (!A ^ G) v (!D ^ B) v (!D ^ !A) v !D v (!D ^ G) v (F ^ B) v (F ^ !A) v (F ^ !D) v (F ^ G))
     28
     29 Eliminate all minterms which constains a false symbol:
     30   • [B,!A]
     31     is eliminated because !A is false.
     32   • [B,!D]
     33     is eliminated because !D is false.
     34   • [!A,B]
     35     is eliminated because !A is false.
     36   • [!A]
     37     is eliminated because !A is false.
     38   • [!A,!D]
     39     is eliminated because !A is false.
     40   • [!A,G]
     41     is eliminated because !A is false.
     42   • [!D,B]
     43     is eliminated because !D is false.
     44   • [!D,!A]
     45     is eliminated because !A is false.
     46   • [!D]
     47     is eliminated because !D is false.
     48   • [!D,G]
     49     is eliminated because !D is false.
     50   • [F,!A]
     51     is eliminated because !A is false.
     52   • [F,!D]
     53     is eliminated because !D is false.
     54
     55
     56 After all:
     57 (B v (B ^ G) v (F ^ B) v (F ^ G))
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

* `<SYMBOL LIST>`

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
