Title: Rebel Guide
css: docs.css

# Rebel Guide (WIP)

---

Copyright (c) 2025 Ufko (ufko.org)

This manual is released under the MIT license.
You may use, copy, modify and distribute it freely,
provided that this notice is preserved.

---

## Table of Contents (Summary of Chapters)

TOC

## Philosophy and Design Goals

Rebel is a small, sharp, and transparent language.  Its
design follows a few strict ideas: no abstraction for
abstraction’s sake, no hidden state, no complex machinery
behind simple constructs.  Everything visible in code
corresponds directly to something happening in the
interpreter.

Rebel is deliberately minimal.  It avoids features that add
convenience at the cost of clarity.  The goal is to provide
a predictable core that encourages disciplined thinking and
precise programming.


### Minimal Surface Area

Rebel has very few concepts:

- symbols
- lists
- sequences
- functions
- contexts
- evaluation rules

These make up almost the entire language.  There are no
classes, packages, generics, annotations, attributes,
decorators, or other layers that multiply mental overhead.
The interpreter stays small because the language stays
small.


### Direct Execution Model

Expressions follow strict and uniform evaluation rules.
There is no syntax hiding behavior, no operator precedence,
and no implicit coercion.  What you write is what gets
evaluated.

An expression is always one of two things:

- a literal value  
- a list representing a function call  

Because of this, reading Rebel code is the same as
understanding how the interpreter will execute it.


### Unix Thinking

Rebel is shaped by the same principles as classic Unix tools:

- composition over frameworks  
- transparency over convenience  
- simplicity over cleverness  
- predictable failure modes  
- explicit control over environment and processes  

Every system-level feature—files, sockets, processes,
pipes—maps directly to the underlying OS.  There are no
wrappers that “simplify” things by hiding details.  If a
user needs deeper control, nothing is in the way.


### No Magic, No Automatic Behavior

Rebel avoids “smart” features that guess what the programmer wants:

- no automatic type conversions  
- no overloading  
- no implicit state changes  
- no silent fallbacks  
- no special-case syntax paths  

Every function does one thing.  When something fails, it
fails loudly, without restructuring control flow or
rewriting values behind the scenes.


### Everything is Inspectable

Rebel treats code as data.  Any expression can be inspected,
transformed, stored, or executed.  The interpreter does not
protect internal details or hide the structure of
definitions.  Functions, contexts, and whole programs can be
introspected or modified at runtime if needed.


### Consistent Data Model

Lists, strings, and arrays form a unified class of
sequences.  Most sequence functions work identically across
all three.  This reduces exceptions and special cases,
making the system easier to reason about.

Closures, contexts, and FOOP objects follow the same rules and require no
special syntax.


### Sharp Rather Than Safe

Rebel does not try to prevent mistakes.  It does not sandbox
the user, it does not enforce “safety patterns,” and it does
not attempt to save the programmer from incorrect
assumptions.

The philosophy is simple:

- Provide clear tools.
- Let the programmer decide how to use them.
- Do not interfere.


### Designed for People Who Think Slowly and Carefully

Rebel favors deliberate, thoughtful programming.  The
language does not encourage rapid hacks or “clever” tricks.
It supports the programmer who prefers clarity,
explicitness, and control.

The core priority is **understanding**, not convenience.


### Summary

Rebel’s design is guided by:

- minimal concepts  
- explicit execution  
- Unix transparency  
- no hidden behavior  
- strong introspection  
- unified data structures  
- trust in the programmer  

The result is a language that is small, predictable, and
extremely direct — a precise tool rather than a platform.


## Running Rebel and Working in the REPL

Rebel can be used in two modes:

1. as an interactive REPL (read–eval–print loop)
2. as a script interpreter for `.rbl` files

Both modes use the same evaluation rules.  
The REPL simply evaluates expressions line by line.


### Starting the REPL

From a terminal:

```
$ rebel
```

Check available options:

```
$ rebel -h
```

You will see a prompt:

```
>
```


### Running a Script File

To execute a `.rbl` file:

```
$ rebel script.rbl
```

Arguments:

```
$ rebel script.rbl arg1 arg2
```

Inside the program:

```
(main-args)
; -> ("script.rbl" "arg1" "arg2")
```


### Preventing Auto-REPL After Script Execution

By default, Rebel **drops into the REPL** after running a
script.  There are two ways to avoid this:

1. Explicitly exit at the end of the script:

```
(exit)
```

2. Or run the interpreter in quiet mode:

```
$ rebel -q script.rbl
```


### Using Rebel as an Executable Script

Add a shebang:

```
#!/usr/bin/env rebel
(println "running")
```

Make it executable:

```
$ chmod +x script.rbl
$ ./script.rbl
```


### Initialization File

If present, Rebel automatically loads:

```
~/.init.rbl
```

This file may contain helper functions, prompt settings, or
custom startup code.


### Reloading Code During Development

Load modules or utility files into the REPL:

```
(load "utils.rbl")
```

When the file changes, simply load it again.  
Rebel evaluates it fresh each time.


### Customizing the Prompt

```
(prompt-event (lambda () "rebel> "))
```


### Debugging Tools

- `trace` — prints function calls  
- `pretty-print` — formatted output  
- `history` — call history for functions  
- `dump` — internal cell information (advanced)

Enable tracing:

```
(trace true)
```


### Inspecting Values

```
(args func)
(source func)
(symbols 'Ctx)
```


### Handling Errors

```
> (/ 1 0)
division by zero error
>
```

Inspect last error:

```
(last-error)
```


### Resetting and Exiting

Reset to top-level:

```
(reset)
```

Exit REPL:

```
(exit)
```
or press **Ctrl+D**.


### Running External Commands

```
(! "ls -l")
```


### Practical Tips

- Keep functions in `.rbl` files and reload them during development.
- Use `.init.rbl` for personal configuration.
- Use `rebel -q` for scripting-only mode.
- Use `trace` sparingly; it is verbose.
- For persistent data, use `write-file` or `save`.


### Summary

Rebel’s execution model is straightforward:

- run scripts: `rebel script.rbl`  
- run without entering REPL: `rebel -q script.rbl` or call `(exit)`  
- start interactive REPL: `rebel`  
- view options: `rebel -h`  
- configure startup in `~/.init.rbl`  

Everything remains minimal and predictable.


## The Basics

Rebel uses a simple and uniform syntax based on classic Lisp
s-expressions.  Everything in the language is built from the
same structural idea:

    (operator arg-1 arg-2 ...)

The first element specifies what to do.  The remaining
elements are arguments, which may be numbers, strings,
lists, symbols, or nested expressions.

This gives Rebel a single core representation for both code
and data.


### Evaluation

Rebel evaluates expressions using a predictable rule:

1) evaluate the first element  
2) this must produce a function or primitive  
3) evaluate all remaining elements  
4) call the function with the evaluated arguments

Example:

```
(+ 1 2) ; -> 3
```

There are no hidden rules, no operator precedence, and no
special syntax beyond a few core forms.


### Symbols

A symbol is a named value:

```
(set 'x 10)
```

The symbol now refers to the number 10:

```
(+ x 5) ; -> 15
```

Symbols can store any kind of value: numbers, strings,
lists, functions, contexts, or even other symbols.


### Quote

Without quoting, every list is treated as code.  
Quoting prevents evaluation:

```
'(+ 1 2)
```

This returns the list literally, without executing it.  
The long form is:

```
(quote (+ 1 2))
```


### Lists

Lists are fundamental data structures.  
A literal list:

```
'(1 2 3)
```

Created at runtime:

```
(list 1 2 3)
```

### Basic operations:

```
(first '(a b c)) ; -> a
(rest  '(a b c)) ; -> (b c)
(cons 1 '(2 3))  ; -> (1 2 3)
(append '(1 2) '(3 4)) ; -> (1 2 3 4)
```

Lists may contain mixed types:

```
'(1 "x" (2 3) nil)
```


### Strings

Strings use double quotes:

```
"hello world"
```

Strings behave like sequences:

```
(first "abc") ; -> "a"
(rest  "abc") ; -> "bc"
```

Many sequence functions work on both lists and strings.


### Functions

Functions are defined either with `define`:

```
(define (square x) (* x x))
```

Or explicitly using `lambda`:

```
(set 'square (lambda (x) (* x x)))
```

Calling a function:

```
(square 5) ; -> 25
```


### Nested Expressions

Expressions can be nested freely:

```
(+ (* 2 3) (* 4 5)) ; -> 26
```

Evaluation proceeds from the innermost expression outward.


### Conditionals

The basic conditional form is:

```
(if cond expr-true expr-false)
```

Example:

```
(if (> x 0)
    "positive"
    "non-positive")
```


### Blocks

Multiple expressions can be evaluated in sequence with `begin`:

```
(begin
  (set 'x 10)
  (set 'y 20)
  (+ x y)) ; -> 30
```

The value of the entire block is the value of its last expression.


### Dynamic Evaluation

Because code is represented as lists, it can be built and evaluated dynamically:

```
(set 'expr '(+ 1 2))
(eval expr) ; -> 3
```

### Destructive Functions

Some Rebel functions modify an existing value in place, while others return a
new value without touching the original.  Functions that update data directly
are called *destructive*.  Examples include operations that insert, remove, or
overwrite elements inside a list, string, or array.

When a function such as `push` or `replace` is described as destructive, it
means that the target object is changed immediately and the symbol referring to
it now points to the updated value.  Non-destructive functions leave the
original value untouched and instead produce a modified copy.

In practice this distinction matters when you rely on persistent data or when
multiple symbols point to the same structure.  Destructive updates affect all
references to that structure, while non-destructive ones do not.

### Comments

Comments begin with a semicolon or #, the semicolon is
prefered:

```
# this is a comment
; this is a comment
(set 'x 1) ; assigns x
```

### Summary

The core ideas of Rebel are:

- uniform s-expression syntax  
- simple evaluation rules  
- no distinction between code and data  
- minimal special forms  
- predictable structure  
- functional composition by nesting

This foundation keeps the language small, transparent, and
expressive.


## Flow Control

Flow control in Rebel is built from a small set of primitives that follow the
same evaluation rules as any other expression.  There is no special syntax and
no operator precedence.  Each form is a simple s-expression that directs the
execution of other expressions.

The key building blocks are conditional evaluation, branching, iteration, and
block construction.


### Conditionals: if, when, unless

The basic conditional is `if`, which evaluates one of two expressions depending
on the result of a test:

```
(if cond expr-then expr-else)
```

Example:

```
(if (> x 0)
    "positive"
    "non-positive")
```

`when` executes its body only when the condition is true:

```
(when (= x 1)
  (println "x is 1"))
```

`unless` is the inverse: it executes when the condition is false.

```
(unless (= x 0)
  (println "x is not zero"))
```


### Multi-branch: cond and case

`cond` selects the first clause whose condition is true:

```
(cond
  ((< x 0)  "negative")
  ((= x 0)  "zero")
  (true     "positive"))
```

`case` compares a value against multiple keys:

```
(case x
  (1 "one")
  (2 "two")
  (3 "three")
  (true "other"))
```


### Sequential Execution: begin and silent

`begin` evaluates its expressions in order and returns the value of the last
one:

```
(begin
  (set 'a 10)
  (set 'b 20)
  (+ a b)) ; -> 30
```

`silent` behaves like `begin` but suppresses console output.  
It is useful in REPL automation and scripts.


### Loops: for, dotimes, while, until, do-while, do-until

A simple counted loop:

```
(for (i 1 5)
  (println i))
```

`dotimes` iterates a fixed number of times:

```
(dotimes (i 3)
  (println i))
```

`while` repeats while the condition is true:

```
(set 'x 5)
(while (> x 0)
  (println x)
  (dec x))
```

`until` repeats until the condition becomes true:

```
(set 'x 0)
(until (= x 3)
  (println x)
  (inc x))
```

`do-while` and `do-until` always execute the body at least once:

```
(do-while
  (println x)
  (> x 0))
```

```
(do-until
  (println x)
  (= x 5))
```


### Iterators: doargs, dolist, dostring, dotree

Rebel provides iteration forms that work directly over structured data.

**Arguments of a function:**

```
(define (show-args)
  (doargs (x)
    (println "arg:" x)))

(show-args 10 "a" '(1 2))  ; prints each argument
```

**List iteration:**

```
(dolist (x '(10 20 30))
  (println x))
```

**String iteration (character-by-character):**

```
(dostring (ch "abc")
  (println ch))
```

**Context iteration:**

```
(dotree (sym MyContext)
  (println sym))
```


### Logical Flow: and, or, not

These forms evaluate left to right and stop as soon as the result is known.

```
(and true 1)       ; -> 1
(and nil  x y)     ; -> nil
(or  nil "x")      ; -> "x"
(not true)         ; -> nil
```


### Error Control: catch and throw

`catch` evaluates an expression and intercepts errors or explicit `throw`:

```
(catch
  (begin
    (/ 1 0))
  "division error")
```

Explicit non-local exits:

```
(catch
  (throw 99 "fail")
  "unused") ; never reached
```

The return value of `throw` becomes the result of the enclosing `catch`.


### Summary

Flow control in Rebel is minimal but expressive:

- if / when / unless for basic branching  
- cond / case for multi-way dispatch  
- begin / silent for sequencing  
- for / dotimes / while / until / do-* for loops  
- dolist / dostring / doargs / dotree for iteration over structures  
- and / or / not for logical flow  
- catch / throw for error handling and non-local exits

This minimal set covers both simple scripts and complex recursive programs with
uniform, predictable behavior.

## Lists

Lists are the core data structure used throughout Rebel.
They provide a flexible way to represent sequences of
values, program structure, trees, and arbitrary composite
objects.  A list is written as a parenthesized sequence of
elements:

```
'(a b c)
```

Rebel treats lists uniformly—there is no difference between
a list that represents data and a list that represents code.
Evaluation rules determine whether a list is executed or
returned as a literal value.


### Creating Lists

Lists can be created in two main ways:

Literal (quoted):

```
'(1 2 3)
```

Constructed at runtime:

```
(list 1 2 3)
```

A list may contain mixed types:

```
'(1 "x" (2 3) nil)
```


### Accessing Elements

Basic selectors:

```
(first '(a b c)) ; -> a
(rest  '(a b c)) ; -> (b c)
```

These operations return new values and do not modify the
original list.


### Building Lists

Prepending using `cons`:

```
(cons 'x '(y z)) ; -> (x y z)
```

Appending two lists:

```
(append '(1 2) '(3 4)) ; -> (1 2 3 4)
```


### Length and Indexing

Length of a list:

```
(length '(a b c d)) ; -> 4
```

Access by zero-based index:

```
(nth 2 '(a b c d)) ; -> c
```


### Modifying Lists

Some operations update the list in place.  
These are destructive:

- push
- pop
- replace
- swap

Example:

```
(set 'lst '(1 2 3))
(push 'x lst)  ; lst becomes (x 1 2 3)
(pop lst)      ; returns x, lst becomes (1 2 3)
```

Destructive operations affect all symbols referencing the
same list.  Non-destructive functions return new lists
without altering the original.


### Nested Lists and Trees

Lists naturally form tree structures:

```
'(a (b (c d)) e)
```

Useful functions for nested lists:

- flat       — flatten nested structure
- ref        — retrieve an element by a path of indices
- ref-all    — retrieve all paths matching a value
- replace    — substitute values inside nested lists
- select     — extract subsets of elements
- explode    — convert strings into lists of characters

These allow lists to act as flexible containers for
hierarchical data.


## Strings

Strings in Rebel are sequences of characters enclosed in
double quotes.  They behave much like lists: most sequence
functions apply to both types without special cases.  This
keeps the language uniform and predictable.

A string literal is written as:

```
"hello world"
```

Strings are immutable as values, but operations that update
a symbol holding a string are considered destructive—Rebel
replaces the stored value with a modified copy.


### String as a Sequence

Many core sequence operations work on strings exactly as
they do on lists:

```
(first "abc") ; -> "a"
(rest  "abc") ; -> "bc"
(length "abc") ; -> 3
(nth 1 "xyz") ; -> "y"
```

This uniformity means you can reason about strings and lists
using the same mental model.


### Concatenation

Strings can be joined using `append`:

```
(append "ab" "cd") ; -> "abcd"
```

Any number of arguments may be concatenated.


### Slicing and Substrings

Use `slice` to extract parts of a string:

```
(slice "abcdef" 1 3) ; -> "bcd"
(slice "abcdef" 3)   ; -> "def"
```

The function never modifies the original string.


### Character Access and Iteration

`nth` returns the character at a given position:

```
(nth 2 "hello") ; -> "l"
```

Characters themselves are strings of length one.

To traverse a string character by character:

```
(dostring (ch "abc")
  (println ch))
```


### Building and Transforming Strings

Rebel includes several helpers:

- `extend`   — append characters or another string
- `replace`  — substitute characters or substrings
- `reverse`  — reverse the order of characters
- `rotate`   — rotate characters by an offset
- `trim`     — remove whitespace
- `upper-case`, `lower-case`, `title-case` — character transformations
- `explode`  — split string into list of characters
- `join`     — join list of strings into one string

Examples:

```
(explode "abc") ; -> ("a" "b" "c")
(join '("a" "b" "c")) ; -> "abc"
(reverse "abc") ; -> "cba"
```


### Parsing

Strings can be processed into tokens using `parse`:

```
(parse "a,b,c" ",") ; -> ("a" "b" "c")
```

Regular expressions are available with `regex` and
`regex-comp` for more advanced matching and extraction.


### Destructive String Updates

Some operations rewrite the value stored in a symbol:

```
(set 's "abc")
(push "x" s)  ; s becomes "xabc"
(pop s)       ; removes first character, returns "x"
```

These updates replace the string bound to `s`.  If other
symbols reference the same string, they will observe the
updated value as well.


### Summary

Strings in Rebel are:

- simple double-quoted sequences  
- compatible with most list-oriented functions  
- easy to slice, search, and transform  
- capable of both destructive and non-destructive updates  
- tightly integrated with sequence operations like append, first, rest, nth

This gives strings a consistent role in the language without
requiring separate string-specific mechanisms.


## Arrays

Arrays in Rebel are fixed-size, multi-dimensional sequences.
They provide structured, index-based storage similar to
lists, but with predictable layout and efficient element
access.  An array may have one or more dimensions, each with
a positive integer size.

Arrays are created with the `array` function:

```
(array 3)       ; one-dimensional array of length 3
(array 2 2)     ; two-dimensional 2×2 array
(array 2 3 4)   ; three-dimensional array
```

Every array element is initialized to `nil` unless
explicitly set.


### Accessing Elements

Use `nth` to access elements by index.  
Indexing is zero-based:

```
(set 'a (array 2 2))
(nth 0 a)         ; -> (nil nil)
(nth 1 a)         ; -> (nil nil)
```

To reach nested elements, call `nth` repeatedly:

```
(set 'm (array 2 2))
(setf (nth 0 m) '(1 2))
(setf (nth 1 m) '(3 4))
(nth 1 (nth 0 m)) ; -> 2
```


### Modifying Elements

Arrays are mutable.  
You can update an element using `setf` and `nth`:

```
(set 'a (array 3))
(setf (nth 1 a) "x")
a  ; -> (nil "x" nil)
```

For nested dimensions:

```
(set 'm (array 2 2))
(setf (nth 1 (nth 0 m)) 42)
m ; -> ((nil 42) (nil nil))
```


### Array Size and Shape

The size of an array is given by its dimensions.  Each
dimension has a fixed size specified at creation time.

```
(length (array 4))     ; -> 4
(length (array 2 3))   ; -> 2
```

To inspect the full structure, convert to a list:

```
(array-list (array 2 2)) ; -> ((nil nil) (nil nil))
```


### Conversion Between Arrays and Lists

`array-list` converts an array to a list representation:

```
(set 'a (array 2 2))
(array-list a) ; -> ((nil nil) (nil nil))
```

Lists can be inserted into arrays using `setf`:

```
(set 'a (array 3))
(setf (nth 0 a) '(x y z))
```

Be aware that improper shapes may cause errors if the
inserted value does not match the expected structure.


### Multi-dimensional Usage

Arrays support nested indexing naturally, allowing you to
model matrices, tables, grids, or multi-axis data layouts.

A simple 2×2 matrix:

```
(set 'm (array 2 2))
(setf (nth 0 m) '(1 2))
(setf (nth 1 m) '(3 4))
m ; -> ((1 2) (3 4))
```

You can implement matrix operations using built-ins like
`transpose`, `multiply`, `det`, and `invert`.


### Destructive vs Non-destructive

Most operations on arrays are destructive: the array
structure is updated in place when you use `setf`.
Non-destructive functions produce lists or new arrays,
leaving the original untouched.

Understanding this distinction is important when multiple
symbols reference the same array.


### Summary

Arrays provide:

- fixed-size, multi-dimensional structure  
- efficient access by index  
- compatibility with list-like operations via `array-list`  
- built-in numeric and matrix utilities  
- clear destructive update semantics through `setf` and `nth`

They complement lists by offering predictable shape and efficient nested
indexing.


## Functions

Functions are central to Rebel.  They define reusable
computations, encapsulate logic, and allow code to be
structured into clear, composable units.  Functions in Rebel
are first-class values: they can be stored in variables,
passed as arguments, and returned from other functions.

A function call is simply a list whose first element
evaluates to a function, and the remaining elements are its
arguments:

```
(+ 3 4)
```

This means that both built-in primitives and user-defined
procedures share the same calling syntax.


### Defining Functions

A function is introduced with `define`:

```
(define (square x)
  (* x x))
```

This creates a symbol named `square` bound to a lambda
expression.  Calling the function:

```
(square 5) ; -> 25
```

You can also use `lambda` directly:

```
(set 'double (lambda (x) (* 2 x)))
(double 10) ; -> 20
```


### Parameters and Arguments

Function parameters are symbols that receive the evaluated
arguments.  Rebel performs normal argument evaluation before
calling the function, unless the form is a macro.

Multiple parameters:

```
(define (add3 a b c)
  (+ a b c))

(add3 1 2 3) ; -> 6
```

A function may also accept no arguments:

```
(define (hello)
  "hi")
(hello) ; -> "hi"
```


### Local Variables: let, letn, letex, local

`let` introduces temporary bindings visible only within its
body:

```
(let ((x 10) (y 20))
  (+ x y)) ; -> 30
```

`local` declares symbols as local within the current function:

```
(define (demo)
  (local (a b))
  (set 'a 1)
  (set 'b 2)
  (+ a b))
```

`letex` expands bindings directly into the expression before
evaluation, useful for building generated code.

`letn` is similar to nested `let` blocks, allowing
sequential initialization.


### Returning Values

A function returns the value of its last expression.  
There is no explicit `return` keyword:

```
(define (max2 a b)
  (if (> a b) a b))
```

Caller:

```
(max2 7 5) ; -> 7
```


### Higher-Order Functions

Because functions are values, they can be passed to other
functions.

Example:

```
(define (apply-twice f x)
  (f (f x)))

(apply-twice square 2) ; -> 16
```

This enables powerful composition patterns without extra
syntax.

Rebel includes built-ins such as:

- `map`       ; apply function to each element of a list  
- `filter`    ; keep only elements satisfying a predicate  
- `exists`    ; check if any element matches a condition  
- `for-all`   ; check if all elements match a condition


### Closures

Functions capture the environment in which they are defined.
This mechanism is called a closure:

```
(define (make-counter)
  (let ((x 0))
    (lambda ()
      (set 'x (+ x 1))
      x)))

(set 'c (make-counter))
(c) ; -> 1
(c) ; -> 2
```

The inner lambda remembers the value of `x` even after
`make-counter` finishes.


### Anonymous Functions

Any `lambda` expression can be used inline without naming
it:

```
((lambda (x y) (+ x y)) 3 4) ; -> 7
```

Anonymous functions are common in mapping, filtering, or
callback-style code.


### curry

`curry` transforms a function of two arguments into a
function of one argument with the first value fixed:

```
(define add3 (curry + 3))
(add3 10) ; -> 13
```

This is convenient for building specialized operations from
general-purpose ones.


### apply

`apply` calls a function using a list of arguments.  
Useful when arguments are constructed dynamically:

```
(apply + '(1 2 3 4)) ; -> 10
```

You can apply both primitive functions and user-defined
lambdas.


### Function Introspection

Functions are data, so their structure can be inspected:

```
(args square) ; returns parameter list
(source square) ; returns code needed to reproduce it
```

This is part of Rebel’s reflective capabilities.


### Summary

Functions in Rebel are:

- first-class values  
- defined with `define` or `lambda`  
- called with uniform s-expression syntax  
- capable of capturing local environments (closures)  
- composable through higher-order patterns  
- fully introspectable using built-in tools

This minimal and consistent model enables concise,
expressive programs with few syntactic rules.


## Evaluation Model

Rebel uses a simple and uniform evaluation model based on
classic Lisp semantics.  Every expression is an
s-expression, and the rules that determine how an expression
is evaluated are consistent across the entire language.

Understanding these rules is essential, because the behavior
of the whole language — functions, macros, flow control,
closures, contexts — follows directly from them.


### What an Expression Is

An expression is either:

- an atom (number, string, symbol, nil, true)
- a list: (operator arg-1 arg-2 ...)

Atoms evaluate to themselves, except symbols, which evaluate
to the value they are bound to.

Lists follow the function call pattern:

1) evaluate the first element  
2) evaluate all arguments  
3) call the resulting function with those arguments


### Symbol Evaluation

A symbol evaluates to whatever value it holds:

```
(set 'x 10)
x         ; -> 10
```

If a symbol has no value, an error is raised.

Quoted symbols do not evaluate:

```
'x         ; -> x
```


### Quoting and Literal Data

Quoting suppresses evaluation entirely.  This is how literal
lists and expressions are written:

```
'(+ 1 2) ; -> (+ 1 2)
```

Without quote, the same expression would be treated as a
function call.

The full form:

```
(quote (+ 1 2))
```


### List Evaluation

A list is treated as a function call unless quoted.

```
(+ 3 4)
```

The evaluation process:

- `+` is evaluated → the built-in addition function
- arguments 3 and 4 evaluate to themselves
- the function is called with arguments → 7


### Special Forms

Some symbols evaluate differently and control evaluation of
their arguments.  These include:

- `if`
- `cond`
- `define`
- `lambda`
- `begin`
- `let`, `letex`, `letn`
- `quote`

Special forms decide which subexpressions are evaluated and
which are not.  For example, `if` evaluates only the
selected branch:

```
(if true 1 (/ 1 0)) ; -> 1  ; division never happens
```


### Macros and Reader-Level Expansion

Macros operate on the *unevaluated* structure of
expressions.  A macro receives raw lists and symbols and
returns a transformed expression that is later evaluated
normally.

```
(define-macro (inc! x)
  (list 'set x (list '+ x 1)))

(set 'n 5)
(inc! n)  ; expands to (set 'n (+ n 1))
n         ; -> 6
```

This mechanism allows the language to be extended without
changing the interpreter.


### Eval

`eval` takes any expression and evaluates it according to
the same rules used for ordinary code:

```
(set 'e '(+ 1 2))
(eval e) ; -> 3
```

Because code is represented as lists, it can be constructed,
transformed, and executed at runtime.


### Building Expressions Programmatically

Expressions can be created by assembling lists:

```
(set 'x 10)
(set 'expr (list '+ x 5))
(eval expr) ; -> 15
```

This unifies data transformation and code generation.


### Function Evaluation and Closures

Functions evaluate their bodies in the environment where
they were defined.

```
(define (make-adder n)
  (lambda (x) (+ x n)))

(set 'add5 (make-adder 5))
(add5 10) ; -> 15
```

The inner lambda keeps access to `n` even after `make-adder`
returns.  This is the closure mechanism and it is part of
the evaluation model.

### Error Propagation

Errors during evaluation are thrown upward until:

- a `catch` form intercepts them, or
- evaluation reaches the top level, where they are printed

```
(catch
  (/ 1 0)
  "division error")
```

The return value is the value of the handler expression.


### Summary

Rebel’s evaluation model is:

- simple and uniform  
- based entirely on s-expression structure  
- predictable due to explicit evaluation rules  
- flexible because code and data share one representation  
- extensible through macros and programmatic construction  
- expressive through closures and higher-order functions

This model is the foundation of the entire language and remains consistent
across all constructs.


## Contexts

Contexts in Rebel are lightweight namespaces.  They group
related symbols together and prevent name collisions between
different parts of a program.  A context acts as a container
for symbols, functions, and even subcontexts.

Contexts behave like associative tables: each symbol inside
a context has its own binding independent from symbols
elsewhere.  This provides isolation without complexity.


### Creating and Switching Contexts

A context is created implicitly when referenced, or
explicitly with `context`:

```
(context 'Math)
```

Switching into a context makes it the default namespace for
symbol creation:

```
(context 'Math)
(set 'pi 3.14159)
```

Here, the symbol `pi` belongs to the `Math` context.


### Accessing Symbols in a Context

To refer to a symbol inside a context without switching, use
prefix notation:

```
Math:pi
```

This reads “symbol `pi` inside context `Math`”.

You can also assign this way:

```
(set 'Math:e 2.71828)
```

Now the context contains two symbols: `pi` and `e`.


### Nested Contexts

Contexts may contain other contexts:

```
(context 'Geometry)
(set 'Geometry:circle (context 'Geometry:circle))
```

Subcontexts follow the same rules:

```
(set 'Geometry:circle:radius 10)
```


### Defining Functions in Contexts

Functions can also be namespaced:

```
(context 'Tools)

(define (Tools:add2 x)
  (+ x 2))

(Tools:add2 10) ; -> 12
```

Switching into a context automatically defines symbols
inside it:

```
(context 'Tools)
(define (mul2 x) (* x 2))

(Tools:mul2 5) ; -> 10
```


### Copying and Exporting Symbols

`def-new` copies a symbol from one context to another:

```
(context 'A)
(set 'A:value 10)

(context 'B)
(def-new 'B:value 'A:value)
B:value ; -> 10
```

This is useful when building modules or exposing parts of a
library.


### Retrieving Symbol Lists

You can list all symbols inside a context using `symbols`:

```
(context 'Net)
(set 'Net:port 80)
(set 'Net:host "localhost")

(symbols 'Net)
; -> (Net:port Net:host)
```


### Using Contexts as Modules

Contexts naturally serve as modules.  
A typical pattern:

```
(context 'Math)

(define (Math:sum lst)
  (apply + lst))

(define (Math:avg lst)
  (/ (Math:sum lst) (length lst)))

(Math:avg '(10 20 30)) ; -> 20
```

This makes dependencies explicit and prevents accidental
name shadowing.


### Default Context: MAIN

Every program starts in the context `MAIN`, which holds all
global symbols not assigned elsewhere.  Switching contexts
does not erase MAIN; it remains accessible:

```
MAIN:x
```

If the name exists in the current context, that binding
takes precedence.


### Context Prefixing

`prefix` returns the full context-qualified name of a
symbol:

```
(prefix 'Math:pi) ; -> "Math:pi"
```

This is helpful for building tools and implementing
introspection.


### Summary

Contexts in Rebel provide:

- lightweight namespaces  
- clear separation of symbols  
- safe modular organization  
- nested namespace hierarchies  
- simple prefix-based access  
- integration with define, lambda, and all built-ins

They avoid the complexity of heavy module systems while
preserving clarity in large programs.


## FOOP (Functional Object Orientation)

FOOP is Rebel’s minimal object system built entirely from
contexts and functions.  There are no classes, inheritance
chains, or special syntax—FOOP uses the existing evaluation
model and namespaces to provide object-like behavior.

A FOOP object is simply a context that contains:

- data fields (symbols holding values)
- methods (functions whose first symbol is `self`)
- optional nested objects or subcontexts

This approach keeps the implementation small while allowing
structured, encapsulated data.


### Creating an Object

An object is just a context:

```
(context 'Point)
```

Assign fields directly into it:

```
(set 'Point:x 0)
(set 'Point:y 0)
```

Methods are functions stored inside the context:

```
(define (Point:move dx dy)
  (set 'Point:x (+ Point:x dx))
  (set 'Point:y (+ Point:y dy)))
```


### self

Inside a FOOP method, `self` is bound to the target object.
It allows methods to refer to the context they belong to,
even if the method is invoked via another symbol.

Example of a method using `self`:

```
(define (Point:reset)
  (set 'self:x 0)
  (set 'self:y 0))
```

`self:x` means “the symbol `x` inside the current object
context”.


### Creating Multiple Instances

To make multiple instances, copy an existing context:

```
(set 'A (new Point))
(set 'B (new Point))
```

Each instance has its own fields and methods.  
Updating one does not modify the other:

```
(set 'A:x 10)
(Point:x) ; -> 0
A:x       ; -> 10
B:x       ; -> 0
```


### Methods and Message Passing

Calling a method is simply calling a namespaced function:

```
(Point:move 3 4)
(Point:x) ; -> 3
(Point:y) ; -> 4
```

Instances behave the same way:

```
(A:move 2 2)
(A:x) ; -> 12
(A:y) ; -> 2
```

The FOOP system works because the first symbol of a function
name determines the context, and thus the object.


### Encapsulation Through Namespacing

All fields and methods of an object live inside its context.
Nothing is special-cased—the value of `A:x` is a normal
symbol, not a hidden slot or member variable.

This means encapsulation is maintained by discipline and
naming, not by restrictions in the language.


### Initializers and Constructors

You can simulate constructors by defining a function that
builds and returns a new object context:

```
(define (make-point x y)
  (let ((obj (new Point)))
    (set 'obj:x x)
    (set 'obj:y y)
    obj))

(set 'P (make-point 5 7))
(P:x) ; -> 5
(P:y) ; -> 7
```

This pattern is simple and powerful.


### Methods Operating on Nested Objects

Because contexts may contain subcontexts, FOOP naturally
represents hierarchical objects:

```
(context 'Rect)
(set 'Rect:pos (new Point))
(set 'Rect:size '(10 20))

(define (Rect:move dx dy)
  (Rect:pos:move dx dy))
```

Objects can delegate functionality to their subcomponents.


### Polymorphism by Context Replacement

Rebel does not enforce static types.  If two contexts
implement the same method names, they can be used
interchangeably:

```
(context 'Dog)
(define (Dog:speak) "woof")

(context 'Cat)
(define (Cat:speak) "meow")

(map (lambda (obj) (obj:speak)) (list (new Dog) (new Cat)))
; -> ("woof" "meow")
```

This is functional polymorphism through namespacing.


### Summary

FOOP provides:

- object-like structures built from contexts  
- per-object fields and methods  
- dynamic dispatch based on context prefix  
- closures and functions as core implementation  
- easy creation of multiple instances  
- simple polymorphism through shared method names  

It avoids the complexity of traditional OO systems while
giving Rebel a practical way to organize data and behavior.


## Sequences

Sequences in Rebel are a unified concept that covers
**lists**, **strings**, and **arrays**.  They all share a
common set of operations—indexing, slicing, appending,
searching, transformation—so that you can work with them
using the same mental model.

This uniformity is one of Rebel’s strongest features.  Most
sequence functions do not care what specific type they
operate on.


### A Unified Model

A sequence is any ordered collection of elements accessible by index:

- lists: variable-length, heterogeneous
- strings: immutable sequences of characters
- arrays: fixed-size, multi-dimensional blocks

Even though these structures differ internally, sequence
operations apply to all of them consistently.


### Basic Access

Common operations across all sequences:

```
(first '(a b c))  ; -> a
(first "abc")     ; -> "a"
(first (array 3)) ; -> nil
```

```
(rest '(a b c))   ; -> (b c)
(rest "abc")      ; -> "bc"
```

```
(nth 1 '(x y z))  ; -> y
(nth 2 "hello")   ; -> "l"
```

`length` returns the number of top-level elements:

```
(length '(1 2 3))  ; -> 3
(length "abc")     ; -> 3
(length (array 4)) ; -> 4
```


### Building and Combining Sequences

Sequences can be extended or combined:

```
(append '(1 2) '(3 4)) ; -> (1 2 3 4)
(append "ab" "cd")     ; -> "abcd"
```

`extend` appends to an existing sequence (destructive when
applied to a symbol):

```
(set 's "ab")
(extend s "c")
s ; -> "abc"
```


### Slicing

`slice` extracts a contiguous portion:

```
(slice '(1 2 3 4) 1 2) ; -> (2 3)
(slice "abcdef" 2 3)   ; -> "cde"
```

When the start index is omitted:

```
(slice "abcdef" 3) ; -> "def"
```


### Transformations

Common transformations work on all sequence types:

- `reverse`  
- `rotate`  
- `flat`  
- `unique`  

Examples:

```
(reverse '(a b c)) ; -> (c b a)
(reverse "abc")    ; -> "cba"
```


### Searching

Rebel provides a set of search functions:

```
(find 3 '(1 2 3 4)) ; -> 2
(find "b" "abc")    ; -> 1
```

Prefix and suffix tests:

```
(starts-with '(a b c) '(a)) ; -> true
(ends-with "hello" "lo")    ; -> true
```

`member` checks for membership:

```
(member 'b '(a b c)) ; -> true
```


### Filtering and Selection

`filter` keeps elements that satisfy a predicate:

```
(filter (lambda (x) (> x 2)) '(1 2 3 4)) ; -> (3 4)
```

`select` can reorder or extract specific indices:

```
(select '(2 0) '(a b c)) ; -> (c a)
```


### Conversion Between Types

Convert a string to a list of characters:

```
(explode "abc") ; -> ("a" "b" "c")
```

Join strings together:

```
(join '("x" "y" "z")) ; -> "xyz"
```

Convert arrays to lists:

```
(array-list (array 2 2)) ; -> ((nil nil) (nil nil))
```


### Nested Sequences

Sequences can be nested into trees:

```
(set 't '(a (b (c d)) e))
```

Retrieve values using paths:

```
(ref t '(1 1)) ; -> c
(ref-all t 'c) ; -> ((1 1))
```

Modify nested structures:

```
(replace t 'c 'X) ; -> (a (b (X d)) e)
```

### Destructive vs Non-destructive

Some sequence functions update an existing structure:

- push
- pop
- extend
- replace
- swap

Others produce new sequences:

- append
- slice
- reverse
- unique
- flat

This is important when multiple symbols reference the same sequence.


### Summary

Sequences provide:

- a unified interface across lists, strings, and arrays  
- consistent access (first, rest, nth)  
- structural manipulation (append, slice, extend, replace)  
- searching and filtering (find, filter, member)  
- type conversion utilities (explode, join, array-list)  
- nested sequence traversal (ref, ref-all)  
- destructive and non-destructive modes depending on the function

This unified model simplifies reasoning and makes Rebel’s
core data structures behave coherently across different
contexts.


## Pattern Matching

Pattern matching in Rebel provides a declarative way to
inspect, compare, and extract structure from lists, strings,
and nested sequences.  Unlike conditional chains or manual
traversal, pattern matching expresses the shape of data
directly and lets the matcher verify or decompose it.

Rebel includes several pattern-oriented functions:

- match
- find / find-all
- starts-with / ends-with
- ref / ref-all
- replace
- unify

These tools cover both structural list matching and string
pattern searches.

### Structural Matching with match

`match` compares a pattern against a list or nested list
structure.  A pattern may include literals, symbols,
wildcards, and nested forms.

Basic example:

```
(match '(a b c) '(a b c)) ; -> true
```

Patterns may contain symbols which bind to matched values:

```
(match '(a x c) '(a b c)) ; x becomes b  -> true
```

Wildcard `_` matches any value without binding:

```
(match '(a _ c) '(a 99 c)) ; -> true
```


### Nested Patterns

Patterns can describe nested structures:

```
(match '(a (b x) y) '(a (b 10) 20)) ; x -> 10, y -> 20
```

If the structure differs, the match fails:

```
(match '(a (b x)) '(a (c 5))) ; -> nil
```


### Binding and Side Effects

When a pattern uses ordinary symbols, they become bound to
the matching values:

```
(match '(x y) '(1 2))
x ; -> 1
y ; -> 2
```

Only symbols appearing in the pattern are affected.


### String Pattern Matching

Strings use a different mechanism centered on search
functions.

`find` locates a substring:

```
(find "b" "abc") ; -> 1
(find "x" "abc") ; -> nil
```

`find-all` returns all matching positions:

```
(find-all "l" "hello") ; -> (2 3)
```

These functions operate by literal substring comparison, not
structural patterns.


### Prefix and Suffix Testing

Two simple helpers check boundaries:

```
(starts-with "hello" "he") ; -> true
(ends-with   "hello" "lo") ; -> true
```

These work on both strings and lists.


### ref and ref-all for Nested Access

`ref` retrieves the element at a given path in a nested
structure:

```
(ref '(a (b (c d))) '(1 1)) ; -> c
```

`ref-all` returns all paths matching a given value:

```
(ref-all '(a (b (c a))) 'a)
; -> ((0) (1 1 1))
```


### replace for Pattern Substitution

`replace` rewrites values inside lists or strings.  
For lists:

```
(replace '(a b a) 'a 'x) ; -> (x b x)
```

For strings:

```
(replace "banana" "a" "x") ; -> "bxnxnx"
```


### unify for Logical Matching

`unify` tests whether two expressions can be made identical
by assigning values to variables (symbols).  It is a more
general and logical version of `match`.

```
(unify '(a x b) '(a 10 b)) ; x -> 10, returns true
(unify '(a x b) '(a y b))  ; assigns x=y, returns true
```

If there is a contradiction, unification fails:

```
(unify '(x x) '(1 2)) ; -> nil
```


### When to Use What

- **match**: structural list matching with binding  
- **find / find-all**: substring search  
- **starts-with / ends-with**: boundary tests  
- **ref / ref-all**: navigate nested structures  
- **replace**: pattern substitution in lists or strings  
- **unify**: logical equivalence with variable binding


### Summary

Pattern matching in Rebel enables:

- declarative structural checks  
- nested pattern decomposition  
- flexible variable binding  
- logical unification  
- sequence search and replacement tools  
- uniform syntax across lists and strings

This makes it easy to express data-shaped logic without
manual traversal or complex conditional code.


## I/O (Input and Output)

Rebel provides simple but powerful tools for reading and
writing data.  These functions cover file I/O, console
interaction, and binary operations.  The API is small,
uniform, and mirrors classic Unix streams.

Input/output in Rebel is synchronous and blocking: the
interpreter reads or writes data directly, without buffering
abstractions or async layers.  This keeps behavior
predictable and minimal.


### Opening and Closing Files

Files are opened using `open`, which returns a file handle:

```
(set 'f (open "data.txt" "read"))
```

Modes include:

- `"read"`
- `"write"`
- `"append"`
- `"read-write"`

Always close a file when done:

```
(close f)
```


### Reading Lines and Characters

`read-line` reads a line from a file or standard input:

```
(set 'f (open "data.txt" "read"))
(read-line f)
```

Reading characters:

```
(read-char f)
```

Reading UTF-8 characters:

```
(read-utf8 f)
```

Binary reads:

```
(read f 8)   ; read 8 bytes
```


### Writing Output

Writing to the console:

```
(print "hello")
(println "world")
```

Writing to a file:

```
(set 'f (open "out.txt" "write"))
(write-line f "hello")
(write f "raw-bytes")
(close f)
```

`device` changes the default output target.


### Whole-File Operations

To read or write an entire file at once:

```
(read-file "data.txt")
```

```
(write-file "out.txt" "contents")
```

`append-file` appends to an existing file:

```
(append-file "log.txt" "entry\n")
```


### peek

`peek` returns the number of pending bytes available on a
file descriptor without blocking:

```
(peek f) ; -> number of bytes ready
```

Useful for interactive streams and piped input.


### Keyboard Input

Reading a single key from the keyboard:

```
(read-key)
```

This does not wait for a newline, unlike read-line.


### Pipes and External Processes

You can execute external commands using `exec`:

```
(set 'p (exec "ls -l"))
(read-line p)
(close p)
```

`exec` opens a bidirectional pipe: you can both send to and
read from the process.


### Saving Data Structures

Objects and contexts can be stored using `save`:

```
(save 'MyData "backup.lsp")
```

This writes an s-expression representation, not a binary
format.


### Searching Inside Files

`search` scans a file for a substring:

```
(search "notes.txt" "keyword")
```

Returns the index of the first match or nil.


### seek

Use `seek` to reposition the file cursor:

```
(set 'f (open "big.bin" "read"))
(seek f 1000)
(read f 16) ; read 16 bytes starting from offset 1000
```


### Summary

Rebel’s I/O system provides:

- straightforward file open/close semantics  
- line, character, UTF-8, and binary reads  
- simple writing primitives  
- whole-file utilities for quick operations  
- external process execution via pipes  
- interactive keyboard input  
- stream introspection with peek  
- file searching and cursor control  

The model stays minimal and close to classic Unix behavior,
giving precise and predictable control over I/O.



## Processes

Rebel provides a small set of primitives for creating and
communicating with external processes.  The model is
synchronous and direct, offering simple access to classic
Unix process behavior: pipes, child processes, message
passing, and cleanup.

Rebel does not hide system-level concepts.  A process is a
real OS process, and communication uses actual file
descriptors or sockets.


### Running External Commands: exec

`exec` starts a child process and opens a bidirectional pipe
connected to it.  You can read output from the process or
write input to it.

```
(set 'p (exec "ls -l"))
(read-line p)   ; read one line from the command
(close p)
```

Anything written to the process goes to its stdin:

```
(set 'p (exec "cat"))
(write p "hello\n")
(read-line p)  ; -> "hello"
(close p)
```


### Unidirectional Pipes: pipe

`pipe` creates a raw pipe pair:

```
(set 'p (pipe))
(write (p 1) "hello")
(read (p 0) 5) ; -> "hello"
```

`p` is a two-element list:  
- `(p 0)` is the read end  
- `(p 1)` is the write end

Useful for custom IPC setups.


### Creating a New Interpreter: process

`process` launches a new Rebel interpreter as a child.
Input and output can be redirected:

```
(set 'c (process "rebel"))
(write-line c "(+ 2 3)")
(read-line c) ; -> "5"
(close c)
```

This allows embedding a separate evaluator or worker
process.


### Lightweight Parallelism: spawn and sync

`spawn` creates a child process to evaluate an expression
asynchronously.  The parent continues immediately.

```
(set 'job (spawn (begin (sleep 500) 42)))
```

`sync` waits for all spawned processes and returns their
results:

```
(sync) ; -> (42)
```

This gives Rebel a simple form of parallel execution using
OS processes.


### Terminating Processes: abort

`abort` stops a process created with `spawn`:

```
(set 'job (spawn (sleep 9999)))
(abort job)
```

The aborted job will not appear in `sync` results.


### Child Process Control: wait-pid

`wait-pid` allows explicit waiting on a child:

```
(wait-pid job)
```

It is useful when managing external processes launched via
`fork` or system tools rather than Rebel primitives.


### Message Passing: send and receive

Rebel includes message-based IPC between processes:

```
(spawn (receive) (println "child got:" (receive)))
(send (sys-info 4) "hello")
```

`send` sends a message to a process.  `receive` retrieves
the next message delivered to the current process.

This mechanism is simple but effective for building parallel
workers.


### Shared Memory: share

`share` exposes a memory region to multiple processes.  It
behaves like a global variable accessible across process
boundaries.

```
(share 'counter 0)
(spawn (set 'counter (+ counter 1)))
(spawn (set 'counter (+ counter 1)))
(sleep 50)
counter ; -> 2
```

Used carefully, it enables fast communication and state
passing.


### Destroying Processes: destroy

`destroy` terminates a process created with `fork` or `process`:

```
(set 'p (process "rebel"))
(destroy p)
```


### Forking

Rebel allows creating a raw OS child process using `fork`:

```
(set 'pid (fork))
(if (= pid 0)
    (println "child")
    (println "parent"))
```

The child has its own execution path and must eventually
exit.


### Summary

Process primitives in Rebel provide:

- external command execution (`exec`)
- bidirectional pipes and raw IPC (`pipe`)
- embedded child interpreters (`process`)
- asynchronous parallel execution (`spawn`, `sync`)
- inter-process messaging (`send`, `receive`)
- shared-memory variables (`share`)
- classic process operations (`fork`, `wait-pid`, `destroy`, `abort`)

The model stays close to Unix: explicit, minimal, and fully
under user control.


## Networking

Rebel provides direct access to TCP, UDP, and raw socket
communication.  The network API is thin and close to the
operating system, exposing socket creation, connection, data
transfer, and event polling.  This enables writing low-level
network tools without additional libraries.

Rebel does not hide networking details—ports, addresses, and
sockets are explicit objects.


### Connecting to a Remote Host: net-connect

`net-connect` opens a TCP connection:

```
(set 's (net-connect "example.com" 80))
(write-line s "GET / HTTP/1.0\n")
(read-line s)
(close s)
```

The returned value is a socket handle.


### Creating a Server: net-listen and net-accept

To create a listening socket:

```
(set 'server (net-listen 8080))
```

Accept a client connection:

```
(set 'client (net-accept server))
(write-line client "hello")
(close client)
```

This is a blocking call—execution waits for a client to
connect.


### Sending and Receiving Data

TCP send:

```
(net-send s "hello")
```

TCP receive:

```
(net-receive s 64) ; read up to 64 bytes
```

UDP send:

```
(net-send-udp "localhost" 9000 "ping")
```

UDP receive:

```
(net-receive-udp 9000)
```


### Checking Socket Status: net-peek and net-select

`net-peek` returns the number of bytes waiting to be read:

```
(net-peek s)
```

`net-select` checks one or more sockets for activity:

```
(net-select (list s1 s2) 1000) ; timeout 1000 ms
```


### Network Information and Tools

Lookup hostnames:

```
(net-lookup "8.8.8.8")
```

Retrieve local socket info:

```
(net-local s)
```

Get peer address:

```
(net-peer s)
```

Switch between IPv4 and IPv6:

```
(net-ipv 6)
```

Find a port by service name:

```
(net-service "http") ; -> 80
```


### Raw Packets

Rebel can construct and send raw IP packets:

```
(net-packet "eth0" some-buffer)
```:

This requires appropriate system permissions.


### Remote Evaluation: net-eval

`net-eval` sends an s-expression to a remote Rebel
process for evaluation.  This allows distributed computation
or remote administration.

Basic usage:

```
(net-eval "192.168.1.5" 4711 '(+ 1 2)) ; -> 3
```

Broadcast to multiple hosts:

```
(net-eval '("host1" "host2") 4711 '(now))
```

Local parallel evaluation:

```
(net-eval '(1 2 3 4) (lambda (x) (* x x)))
; -> (1 4 9 16)
```


### Closing Connections

Sockets must be closed when no longer needed:

```
(net-close s)
```

This frees the file descriptor.


### Error Handling

`net-error` returns the last network error:

```
(net-error)
```

Useful for debugging failed connections or invalid packets.


### Summary

Rebel’s networking API provides:

- direct TCP/UDP socket access  
- server/client construction  
- blocking I/O and event polling  
- hostname and service lookup  
- raw packet injection  
- remote evaluation capability  
- IPv4/IPv6 control  
- explicit resource management

The design follows Unix networking closely: nothing is
abstracted away, giving the programmer full control and
transparency.


## HTTP API

Rebel includes a compact HTTP utility layer for performing
basic web operations.  

These functions provide simple GET/POST/PUT/DELETE requests,
base64 helpers, and JSON/XML parsing.  The API is
synchronous and follows the Unix philosophy: send a request,
wait for the response, return plain data.

The HTTP API does not attempt to be a full browser or
session manager — it is intended for scripts, automation,
and lightweight integrations.


### GET Requests: get-url

Retrieve the contents of a URL:

```
(get-url "https://example.com")
```

The result is a string containing the full response body.
Headers are not provided; this keeps the interface minimal.


### POST Requests: post-url

Send form or structured data to a server:

```
(post-url "https://example.com/submit" "name=ufko&msg=hello")
```

The second argument is transmitted as the request body.  The
returned value is the server’s response body.


### PUT and DELETE: put-url and delete-url

`put-url` uploads or replaces a resource:

```
(put-url "https://example.com/update" "new content")
```

`delete-url` removes a resource:

```
(delete-url "https://example.com/remove")
```

Both return the response body if provided by the server.


### Base64 Encoding and Decoding

Utility functions for handling base64:

```
(base64-enc "hello") ; -> "aGVsbG8="
(base64-dec "aGVsbG8=") ; -> "hello"
```

Useful for HTTP payloads requiring binary or credential
encoding.


### JSON

Parse JSON into Rebel data structures:

```
(json-parse "{\"a\": 1, \"b\": [2,3]}") 
; -> (("a" 1) ("b" (2 3)))
```

If an error occurs, use `json-error` to inspect it:

```
(json-error)
```


### XML

Parse XML into nested lists:

```
(xml-parse "<root><x>10</x></root>")
```

If parsing fails:

```
(xml-error)
```

`xml-type-tags` controls tag and attribute formatting rules.


### Uploading and Handling Events

During long data transfers, HTTP utilities can emit progress
events.  You can register a handler:

```
(xfer-event (lambda (bytes) (println "transferred:" bytes)))
```

This is optional and used only for larger uploads or
downloads.


### Error Handling

Invalid URLs, network failures, incorrect content types, and
protocol errors return `nil`.  For debugging, use:

```
(net-error)
```

to inspect the underlying system-level error.


### Security Notes

The HTTP API provides no cookie store, authentication
helpers, or TLS inspection.  TLS support depends on the
underlying system libraries.  This is intentional — Rebel
leaves control to the user and avoids hidden state.


### Summary

The HTTP API offers:

- GET/POST/PUT/DELETE helpers  
- base64 encoding/decoding  
- JSON and XML parsing  
- transfer event hooks  
- raw string-based responses  
- simple synchronous model  

It is intentionally minimal, making HTTP scripting
straightforward without adding a complex client layer.


## Date and Time

Rebel provides a compact set of functions for working with
dates, timestamps, and elapsed time.  The API is simple and
intentionally limited: the goal is to give scripts access to
clock values, formatted strings, and conversions, not to
recreate a full calendar system.

Rebel measures time in **seconds since the Unix epoch**
(January 1, 1970), returned as integers or floats depending
on context.


### Current Time: now

`now` returns a list containing the current date and time
components:

```
(now)
; -> (year month day hour minute second)
```

The returned list is suitable for logging, timestamps, or
building custom formats.


### Formatting Time: date

`date` converts a timestamp into a human-readable string.  
If no argument is given, it formats the current time.

```
(date)                ; current time as string
(date 1700000000)     ; format specific timestamp
```

The exact format depends on the underlying system locale.


### Parsing Date Strings: date-parse

`date-parse` converts a date string into a Unix timestamp:

```
(date-parse "2025-02-15 12:30:00")
```

The accepted formats vary by platform but typically include
ISO-like representations.  If the string cannot be parsed,
the result is `nil`.


### Converting Components to Timestamp: date-value

`date-value` takes individual components and returns a timestamp:

```
(date-value 2025 1 1 0 0 0)
; -> seconds since epoch
```

This is the inverse of `now`: instead of extracting
components from a timestamp, you build a timestamp from
components.


### Elapsed Time: time

`time` measures the duration required to evaluate an
expression.  The result is milliseconds.

```
(time (sleep 200)) ; -> about 200
```

Useful for profiling or measuring performance
characteristics.


### Milliseconds Since Start of Day: time-of-day

`time-of-day` returns the number of milliseconds since
midnight:

```
(time-of-day)
```

This is often used for sampling, animations, periodic
events, or time-based calculations where only the current
day matters.


### Examples

Getting the current hour:

```
(nth 3 (now))
```

Time difference between two events:

```
(set 't1 (date-parse "2025-01-01 12:00:00"))
(set 't2 (date-parse "2025-01-01 14:00:00"))
(- t2 t1) ; -> 7200 seconds
```

Formatting a custom date:

```
(set 'ts (date-value 2030 12 24 18 0 0))
(date ts)
```


### Summary

Rebel’s date/time facilities provide:

- current local date and time via `now`  
- timestamp formatting with `date`  
- parsing date strings using `date-parse`  
- constructing timestamps with `date-value`  
- timing expressions with `time`  
- day-relative milliseconds via `time-of-day`  

The system is intentionally small and transparent, giving
scripts easy access to essential clock operations without
unnecessary complexity.


## Math and Numbers

Rebel includes a comprehensive set of numeric functions
covering integer arithmetic, floating-point operations,
logarithms, trigonometry, probability tools, special
functions, and random number generation.  The model is
simple: numbers evaluate to themselves, operations are
normal function calls, and all numeric values are immutable.

Rebel supports both integers and IEEE floating-point
numbers.  Most arithmetic functions accept either type and
return the most appropriate numeric form.


### Integer Arithmetic

Basic operations:

```
(+ 1 2) ; -> 3
(- 7 3) ; -> 4
(* 3 4) ; -> 12
(/ 10 2) ; -> 5
(% 10 3) ; -> 1
```

Comparison operators:

```
(< 2 5)   ; -> true
(>= 7 7)  ; -> true
(!= 3 4)  ; -> true
```

Increment/decrement operations:

```
(inc x)   ; increments symbol x
(dec x)   ; decrements symbol x
(++ 10)   ; -> 11  ; returns new value
(-- 10)   ; -> 9
```


### Floating-Point Operations

Many numeric functions automatically return floats:

```
(add 1 2.5) ; -> 3.5
(sub 5 2.2) ; -> 2.8
(mul 2 0.5) ; -> 1.0
(div 7 2)   ; -> 3.5
```

`flt` converts a number into its 32-bit float
representation, mainly for FFI.


### Exponentials and Logarithms

```
(exp 1)     ; -> 2.718281828
(log 10)    ; natural log
(log 100 10) ; log base 10
(pow 2 8)   ; -> 256
```


### Trigonometry

Standard trigonometric and hyperbolic functions:

```
(sin 0)     ; -> 0
(cos 0)     ; -> 1
(tan 0.5)
(asin 0.5)
(acos 0.4)
(atan 1)
(sinh 1)
(cosh 1)
(tanh 1)
(atan2 y x)
```


### Special Functions

Rebel includes a full suite of mathematical special
functions.

Gamma and Beta:

```
(gammaln 5)
(beta 1 2)
(betai 0.5 2 3)
```

Error function:

```
(erf 1)
```

Binomial and factorial-like tools:

```
(binomial 5 2) ; -> 10
(factor 84)    ; -> (2 2 3 7)
```


### Rounding and Sign

```
(floor 3.7) ; -> 3
(ceil  3.1) ; -> 4
(round 2.49) ; -> 2
(sgn -5)     ; -> -1
```

`inf?` tests for infinity, `NaN?` checks for invalid floats.


### Statistical Tools

A collection of functions useful for probability work:

```
(normal 5 2) ; generates a list of 5 normally-distributed floats
(stats '(1 2 3 4 5))
(corr '(1 2 3) '(2 4 6))
```

Critical distributions:

```
(crit-z 0.95)
(crit-t 10 0.95)
(crit-chi2 4 0.95)
```


### Random Numbers

`rand` generates integers:

```
(rand 10) ; -> integer in range [0,10)
```

`random` generates floating-point ranges:

```
(random 3) ; -> list of 3 floats in [0,1)
```

`randomize` shuffles elements of a list:

```
(randomize '(a b c d))
```

Seed control:

```
(seed 123)
```


### Modulo and GCD

```
(mod 10 3) ; -> 1
(gcd 24 60) ; -> 12
```


### Vector and Series Utilities

```
(sequence 1 5) ; -> (1 2 3 4 5)
(series 1.0 1.5 5) ; geometric or arithmetic progression
(ssq '(1 2 3)) ; sum of squares
```


### Summary

Rebel’s numeric system provides:

- full integer and floating-point arithmetic  
- logarithmic, exponential, and power functions  
- complete trigonometry and hyperbolic functions  
- advanced special functions (gamma, beta, erf, etc.)  
- statistical and probabilistic tools  
- random number generators and shuffling  
- rounding, sign, GCD, modulo operations  

The model remains minimal and consistent: numbers evaluate
to themselves, operations are explicit function calls, and
results follow clear numerical rules.


## Predicates

Predicates are functions that answer a yes/no question about a value.  
They return `true` when the condition holds, otherwise `nil`.  
Rebel relies heavily on predicates because flow-control constructs treat any
non-nil value as true.

Predicates cover type checks, numeric tests, emptiness, structural queries, and
special conditions like NaN or infinity.


### Type Predicates

These predicates check the fundamental type of a value:

```
(number? 10)        ; -> true
(integer? 10)       ; -> true
(float? 3.14)       ; -> true
(string? "x")       ; -> true
(list? '(1 2 3))    ; -> true
(array? (array 3))  ; -> true
(symbol? 'a)        ; -> true
(context? MAIN)     ; -> true
```

Some values satisfy multiple predicates (e.g., integers are also numbers).


### Numeric Predicates

Check numeric properties:

```
(even? 4) ; -> true
(odd? 5)  ; -> true
(zero? 0) ; -> true
(zero? 0.0) ; -> true
```

Sign and special cases:

```
(inf? (/ 1 0.0)) ; -> true
(NaN? (/ 0.0 0.0)) ; -> true
```


### Existence and Emptiness

```
(empty? '())   ; -> true
(empty? "")    ; -> true
(nil? nil)     ; -> true
(null? 0)      ; -> true  ; null? treats 0, "", (), nil, 0.0 as null
```

`true?` checks for any non-nil value:

```
(true? 5)     ; -> true
(true? nil)   ; -> nil
```


### Structural Predicates

Membership:

```
(member 'b '(a b c)) ; -> true
```

Lookup-style predicates:

```
(global? 'x)     ; checks if symbol is global
(protected? 'y)  ; symbol protection flag
```


### Symbol and Quoting Predicates

These detect quoting and symbolic state:

```
(quote? '(1 2 3)) ; -> true if explicitly quoted
(symbol? 'x)      ; -> true
```

`legal?` checks if a string can be turned into a symbol:

```
(legal? "abc")    ; -> true
(legal? "1abc")   ; -> nil
```


### File and Directory Predicates

```
(file? "data.txt")     ; -> true if file exists
(directory? "/tmp")    ; -> true if directory exists
```


### System-Level Predicates

Check platform or symbol characteristics:

```
(lambda? (lambda (x) x)) ; -> true
(macro?  some-macro)     ; -> true if macro
(primitive? +)           ; -> true
```

Test if a symbol has any binding at all:

```
(nil? (eval 'unknown)) ; error for unbound symbol
```

To avoid errors, combine with `global?` or store in lookup tables.


### Predicate Use in Flow Control

Predicates integrate naturally with `if`, `when`, `unless`, and loops:

```
(when (empty? lst)
  (println "list is empty"))
```

Any non-nil value counts as true; `nil` counts as false.


### Summary

Predicate functions in Rebel provide:

- type checks for numbers, lists, strings, arrays, symbols, contexts  
- numeric property checks (even?, odd?, zero?, inf?, NaN?)  
- structural checks (member, empty?, null?)  
- file and directory status  
- symbol introspection (lambda?, macro?, primitive?, global?, protected?)  
- quoting and legal symbol detection  

Predicates keep control flow simple and expressive, letting code declare its
intent with minimal ceremony.


## System Interface

Rebel exposes a compact system interface for interacting
with the underlying operating system.  These functions
provide access to environment variables, process
information, signal handling, system resources, and program
arguments.  The design is minimal: no layers, no wrappers —
direct access to OS behavior.

System functions are essential for scripting, automation,
and embedding Rebel in Unix workflows.


### Environment Variables: env

Read or set environment variables:

```
(env "HOME")          ; read
(env "MYVAR" "hello") ; set
```

To list all environment variables:

```
(env)
```


### Program Arguments: main-args

Fetch arguments passed to the Rebel interpreter:

```
(main-args)
; -> ("script.reb" "arg1" "arg2")
```

This is useful when writing executable scripts.


### Exiting the Interpreter: exit

Terminate execution with a return code:

```
(exit 0)
```

A non-zero value signals failure:

```
(exit 1)
```


### System Information: sys-info

`sys-info` retrieves platform and interpreter information.  
Selected fields include memory usage, system type, process ID, etc.

```
(sys-info)
```

Platform type only:

```
(ostype) ; -> string describing OS
```


### Error Handling: last-error and sys-error

`last-error` returns the last error number and message
raised by Rebel:

```
(last-error)
```

`sys-error` returns the most recent OS-level error:

```
(sys-error)
```


### Signals: signal

Install a custom handler for OS signals:

```
(signal 2 (lambda () (println "caught interrupt")))
```

For example, signal 2 is typically CTRL-C (SIGINT).  
A handler may return or terminate the program.


### Sleeping: sleep

Pause execution for a number of milliseconds:

```
(sleep 500) ; half second
```

This is useful for throttling, polling, or simple timing
logic.


### Default Functor: default

Retrieve or set the default functor in a context:

```
(default 'Math) ; returns default symbol of Math context
```

This is a specialized feature used mainly when building
DSL-like constructs.


### Printing and Formatting: pretty-print

Configure how lists and expressions are printed:

```
(pretty-print true)
```

Useful for debugging large nested structures.


### History and Tracing

`history` returns the call history of a function:

```
(history some-function)
```

`trace` enables tracing:

```
(trace true)
```

`trace-highlight` customizes colors or markers used during
tracing:

```
(trace-highlight "[[" "]]")
```


### Reading Expressions Without Evaluating: read-expr

Convert a string into an s-expression without evaluating it:

```
(read-expr "(+ 1 2)")
; -> (+ 1 2)
```


### Locale and Character Settings: set-locale

Change locale-dependent behavior:

```
(set-locale "en_US.UTF-8")
```

This affects number formatting, date output, and some string
operations.


### Resetting to Top Level: reset

Abort the current evaluation and return to the REPL top
level:

```
(reset)
```

Useful in interactive sessions.


### Summary

The System Interface provides:

- environment variable access (`env`)
- command-line argument retrieval (`main-args`)
- interpreter control (`exit`, `reset`)
- system/error inspection (`sys-info`, `last-error`, `sys-error`, `ostype`)
- signal handling (`signal`)
- timing utilities (`sleep`)
- printing and introspection tools (`pretty-print`, `trace`, `history`)
- safe expression parsing (`read-expr`)
- locale configuration (`set-locale`)

These functions connect Rebel to the operating system
directly, keeping scripts simple, transparent, and tightly
integrated with Unix behavior.


## Internals

Internals expose the low-level mechanisms of the Rebel
interpreter.  These functions operate below the normal
language layer and allow you to:

- intercept input before evaluation  
- rewrite expressions on the fly  
- customize the REPL  
- inspect or copy raw memory  
- work with binary structures  
- import C functions via shared libraries  

These tools are powerful and should be used carefully, as
they interact directly with the interpreter’s internal
state.


### Reader-Level Hooks

Rebel provides several hooks that run during expression
parsing and evaluation.


### reader-event

`reader-event` installs a callback that receives each
incoming expression *before* evaluation.  You can rewrite or
transform expressions here.

```
(reader-event (lambda (expr)
  (println "raw:" expr)
  expr))  ; return the expression unchanged
```

Returning a modified expression alters execution.  
Returning nil causes an empty evaluation.


### command-event

`command-event` processes command-line input when Rebel is
started with arguments or when handling HTTP script
execution.

```
(command-event (lambda (args)
  (println "command args:" args)
  args))
```

Typically used in CGI-like environments or tooling automation.


### prompt-event

Customize the interactive REPL prompt:

```
(prompt-event (lambda () "rebel> "))
```

The function must return a string.


### read-expr

Convert a string into an s-expression without evaluating it:

```
(read-expr "(+ 1 2)") ; -> (+ 1 2)
```

This is the safe alternative to `eval` when parsing external data.


### Memory Inspection

These functions operate on raw memory addresses.  
Use them only when absolutely necessary.


### cpymem

Copy bytes between memory locations:

```
(cpymem dest src 32)
```

This is mainly useful in FFI wrappers or binary manipulation.


### dump

Debug memory contents:

```
(dump some-value)
```

Used for inspecting internal cell layout.  Mostly relevant
to implementers or advanced debugging.


### Binary Packing

Rebel can serialize and deserialize binary structs.


### pack

Pack values according to a type specification:

```
(pack "iCf" 100 65 3.14)
```

Types follow a compact signature:  
`i`=int, `C`=char, `f`=float, etc.


### unpack

Reverse operation:

```
(unpack "iCf" buffer)
```

Useful for binary protocols and custom serialization
formats.


### struct

Define a reusable C-struct layout:

```
(struct Point (int x) (int y))
```

This creates pack/unpack helpers for the structure.


### Foreign Function Interface (FFI)

Rebel can dynamically import shared library functions and
call them directly.


### import

Import a function from a shared library:

```
(import "libm.so" "cos" "double" "double")
(cos 0.0)
```

Format:  
`(import library function return-type arg1-type arg2-type ...)`


### callback

Register a Rebel function as a C callback:

```
(define (cb x) (+ x 1))
(callback cb)
```

The returned pointer can be passed to a C function expecting
a callback.


### address, get-*, set-*

Retrieve raw memory addresses or interpret memory as typed
values:

```
(address "hello")
(get-int ptr)
(get-string ptr)
```

These enable low-level interop but require careful handling.


### Internals and Safety

Internal primitives bypass many of Rebel’s safety checks.
You can:

- rewrite evaluation results  
- bypass normal scoping  
- manipulate raw memory  
- construct invalid binary data  
- call arbitrary C functions  

Use them sparingly and isolate such code into small,
well-understood modules.


### Summary

Internals provide:

- expression interception (`reader-event`, `command-event`)  
- REPL customization (`prompt-event`)  
- safe parsing (`read-expr`)  
- raw memory tools (`cpymem`, `dump`, address getters)  
- binary pack/unpack facilities  
- struct definitions for binary layouts  
- full FFI support through `import` and `callback`  

These tools unlock Rebel’s low-level capabilities, bridging
high-level Lisp-like code with system and native C
functionality.


