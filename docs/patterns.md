Title: Rebel Patterns 
css: docs.css

# Rebel Patterns (Field Manual WIP)

---

Copyright (c) 2025 Ufko (ufko.org)

This manual is released under the MIT license.
You may use, copy, modify and distribute it freely,
provided that this notice is preserved.

---

## Table of Contents (Summary of Chapters)

TOC

## Writing Software in Modules

Modules are implemented with contexts. A context is a namespace
that isolates its own symbols, preventing name clashes across
different parts of a program. A common pattern is one context per
file, each file dedicated to a specific task.


### One context per file

```
; file: db.rbl
(context 'db)
;
(define (db:update x y z)
  ...)
;
(define (db:erase x y z)
  ...)
```

```
; file: util.rbl
(context 'util)
;
(define (util:get a b)
  ...)
```

Main module loads the others and coordinates them.

```
; file: app.rbl
(load "util.rbl")
(load "db.rbl")
;
(define (run)
  (db:update a b c)
  (util:get k v)
  ...)
;
(run)
```


### More than one context per file

Multiple contexts may live in one file. Close each context
section by switching back to MAIN.

```
; file: myapp.rbl
(context 'a)
;
(define (a:foo x)
  ...)
;
(context MAIN)
;
(context 'b)
;
(define (b:bar x)
  ...)
;
(context MAIN)
;
(define (main)
  (a:foo 1)
  (b:bar 2))
```

Alternative closing and opening in one step:

```
(context 'a)
;
(define (a:foo x)
  ...)
;
(context 'MAIN:b)
;
(define (b:bar x)
  ...)
;
(context 'MAIN)
;
(define (main)
  (a:foo 1)
  (b:bar 2))
```


### Default function

If a context defines a function with the same name as the
context, calling the context behaves like calling that function.
This allows compact stateful generators.

```
(context 'gen)
;
(define (gen:gen)
  (inc acc))
;
(context MAIN)
;
(gen) ;-> 1
(gen) ;-> 2
(gen) ;-> 3
```

Fibonacci generator:

```
(define (fib:fib)
  (if (not fib:mem)
      (set 'fib:mem '(0 1)))
  (last (push (+ (fib:mem -1) (fib:mem -2))
              fib:mem -1)))
;
(fib) ;-> 1
(fib) ;-> 2
(fib) ;-> 3
(fib) ;-> 5
(fib) ;-> 8
```

Explicit version:

```
(context 'fib)
;
(define (fib:fib)
  (if (not mem)
      (set 'mem '(0 1)))
  (last (push (+ (mem -1) (mem -2)) mem -1)))
;
(context MAIN)
;
(fib) ;-> 1
(fib) ;-> 2
(fib) ;-> 3
(fib) ;-> 5
(fib) ;-> 8
```


### Packaging data with contexts

A context can hold plain data. The default functor gives direct
access.

```
(set 'db:db '(a "b" (c d) 1 2 3 x y z))
;
(db 0) ;-> a
(db 1) ;-> "b"
(db 2 1) ;-> d
(db -1) ;-> z
(3 db) ;-> (1 2 3 x y z)
```


### Passing objects by reference

Passing a context symbol passes the underlying object by
reference, allowing efficient updates.

```
(define (upd data idx expr)
  (if (not (or (lambda? expr) (primitive? expr)))
      (setf (data idx) expr)
      (setf (data idx) (expr $it))))
;
(set 'db:db '(a "b" (c d) 1 2 3 x y z))
;
(upd db 0 99) ;-> a
db:db ;-> (99 "b" (c d) 1 2 3 x y z)
;
(upd db 1 upper-case) ;-> "b"
db:db ;-> (99 "B" (c d) 1 2 3 x y z)
;
(upd db 4 (fn (x) (mul 1.1 x))) ;-> 2.2
db:db ;-> (99 "B" (c d) 1 2.2 3 x y z)
```

Simple wrapper:

```
(define (pop-last data)
  (pop data -1))
;
(pop-last db) ;-> z
db:db ;-> (99 "B" (c d) 1 2.2 3 x y)
```

---


## Local Variables

Rebel provides several ways to work with local variables. Looping
forms such as `dolist`, `dotimes`, `dostring`, `doargs`, `dotree`,
and `for` automatically create a local loop variable that restores
its old value after the loop ends.

Functions like `let`, `letn`, `local`, and `letex` offer explicit
control over temporary bindings inside a block. All locals revert
to their previous values after the block exits.


### Locals in looping forms

Loop variables are always local to the loop body.

```
(dolist (x '(a b c))
  (println x))
;
x ;-> original value restored (or nil if unused)
```


### Locals in let and letn

`let` binds variables to values for the duration of its body.

```
(define (sum-sq a b)
  (let ((x (* a a)) (y (* b b)))
    (+ x y)))
;
(sum-sq 3 4) ;-> 25
```

Alternative syntax (values without inner parentheses):

```
(define (sum-sq a b)
  (let (x (* a a) y (* b b))
    (+ x y)))
```

`letn` allows later initializers to use earlier ones.

```
(letn ((x 1) (y (+ x 1)))
  (list x y)) ;-> (1 2)
```


### local

`local` creates variables initialized to `nil`.

```
(local (a b c)
  (set 'a 10)
  (set 'b 20)
  (list a b c))
;-> (10 20 nil)
```


### letex

`letex` works like `let`, but the bound values are expanded into
the body at macro-expansion time.

```
(letex ((x 1) (y '(a b)) (z "hi")) '(x y z))
;-> (1 (a b) "hi")
;
(letex (x 1 y 2 z 3) '(x y z))
;-> (1 2 3)
```


### Unused parameters as locals

Function parameters are optional. Extra ones become local symbols,
initialized to `nil`. This creates convenient scratch variables.

```
(define (sum-sq a b , x y)
  (set 'x (* a a))
  (set 'y (* b b))
  (+ x y))
;
(sum-sq 3 4) ;-> 25
```

Note: the comma is only a visual separator; it behaves like a
normal symbol.


### Default parameter values

Default values can be assigned in parameter lists.

```
(define (foo (a 1) (b 2))
  (list a b))
;
(foo) ;-> (1 2)
(foo 3) ;-> (3 2)
(foo 3 4) ;-> (3 4)
```


### args as a local substitute

`args` returns the list of arguments not bound by declared
parameters.

```
(define (foo)
  (args))
;
(foo 1 2 3) ;-> (1 2 3)
```

```
(define (foo a b)
  (args))
;
(foo 1 2 3 4 5) ;-> (3 4 5)
```

You can index into `(args)`:

```
(define (foo)
  (+ (args 0) (args 1)))
;
(foo 3 4) ;-> 7
```


### args and local together

A common idiom: use `local` to declare names, then bind them using
`args` and `bind`.

```
(define-macro (foo)
  (local (len width height)
    (bind (args) true)
    (println "len:" len " width:" width " height:" height)))
;
(foo (width 20) (height 30) (len 10))
;-> len:10 width:20 height:30
```


---


## Walking Through Lists and Data

Rebel supports both recursive and iterative patterns for walking
through data. Recursion is expressive but may use more resources.
Iteration is often faster and lighter. Many built-ins such as
`flat` already use recursion internally, so you only need to call
them instead of writing your own.


### Recursion vs iteration

Classic recursive functions are clear, but can be slow because
each call adds overhead and may repeat work.

```
; classic recursion
(define (fib n)
  (if (< n 2) 1
      (+ (fib (- n 1))
         (fib (- n 2)))))
;
(fib 5) ;-> 8
```

Iterative patterns use minimal memory and run much faster.

```
; iterative version
(define (fibo n , f)
  (set 'f '(1 0))
  (dotimes (i n)
    (push (+ (f 0) (f 1)) f)))
;
(fibo 5) ;-> (3 2 1 1 0)
```


### Speed-up with memoization

Memoization caches results for functions so repeated calls with
the same arguments become instant. The macro below wraps any
function and stores its results inside a private context.

```
(define-macro (memoize mem func)
  (set (sym mem mem)
    (letex (f func c mem)
      (lambda ()
        (or (context c (string (args)))
            (context c (string (args))
              (apply f (args))))))))
;
(define (fib1 n)
  (if (< n 2) 1
      (+ (fib1 (- n 1))
         (fib1 (- n 2)))))
;
(memoize fib1-m fib1)
;
(time (fib1-m 25)) ;-> 148
(time (fib1-m 25)) ;-> 0
```

Recursive functions must be memoized using the raw lambda form.

```
(memoize fib2
  (lambda (n)
    (if (< n 2) 1
        (+ (fib2 (- n 1))
           (fib2 (- n 2))))))
;
(time (fib2 100)) ;-> 1
(fib2 80) ;-> 37889062373143906
```


### Walking a tree (nested lists)

Rebel can walk nested lists using classic recursion or using
iteration. The recursive form is direct but slower.

```
(set 'l '(a b c (d e (f g) h i) j k))
;
(define (walk l)
  (cond ((= l '()) true)
        ((atom? (first l))
          (println (first l))
          (walk (rest l)))
        (true
          (walk (first l))
          (walk (rest l)))))
;
(walk l)
```

A simpler recursive walk:

```
(define (walk l)
  (dolist (x l)
    (if (list? x)
        (walk x)
        (println x))))
;
(walk l)
```

Fast, built-in way:

```
(map println (flat l))
; same as:
(dolist (x (flat l)) (println x))
```


### Walking a directory tree

Some structures must be walked recursively because they do not
exist ahead of time — they are discovered as you go. Directory
trees are a typical example.

```
(define (walk-dir d)
  (when (directory? d)
    (dolist (n (directory d))
      (if (and (directory? (append d "/" n))
               (!= n ".") (!= n ".."))
          (walk-dir (append d "/" n))
          (println (append d "/" n))))))
;
(walk-dir "/tmp")
```

---


## Modifying and Searching Lists

Rebel supports multidimensional indexing on nested lists. Elements
can be changed destructively using `push`, `pop`, `setf`,
`set-ref`, `set-ref-all`, `sort`, `reverse` and others. Non-
destructive operations include `nth`, `ref`, `ref-all`, `first`,
`last`, `rest`, etc.

Indexes may be negative. `-1` refers to the last element,
`-2` to the second from the end, and so on.


### Negative indexing

```
(set 'l '(a b c d))
;
(l -1) ;-> d
(l -2) ;-> c
(-3 2 l) ;-> (b c)
;
(set 's "abcd")
;
(s -1) ;-> "d"
(s -2) ;-> "c"
(-3 2 s) ;-> "bc"
```


### push and pop

`push` inserts, `pop` removes. Both modify the list in place.

```
(set 'l '(b c d e f))
;
(push 'a l) ;-> (a b c d e f)
(push 'g l -1) ;-> (a b c d e f g)
(pop l) ;-> a
(pop l -1) ;-> g
(pop l -2) ;-> e
(pop l 1) ;-> c
;
l ;-> (b d f)
```

Nested push/pop via index vector:

```
(set 'l '(a b (c d (e f) g) h i))
;
(push 'x l 2 1) ;-> (a b (c x d (e f) g) h i)
;
l ;-> (a b (c x d (e f) g) h i)
(pop l 2 1) ;-> x
```

Pushing into a sublist accessed by assoc:

```
(set 'lst '((a 1) (b 2) (c 3) (d)))
;
(push 4 (assoc 'd lst) -1) ;-> (d 4)
;
lst ;-> ((a 1) (b 2) (c 3) (d 4))
```

Push/pull symmetry via index vector:

```
(set 'l '(a b (c d (e f) g) h i))
(set 'v '(2 1))
;
(push 'x l v)
;
(ref 'x l) ;-> (2 1)
(pop l v) ;-> x
```


### extend (destructive append)

`extend` inserts multiple lists into the target list.

```
(set 'l '(a b c))
;
(extend l '(d e) '(f g))
;
l ;-> (a b c d e f g)
```

Extend at nested place:

```
(set 'l '(a b "CD" (e f)))
;
(extend (l 3) '(g))
;
l ;-> (a b "CD" (e f g))
```


### Accessing lists (multidimensional indexing)

```
(set 'l '(a b (c d (e f) g) h i))
;
(nth 2 l) ;-> (c d (e f) g)
(nth '(2 2 1) l) ;-> f
(nth '(2 2) l) ;-> (e f)
;
(set 'v '(2 2 1))
(nth v l) ;-> f
;
(l 2 2 1) ;-> f
(l 2 2) ;-> (e f)
;
(l v) ;-> f
```


### rest and slice via implicit indexing

```
(rest '(a b c d e)) ;-> (b c d e)
(rest (rest '(a b c d e))) ;-> (c d e)
;
(1 '(a b c d e)) ;-> (b c d e)
(2 '(a b c d e)) ;-> (c d e)
;
(-2 '(a b c d e)) ;-> (d e)
;
(2 2 '(a b c d e f g)) ;-> (c d)
(-5 3 '(a b c d e f g)) ;-> (c d e)
```


### Selecting more elements

`select` extracts multiple elements in one pass.

```
(set 'l '(a b c d e f g))
;
(select l 1 2 4 -1) ;-> (b c e g)
;
(set 'v '(1 2 4 -1))
(select l v) ;-> (b c e g)
;
(select l 2 2 1 1) ;-> (c c b b)
```


### Filtering and differencing

```
(filter (fn (x) (< 5 x)) '(1 6 3 7 8)) ;-> (6 7 8)
(filter symbol? '(a b 3 c 4 "hi" g)) ;-> (a b c g)
(difference '(1 3 2 5 5 7) '(3 7)) ;-> (1 2 5)
;
(filter (curry < 5) '(1 6 3 7 8)) ;-> (6 7 8)
```


### Changing list elements

```
(set 'l '(a b (c d (e f) g) h i))
;
(setf (l 2 2 1) 'x) ;-> x
l ;-> (a b (c d (e x) g) h i)
;
(setf (l 2 2) 'z) ;-> z
l ;-> (a b (c d z g) h i)
```

Assoc example:

```
(set 'a '((a 1) (b 2) (c 3)))
;
(setf (assoc 'b a) '(b 22)) ;-> (b 22)
a ;-> ((a 1) (b 22) (c 3))
;
(setf (lookup 'c a) 33) ;-> 33
a ;-> ((a 1) (b 22) (c 33))
```


### Anaphoric variable $it

`$it` holds the old element when using `setf`, `replace`,
`set-ref`, and related operations.

```
(set 'l '(0 0 0))
;
(setf (l 1) (+ $it 1)) ;-> 1
(setf (l 1) (+ $it 1)) ;-> 2
(setf (l 1) (+ $it 1)) ;-> 4
;
l ;-> (0 3 0)
```


### replace in simple lists

```
(set 'l '(a b c d e a b c d))
;
(replace 'b l 'B) ;-> (a B c d e a B c d)
```

Replace with predicate:

```
(set 'l '(1 4 22 5 6 89 2 3 24))
;
(replace 10 l 10 <) ;-> (1 4 10 5 6 10 2 3 10)
```

Pattern-based replace:

```
(set 'al '((john 5 6 4) (mary 3 4 7) (bob 4 2 7 9) (jane 3)))
;
(replace '(mary *) al (list 'mary (apply + (rest $it))) match)
;-> ((john 5 6 4) (mary 14) (bob 4 2 7 9) (jane 3))
```

Sum-up all records:

```
(set 'al '((john 5 6 4) (mary 3 4 7) (bob 4 2 7 9) (jane 3)))
;
(replace '(*) al (list ($it 0) (apply + (rest $it))) match)
;-> ((john 15) (mary 14) (bob 22) (jane 3))
;
$0 ;-> 4
```

Replace where elements match pattern `(X X)`:

```
(replace '(X X)
         '((3 10) (2 5) (4 4) (6 7) (8 8))
         (list ($it 0) 'double ($it 1))
         unify)
;-> ((3 10) (2 5) (4 double 4) (6 7) (8 double 8))
;
$0 ;-> 2
```


### replace in nested lists (set-ref / set-ref-all)

```
(set 'd '((mon (ap 20 30) (or 2 4 9)))
         (tue (ap 5) (or 32 1))))
;
(set-ref 'mon d 'tue)
;-> ((tue (ap 20 30) (or 2 4 9)) (tue (ap 5) (or 32 1)))
```

Replace all matches:

```
(set 'd '((mon (ap 20 30) (or 2 4 9)))
         (tue (ap 5) (or 32 1))))
;
(set-ref-all 'ap d "Ap")
;-> ((mon ("Ap" 20 30) (or 2 4 9))
;    (tue ("Ap" 5) (or 32 1)))
```

Complex match example:

```
(set 'd '((mon (ap 20 30) (or 2 4 9)))
         (tue (ap 5) (or 32 1))))
;
(set-ref-all '(or *) d
  (list (first $0) (apply + (rest $it)))
  match)
;-> ((mon (ap 20 30) (or 15))
;    (tue (ap 5) (or 33)))
```


### Passing lists by reference

Using a context lets you pass a list by reference.

```
(set 'data:data '(a b c d e f g h))
;
(define (chg db i val)
  (setf (db i) val))
;
(chg data 3 999) ;-> d
;
data:data ;-> (a b c 999 e f g h)
```

Nested calls preserve references:

```
(set 'l '(r w j s r b))
;
(pop (sort l)) ;-> b
;
l ;-> (j r r s w)
```


### Variable expansion (expand / letex)

Symbol expansion replaces names with their values.

```
(set 'x 2 'a '(d e))
;
(expand '(a x (b c x)) 'x 'a)
;-> ((d e) 2 (b c 2))
```

Expansion inside function factories:

```
(define (raise-to p)
  (expand (fn (b) (pow b p)) 'p))
;
(define sq (raise-to 2))
(define cb (raise-to 3))
;
(sq 5) ;-> 25
(cb 5) ;-> 125
```

Association list expansion:

```
(expand '(a b c) '((a 1) (b 2))) ;-> (1 2 c)
(expand '(a b c) '((a 1) (b 2) (c (x y z))))
;-> (1 2 (x y z))
```

Evaluate association list before expansion:

```
(expand '(a b) '((a (+ 1 2)) (b (+ 3 4)))) ;-> ((+ 1 2) (+ 3 4))
(expand '(a b) '((a (+ 1 2)) (b (+ 3 4))) true) ;-> (3 7)
```

Uppercase variable expansion:

```
(set 'A 1 'Bv 2 'C nil 'd 5 'e 6)
;
(expand '(A (Bv) C d e f))
;-> (1 (2) C d e f)
```

Function factory:

```
(define (raise-to P)
  (expand (fn (b) (pow b P))))
;
(define cb (raise-to 3)) ;-> (lambda (b) (pow b 3))
(cb 4) ;-> 64
```


### letex for expansion

```
(letex ((x 1) (y '(a b c)) (z "hi")) '(x y z))
;-> (1 (a b c) "hi")
```


### Destructuring nested lists

`unify` + `bind` can unpack nested lists into variables.

```
(set 's '((one "two") 3 (four (x y z))))
(set 'p '((A B) C (D E)))
;
(bind (unify p s))
;
A ;-> one
B ;-> "two"
C ;-> 3
D ;-> four
E ;-> (x y z)
```

---


## Program Flow

Rebel supports functional style as well as looping and branching
constructs. All loops behave like expressions: the last evaluated
value becomes the result of the whole loop.

### Loops

Loop variables are always local to the loop and follow dynamic
scoping rules.

```
; dotimes: repeat N times, i goes 0..N-1
(dotimes (i n)
  ...)
```

Locality of loop variable:

```
(dotimes (i 3)
  (print i ":")
  (dotimes (i 3) (print i))
  (println))
;-> 0:012
;-> 1:012
;-> 2:012
```

Looping over lists:

```
(dolist (x lst)
  ...)
```

Looping over strings (character codes):

```
(dostring (c s)
  ...)
```

Looping over symbols in a context, sorted by name:

```
(dotree (sym ctx)
  ...)
```

Numeric loop with step:

```
(for (i start end step)
  ...)
```

Conditional loops:

```
(while cond
  ...)
;
(until cond
  ...)
;
(do-while cond
  ...)
;
(do-until cond
  ...)
```

Loops with break condition (third argument):

```
(dolist (x '(a b c d e f g) (= x 'e))
  (print x))
;-> abcd
```


### Blocks

Blocks are sequences of expressions evaluated in order. Loops
implicitly treat their bodies as blocks. For grouping expressions
inside `if` or `cond`, use `begin`.

```
(begin
  e1
  e2
  e3)
```

`and`, `or`, `let`, `letn` and `local` also form blocks implicitly,
so no `begin` is required.


### Branching

Basic conditional:

```
(if cond expr-true expr-false)
```

No false clause:

```
(if cond expr)
```

Unary form:

```
(if cond)
```

Multiple statements must be wrapped:

```
(if (= x y)
  (begin
    (f1 x)
    (f2 y))
  (begin
    (g1 x y)
    (g2 x y)))
```

`when` evaluates several expressions without `begin`:

```
(when cond
  e1
  e2)
```

`unless` is the negated `when`:

```
(unless cond
  e1
  e2)
```

Multi-condition `if`:

```
(if c1 e1
    c2 e2
    c3 e3
    e-default)
```

`cond` version:

```
(cond
  (c1 e1)
  (c2 e2)
  (true e-last))
```


### Fuzzy flow (amb)

`amb` chooses one of several expressions at random.

```
(amb
  e1
  e2
  e3)
```


### Flow with catch and throw

`catch` wraps a sequence; `throw` aborts immediately and returns a
value from the whole block.

```
(catch
  (dotimes (i 10)
    (if (= i 5) (throw "done"))
    (print i " ")))
;
; output: 0 1 2 3 4
; return: "done"
```


### Leave loops with a break condition

`dotimes`, `dolist` and `for` accept a break condition.

```
(dotimes (x 10 (> (* x x) 9))
  (println x))
;-> 0
;-> 1
;-> 2
;-> 3
```

```
(dolist (i '(a b c nil d e) (not i))
  (println i))
;-> a
;-> b
;-> c
```


### Change flow with and / or

`and` evaluates each expression until one is nil or empty, then
returns it.

```
(and
  e1
  e2
  e3)
```

`or` evaluates each expression until one is non-nil and returns
that value.

```
(or
  e1
  e2
  e3)
```

---


## Error Handling

Several conditions during evaluation can trigger error exceptions.
These include wrong argument types, wrong number of parameters,
invalid syntax, or calling a non-existent function.

### Built-in errors

```
; examples of built-in errors
(foo foo) ;-> invalid function : (foo foo)
(+ "hi") ;-> value expected in function + : "hi"
```


### User-defined errors

Users can raise errors explicitly with `throw-error`.

```
(define (double x)
  (if (= x 99) (throw-error "illegal number"))
  (+ x x))
;
(double 8) ;-> 16
(double 10) ;-> 20
(double 99)
;-> user error : illegal number
;-> called from user defined function double
```


### Error event handlers

An error handler can be installed globally with `error-event`.

```
(define (err-handler)
  (println (last (last-error)) " has occurred"))
;
(error-event 'err-handler)
;
(foo) ;-> ERR: invalid function : (foo) has occurred
```


### Catching errors

`catch` with a second parameter captures both built-in and user
errors without aborting the program.

```
(define (double x)
  (if (= x 99) (throw-error "illegal number"))
  (+ x x))
;
(catch (double 8) 'r) ;-> true
r ;-> 16
;
(catch (double 99) 'r) ;-> nil
(print r)
;-> user error : illegal number
;-> called from user defined function double
;
(catch (double "hi") 'r) ;-> nil
(print r)
;-> value expected in function + : x
;-> called from user defined function double
```

When the call succeeds:

- `catch` → `true`
- result stored in symbol given as 2nd argument

When it fails:

- `catch` → `nil`
- symbol contains the error message


### Operating system errors

Some OS-level failures are not captured as Rebel exceptions. They
can be inspected with `sys-error`.

```
(open "no-file" "r") ;-> nil
(sys-error) ;-> (2 "No such file or directory")
;
; reset errno
(sys-error 0) ;-> (0 "Unknown error: 0")
```

---


## Functions as Data

Functions in Rebel are first-class objects. They can be inspected,
modified, stored, passed around, and constructed at runtime. A
function is simply a lambda expression attached to a symbol.


### Manipulate after definition

```
(define (double x) (+ x x))
;-> (lambda (x) (+ x x))
;
(first double) ;-> (x)
(last double) ;-> (+ x x)
;
; fuzzy double
(setf (nth 1 double) '(mul (normal x (div x 10)) 2))
;
(double 10) ;-> 20.31445313
(double 10) ;-> 19.60351563
```

`lambda` is not a symbol but an attribute attached to a list.

Right-associative append:

```
(append (lambda) '((x) (+ x x)))
;-> (lambda (x) (+ x x))
;
(append (fn) '((x) (+ x x)))
;-> (lambda (x) (+ x x))
;
(set 'double (append (lambda) '((x) (+ x x))))
;
(double 10) ;-> 20
```

Left-associative cons:

```
(cons '(x) (lambda))
;-> (lambda (x))
```

Lambda expressions never lose first-class properties. `fn` is
a short alias.


### Mapping and applying

```
(define (double x) (+ x x))
;
(map double '(1 2 3 4 5)) ;-> (2 4 6 8 10)
```

Apply a function to list elements:

```
(apply + (sequence 1 10)) ;-> 55
```


### Functions making functions

Using `expand`:

```
(define (raise-to p)
  (expand (fn (b) (pow b p)) 'p))
;
(define sq (raise-to 2))
(define cb (raise-to 3))
;
(sq 5) ;-> 25
(cb 5) ;-> 125
```

Using `letex`:

```
(define (raise-to p)
  (letex (x p) (fn (b) (pow b x))))
```

Currying:

```
(define add-one (curry add 1))
(define by-ten (curry mul 10))
;
(add-one 5) ;-> 6
(by-ten 1.23) ;-> 12.3
```


### Functions with memory (namespace state)

```
(define (gen:gen)
  (inc gen:sum))
;
(gen) ;-> 1
(gen) ;-> 2
(gen) ;-> 3
```

Initializer:

```
(define (gen:init x)
  (setq gen:sum x))
;
(gen:init 20) ;-> 20
(gen) ;-> 21
(gen) ;-> 22
```


### Functions using self-modifying code

Accumulator:

```
(define (sum (x 0))
  (inc 0 x))
;
(sum 1) ;-> 1
(sum 2) ;-> 3
(sum 100) ;-> 103
(sum) ;-> 103
;
sum ;-> (lambda ((x 0)) (inc 103 x))
```

Stream factory:

```
(define (make-stream lst)
  (letex (stream lst)
    (lambda () (pop 'stream))))
;
(set 'l '(a b c d e f g h))
(define s (make-stream l))
;
(s) ;-> a
(s) ;-> b
(s) ;-> c
```

Works for strings too:

```
(set 'str "abcddefgh")
(define s (make-stream str))
;
(s) ;-> "a"
(s) ;-> "c"
```

---

## Text Processing

Rebel provides several tools for scanning, tokenizing, modifying
and constructing strings. Many operations use regular expressions,
and pattern matches populate the system variables `$0` to `$15`.

### Regular expressions — functions that use them

directory       → filter directory entries  
ends-with       → test string suffix  
find            → position of a match  
find-all        → list of all matches  
parse           → split text at regex separators  
regex           → full match details, offsets, lengths  
replace         → replace matches using expressions  
search          → search patterns in files  
starts-with     → test string prefix  


### Scanning text with replace

`replace` is often used as a scanner: the regex describes the
token, and the replacement expression collects results.

```
; tokenize file names from HTML
(set 'page (get-url "http://example.org/files/"))
;
(replace {>(.*rbl)<} page (first (push $1 links)) 0)
;
(dolist (f links)
  (write-file f (get-url (append "http://example.org/files/" f)))
  (println "->" f))
;
(exit)
```

Curly braces `{…}` avoid escape hell inside regex patterns.


### Scanning text with find-all

Shorter and clearer: `find-all` returns all matches directly.

```
(set 'links (find-all {>(.*rbl)<} page $1))
```

A transformation can be applied to each match:

```
(find-all {(new)(rebel)} "newREBELisNEWREBEL" (append $2 $1) 1)
;-> ("REBELnew" "REBELNEW")
```


### Tokenizing with parse

With `parse`, the regex describes the separators instead of the
tokens.

```
(set 's "1 2,3,4 5, 6 7  8")
;
(parse s {,\ *|\ +,*} 0)
;-> ("1" "2" "3" "4" "5" "6" "7" "8")
```

Without `{}`, backslashes would need doubling.


### Appending strings efficiently

`append` and `join` are efficient ways to build large strings.

```
(set 'lst (map string (rand 1000 100)))
;-> ("976" "329" ... "425")
;
; slow method
(set 'big "")
(dolist (x lst)
  (set 'big (append big x)))
;
; faster — 50×
(apply append lst)
```

Fastest approach: build a list, then join.

```
(join lst) ;-> "976329368692425..."
(join lst "-") ;-> "976-329-368..."
```


### Growing strings in place

Use `extend` to append, or `push` to insert at arbitrary positions.

```
; grow in place using extend
(set 's "")
(extend s "AB" "CD")
;
s ;-> "ABCD"
```

Extend inside nested structure:

```
(set 'l '(a b "CD" (e f)))
;
(extend (l 2) "E")
;
l ;-> (a b "CDE" (e f))
```

Push into string:

```
(set 's "")
(push "AB" s -1)
(push "CD" s -1)
;
s ;-> "ABCD"
```


### Rearranging characters

`select` works on strings just like on lists.

```
(set 's "eilnpsw")
(select s '(3 0 -1 2 1 -2 -3))
;-> "newrebl"
;
(select s 3 0 -1 2 1 -2 -3)
;-> "newrebl"
```


### Modifying strings

Operations that modify strings destructively:

extend  → append string  
push/pop → insert/remove chars  
replace → pattern replacement  
setf → change one or more characters  

`replace` with `""` removes matches.

UTF-8 is handled at character boundaries (not bytes) when indexing.


### Example destructive operations

```
(set 's "abcdef")
;
(setf (s 2) "XY") ; replace char at index 2
;
s ;-> "abXYdef"
```

---


## Dictionaries and Hashes

Rebel allows hash-like key/value access by using the default
functor of a namespace. A context whose default functor contains
no value behaves like a hash table. Keys are strings (or numbers
converted to strings internally). Values may be any expression.


### Hash-like access using a namespace

```
; create namespace and default functor
(define myhash:myhash)
;
; or using a predefined context template
(new Tree 'myhash)
```

Setting values:

```
(myhash "var" 123)
(myhash "foo" "hello")
(myhash "bar" '(q w e r t y))
(myhash "!*@$" '(a b c))
(myhash 555 42)     ; numeric key converted to string
```

Retrieving:

```
(myhash "var") ;-> 123
(myhash 555)   ;-> 42
```

Deleting (set value to nil):

```
(myhash "bar" nil)
```


### Keys and internal representation

Rebel prepends an underscore to keys internally to avoid symbol
collisions. The user never sees this during normal use.


### Convert hash to association list

```
(myhash)
;-> (("!*@$" (a b c)) ("foo" "hello") ("var" 123))
```

Inspect raw symbols:

```
(symbols myhash)
;-> (myhash:myhash myhash:_!*@$ myhash:_foo myhash:_var)
```


### Build a dictionary from an association list

```
(set 'alist '(("one" 1) ("two" 2) ("three")))
;
(myhash alist)
;
(myhash)
;-> (("!*@$" (a b c))
;    ("foo" "hello")
;    ("one" 1)
;    ("three" nil)
;    ("two" 2)
;    ("var" 123))
```


### Saving and loading dictionaries

Use `save` to serialize a namespace to a file.

```
(save "myhash.rbl" 'myhash)
```

Reload later:

```
(load "myhash")
```

Hash namespaces behave similarly to those created by statistical
training functions: string keys become internal symbols
(prepended with `_`). Such namespaces can be accessed using the
same default-functor mechanism.

---


## TCP/IP Client Server

Rebel provides simple TCP/IPv4 and IPv6 socket functions. Server
and client can communicate using blocking send/receive calls.


### Open connection (persistent session)

The server keeps the connection open until the client closes it,
then accepts a new one.

```
(constant 'max-bytes 1024)
;
; server listens
(if (not (set 'listen (net-listen 123)))
    (print (net-error)))
;
(while (not (net-error))
  (set 'conn (net-accept listen)) ; blocking accept
  (while (not (net-error))
    (net-receive conn msg max-bytes)
    ; process msg from client
    ; build reply
    (net-send conn reply)))
```

Client:

```
; client connects
(if (not (set 'conn (net-connect "host.com" 123)))
    (println (net-error)))
;
(constant 'max-bytes 1024)
;
(while (not (net-error))
  ; prepare message
  (net-send conn out)
  (net-receive conn inp max-bytes)
  ; process inp
  )
```


### Closed transaction (one request per connection)

The server closes the socket after each request/response pair.

```
; server
(constant 'max-bytes 1024)
;
(while (not (net-error))
  (set 'conn (net-accept listen)) ; blocking
  (net-receive conn msg max-bytes)
  ; process msg
  ; build reply
  (net-send conn reply)
  (close conn))
```

Client reconnects for every request:

```
; client
(unless (set 'conn (net-connect "host.com" 123))
  (println (net-error))
  (exit))
;
(constant 'max-bytes 1024)
;
; prepare message
(net-send conn out)
(net-receive conn inp max-bytes)
; process inp
```

These patterns cover the two common modes: persistent sessions and
single-transaction connections.

---


## UDP Communications

UDP is fast, lightweight and supports multicast. It offers no
guarantees about delivery order or completeness, which is fine for
local networks, telemetry, device control, or custom lightweight
protocols. Messages are connectionless.


### Open connection (continuous server)

Both server and client bind sockets with `net-listen` using the
"udp" option. The server may receive packets from many clients.
`net-send-to` extracts the target from the received message.

```
; server
(set 'sock (net-listen 10001 "localhost" "udp"))
(if sock (println "server on port " 10001)
        (println (net-error)))
;
(while (not (net-error))
  (set 'msg (net-receive-from sock 255))
  (println "-> " msg)
  (net-send-to
      (first (parse (nth 1 msg) ":"))
      (nth 2 msg)
      (upper-case (first msg))
      sock))
;
(exit)
```

Client:

```
(set 'sock (net-listen 10002 "" "udp"))
(if (not sock) (println (net-error)))
;
(while (not (net-error))
  (print "enter -> ")
  (net-send-to "127.0.0.1" 10001 (read-line) sock)
  (net-receive sock buff 255)
  (println "=> " buff))
;
(exit)
```


### Closed transaction

Used for controlling hardware or equipment. No setup, no loop:
just send/receive datagrams.

```
; receive up to 20 bytes
(net-receive-udp 1001 20)
;
; receive with timeout (5 seconds)
(net-receive-udp 1001 20 5000000)
;
; sender
(net-send-udp "host.com" 1001 "Hello")
```

Note: Win32 and Unix differ when sender/receiver lengths mismatch.


### Multi-cast communications

Server subscribes to a multicast address. Clients send to the same
multicast group.

```
; server
(net-listen 4096 "226.0.0.1" "multi")
(net-receive-from 5 20)
```

Client I:

```
(net-connect "226.0.0.1" 4096 "multi")
(net-send 3 "hello")
```

Client II:

```
(net-connect "" 4096 "multi")
(net-send-to "226.0.0.1" 4096 "hello" 3)
```

`net-receive-from` blocks. To avoid blocking, use `net-select` or
`net-peek`.


---


## Non-blocking Communications

By default, receive operations block until data arrives. Rebel
provides `net-select` and `net-peek` to avoid blocking and allow
polling loops or background work while waiting.


### Using net-select

`net-select` checks if data is available on a socket (or multiple
sockets). It can wait with a timeout in microseconds.

```
; poll for data with 100ms timeout
(while (not (net-select conn "r" 100000))
  (do-something-while-waiting ...))
;
(net-receive conn msg 255)
```

`conn` may be a socket number or a list of sockets to watch.


### Using net-peek

`net-peek` returns the number of bytes waiting to be read. Zero
means no data yet.

```
(while (= (net-peek sock) 0)
  (do-something-while-waiting ...))
;
(net-receive sock msg 255)
```

---


## Controlling Other Applications

Rebel can launch external programs and communicate with them
through STDIO pipes, TCP/IP, UDP, or named FIFOs. For simple
one-shot commands, `exec` is enough. For longer sessions, use
`process` together with pipes or sockets.


### Using exec (simple, one-command exchange)

```
(exec "ls *.c")
;-> ("foo.c" "bar.c" "net.c" "unix.c" ...)
```

`exec` starts a process, reads its STDOUT into a list of strings
and returns it. This is good for short interactions.


### STD I/O pipes (long-running, bidirectional)

`process` allows you to provide separate pipes for STDIN and
STDOUT of the child process.

```
; setup communication
(map set '(myin tcout) (pipe))
(map set '(tcin myout) (pipe))
(process "/usr/bin/wish" tcin tcout)
;
; send GUI script
(write myout
[text]
wm geometry . 250x90
wm title . "Tcl/Tk + Rebel"
bind . <Destroy> {puts {(exit)}}
[/text])
;
; event loop
(while (read-line myin)
  (eval-string (current-line)))
```

This technique works for any interpreter or CLI program that uses
STDIN/STDOUT.


### Communicate via TCP/IP

Example pattern for driving a GTK-server over TCP.

```
(define (gtk s , tmp)
  (net-send conn s)
  (net-receive conn tmp 64)
  tmp)
;
(process "gtk-server tcp localhost:50000")
(sleep 1000)
;
(set 'conn (net-connect "localhost" 50000))
;
(set 'r (gtk "gtk_init NULL NULL"))
(set 'r (gtk "gtk_window_new 0"))
; ...
```


### Communicate via named FIFO

A FIFO behaves like a file. The client writes into it, then reads a
response.

```
(exec "mkfifo myfifo")
; or using libc directly:
(import "/lib/libc.so.6" "mkfifo")
(mkfifo "/tmp/myfifo" 0777)
;
(define (gtk s)
  (set 'h (open "myfifo" "write"))
  (write h s)
  (close h)
  ;
  (set 'h (open "myfifo" "read"))
  (read h tmp 20)
  (close h)
  tmp)
```


### Communicate via UDP

UDP does not create a connection; it only binds a port and sends
datagrams.

```
(define (gtk s , tmp)
  (net-send-to "localhost" 50000 s sock)
  (net-receive sock 'tmp net-buffer)
  tmp)
;
(define (start)
  (process "gtk-server udp localhost:50000")
  (sleep 500)
  (set 'sock (net-listen 50001 "localhost" "udp")))
;
(set 'r (gtk "gtk_init NULL NULL"))
(set 'r (gtk "gtk_window_new 0"))
; ...
```

---


## Launching Apps (Blocking)

Rebel can run external commands in blocking mode. This is useful
for quick shell escapes or running tools that do not require
interactive communication.


### Shell execution

The `!` form executes a command through the system shell and
blocks until it finishes.

```
(! "ls -ltr")
```

A variant works only on the Rebel command line: the `!` must be
the first character.

```
!ls -ltr
```

This acts like a shell escape (similar to editors like vi). It is
useful for invoking tools without leaving the REPL entirely.


### Capturing STDOUT

Use `exec` to run a command and capture its output into a list of
strings.

```
(exec "ls /")
;-> ("bin" "etc" "home" "lib")
```


### Feeding STDIN

`exec` can also send input to a command’s STDIN:

```
(exec "script.cgi" cgi-in)
```

`cgi-in` is a string that becomes the STDIN for the executed
program.

Note: in this form, the output is printed directly to the screen
and cannot be captured. For bidirectional STDIN/STDOUT, use
`process` with `pipe`.

---


## Semaphores and Shared Memory

Semaphores and shared memory allow separate processes to
synchronize and exchange data without sockets or files. One
process can act as a producer writing data into shared memory,
while another acts as a consumer waiting for it.

This technique is very fast but also error-prone when more than
two processes interact. For larger systems, see the Cilk and
messaging patterns in later chapters.


### Producer–consumer example

Two child processes are created: one produces values, the other
consumes them. A shared memory cell holds the current value.
Two semaphores coordinate access.

```
(constant 'wait -1 'sig 1 'release 0)
;
(define (consumer n)
  (set 'i 0)
  (while (< i n)
    (semaphore cons-sem wait)
    (println (set 'i (share data)) " <-")
    (semaphore prod-sem sig))
  (exit))
;
(define (producer n)
  (for (i 1 n)
    (semaphore prod-sem wait)
    (println "-> " (share data i))
    (semaphore cons-sem sig))
  (exit))
;
(define (run n)
  (set 'data (share))
  (share data 0)
  ;
  (set 'prod-sem (semaphore))
  (set 'cons-sem (semaphore))
  ;
  (set 'prod-pid (fork (producer n)))
  (set 'cons-pid (fork (consumer n)))
  ;
  (semaphore prod-sem sig)
  (wait-pid prod-pid)
  (wait-pid cons-pid)
  ;
  (semaphore cons-sem release)
  (semaphore prod-sem release))
;
(run 10)
(exit)
```

---


## Message Exchange

Processes launched with `spawn` can exchange messages: parent → child,
child → parent, or child → child routed cez parent. Each spawned
process has two internal queues (in/out). When a receive queue is
empty, `receive` returns nil. When a send queue is full, `send`
returns nil. Using `until` makes these operations blocking.


### Blocking send / receive

```
; blocking sender
(until (send pid msg))
;
; blocking receiver
(until (receive pid msg))
```


### Blocking message exchange

Parent starts child processes, then waits for input from each child.
`sync` returns list of child PIDs with pending messages.

```
; child sends random numbers
(define (child-proc)
  (set 'pp (sys-info -4))
  (while true
    (until (send pp (rand 100)))))
;
; parent starts 5 children
(dotimes (i 5)
  (spawn 'r (child-proc) true))
;
(for (i 1 3)
  (dolist (cpid (sync))
    (until (receive cpid msg))
    (print "pid:" cpid "->>" (format "%-2d  " msg)))
  (println))
;
(abort)
(exit)
```


### Non-blocking message exchange

Messages are sent and received as fast as possible. Not all messages
are guaranteed to arrive (queues may overflow).

```
(set 'start (time-of-day))
;
(define (child-proc)
  (set 'pp (sys-info -4))
  (while true
    (send pp (rand 100))))
;
(dotimes (i 5)
  (spawn 'r (child-proc) true))
;
(set 'n 1000)
;
(until fin
  (if (= (inc cnt) n) (set 'fin true))
  (dolist (cpid (receive))
    (receive cpid msg)
    (if msg (print "pid:" cpid "->" (format "%-2d  \r" msg)))))
;
(abort)
(sleep 300)
(exit)
```


### Message timeouts

Blocking receive can be given a timeout limit.

```
(define (recv-timeout pid msec)
  (let ((start (time-of-day)) (msg nil))
    (until (receive pid msg)
      (if (> (- (time-of-day) start) msec)
          (throw-error "timeout")))
    msg))
;
(recv-timeout pid 1000)
```


### Evaluating messages

Messages can contain expressions evaluated by the receiver. This
allows routing messages or modifying state in another process.

Router example: child A sends expressions to parent; parent evaluates
them and relays messages to child B.

```
; sender child A
(set 'A
  (spawn 'r
    (begin
      (dotimes (i 3)
        (set 'pp (sys-info -4))
        (set 'msg
             '(until (send B (string "greetings from " A))))
        (until (send pp msg)))
      (until (send pp
        '(begin
           (sleep 200)
           (println "parent exiting ...\n")
           (set 'fin true)))))
    true))
;
; receiver child B
(set 'B
  (spawn 'r
    (begin
      (set 'pp (sys-info -4))
      (while true
        (until (receive pp msg))
        (println msg)
        (unless (= msg (string "greetings from " A))
          (println "ERROR: " msg))))
    true))
;
; proxy loop in parent
(until fin
  (if (receive A msg)
      (eval msg)))
;
(abort)
(exit)
```


### Acting as a proxy

Child A sends:

```
(until (send B (string "greetings from " A)))
```

The parent evaluates this expression and forwards the message to B.

Child A can also signal the parent to stop:

```
(until (send pp '(set 'fin true)))
```

The parent evaluates `set` and terminates its message loop.

The short sleep in A ensures output appears in correct order.

--


## Databases and Lookup Tables

For tables up to a few hundred records, association lists are
simple and fast. For larger datasets, use dictionaries and hashes
(see chapter 11).


### Association lists

Association lists map a key to a record. Pushing at index `-1`
is optimized.

```
(push '("John Doe" "123-5555" 1200.00) persons -1)
(push '("Jane Doe" "456-7777" 2000.00) persons -1)
;
persons
;-> (("John Doe" "123-5555" 1200.00)
;    ("Jane Doe" "456-7777" 2000.00))
```

Lookup with `assoc`:

```
(assoc "John Doe" persons)
;-> ("John Doe" "123-5555" 1200.00 male)
;
(assoc "Jane Doe" persons)
;-> ("Jane Doe" "456-7777" 2000.00 female)
```

`lookup` combines `assoc` + positional extraction:

```
(lookup "John Doe" persons 0)   ;-> "123-5555"
(lookup "John Doe" persons -1)  ;-> male
(lookup "Jane Doe" persons 1)   ;-> 2000.00
(lookup "Jane Doe" persons -2)  ;-> 2000.00
```

Updating a record:

```
(setf (assoc "John Doe" persons)
      '("John Doe" "123-5555" 900.00 male))
;
(setf (assoc "John Doe" persons) (update-person $it))
```

Deleting:

```
(replace (assoc "John Doe" persons) persons)
```


### Nested associations

Values inside associations can themselves be association lists.

```
(set 'persons
  '(
    ("Anne" (address (country "USA")   (city "New York")))
    ("Jean" (address (country "France") (city "Paris")))
  ))
```

`assoc` can take a vector of keys:

```
; one key
(assoc "Anne" persons)
;-> ("Anne" (address (country "USA") (city "New York")))
;
; two keys
(assoc '("Anne" address) persons)
;-> (address (country "USA") (city "New York")))
;
; three keys
(assoc '("Anne" address city) persons)
;-> (city "New York")
;
(set 'anne-city '("Anne" address city))
(assoc anne-city persons)
;-> (city "New York")
```


### Updating nested associations

Use `setf` + `assoc` with multiple keys:

```
(setf (assoc '("Anne" address city) persons)
      '(city "Boston"))
;-> (city "New York")
```

`setf` always returns the previous value.


### Combining associations and hashes

Hashes and FOOP objects can form an in-memory structured database.

```
(new Tree 'person)
(new Class 'address)
(new Class 'city)
(new Class 'telephone)
;
(person "John Doe"
  (address
    (city "Small Town")
    (telephone 5551234)))
;
(lookup 'telephone (person "John Doe"))
;-> 5551234
;
(setf (lookup 'telephone (person "John Doe")) 1234567)
(setf (lookup 'city (person "John Doe")) (lower-case $it))
;
(person "John Doe")
;-> (address (city "small town") (telephone 1234567))
```

---


## Distributed Computing

Rebel can distribute work across multiple machines or processes.
Remote expressions are evaluated using `net-eval`, which sends
code to remote nodes and collects results in blocking or callback
mode. Remote files can be accessed through HTTP-style URLs.

### Running a server node

A Rebel server is simply a Rebel process listening on a TCP port
and acting like a REPL plus minimal HTTP handler.

Start a stateful server:

```
rebel -c -d 4711 &
;
rebel myprog.rbl -c -d 4711 &
;
rebel myprog.rbl -c -w /home/node25 -d 4711 &
```

`-c` removes prompts, `-d` sets the port, `-w` sets the working
directory. After every closed connection Rebel resets its stack and
returns to MAIN, but keeps program and variable data.


### Stateless servers via inetd or xinetd

On Unix, inetd/xinetd can launch a fresh Rebel process for each
connection.

Add to `/etc/services`:

```
net-eval 4711/tcp
```

Add to `/etc/inetd.conf`:

```
net-eval stream tcp nowait rebel /usr/bin/rebel -c
;
net-eval stream tcp nowait rebel /usr/bin/rebel myprog.rbl -c
;
net-eval stream tcp nowait rebel /usr/bin/rebel -c -w /home/node
```

xinetd example (`/etc/xinet.d/net-eval`):

```
service net-eval
{
 socket_type = stream
 wait = no
 user = rebel
 server = /usr/bin/rebel
 port = 4711
 server_args = -c -w /home/node
}
```


### Testing the server using telnet

```
telnet localhost 4711
;
telnet 192.168.1.100 4711
```

Multiline expressions must be enclosed in `[cmd]` and `[/cmd]`.

```
[cmd]
(+ 3 4)
[/cmd]
```


### Testing with netcat

```
echo '(symbols) (exit)' | nc localhost 4711
;
echo '(symbols) (exit)' | nc 192.168.1.100 4711
```


### Testing from Rebel with net-eval

```
(net-eval "localhost" 4711 "(+ 3 4)" 1000)
;-> 7
;
(net-eval "192.168.1.100" 4711 {(upper-case "hello")} 1000)
;-> "HELLO"
```

Multiline content is automatically wrapped.


### HTTP testing with a browser

```
http://localhost:4711//usr/share/rebel/manual.html
```

The double slash means a file path relative to root on the server.


### Remote evaluation

```
(set 'res
  (net-eval '(
    ("192.168.1.100" 4711 {(+ 3 4)})
    ("192.168.1.101" 4711 {(+ 5 6)})
    ("192.168.1.102" 4711 {(+ 7 8)})
    ("192.168.1.103" 4711 {(+ 9 10)})
    ("192.168.1.104" 4711 {(+ 11 12)})
  ) 1000))
;
(println "result: " res)
(exit)
```

Typical output:

```
result: (7 11 15 19 23)
```


### Remote evaluation with callbacks

```
(define (idle-loop p)
  (if p (println p)))
;
(net-eval '(
  ("192.168.1.100" 4711 {(+ 3 4)})
  ("192.168.1.101" 4711 {(+ 5 6)})
  ("192.168.1.102" 4711 {(+ 7 8)})
  ("192.168.1.103" 4711 {(+ 9 10)})
  ("192.168.1.104" 4711 {(+ 11 12)})
) 1000 idle-loop)
(exit)
```

Callback output:

```
("192.168.1.100" 4711 7)
("192.168.1.101" 4711 11)
("192.168.1.102" 4711 15)
("192.168.1.103" 4711 19)
("192.168.1.104" 4711 23)
```


### Dynamic net-eval parameter setup

Define node list:

```
(set 'nodes '(
  ("192.168.1.100" 4711)
  ("192.168.1.101" 4711)
  ("192.168.1.102" 4711)
  ("192.168.1.103" 4711)
  ("192.168.1.104" 4711)
))
```

Program template:

```
(set 'program [text]
(begin
  (map set '(from to node) '(%d %d %d))
  (for (x from to)
    (if (= 1 (length (factor x)))
        (push x primes -1)))
primes)
[/text])
```

Callback:

```
(define (idle-loop p)
  (when p
    (println (p 0) ":" (p 1))
    (push (p 2) primes)))
```

Sending tasks:

```
(set 'result
  (net-eval (list
    (list (nodes 0 0) (nodes 0 1) (format program 0     99999  1))
    (list (nodes 1 0) (nodes 1 1) (format program 100000 199999 2))
    (list (nodes 2 0) (nodes 2 1) (format program 200000 299999 3))
    (list (nodes 3 0) (nodes 3 1) (format program 300000 399999 4))
    (list (nodes 4 0) (nodes 4 1) (format program 400000 499999 5))
  ) 20000 idle-loop))
;
(set 'primes (sort (flat primes)))
(save "primes" 'primes)
(exit)
```


### File transfer using HTTP URLs

```
(write-file  "http://127.0.0.1:4711//home/node/file.txt" "msg-")
;-> "14 bytes transferred ..."
;
(append-file "http://127.0.0.1:4711//home/node/file.txt" "more")
;-> "9 bytes transferred ..."
;
(read-file   "http://127.0.0.1:4711//home/node/file.txt")
;-> "msg-more"
```

Errors:

```
(read-file "http://127.0.0.1:4711//home/node/no.txt")
;-> "ERR:404 File not found ..."
```


### Remote load/save

```
(load "http://192.168.1.2:4711//usr/share/rebel/mysql5.rbl")
(save "http://192.168.1.2:4711//home/rebel/db.rbl" 'db)
```


### Local domain sockets

Rebel supports local Unix domain sockets for fast IPC on the same
machine using:

- net-eval  
- net-listen  
- net-connect  
- net-accept  
- net-receive  
- net-select  
- net-send  

---


## HTTPD Web Server Only Mode

Rebel normally uses `-c` mode for combined net-eval and HTTP
serving. In contrast, `-http` mode accepts only HTTP requests and
ignores command-line and net-eval traffic. This mode is suitable
for exposing Rebel as a minimal web server, but it must be secured
carefully by restricting accessible paths.


### Environment variables

In both `-c` and `-http` modes, the following variables are set:

DOCUMENT\_ROOT  
REQUEST\_METHOD  
SERVER\_SOFTWARE  
QUERY\_STRING  

If supplied by the client, the server also sets:

CONTENT\_TYPE  
CONTENT\_LENGTH  
HTTP\_HOST  
HTTP\_USER_AGENT  
HTTP\_COOKIE  


### Pre-processing incoming requests

`command-event` allows rewriting or filtering incoming requests
before Rebel evaluates them. A startup file can be used to install
the handler.

Example xinetd entry:

```
server_args = httpd-conf.rbl -http -w /home/node
```

Shell start:

```
rebel httpd-conf.rbl -http -d 80 -w /home/www &
```

Example request filter (`httpd-conf.rbl`):

```
; filter and translate HTTP requests
(command-event
  (fn (s)
    (let (req nil)
      (if (find "?" s)
        (begin
          (set 'req (first (parse s "?")))
          (if (ends-with req ".exe")
              (set 'req "GET /errorpage.html")
              (set 'req s)))
        (set 'req s))
      req)))
;
```

Any CGI request ending in `.exe` is rejected and redirected.


### CGI processing in HTTP mode

CGI programs must end with `.cgi` and be executable.

Minimal CGI:

```
#!/usr/bin/rebel
(print "Content-type: text/html\r\n\r\n")
(println "<h2>Hello World</h2>")
(exit)
;
```

If a CGI script starts its output with `"Status:"`, the default
HTTP header is suppressed and the script must supply all headers.

Redirect example:

```
#!/usr/bin/rebel
(print "Status: 301 Moved Permanently\r\n")
(print "Location: http://example.org/index.cgi\r\n\r\n")
(exit)
;
```

A CGI helper module (cgi.rbl) typically provides routines for GET
and POST parsing, cookie access, etc.


### Media types in HTTP modes

In both `-c` and `-http`, Rebel chooses media types based on file
extension:

.avi       video/x-msvideo  
.css       text/css  
.gif       image/gif  
.htm       text/htm  
.html      text/html  
.jpg       image/jpg  
.js        application/javascript  
.mov       video/quicktime  
.mp3       audio/mpeg  
.mpg       video/mpeg  
.pdf       application/pdf  
.png       image/png  
.wav       audio/x-wav  
.zip       application/zip  
default    text/plain  

---


## 23. Extending Rebel

### FFI Quickstart

This is the fastest way to understand Rebel’s FFI.  
Write a small C function, build a `.so`, import it, call it.

C code:

```
int add2(int x) {
  return x + 2;
}
```

Compile:

```
cc -fPIC -shared -o add2.so add2.c
```

Rebel:

```
(import "./add2.so" "add2")
(add2 40) ;-> 42
```

That’s it.  
No wrappers, no config files, no type descriptions, no build system.  
You pass raw values, C receives them exactly, and returns exactly what it wants.

---

## Full FFI Reference

Rebel’s FFI is intentionally low-level and predictable.  
Nothing is wrapped, nothing is abstracted, and nothing is hidden.  
You pass raw pointers, buffers, integers, and strings.  
C sees exactly what you send, and you get exactly what C returns.

There is no marshalling layer.  
No automatic memory management.  
No type negotiation.  
No runtime safety net.

If you pass a bad pointer, the process dies.  
If you follow the ABI, the call is as fast as calling C from C.

The benefit: Rebel can be extended with C libraries in minutes, without
tooling, without glue code, and without foreign type systems.  
You control everything.

---

### Building a shared library

Example C file:

```
#include <stdio.h>
#include <ctype.h>

int foo1(char *ptr, int n) {
  printf("str:%s n:%d\n", ptr, n);
  return n * 10;
}

char *foo2(char *ptr, int n) {
  char *p = ptr;
  while (*p) { *p = toupper(*p); p++; }
  return ptr;
}
```

Build:

```
cc -fPIC -shared -o testlib.so testlib.c
```

---

### Importing symbols

```
(import "./testlib.so" "foo1")
(import "./testlib.so" "foo2")
(foo1 "abc" 3) ;-> prints + returns 30
(set 'p (foo2 "hello" 1))
(get-string p) ;-> "HELLO"
```

---

### Passing arguments

Rebel can pass integers, floats (wrapped in `flt`), strings, and binary
buffers using `pack`.

- integer  
  - `(foo 123)`  
  - `foo(int x)`

- double  
  - `(foo 1.23)`  
  - `foo(double x)`

- float  
  - `(foo (flt 1.23))`  
  - `foo(float x)`

- string  
  - `(foo "hello")`  
  - `foo(char *s)`

- int array  
  - `(foo (pack "d d d" 1 2 3))`  
  - `foo(int a[])`

- float array  
  - `(foo (pack "f f f" (flt 1.2) (flt 3.4) (flt 5.6)))`  
  - `foo(float a[])`

- double array  
  - `(foo (pack "lf lf lf" 1.2 3.4 5.6))`  
  - `foo(double a[])`

- string array  
  - `(foo (pack "lu lu lu" "a" "b" "c"))`  
  - `foo(char *a[])`

---

### Reading return values

- integer  
  - `(set 'n (foo x y))`

- pointer to double  
  - `(get-float (foo x))`

- pointer to string  
  - `(get-string (foo x))`

- integer array  
  - `(unpack "ld ld ld" (foo x))`

- float array  
  - `(unpack "f f f" (foo x))`

- double array  
  - `(unpack "lf lf lf" (foo x))`

- string array  
  - `(map get-string (unpack "ld ld ld" (foo x)))`

---

### Working with C structures

C struct:

```
#include <stdlib.h>
#include <string.h>

struct item {
  int n;
  char *s;
};

struct item *make_item(char *txt, int v) {
  struct item *p = malloc(sizeof(struct item));
  p->s = malloc(strlen(txt) + 1);
  strcpy(p->s, txt);
  p->n = v;
  return p;
}
```

Compile:

```
cc -fPIC -shared -o item.so item.c
```

Rebel example:

```
(import "./item.so" "make_item")
(set 'p (make_item "yo" 7))
(get-int p) ; n
(get-string (get-int (+ p 8))) ; s on LP64
```

To discover offsets reliably, compile:

```
#include <stdio.h>
#include <stddef.h>
#include "item.h"
int main(){
  printf("n=%zu s=%zu\n",
    offsetof(struct item,n),
    offsetof(struct item,s));
}
```

---

### Memory management

Rebel never frees foreign memory automatically.

```
(import "libc.so" "free")
(set 'p (make_item "hi" 9))
(free p)
```

If the C API provides its own destructor, use that instead.

---

### Buffers mutated by C

```
(set 'buf (pack "c c c" 97 98 99))
(callC buf)
(unpack "c c c" buf)
```

---

### Alignment and padding

C compilers insert padding.  
Rebel treats memory as a flat byte array.

Example:

```
struct X {
  short a;
  int   b;
  short c;
};
```

Likely LP64 layout: a(2), pad(2), b(4), c(2), pad(2).  
Total 12 bytes.

Rebel:

```
(unpack "lu lu lu" (foo))
```

Another layout:

```
struct Y {
  short a;
  short b;
  int   c;
};
```

Total 8 bytes.

Rebel:

```
(unpack "u u lu" (foo))
```

---

### Wrapper libraries

If a C library is inconvenient to call, write a thin wrapper.

```
#include <stdarg.h>

double realfn(double x, int y, double z) {
  return (x + z) * y;
}

double *wrap(int argc, ...) {
  static double r;
  va_list ap;
  va_start(ap, argc);
  double x = va_arg(ap, double);
  int    y = va_arg(ap, int);
  double z = va_arg(ap, double);
  va_end(ap);
  r = realfn(x, y, z);
  return &r;
}
```

Compile:

```
cc -fPIC -shared -o wrap.so wrap.c
```

Rebel:

```
(import "./wrap.so" "wrap")
(get-float (wrap 3 1.2 3 1.4))
```

---

### Callbacks from C into Rebel

C library:

```
typedef void (*cb_t)(int);
void run(cb_t fn) {
  fn(123);
}
```

Compile:

```
cc -fPIC -shared -o cb.so cb.c
```

Rebel:

```
(import "./cb.so" "run")
(define (cb x) (println x))
(run (callback 1 'cb))
```

---

### Pack formats

- `d`  
  - 32-bit signed int  
  - C: `int`

- `f`  
  - 32-bit float  
  - C: `float`

- `lf`  
  - 64-bit float  
  - C: `double`

- `u`  
  - 16-bit unsigned  
  - C: `unsigned short`

- `c`  
  - byte  
  - C: `char`

- `lu`  
  - pointer-sized unsigned  
  - C: `uintptr_t`

- `ld`  
  - pointer-sized signed  
  - C: `intptr_t`

---

### Summary

Rebel’s FFI is direct, transparent, and extremely easy to use.
No foreign runtimes, no layers, no indirection.

You write C, build a `.so`, import it, and call it.

If you can write a C function, you can extend Rebel infinitely.
If you know pointers and offsets, you can call any Unix library.
If you want raw speed and control, FFI is the sharpest tool in the box.
