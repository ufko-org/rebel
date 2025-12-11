Title: Rebel Reference Manual
css: docs.css

# Rebel Reference Manual (WIP)

---

Copyright (c) 2025 Ufko (ufko.org)

This manual is released under the MIT license.
You may use, copy, modify and distribute it freely,
provided that this notice is preserved.

---

<a name="f-bang"></a>
## !

```
syntax: (! str-shell-command)
```

Description:

Executes the external command contained in
str-shell-command. The call blocks until the invoked
program terminates. The return value is the exit status
reported by the operating system.

Use exec when capturing standard output or providing
standard input is required. Use process for non-blocking
execution or when redirecting streams through pipes.

A leading ! at the start of a line in the interactive
shell is a special operator, not a function call. In
that form, parentheses are omitted and the remainder of
the line is forwarded directly to the system shell.

Examples:

```
(! "nvi2")
(! "ls -ltr")

; interactive shell operator form
!ls -ltr
```

Notes:

- The inline REPL form works only when ! is the first
  character on the input line.
- The function always uses the system’s default command
  interpreter.

See: [exec](#f-exec), [process](#f-process)

---


<a name="f-dollar"></a>
## $

```
syntax: ($ int-idx)
```

Description:

Provides indexed access to the predefined system
variables $0, $1, $2–$15. These variables are assigned
by functions that perform regular expression matching or
token extraction. Each storing function updates the
variables as part of its operation, and the stored
values remain available until the next regex-driven
call.

The variables may be referenced directly by name or
through numeric indexing with this function. Using ($
int-idx) enables programmatic access when the index is
not known in advance.

Examples:

```
(set 'txt "http://example.org:80")
(find "http://(.*):(.*)" txt 0)  ; → 0

$0     ; → "http://example.org:80"
$1     ; → "example.org"
$2     ; → "80"

($ 0)  ; → "http://example.org:80"
($ 1)  ; → "example.org"
($ 2)  ; → "80"
```

Notes:

- Only functions that employ regular expressions update
  the system variables.
- Access via ($ int-idx) is equivalent to referencing
  the variables $0–$15 directly.

See: [find](#f-find), [find-all](#f-find-all),
[regex](#f-regex), [replace](#f-replace), [parse](#f-parse)

---

<a name="f-plus"></a>
## +

```
syntax: (+ int-1 [int-2 ...])
```

Description:

Computes the sum of all arguments. Each value is treated
as an integer. Floating point arguments are truncated
toward zero before the addition. Values exceeding the
machine integer limits wrap around after the operation.
Arguments evaluating to NaN are treated as zero.

Examples:

```
(+ 1 2 3 4 5)           ; → 15
(+ 1 2 (- 5 2) 8)       ; → 14
(+ 1.2 3.9)             ; → 4
```

Notes:

- All floating point values are truncated toward zero.
- Overflow and underflow wrap around to the opposite
  integer range limit.

See: [minus](#f-minus), [star](#f-star), [slash](#f-slash),
[%](#f-percent), [mod](#f-mod)

---


<a name="f-minus"></a>
## -

```
syntax: (- int-1 [int-2 ...])
```

Description:

Subtracts each subsequent argument from the running
result. All arguments are treated as integers. Floating
point arguments are truncated toward zero. With a single
argument, the sign of that value is reversed. Results
that exceed the integer range wrap around. Arguments
evaluating to NaN are treated as zero.

Examples:

```
(- 10 3 2 1)            ; → 4
(- (* 3 4) 6 1 2)       ; → 3
(- 123)                 ; → -123
(map - '(10 20 30))     ; → (-10 -20 -30)
```

Notes:

- One argument negates the value.
- Floating point input is truncated toward zero.
- Overflow and underflow wrap around the integer range.

See: [plus](#f-plus), [star](#f-star), [slash](#f-slash),
[%](#f-percent)

---


<a name="f-star"></a>
## *

```
syntax: (* int-1 [int-2 ...])
```

Description:

Computes the product of all arguments. Every value is
converted to an integer by truncating floating point
inputs toward zero. Results that exceed the integer
range wrap around. Arguments evaluating to NaN are
treated as zero before multiplication.

Examples:

```
(* 1 2 3)              ; → 6
(* 10 (- 8 2))         ; → 60
```

Notes:

- Floating point arguments are truncated toward zero.
- Overflow and underflow wrap around the integer range.
- NaN values behave as zero.

See: [plus](#f-plus), [minus](#f-minus), [slash](#f-slash),
[%](#f-percent)

---


<a name="f-slash"></a>
## /

```
syntax: (/ int-1 [int-2 ...])
```

Description:

Divides the running result by each subsequent argument.
All values are converted to integers by truncating
floating point inputs toward zero. Division by zero
causes an error. When the result exceeds the integer
range, it wraps around. Arguments evaluating to NaN are
treated as zero, which triggers a division-by-zero
error.

Examples:

```
(/ 12 3)               ; → 4
(/ 120 3 20 2)         ; → 1
```

Notes:

- Floating point arguments are truncated toward zero.
- Division by zero raises an error.
- Overflow and underflow wrap around the integer limits.

See: [plus](#f-plus), [minus](#f-minus), [star](#f-star),
[%](#f-percent)

---


<a name="f-percent"></a>
## %

```
syntax: (% int-1 [int-2 ...])
```

Description:

Performs successive integer modulo operations. Each
argument is converted to an integer by truncating any
floating point value toward zero. The running result is
replaced by (mod result int-i) for each argument. A
division-by-zero condition raises an error. When the
result exceeds the integer range, it wraps around.
Values evaluating to NaN are treated as zero and cause a
division-by-zero error.

Floating point numbers should be handled with the mod
function instead, which preserves fractional behavior.

Examples:

```
(% 10 3)               ; → 1
(% -10 3)              ; → -1
```

Notes:

- Floating point inputs are truncated toward zero.
- Division by zero raises an error.
- Overflow and underflow wrap around the integer limits.
- For floating point modulo, use mod.

See: [plus](#f-plus), [minus](#f-minus), [star](#f-star),
[slash](#f-slash), [mod](#f-mod)

---


<a name="f-plusplus"></a>
## ++

```
syntax: (++ place [num ...])
```

Description:

Increments the value stored at place using integer
arithmetic. When no num argument is supplied, the value
is increased by 1. When one or more arguments are given,
each value is truncated toward zero before being added
to the running total.

If place refers to a symbol containing nil, it is
treated as if it contained 0. When place refers to a
list position, the element at that position is updated
in place. If place is an expression that yields a
number, the incremented result is returned but not
stored.

All calculations use integer limits. Values exceeding
9,223,372,036,854,775,807 wrap around to negative
numbers. Values smaller than
-9,223,372,036,854,775,808 wrap around to positive
numbers. Arguments evaluating to NaN are treated as 0.

Examples:

```
(set 'n 1)
(++ n)                 ; → 2

(set 'n 3.8)
(++ n)                 ; → 4
(++ n 1.3)             ; → 5

(set 'lst '(1 2 3))
(++ (lst 1) 2)         ; → 4
lst                    ; → (1 4 3)
```

Notes:

- Floating point arguments are truncated toward zero.
- Nil is treated as 0 when used as a place.
- Wrap-around occurs on integer overflow or underflow.
- For floating point incrementing, use inc.

See: [--](#f-minusminus), [inc](#f-inc)

---


<a name="f-minusminus"></a>
## --

```
syntax: (-- place [num ...])
```

Description:

Decrements the value stored at place using integer
arithmetic. When no num argument is supplied, the value
is decreased by 1. When additional arguments are given,
each value is truncated toward zero before being
subtracted from the running result.

If place refers to a symbol containing nil, it behaves
as if it contained 0. When place selects an element in a
list, that element is updated in place. If place is an
expression that merely yields a number, the decremented
result is returned but not stored.

All operations use the full integer range. Values that
exceed 9,223,372,036,854,775,807 wrap around to negative
numbers. Values below -9,223,372,036,854,775,808 wrap
around to positive numbers. Arguments evaluating to NaN
are treated as 0.

Examples:

```
(set 'n 1)
(-- n)                 ; → 0

(set 'n 3.8)
(-- n)                 ; → 2
(-- n 1.3)             ; → 1

(set 'lst '(1 2 3))
(-- (lst 1) 2)         ; → 0
lst                    ; → (1 0 3)
```

Notes:

- All floating point arguments are truncated toward zero.
- Nil is treated as 0 when used as a place.
- Integer overflow and underflow wrap around.
- For floating point decrementing, use dec.

See: [++](#f-plusplus), [dec](#f-dec)

---


<a name="f-lt"></a>
## <

```
syntax: (< exp-1 [exp-2 ...])
```

Description:

Evaluates all expressions and compares each pair in
sequence. The result is true only if every comparison
satisfies the < relation. As soon as one comparison
fails, the function returns nil.

With a single argument, the operator compares the value
against 0. This checks whether the number is negative.

All data types can be compared. Lists are compared
element by element; earlier elements have higher
significance. When lists share all elements but differ
in length, the longer list is greater. Mixed types
follow a fixed type ordering, and numeric types are
converted as needed. Floating point values are truncated
toward zero for bigint comparison rules.

Examples:

```
(< 3 5 8 9)                      ; → true
(< "a" "c" "d")                  ; → true
(< '(3 4) '(1 5))                ; → nil
(< '(1 2 3) '(1 2))              ; → nil
(< 1.2 6 "Hello")                ; → true
(< nil true)                     ; → true
(< '(((a b))) '(((b c))))        ; → true
(< '( (a (b c)) '(a (b d)) '(a (b (d))) ))  ; → true

; single-argument form (compares with 0)
(< -1)                           ; → true
(< 1)                            ; → nil
```

Notes:

- One argument means comparison against 0.
- Lists compare recursively by element order.
- Mixed types use a fixed precedence:
  nil < true < number < string < symbol < primitive
  < quoted list < list < lambda < lambda-macro.

See: [>](#f-gt), [=](#f-eq), [<=](#f-le),
[>=](#f-ge), [!=](#f-neq)

---


<a name="f-gt"></a>
## >

```
syntax: (> exp-1 [exp-2 ...])
```

Description:

Evaluates all expressions and compares them in order
using the > relation. The result is true only when every
comparison succeeds. If any comparison fails, evaluation
stops and nil is returned.

With one argument, the value is compared against 0. This
checks whether the number is positive.

All types may be compared. Lists are compared element by
element; earlier elements have greater significance than
later ones. If lists share all compared elements but
differ in length, the longer list is considered greater.
Mixed-type comparisons follow a fixed type ordering.
Floating point values are truncated toward zero for
integer comparison.

Examples:

```
(> 4 2 3 6)                     ; → nil
(> '(1 2 3) '(1 2))             ; → true
(> 5 1 -3 -9)                   ; → true
(> "z" "b" "a")                 ; → true

; single-argument form (compare with 0)
(> 1)                           ; → true    ; positive
(> -1)                          ; → nil     ; not positive

(map > '(1 3 -4 -3 1 2))        ; → (true true nil nil true true)
```

Notes:

- One argument compares the value against 0.
- List comparison is recursive and length-sensitive.
- Mixed types follow type precedence:
  nil < true < number < string < symbol < primitive
  < quoted list < list < lambda < lambda-macro.

See: [<](#f-lt), [=](#f-eq), [<=](#f-le),
[>=](#f-ge), [!=](#f-neq)

---


<a name="f-eq"></a>
## =

```
syntax: (= exp-1 [exp-2 ...])
```

Description:

Evaluates all expressions and compares them pairwise for
equality. The result is true only when each comparison
returns true. If any pair does not match, the function
returns nil.

When called with a single argument, the value is
compared against 0, which allows checking for zero.

Equality applies to all data types. Numbers are compared
as integers after truncation toward zero. Strings,
symbols, and atoms compare by value. Lists are compared
recursively; they are equal only when every element in
corresponding positions is equal and both lists have the
same length. Mixed-type comparisons follow a fixed
precedence when determining ordering, but for equality
the types must match after conversion rules.

Examples:

```
(= '(5 7 8) '(5 7 8))           ; → true
(= 10 10 10)                    ; → true
(= 3 5)                         ; → nil
(= "a" "a")                     ; → true
(= '(1 2) '(1 2 3))             ; → nil

; single-argument form (compare with 0)
(= 0)                           ; → true
(= 123)                         ; → nil
```

Notes:

- One argument means comparison against 0.
- Lists must match in both structure and length.
- Numeric values are truncated toward zero.
- Mixed-type equality applies only when types become
  equivalent under conversion rules.

See: [<](#f-lt), [>](#f-gt), [<=](#f-le),
[>=](#f-ge), [!=](#f-neq)

---


<a name="f-le"></a>
## <=

```
syntax: (<= exp-1 [exp-2 ...])
```

Description:

Evaluates all expressions and compares each consecutive
pair using the <= relation. The result is true only if
every comparison succeeds. When any comparison fails, the
function returns nil immediately.

With a single argument, the operator compares the value
against 0, allowing a check for “less than or equal to
zero.”

All data types may be compared. Lists compare
element-by-element; earlier elements have higher
significance. If the lists share all compared elements
but differ in length, the shorter list is considered
less. Mixed-type comparisons follow a fixed type
precedence. Numeric values are compared as integers,
with floating point inputs truncated toward zero.

Examples:

```
(<= 1 2 2 5)                    ; → true
(<= 5 3 1)                      ; → nil
(<= '(1 2) '(1 2 3))            ; → true
(<= "a" "b" "b" "z")            ; → true

; single-argument form (compare with 0)
(<= 0)                          ; → true
(<= -3)                         ; → true
(<= 5)                          ; → nil
```

Notes:

- One argument means comparison with 0.
- List comparison is recursive and length-sensitive.
- Floating point values are truncated toward zero.
- Type precedence determines ordering for mixed types.

See: [<](#f-lt), [>](#f-gt), [=](#f-eq),
[>=](#f-ge), [!=](#f-neq)

---


<a name="f-ge"></a>
## >=

```
syntax: (>= exp-1 [exp-2 ...])
```

Description:

Evaluates all expressions and compares each pair using
the >= relation. The result is true only when every
comparison succeeds. If any comparison fails, evaluation
stops and nil is returned.

With a single argument, the operator compares the value
against 0. This makes it possible to test whether a
number is non-negative.

All data types may be compared. Lists compare
recursively: earlier elements have higher significance,
and when all elements match but lengths differ, the
longer list is considered greater. Mixed-type
comparisons follow a fixed precedence. Numeric
comparisons follow integer rules, and floating point
arguments are truncated toward zero.

Examples:

```
(>= duba aba)                    ; → true
(>= 9 3 3 -1)                    ; → true
(>= '(1 2 3) '(1 2))             ; → true
(>= "z" "z" "a")                 ; → true
(>= "a" "b")                     ; → nil

; single-argument form (compare with 0)
(>= 1)                           ; → true
(>= 0)                           ; → true
(>= -1)                          ; → nil
```

Notes:

- One argument means comparison with 0.
- Lists compare by structure and then length.
- Floating point values are truncated toward zero.
- Type precedence defines ordering for mixed types.

See: [<](#f-lt), [>](#f-gt), [=](#f-eq),
[<=](#f-le), [!=](#f-neq)

---


<a name="f-neq"></a>
## !=

```
syntax: (!= exp-1 [exp-2 ...])
```

Description:

Evaluates all expressions and checks that each
consecutive pair is *not equal*. The result is true only
when every comparison evaluates to inequality. If any
pair is equal, the function returns nil.

With a single argument, the operator compares the value
against 0. This allows checking whether a value is “not
zero.”

Equality rules follow the same semantics as the =
operator. Numbers are compared as integers, with floating
point values truncated toward zero. Lists compare
recursively by structure and length. Mixed-type
comparisons follow the general type precedence rules.

Examples:

```
(!= 1 4 3 7 3)                  ; → true
(!= 5 5)                        ; → nil
(!= '(1 2) '(1 2 3))            ; → true
(!= "a" "b" "b")                ; → nil

; mixed-type chain
(!= 1 "x" '(1 2) true)          ; → true

; single-argument form (compare with 0)
(!= 0)                          ; → nil
(!= 123)                        ; → true
```

Notes:

- One argument means comparison with 0.
- Inequality fails if any pair is equal.
- Floating point values are truncated toward zero.
- List inequality considers both structure and length.

See: [=](#f-eq), [<](#f-lt), [>](#f-gt),
[<=](#f-le), [>=](#f-ge)

---


<a name="f-shiftl"></a>
## <<

```
syntax: (<< int-1 int-2 [int-3 ...])
syntax: (<< int-1)
```

Description:

Performs an arithmetic left shift on int-1. Each shift
argument specifies how many bits to shift. The result of
each step becomes the input to the next. For integers,
the allowed shift distance is up to 63 bits on 64-bit
platforms.

When only one argument is supplied, the value is shifted
left by 1 bit. All operations use integer arithmetic,
with wrap-around on overflow.

Examples:

```
(<< 1 3)          ; → 8
(<< 1 2 1)        ; → 8
(<< 3)            ; → 6
```

Notes:

- One argument shifts by 1 bit.
- Multiple shift arguments are applied sequentially.
- Overflow wraps around the integer range.

See: [>>](#f-shiftr)

---


<a name="f-shiftr"></a>
## >>

```
syntax: (>> int-1 int-2 [int-3 ...])
syntax: (>> int-1)
```

Description:

Performs an arithmetic right shift on int-1. Each shift
argument specifies how many bits to move. The result of
each shift becomes the input to the next. Arithmetic
right shifting duplicates the most significant bit,
preserving the sign of the number.

When only one argument is given, the value is shifted
right by 1 bit. Shift distances up to 63 bits are valid
on 64-bit platforms. Operations use integer arithmetic
with wrap-around on overflow.

Examples:

```
(>> 1024 10)                    ; → 1
(>> 160 2 2)                    ; → 10
(>> 8)                          ; → 4

; arithmetic shift keeps the sign bit
(>> 0x8000000000000000 1)       ; → 0xC000000000000000
```

Notes:

- One argument shifts by 1 bit.
- Shifts apply sequentially when several arguments are
  given.
- Arithmetic shifting preserves the sign bit.
- Overflow wraps within the integer range.

See: [<<](#f-shiftl)

---


<a name="f-andbit"></a>
## &

```
syntax: (& int-1 int-2 [int-3 ...])
```

Description:

Performs a bitwise AND operation. The value in int-1 is
combined with int-2 using AND, and the result is then
combined with each additional argument in sequence.

All arguments are treated as integers. Floating point
inputs are truncated toward zero before the operation.
The result always fits in the machine integer range.

Examples:

```
(& 0xAABB 0x000F)    ; → 11    ; 0xB
(& 255 1 1 1)        ; → 1
(& 42 63)            ; → 42
```

Notes:

- All arguments are processed left to right.
- Floating point inputs are truncated toward zero.
- The operation is purely bitwise, without sign
  extension rules used in shifts.

See: [|](#f-orbit), [^](#f-xorbit), [~](#f-notbit)

---


<a name="f-orbit"></a>
## |

```
syntax: (| int-1 int-2 [int-3 ...])
```

Description:

Performs a bitwise OR operation. The value in int-1 is
combined with int-2, and the resulting value is then
combined with each remaining argument in order. All
arguments are converted to integers by truncating
floating point values toward zero.

The operation sets each bit in the result if that bit is
set in any of the processed arguments.

Examples:

```
(| 0x10 0x80 2 1)     ; → 147
(| 1 4 16)            ; → 21
(| 0 0 7)             ; → 7
```

Notes:

- Arguments are processed left to right.
- Floating point inputs are truncated toward zero.
- The result reflects the bitwise OR across all
  arguments.

See: [&](#f-andbit), [^](#f-xorbit), [~](#f-notbit)

---


<a name="f-xorbit"></a>
## ^

```
syntax: (^ int-1 int-2 [int-3 ...])
```

Description:

Performs a bitwise XOR operation. The value in int-1 is
combined with int-2 using XOR, and the resulting value is
then combined with each following argument in sequence.
All arguments are treated as integers, with floating
point values truncated toward zero.

Each bit in the result is set when the corresponding bit
is set in an odd number of the processed arguments.

Examples:

```
(^ 0xAA 0x55)         ; → 255
(^ 15 8 7)            ; → 0
(^ 42 42)             ; → 0
```

Notes:

- XOR clears bits that appear an even number of times.
- Floating point inputs are truncated toward zero.
- Arguments are processed left to right.

See: [&](#f-andbit), [|](#f-orbit), [~](#f-notbit)

---


<a name="f-notbit"></a>
## ~

```
syntax: (~ int)
```

Description:

Performs a bitwise NOT operation on int. All bits in the
integer are inverted. The result is computed using the
full machine integer width. Floating point values are
truncated toward zero before the operation.

Examples:

```
(format "%X" (~ 0xFFFFFFAA))   ; → "55"
(~ 0xFFFFFFFF)                 ; → 0
(~ 0)                          ; → -1
```

Notes:

- Bitwise NOT flips every bit of the integer.
- Floating point inputs are truncated toward zero.
- The output reflects inversion across the full integer
  width.

See: [&](#f-andbit), [|](#f-orbit), [^](#f-xorbit)

---


<a name="f-colon"></a>
## :

```
syntax: (: sym-function list-object [args ...])
```

Description:

Acts as an operator that resolves a method name inside a
class context and applies it to an object. In Rebel, an
object is a list whose first element is the symbol of
its class context. The class context defines the methods
applicable to instances of that class. The colon operator
constructs a context-qualified symbol by combining the
class name found in the object with sym-function, then
invokes that method with the object and any additional
arguments.

No space is required between the colon and the method
name. The operator enables polymorphism by selecting the
correct method implementation based on the object’s
class. Additional arguments are passed directly to the
method. Inside methods the self function is used to
access or modify elements of the target object.

Examples:

```
(define (Rectangle:area)
  (mul (self 3) (self 4)))

(define (Circle:area)
  (mul (pow (self 3) 2) (acos 0) 2))

(define (Rectangle:move dx dy)
  (inc (self 1) dx)
  (inc (self 2) dy))

(define (Circle:move p dx dy)
  (inc (self 1) dx)
  (inc (self 2) dy))

(set 'r '(Rectangle 5 5 10 20))    ; x y width height
(set 'c '(Circle 1 2 10))          ; x y radius

(:area r)                          ; → 200
(:area c)                          ; → 314.1592654

(map (curry :area) (list r c))     ; → (200 314.1592654)

(:move r 2 3)
r                                  ; → (Rectangle 7 8 10 20)

(:move c 4 5)
c                                  ; → (Circle 5 7 10)
```

Notes:

- The object’s first element determines which namespace
  supplies the method.
- Methods operate on the object via self.
- No whitespace is required between : and the method
  name.
- The operator provides simple polymorphism based on the
  object’s class context.

See: [self](#f-self), [curry](#f-curry)

---


<a name="f-abort"></a>
## abort

```
syntax: (abort int-pid)
syntax: (abort)
```

Description:

Terminates child processes created with spawn. In the
first form, abort sends a termination request to the
specific child identified by int-pid. The process must
have been created using spawn. Processes started via
fork are not affected; destroy must be used for those.

In the second form, abort terminates all child processes
spawned by the current process. The function returns
true when the requested terminations have been issued.

The abort function is part of the Cilk-style process
coordination interface used for structured parallel
execution.

Examples:

```
(abort 2245)        ; → true   ; abort one child

(abort)             ; → true   ; abort all spawned children
```

Notes:

- Only affects processes created by spawn.
- Processes created by fork require destroy.
- Used for synchronization in Cilk-style parallel models.

See: [spawn](#f-spawn), [destroy](#f-destroy)

---


<a name="f-abs"></a>
## abs

```
syntax: (abs num)
```

Description:

Returns the absolute value of num. Integer arguments are
processed in bigint mode. Floating point arguments keep
their fractional part. Negative values are converted to
their positive counterparts, while positive values and
zero remain unchanged.

Examples:

```
(abs -3.5)          ; → 3.5
(abs 0)             ; → 0
(abs -9223372036854775808)   ; → 9223372036854775808
```

Notes:

- Works on both integers and floating point numbers.
- In bigint range, negative minimum values map to their
  positive counterparts through standard absolute-value
  conversion rules.

See: [sign](#f-sign)

---


<a name="f-acos"></a>
## acos

```
syntax: (acos num-radians)
```

Description:

Computes the arc cosine of num-radians. The argument is
interpreted as a floating point value. The result is the
angle whose cosine equals the input value. The output is
given in radians. Values outside the domain [-1, 1]
produce NaN.

Examples:

```
(acos 1)                 ; → 0
(cos (acos 1))           ; → 1
(acos 0)                 ; → 1.5707963268
```

Notes:

- The function returns NaN when the input is outside the
  valid range [-1, 1].
- All calculations use floating point arithmetic.

See: [cos](#f-cos), [asin](#f-asin), [atan](#f-atan)

---


<a name="f-acosh"></a>
## acosh

```
syntax: (acosh num-radians)
```

Description:

Computes the inverse hyperbolic cosine of num-radians.
The result is the value whose hyperbolic cosine equals
the argument. Valid input begins at 1. Values below 1
produce NaN. The return value is a floating point number
expressed in radians.

Examples:

```
(acosh 2)                 ; → 1.316957897
(cosh (acosh 2))          ; → 2
(acosh 0.5)               ; → NaN
```

Notes:

- Domain is [1, ∞). Values < 1 yield NaN.
- Uses floating point arithmetic.

See: [cosh](#f-cosh), [asinh](#f-asinh), [atanh](#f-atanh)

---


<a name="f-add"></a>
## add

```
syntax: (add num-1 [num-2 ...])
```

Description:

Returns the floating point sum of all arguments. Each
value may be an integer or a float, but the result is
always a floating point number. The calculation follows
normal floating point rules. If any argument evaluates
to NaN, the final result is NaN.

Examples:

```
(add 2 3.25 9)           ; → 14.25
(add 1 2 3 4 5)          ; → 15
```

Notes:

- Always returns a floating point number, even when all
  inputs are integers.
- Any NaN produced during evaluation propagates to the
  result.

See: [sub](#f-sub), [mul](#f-mul), [div](#f-div)

---


<a name="f-address"></a>
## address

```
syntax: (address int)
syntax: (address float)
syntax: (address str)
```

Description:

Returns the memory address of an integer, a floating
point number, or a string. The address is an integer
value referring to the underlying storage of the data
object. This function is commonly used when passing
pointers to external C functions imported with import.

The returned address may be used for pointer arithmetic,
as shown in string-access examples. When the argument is
a string passed directly into a C function, the system
automatically supplies the string’s address, so an
explicit call to address is not required in such cases.

The function must be applied only to objects stored in
variables, not to temporary values produced by
expressions. Temporary objects may be relocated or freed
after evaluation, making their addresses invalid.

Examples:

```
(set 's "\001\002\003\004")
(get-char (+ (address s) 3))        ; → 4

(set 'x 12345)

; big-endian architectures
(get-long (address x))              ; → 12345
(get-int (+ (address x) 4))         ; → 12345

; little-endian architectures
(get-int (address x))               ; → 12345

; architecture-independent: integers are 64-bit
(set 'x 1234567890)
(get-long (address x))              ; → 1234567890
```

Notes:

- Intended for use with persistent objects referenced by
  variables.
- Do not use on intermediate expression results.
- Useful for passing pointers to C functions imported
  with import.

See: [get-char](#f-get-char), [get-int](#f-get-int),
[get-long](#f-get-long), [get-float](#f-get-float)

---


<a name="f-amb"></a>
## amb

```
syntax: (amb exp-1 [exp-2 ...])
```

Description:

Evaluates each argument and returns one of the resulting
values chosen at random. All arguments are treated as
independent expressions, and exactly one of them becomes
the final result. The selection is uniform across the
number of supplied expressions.

The random choice is driven by the same internal source
used by rand. To obtain random floating point numbers,
use random, randomize, or normal. To make the selection
sequence repeatable, initialize the generator with seed.

Examples:

```
(amb 'a 'b 'c 'd 'e)       ; → one of: a b c d e

(dotimes (i 10)
  (print (amb 3 5 7)))     ; → 35777535755
```

Notes:

- One and only one expression result is selected.
- Uses the same internal generator as rand.
- Seed can be used to reproduce the random sequence.

See: [rand](#f-rand), [seed](#f-seed), [random](#f-random)

---


<a name="f-and"></a>
## and

```
syntax: (and exp-1 [exp-2 ...])
```

Description:

Evaluates each expression from left to right and returns
the value of the last one. If any expression evaluates
to nil or to the empty list (), evaluation stops
immediately and that value is returned. When called with
no arguments, the result is true.

This operator provides short-circuit behavior: evaluation
continues only as long as all intermediate results are
non-nil and not ().

Examples:

```
(set 'x 10)                         ; → 10
(and (< x 100) (> x 2))             ; → true
(and (< x 100) (> x 2) "passed")    ; → "passed"
(and '())                           ; → ()
(and true)                          ; → true
(and)                               ; → true
```

Notes:

- Stops evaluation at the first nil or ().
- No arguments return true.
- Returns the last evaluated non-nil value when all
  expressions succeed.

See: [or](#f-or), [not](#f-not)

---


<a name="f-append"></a>
## append

```
syntax: (append list-1 [list-2 ...])
syntax: (append array-1 [array-2 ...])
syntax: (append str-1 [str-2 ...])
```

Description:

Builds and returns a new list, array, or string by
concatenating all supplied arguments. The original data
objects are never modified. The behavior depends on the
type of the first argument.

When the first argument is a list, all lists are joined
sequentially into a new list. When the first argument is
an array, each array’s rows are appended to produce a
new array with increased row count. When the first
argument is a string, all strings are concatenated into a
new string. Binary strings containing zero bytes are
preserved accurately.

For linkage characters or custom separators, use join.
Use string to convert values and concatenate in one step.
To modify an existing list or string in place, use extend
or push.

Examples:

```
(append '(1 2 3) '(4 5 6) '(a b))
; → (1 2 3 4 5 6 a b)

(set 'lst '("hello" "world"))
(append lst '("here" "I am"))
; → ("hello" "world" "here" "I am")

(set 'A (array 3 2 (sequence 1 6)))
; → ((1 2) (3 4) (5 6))
(set 'B (array 2 2 (sequence 7 10)))
; → ((7 8) (9 10))

(append A B)
; → ((1 2) (3 4) (5 6) (7 8) (9 10))

(append B B B)
; → ((7 8) (9 10) (7 8) (9 10) (7 8) (9 10))

(set 'more " how are you")
(append "Hello " "world," more)
; → "Hello world, how are you"
```

Notes:

- Does not modify any of the input arguments.
- Preserves binary strings without truncation at zero
  bytes.
- For insertion into an existing list or string, use
  extend or push.

See: [join](#f-join), [extend](#f-extend), [push](#f-push)

---


<a name="f-append-file"></a>
## append-file

```
syntax: (append-file str-filename str-buffer)
```

Description:

Appends the content of str-buffer to the file specified
by str-filename. If the file does not yet exist, it is
created automatically and the function behaves like
write-file. The return value is the number of bytes
written. On failure, the function returns nil.

When operating on local files, sys-error can be queried
for additional information. When the target is a URL,
net-error provides extended diagnostics.

The function accepts both file paths and URLs with the
http:// or file:// prefix. When given an HTTP URL,
append-file behaves like put-url with the
"Pragma: append\r\n" header automatically supplied. If
the remote file does not exist, it is created before the
append. This mechanism can also be used to transfer data
to remote Rebel nodes.

Examples:

```
(write-file "myfile.txt" "ABC")
(append-file "myfile.txt" "DEF")

(read-file "myfile.txt")
; → "ABCDEF"

(append-file "http://example.com/message.txt"
             "More message text.")
```

Notes:

- Returns the number of bytes written or nil on failure.
- Creates the file automatically if it does not exist.
- For HTTP URLs, uses an append-capable request similar
  to put-url.
- For errors, use sys-error (files) or net-error (URLs).

See: [read-file](#f-read-file), [write-file](#f-write-file)

---


<a name="f-apply"></a>
## apply

```
syntax: (apply func list [int-reduce])
syntax: (apply func)
```

Description:

Invokes func using the elements of list as its arguments.
The function in func may be a primitive, a user-defined
function, or a lambda expression. Only functions that
evaluate all of their arguments may be used; special
forms that evaluate selectively (such as dotimes or
case) will fail when used with apply.

In the second syntax form, apply simply calls func
without any arguments.

When int-reduce is supplied, it specifies how many
arguments func consumes. The function is then applied
repeatedly in left-associative order: the result of each
application becomes the first argument of the next
application, and the remaining arguments are taken
successively from list. This mechanism allows
a two-argument function to be extended into a variadic
one.

Examples:

```
(apply + '(1 2 3 4))
; → 10

(set 'lst '(3 4 5))
(apply * lst)
; → 60

(apply sqrt '(25))
; → 5

(apply (lambda (x y) (* x y)) '(3 4))
; → 12

; reduce mode example

(define (gcd_ a b)
  (let (r (% b a))
    (if (= r 0) a (gcd_ r a))))

(define-macro (my-gcd)
  (apply gcd_ (map eval (args)) 2))

(my-gcd 12 18 6)     ; → 6
(my-gcd 12 18 6 4)   ; → 2
```

Notes:

- Only works with functions that evaluate all arguments.
- Special forms (dotimes, case, etc.) cannot be used.
- int-reduce applies func repeatedly in left-associative
  order.

See: [map](#f-map), [lambda](#f-lambda)

---


<a name="f-args"></a>
## args

```
syntax: (args)
syntax: (args int-idx-1 [int-idx-2 ...])
```

Description:

Returns all unbound arguments of the currently executing
function or macro. Only arguments not already matched to
formal parameters remain in this list. The function is
useful for defining functions or macros that accept a
variable number of parameters. For hygienic macros,
args avoids accidental variable capture because the
returned values belong strictly to the caller’s context.

When called with one or more indices, args returns the
corresponding elements from the unbound argument list.
Indices may be nested-access indices, allowing traversal
inside structured data such as lists.

Examples:

```
(define-macro (print-line)
  (dolist (x (args))
    (print x "\n")))
(print-line "hello" "World")
; prints each argument on its own line

(define-macro (foo)
  (print (args 2) (args 1) (args 0)))
(foo x y z)
; → zyx

(define (bar)
  (args 0 2 -1))
(bar '(1 2 (3 4)))
; → 4

; bound vs unbound arguments
(define (f a b)
  (args))

(f 1 2)
; → ()

(f 1 2 3 4 5)
; → (3 4 5)
```

Notes:

- Returns only arguments not already bound to local
  variables of the active function or macro.
- Calling (args) inside a macro and passing it as an
  argument to *another* macro is invalid, because args
  would not be evaluated in the target macro’s
  environment.
- Safe to use as a parameter to functions.

See: [define](#f-define), [define-macro](#f-define-macro)

---


<a name="f-array"></a>
## array

```
syntax: (array int-n1 [int-n2 ...] [list-init])
```

Description:

Creates an array with int-n1 elements. Additional integer
dimensions define a multidimensional array, supporting up
to sixteen dimensions. When list-init is supplied, its
elements initialize the array. If list-init has fewer
elements than required, its contents repeat until all
array positions are filled. Elements may be of any type.

Multidimensional arrays are stored as arrays of arrays.
Arrays provide efficient random indexing compared to
lists. Most list operations work on arrays, but not all;
details depend on the specific function. Replacing whole
rows requires care to ensure the replacement itself is an
array and not a list.

Arrays can be converted to lists using array-list, and
lists can be converted back by flattening them with flat
before constructing a new array.

Serialization using save or source preserves array
structure by writing the necessary array expression.

Examples:

```
(array 5)
; → (nil nil nil nil nil)

(array 5 (sequence 1 5))
; → (1 2 3 4 5)

(array 10 '(1 2))
; → (1 2 1 2 1 2 1 2 1 2)

(set 'a (array 3 4 (sequence 1 12)))
; → ((1 2 3 4) (5 6 7 8) (9 10 11 12))

(setf (a 2 3) 99)
; → 99
a
; → ((1 2 3 4) (5 6 7 8) (9 10 11 99))

(setf (a 1 1) "hello")
a
; → ((1 2 3 4) (5 "hello" 7 8) (9 10 11 99))

(setf (a 1) '(a b c d))
a
; → ((1 2 3 4) (a b c d) (9 10 11 99))

(nth 1 a)
; → (a b c d)

; array indexing and slicing
(a 1)
; → (a b c d)

(a 0 -1)
; → 4

(2 a)
; → ((9 10 11 99))

(-3 2 a)
; → ((1 2 3 4) (a b c d))

(array-list a)
; → ((1 2 3 4) (a b c d) (1 2 3 99))

(set 'lst '((1 2) (3 4)))
(set 'arr (array 2 2 (flat lst)))
; → ((1 2) (3 4))

(array? a)
; → true

(array? (array-list a))
; → nil

; serialization example
(set 'a (array 3 4 (sequence 1 12)))
(save "array.rbl" 'a)
;; saved file includes array expression
```

Notes:

- Up to sixteen dimensions are supported.
- list-init repeats until the array is fully initialized.
- Converting list rows requires care: use arrays, not
  lists, when replacing rows.
- array-list converts arrays to lists; flat assists in
  reconstructing arrays from list data.

See: [array-list](#f-array-list), [array?](#f-arrayp),
[flat](#f-flat)

---


<a name="f-array-list"></a>
## array-list

```
syntax: (array-list array)
```

Description:

Converts an array into a list structure. The conversion
is recursive: every row of a multidimensional array is
turned into a list, and nested arrays become nested
lists. The original array is left unchanged.

Examples:

```
(set 'a (array 3 4 (sequence 1 12)))
; → ((1 2 3 4) (5 6 7 8) (9 10 11 12))

(set 'lst (array-list a))
; → ((1 2 3 4) (5 6 7 8) (9 10 11 12))

(list (array? a) (list? lst))
; → (true true)
```

Notes:

- Produces a structural list representation of the
  array.
- Does not modify the original array.

See: [array](#f-array), [array?](#f-arrayp)

---


<a name="f-arrayp"></a>
## array?

```
syntax: (array? exp)
```

Description:

Checks whether exp is an array. The function returns true
when the expression is an array object and nil otherwise.
Nested or converted list structures do not qualify as
arrays, even when their shape resembles one.

Examples:

```
(set 'M (array 3 4 (sequence 1 4)))
; → ((1 2 3 4) (1 2 3 4) (1 2 3 4))

(array? M)
; → true

(array? (array-list M))
; → nil
```

Notes:

- Returns true only for actual array objects.
- Lists created from arrays using array-list are not
  arrays.

See: [array](#f-array), [array-list](#f-array-list)

---


<a name="f-asin"></a>
## asin

```
syntax: (asin num-radians)
```

Description:

Computes the arcsine of num-radians and returns the
result as a floating point value in radians. The input
must lie within the domain [-1, 1]. Values outside this
range return NaN. The output is the angle whose sine
equals the argument.

Examples:

```
(asin 1)                 ; → 1.570796327
(sin (asin 1))           ; → 1
```

Notes:

- Domain: [-1, 1]. Outside values produce NaN.
- Result is expressed in radians.

See: [sin](#f-sin), [acos](#f-acos), [atan](#f-atan)

---


<a name="f-asinh"></a>
## asinh

```
syntax: (asinh num-radians)
```

Description:

Computes the inverse hyperbolic sine of num-radians and
returns the floating point result in radians. The value
represents the number whose hyperbolic sine equals the
input. All real values are valid; the function has no
domain restrictions.

Examples:

```
(asinh 2)               ; → 1.443635475
(sinh (asinh 2))        ; → 2
```

Notes:

- Accepts all real numbers.
- Result is the inverse of sinh.

See: [sinh](#f-sinh), [acosh](#f-acosh), [atanh](#f-atanh)

---


<a name="f-assoc"></a>
## assoc

```
syntax: (assoc exp-key list-alist)
syntax: (assoc list-exp-key list-alist)
```

Description:

Searches an association list for an entry whose first
element matches the given key. In the first form, exp-key
is a single key expression. The function scans each
member-list of list-alist; when the first element equals
exp-key, that member-list is returned. If no match is
found, the result is nil.

In the second form, list-exp-key is a list of keys used
to traverse nested association lists. Each key selects a
sublist, allowing lookup inside multilevel structures.

When list-alist is a context symbol, assoc operates on
its default functor, enabling large association lists to
be referenced without copying.

assoc may be combined with setf to modify association
lists in place.

Examples:

```
(assoc 1 '((3 4) (1 2)))
; → (1 2)

(set 'db '((kiwi 12) (mango 77 5) (plum 9)))
(assoc 'mango db)
; → (mango 77 5)

(assoc 'papaya db)
; → nil

; modifying an association
(setf (assoc 'plum db) '(plum 10))
db
; → ((kiwi 12) (mango 77 5) (plum 10))

; nested association lists
(set 'records '(
  (u001 (name "Nora")
        (address (country "Japan") (city "Kyoto")))
  (u002 (name "Liam")
        (address (country "Brazil") (city "Recife")))
))

(assoc '(u001 address) records)
; → (address (country "Japan") (city "Kyoto"))

(assoc '(u001 address city) records)
; → (city "Kyoto")

; using a context as the association list
(set 'people:people '(
  (u001 (name "Nora")
        (address (country "Japan") (city "Kyoto")))
  (u002 (name "Liam")
        (address (country "Brazil") (city "Recife")))
))

(define (get-city db id)
  (last (assoc (list id 'address 'city) db)))

(get-city people 'u001)
; → "Kyoto"
```

Notes:

- assoc returns the full matching pair or sublist.
- list-exp-key allows multilevel lookup.
- When used with a context, assoc operates on the
  context's default functor.
- For modifications, combine assoc with setf.
- lookup provides lookup + extraction in one step.

See: [lookup](#f-lookup), [setf](#f-setf)

---


<a name="f-atan"></a>
## atan

```
syntax: (atan num-radians)
```

Description:

Computes the arctangent of num-radians and returns the
result as a floating point value in radians. The output
is the angle whose tangent equals the input. All real
values are valid; the function has no domain
restrictions.

Examples:

```
(atan 1)              ; → 0.7853981634
(tan (atan 1))        ; → 1
```

Notes:

- Accepts any real-valued input.
- Result is expressed in radians.

See: [tan](#f-tan), [asin](#f-asin), [acos](#f-acos)

---


<a name="f-atan2"></a>
## atan2

```
syntax: (atan2 num-Y-radians num-X-radians)
```

Description:

Computes the arctangent of Y / X while using the signs of
both arguments to determine the correct quadrant of the
result. The return value is a floating point angle in
radians. Unlike atan, which cannot distinguish between
opposite quadrants, atan2 provides the full range
(-π, π], making it suitable for converting Cartesian
coordinates to polar form.

Examples:

```
(atan2 1 1)                        ; → 0.7853981634
(div (acos 0) (atan2 1 1))         ; → 2
(atan2 0 -1)                       ; → 3.141592654
(= (atan2 1 2) (atan (div 1 2)))   ; → true
```

Notes:

- Determines the correct quadrant using the signs of X
  and Y.
- Returns a principal value in radians.
- Useful for coordinate transformations.

See: [atan](#f-atan), [sin](#f-sin), [cos](#f-cos)

---


<a name="f-atanh"></a>
## atanh

```
syntax: (atanh num-radians)
```

Description:

Computes the inverse hyperbolic tangent of num-radians.
The result is the value whose hyperbolic tangent equals
the input. The valid domain is (-1, 1). Inputs with an
absolute value greater than 1 produce NaN. An-value ±1
returns positive or negative infinity, respectively.

Examples:

```
(atanh 0.5)              ; → 0.5493061443
(tanh (atanh 0.5))       ; → 0.5
(atanh 1.1)              ; → NaN
(atanh 1)                ; → inf
```

Notes:

- Domain: |x| < 1.  
  |x| > 1 → NaN  
  |x| = 1 → ±inf
- Result is expressed in radians.

See: [tanh](#f-tanh), [asinh](#f-asinh), [acosh](#f-acosh)

---


<a name="f-atomp"></a>
## atom?

```
syntax: (atom? exp)
```

Description:

Checks whether exp evaluates to an atom. An expression is
considered an atom if it evaluates to one of the
following: nil, true, an integer, a float, a string, a
symbol, or a primitive. Structures such as lists,
lambda expressions, lambda-macro expressions, and quoted
lists are not atoms.

Examples:

```
(atom? '(1 2 3))
; → nil

(and (atom? 123)
     (atom? "hello")
     (atom? 'foo))
; → true

(atom? ''foo)
; → nil
```

Notes:

- atom? tests the evaluated expression, not its literal
  form.
- Nested quoting yields non-atomic structures.

See: [list?](#f-listp), [symbol?](#f-symbolp)

---



<a name="f-base64-dec"></a>
## base64-dec

```
syntax: (base64-dec str)
```

Description:

Decodes the BASE64-encoded string in str and returns the
decoded byte sequence as a string. The input is not
validated for correctness; invalid or malformed BASE64
data may yield unspecified output. The implementation is
compatible with RFC 4648 and is derived from routines
used in Unix curl.

Examples:

```
(base64-dec "SGVsbG8gV29ybGQ=")
; → "Hello World"
```

Notes:

- Does not perform strict validation of BASE64 input.
- For encoding, use base64-enc.

See: [base64-enc](#f-base64-enc)

---


<a name="f-base64-enc"></a>
## base64-enc

```
syntax: (base64-enc str [bool-flag])
```

Description:

Encodes str into BASE64 format. BASE64 groups input bytes
in sets of three (24 bits) and produces four encoded
bytes (32 bits). Each 6-bit segment maps to one of 64
characters: A–Z, a–z, 0–9, +, and /. The character = is
used as padding when the input does not align to 3-byte
blocks. The result is a printable representation suitable
for embedding binary data in text-oriented formats.

When bool-flag is omitted or false, the empty string ""
is encoded as "====". When bool-flag is true, the empty
string encodes to "". Both encodings decode back to ""
using base64-dec.

The function returns the encoded string. No line breaks
are inserted; the result is a continuous BASE64 sequence.
The implementation conforms to RFC 4648 and is derived
from Unix curl routines.

Examples:

```
(base64-enc "Hello World")
; → "SGVsbG8gV29ybGQ="

(base64-enc "")
; → "===="

(base64-enc "" true)
; → ""
```

Notes:

- Suitable for encoding binary data for text protocols
  such as XML-RPC.
- The empty-string behavior depends on bool-flag.
- No CR/LF insertion in long sequences.

See: [base64-dec](#f-base64-dec)

---

<a name="f-bayes-query"></a>
## bayes-query

```
syntax: (bayes-query list-L context-D [bool-chain [bool-probs]])
```

Description:

Computes category probabilities for the tokens in list-L
using the trained dictionary context-D. The dictionary
contains token frequencies or probability values for one
or more categories. The function returns one probability
per category.

Two compounding modes exist:

1. Inverse Chi2 mode (default):
   Frequencies are combined using the inverse Chi2
   method. Zero-frequency tokens reduce a category but do
   not remove it.
2. Chain Bayesian mode (bool-chain = true):
   Posterior values become priors for the next token.
   Zero-frequency tokens eliminate the affected category
   from subsequent steps.

When bool-probs is true, dictionary entries are treated
as probabilities instead of raw counts.

The returned list contains probability values in the same
order as the categories stored in context-D. A higher
value means a higher estimated probability that the
tokens in list-L belong to that category. Values are real
numbers in [0.0, 1.0]. In Chain Bayesian mode, eliminated
categories may prevent the list from summing to 1.0.


Examples:

**1. Classification using two short sample texts**

- Category A text: sunny day bright sky
- Category B text: storm rain dark clouds

Token frequencies:

```
(set 'Sample:A '(
  (sunny  1)
  (day    1)
  (bright 1)
  (sky    1)
))

(set 'Sample:B '(
  (storm  1)
  (rain   1)
  (dark   1)
  (clouds 1)
))

(set 'Sample:total '(
  (A-total 4)
  (B-total 4)
))
```

Tokens similar to category A:

```
(bayes-query '(bright sky) Sample)
;-> (0.8571428571 0.1428571428)

; first value = probability for category A
; second value = probability for category B
```

Tokens similar to category B:

```
(bayes-query '(dark rain) Sample)
;-> (0.1428571428 0.8571428571)

; higher value indicates the more likely category
```

**2. Chain Bayesian mode**

```
(set 'C:pos '(8 18))
(set 'C:neg '(2 72))
(set 'C:total '(10 90))
```

```
(bayes-query '(test-positive) C true)
;-> (0.3076923077 0.6923076923)

(bayes-query '(test-positive test-positive) C true)
;-> (0.64 0.36)
```

**3. Using direct probabilities**

```
(set 'P:pos '(0.8 0.2))
(set 'P:neg '(0.2 0.8))
(set 'P:total '(0.1 0.9))
```

```
(bayes-query '(test-positive) P true true)
;-> (0.3076923077 0.6923076923)

(bayes-query '(test-positive test-positive) P true true)
;-> (0.64 0.36)

(bayes-query '(test-positive test-positive test-positive) P true true)
;-> (0.8767123288 0.1232876712)
```

Notes:

- Inverse Chi2 mode tolerates missing tokens.
- Chain Bayesian mode eliminates categories with
  zero-frequency tokens for the observed items.
- list-L should refer to known tokens unless category
  elimination is intended.

See: [bayes-train](#f-bayes-train), [parse](#f-parse)

---


<a name="f-bayes-train"></a>
## bayes-train

```
syntax: (bayes-train list-M1 [list-M2 ... ] sym-context-D)
```

Description:

Builds or updates a Bayesian dictionary inside the context
sym-context-D. Each list Mi represents a category. The
function counts how many times each token appears in each
category and stores the frequencies inside the dictionary.
The returned value is a list of total token counts per
category.

Only symbols and strings are used as valid tokens. String
tokens are converted into internal symbols by prepending
an underscore before insertion. A symbol named total is
created containing the total counts for each category.

The dictionary produced by bayes-train is the data model
used by bayes-query. The order of categories in the lists
is the same order bayes-query uses when returning
probabilities.

Training can occur in one or more stages. When the context
already contains trained data with the same number of
categories, new counts are added and totals updated.

**Relation to bayes-query**

bayes-train constructs the frequency model that
bayes-query evaluates. The minimal text-classification
example shown in bayes-query can be reproduced entirely
using bayes-train. This provides a complete training and
classification workflow inside Rebel.

**Example: two small “documents” forming two categories**

- Category A text: sunny day bright sky 
- Category B text: storm rain dark clouds

These two lists define the categories A and B. Each token
will be counted and stored inside the dictionary Weather.

```
(bayes-train
  '(sunny day bright sky)
  '(storm rain dark clouds)
  'Weather)
;-> (4 4)
```

The context Weather now contains token-frequency entries
for both categories:

```
Weather:sunny   ;-> (1 0)
Weather:day     ;-> (1 0)
Weather:bright  ;-> (1 0)
Weather:sky     ;-> (1 0)

Weather:storm   ;-> (0 1)
Weather:rain    ;-> (0 1)
Weather:dark    ;-> (0 1)
Weather:clouds  ;-> (0 1)

Weather:total   ;-> (4 4)
```

These frequencies form the exact model used by
bayes-query in the sample classification examples.


**Using the trained dictionary for classification**

The same model can now classify short texts:

Tokens typical for category A:

```
(bayes-query '(bright sky) Weather)
;-> (0.8571428571 0.1428571428)

; first value = probability for category A
; second value = probability for category B
```

Tokens typical for category B:

```
(bayes-query '(dark rain) Weather)
;-> (0.1428571428 0.8571428571)

; higher value indicates the more likely category
```

This demonstrates the complete cycle:

- bayes-train builds the model
- bayes-query evaluates new data against that model

**Example: training with strings**

String tokens become symbols by prepending "_". The
following example shows three categories, trained
simultaneously:

```
(bayes-train
  '("one" "two" "two" "three")
  '("three" "one" "three")
  '("one" "two" "three")
  'Nums)
;-> (4 3 3)

Nums:_one    ;-> (1 1 1)
Nums:_two    ;-> (2 0 1)
Nums:_three  ;-> (1 2 1)
Nums:total   ;-> (4 3 3)
```

Trained contexts can be used like hash tables:

```
(Nums "two")   ;-> (2 0 1)
(Nums "three") ;-> (1 2 1)
```

**Explicit model construction without bayes-train**

When the data set is small and token counts are known
directly, the model may be created manually without
training:

```
(set 'Data:tested-positive '(8 18))
(set 'Data:tested-negative '(2 72))
(set 'Data:total '(10 90))
```

Such data sets can still be evaluated using bayes-query.

**Multi-stage training**

Large corpora can be trained in several batches:

```
(bayes-train  docA-part1 '() 'Model)
(bayes-train  docA-part2 '() 'Model)
(bayes-train  '() docB-part1 'Model)
(bayes-train  '() docB-part2 'Model)
```

bayes-train will update all counts and maintain correct
totals. All batches must provide the same number of
categories as the original model.

Notes:

- A category list may be empty to indicate missing
  training data for that category.
- Token order inside lists is irrelevant; only frequency
  matters.
- The total symbol cannot be used as a token name inside
  the training lists.
- Contexts used for training must be created in MAIN when
  training from inside another context.

See: [bayes-query](#f-bayes-query), [context](#f-context)

---

<a name="f-begin"></a>
## begin

```
syntax: (begin body)
```

Description:

Evaluates the expressions in body in sequence and returns
the value of the last one. begin is used to group multiple
expressions where only a single expression is normally
allowed. It is commonly used inside conditional forms.

Many built-in control structures—such as cond, define,
doargs, dolist, dostring, dotimes, when, and while—
already accept multiple expressions in their bodies, but
begin is useful in forms like if, where only one body
expression is permitted.

The silent function behaves like begin but suppresses
console output from the final result.

Examples:

```
(begin
  (print "This is a block of 2 expressions\n")
  (print "================================"))
```

Notes:

- Returns the value of the last expression in body.
- Groups multiple expressions where only one is allowed.
- silent is the same as begin but without printed output.

See: [silent](#f-silent), [if](#f-if), [cond](#f-cond)

---


<a name="f-beta"></a>
## beta

```
syntax: (beta cum-a num-b)
```

Description:

Computes the Beta function of two arguments a and b,
derived from the log Gamma function gammaln. The value
is calculated using the relation

```
beta(a, b) = exp(gammaln(a) + gammaln(b) - gammaln(a + b))
```

and returned as a floating point number. The function is
typically used with positive real parameters.

Examples:

```
(beta 1 2)
; → 0.5
```

Notes:

- Implemented via gammaln to improve numerical stability
  for a wide range of arguments.
- Commonly used in probability and statistics, especially
  in conjunction with the Beta distribution.

See: [gammaln](#f-gammaln)

---


<a name="f-betai"></a>
## betai

```
syntax: (betai num-x num-a num-b)
```

Description:

Computes the incomplete Beta function at point x using
parameters a and b. The result corresponds to the
cumulative probability of the Beta distribution at x.
The function is frequently used to obtain cumulative
binomial probabilities.

The cumulative binomial probability pev of an event with
probability p occurring k or more times in N trials can
be expressed as:

```
pev = Betai(p, k, N - k + 1)
```

Examples:

```
(betai 0.5 3 8)
; → 0.9453125
```

Notes:

- Useful for computing binomial tail probabilities and
  for statistical distributions derived from Beta.
- Parameters a and b must be positive.
- Related to the standard Beta function and the Gamma
  function.

See: [binomial](#f-binomial), [beta](#f-beta)

---


<a name="f-bigint"></a>
## bigint

```
syntax: (bigint number)
syntax: (bigint string)
```

Description:

Converts a number or a numeric string into a big integer.
Floating point inputs are converted by truncating toward
zero after binary-to-decimal transformation, which may
introduce rounding artifacts. Integer inputs convert
exactly.

When the argument is a string, it is parsed from the
beginning until a non-numeric character appears. The
parsed part is then converted into a big integer. A
decimal point ends numeric parsing, so only the integer
portion is used.

The result is returned in big integer format, marked with
an `L` suffix when printed.

Examples:

```
(bigint 12345)
; → 12345L

(bigint 1.234567890e30)
; → 1234567889999999957361000000000L

(set 'n 567890)
(bigint n)
; → 567890L

(bigint "-54321")
; → -54321L

(bigint "123.45")
; → 123L

(bigint "123hello")
; → 123L
```

Notes:

- Floating point inputs may convert inexactly due to
  binary/decimal rounding limits.
- String parsing stops at the first non-numeric
  character.
- Only integer portions of decimal strings are used.

See: [int](#f-int), [float](#f-float)

---


<a name="f-bind"></a>
## bind

```
syntax: (bind list-variable-associations [bool-eval])
```

Description:

Takes an association list of symbol–value pairs and
assigns each symbol its corresponding value. When
bool-eval is omitted or nil, the values are assigned
exactly as they appear. When bool-eval is true, each
value is evaluated before assignment.

The return value of bind is the value of the last
association in the list.

bind is commonly used together with unify, which
produces association lists suitable for destructuring.

Examples:

```
(set 'lst '((a (+ 3 4)) (b "hello")))
(bind lst)
; → "hello"

a  ; → (+ 3 4)
b  ; → "hello"

(bind lst true)
; → "hello"

a  ; → 7


; using bind with unify
(bind (unify '(p X Y a) '(p Y X X)))
; → a

X  ; → a
Y  ; → a


; destructuring example
(set 'structure '((one "two") 3 (four (x y z))))
(set 'pattern   '((A B)       C (D E)))

(bind (unify pattern structure))

A  ; → one
B  ; → "two"
C  ; → 3
D  ; → four
E  ; → (x y z)
```

Notes:

- Assigns all symbols appearing in the association list.
- With bool-eval = true, values are evaluated first.
- Often used with unify for de-structuring patterns.

See: [unify](#f-unify), [set](#f-set)

---


<a name="f-binomial"></a>
## binomial

```
syntax: (binomial int-n int-k float-p)
```

Description:

Computes the probability that an event with probability p
occurs exactly k times in n independent trials. The
function evaluates the classical binomial distribution.
The definition used by Rebel is:

```
binomial = pow(p, k) *
           pow(1.0 - p, n - k) *
           n! / (k! * (n - k)!)
```

The value is returned as a floating point number. The
factorials follow standard integer factorial semantics,
and pow(x, y) denotes exponentiation. This function
represents the *non-cumulative* version of the binomial
distribution.

Examples:

```
(binomial 10 3 0.5)
; → 0.1171875
```

Notes:

- Models the probability of exactly k successes in n
  Bernoulli trials.
- For cumulative binomial probability, use betai.

See: [betai](#f-betai), [beta](#f-beta)

---


<a name="f-bits"></a>
## bits

```
syntax: (bits int [bool])
```

Description:

Converts an integer into either a binary string or a list
of bit values. When bool is omitted or nil, the result is
a string of `'1'` and `'0'` characters, ordered from the
most significant bit to the least significant bit.

When bool evaluates to anything non-nil, bits returns a
list of booleans representing the binary form of the
integer, ordered from the lowest bit to the highest bit:
`true` for bit value 1 and `nil` for bit value 0. This
ordering allows direct indexing and use in flow control.

The function `int` with base 2 is the inverse of bits.

Examples:

```
(bits 1234)
; → "10011010010"

(int (bits 1234) 0 2)
; → 1234

(bits 1234 true)
; → (nil true nil nil true nil true true nil nil true)

((bits 1234 true) 0)
; → nil
```

Notes:

- String form: highest → lowest bit.
- List form: lowest → highest bit (indexable).
- `int` with base `2` reverses bits back to the integer.

See: [int](#f-int), [bit-and](#f-bit-and), [bit-or](#f-bit-or)

---


<a name="f-callback"></a>
## callback

```
syntax: (callback int-index sym-function)
syntax: (callback sym-function str-return-type [str-param-type ...])
syntax: (callback sym-function)
```

Description:

Creates a C-callable function pointer which invokes the
Rebel function stored in sym-function. The mechanism
supports two independent modes. In the simple form the
first parameter selects one of sixteen static callback
slots numbered 0 to 15. Each slot contains a built-in
C function which transfers control into the runtime.
Assigning sym-function to a slot causes external C code
calling the slot pointer to enter the Rebel evaluator
and run the assigned function. This simple form accepts
up to eight parameters as raw integer or pointer
values. The slot pointer remains valid until reassigned
with a new sym-function.

The extended syntax uses libffi closures. A closure is
allocated dynamically and configured with a call
interface describing the return type and parameter
types. When external C code calls the closure address,
libffi performs type conversions, constructs a Rebel
argument list and invokes sym-function. After the
function finishes, libffi converts the returned value
back to the declared C type. This form has no slot
limits and can represent any combination of integer,
floating point or pointer parameters. Pointer values
arriving from C are represented as numbers and may be
inspected using get-int, get-long or get-string as
needed for buffer access.

The third syntax retrieves the previously created
callback pointer for sym-function. This avoids building
additional closures when the same pointer must be
passed to multiple C functions.

Both simple and extended forms coexist. The first form
is useful when a fixed stable pointer is required
without allocating new closures. The extended form is
used when precise type information is needed, or when
a large number of callback functions must be created.

Examples:

```
;; Simple syntax using slot 0
(define (hello)
  ;; External C code calling the slot pointer
  ;; will print this string.
  "hello world")

(set 'p0 (callback 0 'hello))
;; p0 now contains a C-callable pointer. Any C
;; function which receives p0 and calls it will
;; execute (hello).

;; Extended syntax with typed parameters
(define (sum a b)
  (+ a b))

(set 'ps (callback 'sum "int" "int" "int"))
;; ps is a dynamically created libffi closure
;; which accepts two integers and returns an
;; integer result.
```

Notes:

- Simple callbacks accept integer or pointer values
  without conversions.
- Extended callbacks use libffi and support precise
  return and parameter types.
- The third syntax retrieves the existing callback
  pointer for reuse.

See: [import](#f-import), [get-int](#f-get-int), [get-long](#f-get-long),
[pack](#f-pack), [unpack](#f-unpack)

---


<a name="f-case"></a>
## case

```
syntax: (case exp-switch (exp-1 body-1) [(exp-2 body-2) ... ])
```

Description:

Evaluates exp-switch, then compares its value against
each clause expression. Clause expressions are not
evaluated; they are matched literally. When a match is
found, all forms in the matching body are evaluated in
sequence, and the value of the final form becomes the
result of the case expression. If none of the clause
values match exp-switch, the body of the last clause is
executed and serves as a default branch.

Examples:

```
(define (translate n)
  (case n
    (1 "one")
    (2 "two")
    (3 "three")
    (4 "four")
    (true "Can't translate this")))

(translate 3)   → "three"
(translate 10)  → "Can't translate this"
```

See: [if](#f-if), [cond](#f-cond), [when](#f-when)

---


<a name="f-catch"></a>
## catch

```
syntax: (catch exp symbol)
syntax: (catch exp)
```

Description:

Evaluates exp under a handler capable of intercepting
throws and runtime errors. In the first syntax catch
returns a status flag and stores the outcome of exp in
symbol. When evaluation finishes normally, catch
returns true and symbol receives the value of exp. When
a runtime error occurs, catch returns nil and symbol is
set to the formatted error message. When a throw is
triggered during evaluation, catch returns true and
symbol receives the argument supplied to throw. This
form is used when errors or explicit throws are part of
normal program flow and must be handled without aborting
the surrounding computation.

In the second syntax catch returns the normal value of
exp, or—if a throw is executed during evaluation—the
argument passed to throw. All intermediate evaluation
frames are discarded and control jumps directly to the
catch expression. This form is commonly used for
breaking out of nested iteration or forcing an early
return from functions or expression blocks.

Both forms install a temporary handler active only for
the dynamic extent of exp.

Examples:

```
(catch (dotimes (x 1000) 
  (if (= x 500) (throw x))))  → 500

(define (foo x)
   …
  (if condition (throw 123))
    …
  456)

(catch (foo p))  → 123       ;; condition true
(catch (foo p))  → 456       ;; condition false

(catch (func 3 4) 'result)  → nil
result  
→ "ERR: invalid function in function catch : (func 3 4)"

(constant 'func +)
(catch (func 3 4) 'result)  → true
result                      → 7

(catch (dotimes (x 100) 
  (if (= x 50) (throw "fin"))) 'result)  → true
result  → "fin"
```

See: [throw](#f-throw), [throw-error](#f-throw-error),
[error-event](#f-error-event), [error-number](#f-error-number),
[error-text](#f-error-text)

---


<a name="f-ceil"></a>
## ceil

```
syntax: (ceil number)
::-

Description:

Returns the smallest integer value not less than number.
The result is returned as a floating-point value. When
number is already an integer value, it is returned
unchanged. Negative arguments are rounded upward toward
zero, positive arguments are rounded upward away from
zero.

Examples:

```
(ceil -1.5)  → -1
(ceil 3.4)   → 4
```

See: [floor](#f-floor)

---


<a name="f-change-dir"></a>
## change-dir

```
syntax: (change-dir str-path)
```

Description:

Changes the current working directory to str-path. When
the directory change succeeds, the function returns
true. If the path does not exist or cannot be used as a
working directory, the function returns nil. The change
affects all subsequent file-system operations performed
by the current Rebel process.

Examples:

```
(change-dir "/etc")
```

See: [current-dir](#f-current-dir)

---


<a name="f-char"></a>
## char [utf8]

```
syntax: (char str [int-index [true]])
syntax: (char int)
```

Description:

Extracts a character from a string or constructs a
single-character string from an integer code. When the
first argument is a string, char selects the character
at int-index and returns its numeric code. If
int-index is omitted, index 0 is used. Negative indexes
address positions from the end of the string. On UTF-8
enabled builds the index normally refers to logical
UTF-8 characters. When the optional true flag is
supplied, the string is treated as a raw byte array and
the index refers to an 8-bit byte offset.

An empty string yields nil. Both (char 0) and (char nil)
return the single byte string "\000".

When the first argument is an integer, char returns a
string containing the character with that code. On
UTF-8 enabled builds the integer is interpreted as a
Unicode code point and a UTF-8 encoded character is
returned.

Examples:

```
(char "ABC")          → 65
(char "ABC" 1)        → 66
(char "ABC" -1)       → 67
(char "B")            → 66
(char "Ω")            → 937
(char "Ω" 1 true)     → 169

(char 65)  → "A"
(char 66)  → "B"

(char (char 65))  → 65

(map char (sequence 1 255))

(char (& (char "生") (char "死"))) → 愛
```

See: [setf](#f-setf), [slice](#f-slice), [length](#f-length)

---


<a name="f-chop"></a>
## chop [utf8]

```
syntax: (chop str [int-chars])
syntax: (chop list [int-elements])
```

Description:

Returns a copy of the given string or list with the
specified number of elements removed from the end. When
the first argument is a string, chop omits the last
int-chars characters and returns the shortened copy. If
int-chars is omitted, one character is removed. The
original string is not modified. On UTF-8 enabled
builds character counting refers to logical UTF-8
characters.

When the first argument is a list, chop returns a
shallow copy of the list with int-elements removed from
the end. If int-elements is omitted, one element is
removed. The original list remains unchanged.

Examples:

```
(set 'str "Rebelion")  → "Rebelion"

(chop str)    → "Rebelio"
(chop str 2)  → "Rebeli"

str  → "Rebelion"

(set 'lst '(a b (c d) e))

(chop lst)    → (a b (c d))
(chop lst 2)  → (a b)

lst  → (a b (c d) e)
```

See: [slice](#f-slice), [append](#f-append), [join](#f-join)

---


<a name="f-clean"></a>
## clean

```
syntax: (clean exp-predicate list)
```

Description:

Applies exp-predicate to each element of list and
returns a new list containing only those elements for
which the predicate evaluates to false. Elements for
which the predicate returns true are removed. The
behavior is equivalent to using filter with a negated
predicate. The predicate may be any built-in test,
user-defined function or lambda expression. The order
of elements is preserved in the returned list.

Examples:

```
(clean symbol? '(1 2 d 4 f g 5 h))   → (1 2 4 5)

(filter symbol? '(1 2 d 4 f g 5 h))  → (d f g h)

(define (big? x) (> x 5))        → (lambda (x) (> x 5))

(clean big? '(1 10 3 6 4 5 11))  → (1 3 4 5)

(clean <= '(3 4 -6 0 2 -3 0))  → (3 4 2)

(clean (curry match '(a *)) '((a 10) (b 5) (a 3) (c 8) (a 9)))
→ ((b 5) (c 8))
```

See: [filter](#f-filter), [index](#f-index),
[difference](#f-difference), [intersect](#f-intersect)

---


<a name="f-close"></a>
## close

```
syntax: (close int-file)
```

Description:

Closes the file or device associated with int-file. The
value in int-file must be a file handle previously
obtained from an open operation or from device. When
the close operation succeeds, the function returns
true; if the handle is invalid or the operation fails,
nil is returned. Closing the device handle resets it to
0, restoring the screen device as the active output
target.

Examples:

```
(close (device))  → true
(close 7)         → true
(close handle)   → true
```

See: [open](#f-open), [read](#f-read), [write](#f-write),
[device](#f-device)

---


<a name="f-collect"></a>
## collect

```
syntax: (collect exp [int-max-count])
```

Description:

Repeatedly evaluates exp and accumulates each non-nil
result into a list. Evaluation continues until exp
returns nil, which terminates the collection, or until
int-max-count elements have been gathered when the
optional limit is specified. Each iteration reevaluates
exp from scratch, and all collected values are appended
to the result list in the order produced. If the first
evaluation of exp yields nil, an empty list is
returned.

Examples:

```
(set 'x 0)
(collect (if (<= (inc x) 10) x))
→ (1 2 3 4 5 6 7 8 9 10)

(set 'x 0)
(collect (if (<= (inc x) 10) x) 6)
→ (1 2 3 4 5 6)
```

See: [while](#f-while), [dotimes](#f-dotimes),
[for](#f-for)

---


<a name="f-command-event"></a>
## command-event

```
syntax: (command-event sym-event-handler)
syntax: (command-event func-event-handler)
syntax: (command-event nil)
```

Description:

Installs a user-defined handler which receives each
command line before it is evaluated. The handler must
return a string. The returned string becomes the line
executed by the interpreter. Returning the empty string
prints a prompt without evaluating any expression.
Passing nil removes the currently installed handler.

The handler may be a symbol naming a function or a
lambda expression. In interactive mode the handler can
rewrite, filter or suppress input lines before they
reach the evaluator, allowing full customization of
REPL input behavior. In http mode (enabled with the
-http option) the handler receives the raw HTTP request
line and may normalize, rewrite or block requests
before they are dispatched to the built-in HTTP
processor. Any non-string return value leaves the
original input line unchanged.

Examples:

```
;; treat plain words as shell commands
(command-event
  (fn (s)
    (if (starts-with s "[a-zA-Z]" 0)
        (append "!" s)
        s)))

;; set a prompt and add simple command translation
(prompt-event
  (fn (ctx)
    (append (real-path) "> ")))

(command-event
  (fn (s)
    (if
        (starts-with s "cd")
        (string " " (true? (change-dir (last (parse s " ")))))

        (starts-with s "[a-zA-Z]" 0)
        (append "!" s)

        true s)))
```

Examples (http mode):

```
;; httpd-conf.rbl
;; filter and rewrite HTTP request lines in -http mode
;; block access to *.ext resources

(command-event
  (fn (s)
    (let (request s)
      (when (find "?" s)
        (set 'request (first (parse s "?")))
        (when (ends-with request ".ext")
          (set 'request "GET /forbidden.html")) )
      request)))

;; Running:
;;   rebel httpd-conf.rbl -http -d80 -w ./httpdoc
;;
;; Incoming:
;;   GET /secret.ext?user=abc
;;
;; Translated to:
;;   GET /forbidden.html
```

Notes:

- The handler must return a string. Any non-string
  return value leaves the original input line unchanged.
- Returning the empty string suppresses evaluation and
  prints only a prompt.
- In http mode the handler receives unmodified HTTP
  request lines. The rewritten line is passed to the
  built-in HTTP processor.
- The maximum length of any input command line or HTTP
  header line is limited to 512 characters.

See: [prompt-event](#f-prompt-event)

---


<a name="f-cond"></a>
## cond

```
syntax: (cond (exp-condition-1 body-1) [(exp-condition-2 body-2) ... ])
```

Description:

Evaluates each exp-condition in order until one of them
yields a value other than nil or the empty list. When a
condition succeeds, its corresponding body is evaluated
and the resulting value becomes the value of the entire
cond expression. If a body is omitted, the value of the
successful exp-condition is returned. When no condition
succeeds, the value of the last evaluated conditional
expression is returned, which is typically nil or an
empty list.

cond is used for multi-branch conditional evaluation. It
behaves like a generalized form of if. When several
conditions must be tested in sequence, cond avoids
nested if forms and allows compact expression of
multiple branches. If used with several condition/body
pairs, the function if behaves similarly but requires no
extra parentheses around each pair.

Examples:

```
(define (classify x)
  (cond
    ((< x 0) "negative")
    ((< x 10) "small")
    ((< x 20) "medium")
    ((>= x 30) "big")))

(classify 15)   → "medium"
(classify 22)   → "nil"
(classify 100)  → "big"
(classify -10)  → "negative"

(cond ((+ 3 4)))  → 7
```

See: [if](#f-if), [and](#f-and), [or](#f-or)

---


<a name="f-cons"></a>
## cons

```
syntax: (cons exp-1 exp-2)
```

Description:

Creates a new list by combining the values of exp-1 and
exp-2. When exp-2 evaluates to a list, its elements are
prefixed with the value of exp-1. When exp-2 evaluates
to anything other than a list, a two-element list is
constructed from the evaluated arguments. The function
never produces dotted pairs; combining two non-list
values always yields a proper list.

If exp-2 is nil, the result is a list whose first element
is the value of exp-1 and whose second element is nil.
Because nil is a Boolean value rather than an empty list,
this behavior differs from dialects where nil is treated
as an empty list or as a synonym for it.

When called with a single argument, cons produces a list
containing that argument. When called without arguments,
it returns an empty list. The operation is effectively
the inverse of extracting the head and tail of a list
using first and rest; when a list has exactly two
elements, first and last form the corresponding pair.

Examples:

```
(cons 'a 'b)            → (a b)
(cons 'a '(b c))        → (a b c)
(cons (+ 3 4) (* 5 5))  → (7 25)
(cons '(1 2) '(3 4))    → ((1 2) 3 4)
(cons nil 1)            → (nil 1)
(cons 1 nil)            → (1 nil)
(cons 1)                → (1)
(cons)                  → ()

(cons (first '(a b c)) (rest '(a b c)))  → (a b c)
(cons (first '(x y)) (last '(x y)))      → (x y)
```

See: [first](#f-first), [rest](#f-rest), [last](#f-last)

---


<a name="f-constant"></a>
## constant [!]

```
syntax: (constant sym-1 exp-1 [sym-2 exp-2] ...)
```

Description:

Assigns values to symbols and marks those symbols as
protected. A protected symbol cannot be modified by set,
define or define-macro; any attempt to overwrite it
raises an error. A protected symbol can only be changed
by calling constant again. Only symbols in the current
context may be protected, preventing accidental changes
to names defined in other contexts. The last initializer
expression is optional.

Symbols created with set, define or define-macro can be
protected retroactively by using constant on them. Since
a function definition is internally just an assignment
of a lambda value to a symbol, protecting a function name
behaves the same way as protecting a variable.

The final value assigned by constant is returned as the
result of the call.

Examples:

```
(constant 'aVar 123)  → 123
(set 'aVar 999)
ERR: symbol is protected in function set: aVar

(define (double x) (+ x x))

(constant 'double)

;; equivalent to

(constant 'double (fn (x) (+ x x)))

(constant 'squareroot sqrt)  → sqrt <406C2E>
(constant '+ add)            → add <4068A6>
```

Notes:

- Protected symbols can only be reassigned by constant.
- Only symbols in the current context may be protected.
- Renaming built-in functions using constant incurs no
  performance penalty. The displayed hexadecimal address
  is the internal pointer to the function and is platform
  dependent.

See: [set](#f-set), [define](#f-define), [define-macro](#f-define-macro)

---


<a name="f-context"></a>
## context

```
syntax: (context [sym-context])
syntax: (context sym-context str | sym [exp-value])
```

Description:

Switches the current namespace to sym-context. Any
symbols created by evaluating expressions, loading
source files or using eval-string are placed into the
active context. When the context does not yet exist, it
is created. Calling context without arguments returns
the currently active context. Because context symbols
evaluate to themselves, an existing context may be
activated without quoting.

Symbols inside the active context are referenced by
their plain names. Symbols in other contexts are
accessed using the prefix context-name followed by a
colon. Assignments may target symbols in any context
using the same prefix form. A symbol reference using a
context prefix implicitly creates the context if it does
not yet exist.

Contexts can be copied with new. The copy behaves as an
independent namespace whose symbols can be accessed
directly or by a variable holding a reference to that
context.

In the second syntax, context creates or updates a
symbol inside sym-context without switching to it. This
form acts as namespace-based key/value storage. It does
not change the active context and should not be used as
a general hash mechanism.

Examples:

```
;; create and switch to GRAPH
(context 'GRAPH)

(define (draw x y) (+ x y))
(set 'value 123)

(symbols) → (draw value)

;; switch back to MAIN
(context MAIN)

GRAPH:value        → 123
(GRAPH:draw 2 3)   → 5
(set 'GRAPH:value 999)
GRAPH:value        → 999

;; implicit context creation
(set 'person:age 0)
(set 'person:address "")
person:age         → 0

;; copy a context
(new person 'JohnDoe) → JohnDoe
(set 'JohnDoe:age 99)
JohnDoe:age            → 99

;; refer to a context through a variable
(set 'human JohnDoe)
human:age              → 99
(set 'human:address "1 Main Street")
JohnDoe:address        → "1 Main Street"

;; switching using an evaluated context
(context 'ctxA)
(context MAIN)
(set 'old ctxA)
(context 'ctxB)
(context MAIN:old)  → ctxA

;; second syntax: store values in a namespace without switching
(context 'Cfg "mode" "debug") → "debug"
(context 'Cfg 'level 3)       → 3

Cfg:mode  → "debug"
Cfg:level → 3
```

Notes:

- Contexts are created implicitly when referenced using
  the prefix form (ctx:symbol).
- Two contexts may contain symbols with the same name
  without interfering with each other.
- If a plain symbol already exists, referring to it as a
  context redefines it as a context.
- The second syntax only stores data; it does not switch
  the active context.

See: [context?](#f-contextq), [new](#f-new), [symbols](#f-symbols)

---


<a name="f-contextp"></a>
## context?

```
syntax: (context? exp)
syntax: (context? exp str-sym)
```

Description:

Predicate for testing context values. In the first
syntax, context? returns true only when exp evaluates to
a context; otherwise it returns nil.

In the second syntax, exp must evaluate to a context.
The function then checks whether a symbol named str-sym
exists inside that context. The symbol name must be
given as a string. The result is true when the symbol is
present and nil when it is absent.

Examples:

;; test for context value
(context? MAIN)        → true

(set 'x 123)
(context? x)           → nil

;; implicit context creation and referencing
(set 'data:msg "hello")
(set 'ctx data)
(context? ctx)         → true

;; test for symbol existence inside a context
(context? data "msg")  → true
(context? data "xy")   → nil

Notes:

- The second syntax does not evaluate str-sym; it must be
  a literal symbol name given as a string.
- The function only tests existence. It does not read or
  modify symbols inside the context.

See: [context](#f-context), [symbols](#f-symbols)

---



<a name="f-copy"></a>
## copy

```
syntax: (copy exp)
syntax: (copy int-addr [bool-flag])
```

Description:

Creates a new independent value from the evaluation of
exp. Some functions modify their arguments in place,
changing the original list, array, or string. Wrapping
the argument with copy forces these functions to operate
on a duplicate, leaving the source data unchanged. The
returned value is the modified duplicate, while the
original stays intact.

A second form exists for low-level interop. When
bool-flag is true, int-addr is treated as a pointer to a
Rebel expression stored in memory. copy retrieves a full
duplicate of that expression. This mode is useful for
bridging external C routines that construct Rebel data in
native memory and expose only an address. The duplicate
behaves as a normal Rebel value and does not reference
the original memory block.

Examples:

```
(set 'lst '(a b c d e f)) ;-> (a b c d e f)

(replace 'c (copy lst)) ;-> (a b d e f)
lst ;-> (a b c d e f)

(set 'str "rebel-lang") ;-> "rebel-lang"
(rotate (copy str)) ;-> "grebel-lan"
str ;-> "rebel-lang"

(set 'x "hello world") ;-> "hello world"
(copy x) ;-> "hello world"

(copy (first (dump x)) true) ;-> "hello world"
```

See: [replace](#f-replace), [rotate](#f-rotate), [dump](#f-dump)

---


<a name="f-copy-file"></a>
## copy-file

```
syntax: (copy-file str-from-name str-to-name)
```

Description:

Copies the file located at str-from-name into the path
specified by str-to-name. The function returns true when
the operation completes successfully. If the copy fails,
the result is nil. Failures may occur when the source
does not exist, the destination cannot be written, or an
I/O error interrupts the transfer.

Examples:

```
(copy-file "/home/me/rebel/data.rbl" "/tmp/data.rbl") ;-> true
```

See: [rename-file](#f-rename-file), [delete-file](#f-delete-file)

---


<a name="f-corr"></a>
## corr

```
syntax: (corr list-vector-X list-vector-Y)
```

Description:

Computes the Pearson product-moment correlation between
two numerical vectors. list-vector-X and list-vector-Y
must have the same length. The function returns six
numeric values describing the linear relationship
between the variables, including effect size, regression
parameters, and significance estimates.

Returned values:

```
r       correlation coefficient
b0      regression intercept
b1      regression slope
t       t statistic for the slope
df      degrees of freedom
p       two-tailed probability for t
```

Examples:

```
(set 'study-time '(90 100 130 150 180 200 220 300 350 400))
;-> (90 100 130 150 180 200 220 300 350 400)

(set 'test-errors '(25 28 20 20 15 12 13 10 8 6))
;-> (25 28 20 20 15 12 13 10 8 6)

(corr study-time test-errors)
;-> (-0.926 29.241 -0.064 -6.944 8 0.0001190)
```

See: [stats](#f-stats), [mean](#f-mean), [pow](#f-pow)

---


<a name="f-cos"></a>
## cos

```
syntax: (cos num-radians)
```

Description:

Computes the cosine of num-radians and returns the
floating-point result. The argument is interpreted as an
angle in radians. The function accepts any real number
and produces a value in the range -1 to 1.

Examples:

```
(cos 1) ;-> 0.5403023059

(set 'pi (mul 2 (acos 0))) ;-> 3.141592654
(cos pi) ;-> -1
```

See: [sin](#f-sin), [tan](#f-tan), [acos](#f-acos), [mul](#f-mul)

---


<a name="f-cosh"></a>
## cosh

```
syntax: (cosh num-radians)
```

Description:

Computes the hyperbolic cosine of num-radians. The
mathematical definition is (exp x + exp -x) / 2. The
result is always positive and grows rapidly for large
inputs. Extremely large arguments may overflow and return
inf.

Examples:

```
(cosh 1) ;-> 1.543080635
(cosh 10) ;-> 11013.23292
(cosh 1000) ;-> inf

(= (cosh 1) (div (add (exp 1) (exp -1)) 2))
;-> true
```

See: [sinh](#f-sinh), [tanh](#f-tanh), [exp](#f-exp), [div](#f-div)

---


<a name="f-count"></a>
## count

```
syntax: (count list-1 list-2)
::-

Description:

Counts how many times each element from list-1 appears
inside list-2. The function returns a list of counts in
the same order as elements in list-1. list-1 should
contain unique items. If list-1 contains duplicates, only
the first occurrence receives a count, and all following
duplicates receive 0.

This function is often used to generate frequency tables.
Any type of comparable element may be counted, including
numbers, symbols, and strings.

Examples:

```
(count '(1 2 3) '(3 2 1 4 2 3 1 1 2 2))
;-> (3 4 2)

(count '(z a) '(z d z b a z y a))
;-> (3 2)

(set 'lst (explode (read-file "myFile.txt")))
;-> list of characters

(set 'letter-counts (count (unique lst) lst))
;-> frequency data
```

See: [unique](#f-unique), [explode](#f-explode),
[read-file](#f-read-file)

---


<a name="f-cpymem"></a>
## cpymem

```
syntax: (cpymem int-from-address int-to-address int-bytes)
```

Description:

Copies raw memory from one address to another. The first
argument specifies the source address, the second the
destination, and the third the number of bytes to copy.
This function operates directly on process memory and can
modify any Rebel value or internal cell structure.

cpymem is intended only for situations where low-level
memory access is required. Incorrect use may corrupt
data, destabilize the interpreter, or crash the entire
process. It is primarily useful when interacting with C
code that exposes raw memory, or when constructing or
inspecting internal cell layouts at a byte level.

Examples:

```
(set 's "0123456789") ;-> "0123456789"

(cpymem "xxx" (+ (address s) 5) 3)
s ;-> "01234xxx89"
```

Example: building a tiny machine-code function

The following shows how cpymem may attach executable
bytes to a function cell and run native code. This works
only on platforms where executable memory and calling
conventions permit it.

```
; 32-bit x86 add-two-integers machine code
(set 'foo-code
  (append
    (pack "bbbbbbbbbb"
      0x55 0x8B 0xEC 0x8B 0x45 0x08 0x03 0x45 0x0C 0x5D)
    (pack "b" 0xC3)))

; create function cell template
(constant 'foo print)

; write call type tag
(cpymem (pack "ld" 4360) (first (dump foo)) 4)

; write code pointer
(cpymem (pack "ld" (address foo-code))
        (+ (first (dump foo)) 12) 4)

; copy symbol name address
(set 'sym-name (first (unpack "lu" (+ (address 'foo) 8))))
(cpymem (pack "ld" sym-name)
        (+ (first (dump foo)) 8) 4)

(foo 3 4)
;-> 7
```

See: [dump](#f-dump), [address](#f-address), [pack](#f-pack),
[unpack](#f-unpack), [append](#f-append)

---


<a name="f-cpymem"></a>
## cpymem

```
syntax: (cpymem int-from-address int-to-address int-bytes)
::-

Description:

Copies raw memory from one address to another. The first
argument specifies the source address, the second the
destination, and the third the number of bytes to copy.
The function operates directly on process memory and can
alter any Rebel value or internal cell structure.

cpymem is intended only for situations requiring
low-level memory manipulation. Incorrect use may corrupt
data, break internal invariants, or crash the process.
It is suitable for exchanging binary blocks with C code
or inspecting memory layouts obtained through dump and
address.

Examples:

```
(set 's "0123456789") ;-> "0123456789"

(cpymem "xxx" (+ (address s) 5) 3)
s ;-> "01234xxx89"
```

Notes:

- Writing into memory that was not allocated by Rebel
  may cause immediate crashes.
- Executable code injection or function construction is
  platform-specific and not guaranteed to work on
  64-bit UNIX systems.

See: [dump](#f-dump), [address](#f-address),
[pack](#f-pack), [unpack](#f-unpack)

---


<a name="f-crc32"></a>
## crc32

```
syntax: (crc32 str-data)
```

Description:

Computes a 32-bit CRC (Cyclic Redundancy Check) value for
the bytes contained in str-data. The calculation starts
with an initial CRC of 0xffffffff and processes each byte
using the standard polynomial method published by w3.org.
The result is an unsigned 32-bit integer suitable for
basic integrity checks.

CRC values are typically used to detect corruption in
data streams, files, or network transfers where the data
source is not considered fully reliable.

Examples:

```
(crc32 "abcdefghijklmnopqrstuvwxyz")
;-> 1277644989
```

See: [md5](#f-md5), [sha256](#f-sha256), [read-file](#f-read-file)

---


<a name="f-crit-chi2"></a>
## crit-chi2

```
syntax: (crit-chi2 num-probability int-df)
```

Description:

Computes the critical Chi^2 value for a given confidence
probability and degrees of freedom. The returned number
is the threshold above which an observed Chi^2 statistic
would be considered significant under the null
hypothesis. num-probability is the confidence level, and
int-df is the number of degrees of freedom.

This function is typically used in hypothesis testing to
find the cutoff point for Chi^2 distributions. A higher
probability yields a larger critical value.

Examples:

```
(crit-chi2 0.01 4)
;-> 13.27670443
```

See: [prob-chi2](#f-prob-chi2)

---


<a name="f-crit-f"></a>
## crit-f

```
syntax: (crit-f num-probability int-df1 int-df2)
```

Description:

Computes the critical value of the F distribution for a
given confidence probability. num-probability is the
desired confidence level, and int-df1 and int-df2 are the
numerator and denominator degrees of freedom. The
returned number is the cutoff point above which an
observed F statistic would be considered significant
under the null hypothesis.

This function is used in variance comparisons and general
F-tests where the shape of the distribution depends on
two separate degrees of freedom.

Examples:

```
(crit-f 0.05 10 12)
;-> 2.753386727
::-

See: [prob-f](#f-prob-f)

---


<a name="f-crit-t"></a>
## crit-t

```
syntax: (crit-t num-probability int-df)
```

Description:

Computes the critical value of the Student t distribution
for a given confidence probability and degrees of
freedom. num-probability specifies the desired confidence
level, and int-df is the number of degrees of freedom.
The returned value is the threshold above which an
observed t statistic would be judged significant under
the null hypothesis.

This function is commonly used in one-sample and
two-sample t-tests to determine whether the magnitude of
a t statistic is large enough to reject the null
hypothesis.

Examples:

```
(crit-t 0.05 14)
;-> 1.761310142
```

See: [prob-t](#f-prob-t)

---


<a name="f-crit-z"></a>
## crit-z

```
syntax: (crit-z num-probability)
```

Description:

Computes the critical value of the standard normal
distribution for a given cumulative probability.
num-probability specifies the point on the Z curve where
the cumulative area reaches that probability. The
returned value is used when evaluating statistical
significance or constructing confidence intervals based
on normal theory.

Examples:

```
(crit-z 0.999)
;-> 3.090232372
```

See: [prob-z](#f-prob-z)

---


<a name="f-current-line"></a>
## current-line

```
syntax: (current-line)
```

Description:

Returns the string most recently read by read-line. This
value is also used implicitly by write-line when no
explicit string argument is supplied. current-line is
useful for implementing streaming text filters that
process input line by line.

Examples:

```
#!/usr/bin/env rebel

(set 'inFile (open (main-args 2) "read"))

(while (read-line inFile)
  (if (starts-with (current-line) ";;")
      (write-line)))

(exit)
```

Invoking the script:

```
./filter myfile.rbl
```

Displays all lines beginning with ";;" from the given
file.

See: [read-line](#f-read-line), [write-line](#f-write-line),
[main-args](#f-main-args), [starts-with](#f-starts-with)

---


<a name="f-curry"></a>
## curry

```
syntax: (curry func exp)
```

Description:

Creates a new single-argument function by fixing the
first argument of a two-argument function. curry does not
evaluate func or exp when it is called. Instead, both are
captured and evaluated later when the resulting function
is invoked. The transformation turns a function f(x,y)
into a new function g(y) that behaves like f(exp, y).

This mechanism is useful for building specialized
predicates, filters, and mapping functions without
creating explicit lambda expressions.

Examples:

```
(set 'f (curry + 10))
;-> (lambda ($x) (+ 10 $x))

(f 7)
;-> 17

(filter (curry match '(a *))
        '((a 10) (b 5) (a 3) (c 8) (a 9)))
;-> ((a 10) (a 3) (a 9))

(clean (curry match '(a *))
       '((a 10) (b 5) (a 3) (c 8) (a 9)))
;-> ((b 5) (c 8))

(map (curry list 'x) (sequence 1 5))
;-> ((x 1) (x 2) (x 3) (x 4) (x 5))
```

See: [lambda](#f-lambda), [apply](#f-apply),
[match](#f-match), [map](#f-map), [filter](#f-filter),
[clean](#f-clean)

---


<a name="f-date"></a>
## date [utf8]

```
syntax: (date)
syntax: (date int-secs [int-offset])
syntax: (date int-secs int-offset str-format)
```

Description:

Returns a string representation of the current local date
and time. When int-secs is supplied, it is interpreted as
a UTC-based timestamp and converted to local time. An
optional int-offset, in minutes, adjusts the timestamp
before conversion. Invalid int-secs produces nil. When a
custom str-format is given, formatting is performed by
the system strftime implementation. Invalid formats
produce "".

Format specifiers:

```
%a   abbreviated weekday name
%A   full weekday name
%b   abbreviated month name
%B   full month name
%c   locale date and time representation
%d   day of month (01–31)
%H   hour (00–23)
%I   hour (01–12)
%j   day of year (001–366)
%m   month (01–12)
%M   minute (00–59)
%p   am/pm indicator
%S   second (0–61, leap seconds)
%U   week number, Sunday first week
%w   weekday number, Sunday = 0
%W   week number, Monday first week
%x   locale date without time
%X   locale time without date
%y   year without century (00–99)
%Y   year with century
%z   time-zone offset/name
%Z   time-zone abbreviation
%%   literal '%'
```

Examples:

```
(date)
;-> "Fri Oct 29 09:56:58 2004"

(date (date-value))
;-> "Sat May 20 11:37:15 2006"

(date (date-value) 300)
;-> "Sat May 20 16:37:19 2006"

(date 0)
;-> "Wed Dec 31 16:00:00 1969"

(date 0 (now 0 -2))
;-> "Thu Jan  1 00:00:00 1970"
```

Locale examples:

```
(set-locale "de_DE.UTF-8")

(date (date-value) 0 "%A %-d. %B %Y")
;-> "Montag  7. März 2005"

(set-locale "C")

(date (date-value) 0 "%A %B %d %Y")
;-> "Monday March 07 2005"

(set-locale "de_DE.UTF-8")

(date (date-value) 0 "%x")
;-> "07.03.2005"

(set-locale "C")

(date (date-value) 0 "%x")
;-> "03/07/05"
```

Notes:

- "%-d" removes leading zero on Linux/FreeBSD.
- "%e" removes leading zero on OpenBSD, Solaris, macOS.
- "%#d" removes leading zero on Windows.
- Exact output depends on system locale and C library.

See: [date-value](#f-date-value), [date-list](#f-date-list),
[date-parse](#f-date-parse), [time-of-day](#f-time-of-day),
[time](#f-time), [now](#f-now)

---


<a name="f-date-list"></a>
## date-list

```
syntax: (date-list int-seconds [int-index])
syntax: (date-list)
```

Description:

Converts a UTC timestamp into a list containing year,
month, day of month, hour, minute, second, day of year,
and day of week. The timestamp is interpreted as the
number of seconds since January 1st, 1970 00:00:00 UTC.
The weekday value ranges from 1 to 7 for Monday through
Sunday. When called without arguments, date-list uses the
current timestamp obtained from date-value.

If int-index is supplied, only the selected element of
the list is returned. Negative indices count from the end
of the list.

date-list is the inverse of date-value.

Format of the returned list:

```
(year month day hour minute second day-of-year day-of-week)
```

Examples:

```
(date-list 1282479244)
;-> (2010 8 22 12 14 4 234 1)

(date-list 1282479244 0)
;-> 2010

(date-list 1282479244 -2)
;-> 234

(date-value (date-list 1282479244))
;-> 1282479244

(date-list 0)
;-> (1970 1 1 0 0 0 1 4)
```

See: [date-value](#f-date-value), [date](#f-date),
[now](#f-now), [time](#f-time)

---


<a name="f-date-parse"></a>
## date-parse

```
syntax: (date-parse str-date str-format)
```

Description:

Parses a textual date according to strftime-style
directives in str-format and returns the corresponding
UTC timestamp as seconds since January 1st, 1970
00:00:00. The result ranges from 0 up to 2147472000,
which corresponds to dates before the 2038 boundary.

The parsing rules match those used by date for formatting.
This allows round-tripping between formatted strings and
timestamps when format and locale are consistent.

Examples:

```
(date-parse "2007.1.3" "%Y.%m.%d")
;-> 1167782400

(date-parse "January 10, 07" "%B %d, %y")
;-> 1168387200

(date-list (date-parse "2010.10.18 7:00"
                       "%Y.%m.%d %H:%M"))
;-> (2010 10 18 7 0 0 290 1)
```

Notes:

- The function requires system support for strptime-style
  parsing and may not be available on all platforms.

See: [date](#f-date), [date-list](#f-date-list),
[date-value](#f-date-value)

---


<a name="f-date-value"></a>
## date-value

```
syntax: (date-value int-year int-month int-day [int-hour int-min int-sec])
syntax: (date-value list-date-time)
syntax: (date-value)
```

Description:

Converts a given UTC date and time into the number of
seconds since January 1st, 1970 00:00:00. The first form
accepts separate numeric fields for year, month, and day,
with hour, minute, and second optional. The second form
accepts a list containing the same fields in the same
order. Missing time fields default to zero. In the third
form, date-value returns the timestamp for the current
system time.

The conversion always uses UTC and does not apply the
local time zone. date-value is the inverse operation of
date-list.

Examples:

```
(date-value 2002 2 28)
;-> 1014854400

(date-value '(2002 2 28))
;-> 1014854400

(date-value 1970 1 1 0 0 0)
;-> 0

(date (date-value (now)))
;-> "Wed May 24 10:02:47 2006"

(date (date-value))
;-> "Wed May 24 10:02:47 2006"

(date)
;-> "Wed May 24 10:02:47 2006"

(date-list 1014854400)
;-> (2002 2 28 0 0 0)

(date-value (date-list 1014854400))
;-> 1014854400
```

See: [date](#f-date), [date-list](#f-date-list),
[date-parse](#f-date-parse), [time-of-day](#f-time-of-day),
[time](#f-time), [now](#f-now)

---


<a name="f-debug"></a>
## debug [!]

```
syntax: (debug func)
```

Description:

Enables tracing and evaluates func. This is shorthand for
turning trace on, invoking the expression, and then
restoring the previous trace state. While trace is
enabled, each function call and return is printed, and
errors do not abort execution. Instead, the function that
encounters an exception returns 0 or nil, allowing program
state and variables to be inspected during debugging.

Examples:

```
; manual form
(trace true)
(my-func a b c)
(trace nil)

; equivalent shortcut
(debug (my-func a b c))
```

See: [trace](#f-trace)

---

<a name="f-dec"></a>
## dec [!]

```
syntax: (dec place [num])
```

Description:

Decrements the numeric value stored in place and returns
the updated result. The operation always uses floating-
point arithmetic. When num is omitted, the decrement is
1.0; otherwise num specifies the amount to subtract.
Integer values are converted to floating point before the
operation. place may be a symbol, a position inside a
list, or any writable location yielding a number.

If place evaluates to nil, it is treated as 0.0 before
the decrement. When place references a writable structure
element, that element is updated in place.

Examples:

```
(set 'x 10)
;-> 10

(dec x)
;-> 9
x
;-> 9

(dec x 0.25)
;-> 8.75
x
;-> 8.75

z
;-> nil
(dec z)
;-> -1

(set 'z nil)
(dec z 0.01)
;-> -0.01

(set 'l '(1 2 3 4))
;-> (1 2 3 4)

(dec (l 3) 0.1)
;-> 3.9

(dec (first l))
;-> 0

l
;-> (0 2 3 3.9)

(dec (+ 3 4))
;-> 6
```

Notes:

- dec performs floating-point arithmetic.
- Use -- for integer-mode decrement.
- Use inc for floating-point increment.

See: [--](#f-minusminus), [inc](#f-inc), [set](#f-set)

---


<a name="f-def-new"></a>
## def-new

```
syntax: (def-new sym-source [sym-target])
```

Description:

Creates a new symbol by copying the definition and
contents of sym-source. Only the referenced symbol is
copied, not the entire context. When sym-target is
omitted, a symbol of the same name is created in the
current context. In this case, all symbol references
belonging to the source context are rewritten so they
point into the current context. The current context must
not be MAIN.

When sym-target is provided, the new symbol is created in
the context referenced by sym-target. Both the name and
the destination context may change. Any symbol reference
that originally pointed into the source context is
rewritten to point into the target context. def-new
returns the newly created symbol.

This mechanism allows fine-grained copying of functions,
data structures, or context members, and enables creation
of isolated, statically scoped functions by assigning
each copy to its own namespace.

Examples:

```
(set 'ctx1:var '(ctx1:x ctx1:y))
;-> (ctx1:x ctx1:y)

(def-new 'ctx1:var 'ctx2:myvar)
;-> ctx2:myvar

ctx2:myvar
;-> (ctx2:x ctx2:y)

(context 'ctx3)
;-> ctx3

(def-new 'ctx1:var)
;-> var

var
;-> (x y)
```

Creating a function with its own namespace:

```
(set 'temp (lambda (x) (+ x x)))
;-> (lambda (x) (+ x x))

(def-new 'temp 'double:double)
;-> double:double

(double 10)
;-> 20

double:double
;-> (lambda (double:x) (+ double:x double:x))
```

Building a helper for statically scoped accumulators:

```
(define (def-static s body)
  (def-new 'body (sym s s)))

(def-static 'acc (lambda (x)
  (inc sum x)))

(acc 1)
;-> 1
(acc 1)
;-> 2
(acc 8)
;-> 10
```

Notes:

- def-new copies one symbol only; use new to copy an
  entire context.
- All internal references pointing to the original
  context are rewritten to the target context.
- Useful for constructing context-local closures or
  configuring context objects piece by piece.

See: [new](#f-new), [context](#f-context), [sym](#f-sym),
[define](#f-define)

---


<a name="f-default"></a>
## default

```
syntax: (default context)
```

Description:

Returns the value stored in the default functor of the
given context. The default functor is the symbol whose
name is identical to the context name. Many expressions
implicitly fall back to the default functor when a bare
context name appears; default provides explicit access to
that value.

Examples:

```
(define ctx:ctx 123)
;-> 123

(default ctx)
;-> 123

(setf (default ctx) 456)
;-> 456

(set 'c ctx)
;-> ctx

(default c)
;-> 456

ctx:ctx
;-> 456
```

Notes:

- The default functor is the symbol whose name matches
  the context name.
- Reading or writing the default functor is useful when
  an expression does not automatically resolve to it.
- Writable via setf.

See: [context](#f-context), [define](#f-define),
[setf](#f-setf)

---


<a name="f-define"></a>
## define

```
syntax: (define (sym-name [sym-1 ...]) body-1 [body-2 ...])
syntax: (define sym-name exp-value)
```

Description:

Defines a named function. The parameter rules and function
semantics are identical to those of fn. For anonymous
functions, see [fn](#f-fn).

In the first syntax, sym-name becomes a function that
accepts the given parameters and evaluates the body
expressions. In the second syntax, sym-name is assigned
exp-value, which is typically a function value created by
fn.

Extra arguments passed to a defined function are available
through the system symbol $args or via the function (args).

The function returns the value of the last body expression.

Examples:

```
; basic named function
(define (inc x) (+ x 1))
(inc 10)
;-> 11

; multiple parameters
(define (add3 a b c) (+ a b c))
(add3 1 2 3)
;-> 6

; default parameters
(define (scale x (f 2)) (* x f))
(scale 10)
;-> 20
(scale 10 5)
;-> 50

; defining using fn explicitly
(define add2 (fn (x y) (+ x y)))
(add2 3 4)
;-> 7

; variable arity using $args
(define (sum-all) (apply + $args))
(sum-all 1 2 3 4 5)
;-> 15

; same using (args)
(define (sum2) (apply + (args)))
(sum2 1 2 3 4 5)
;-> 15
```

Notes:

- define creates a named function in the current context.
- Extra arguments are available as $args or via (args).
- Parameters follow dynamic scoping rules.
- For anonymous functions, see [fn](#f-fn).
- For macro definitions, use [define-macro](#f-define-macro).

See: [fn](#f-fn), [apply](#f-apply), [define-macro](#f-define-macro)

---


<a name="f-define-macro"></a>
## define-macro

```
syntax: (define-macro (sym-name [sym-param-1 ...]) body)
syntax: (define-macro (sym-name [(sym-param-1 exp-default) ...]) body)
```

Description:

Creates a named runtime macro. When the macro is called,
its arguments arrive **unevaluated**. Inside the macro you
decide which arguments to evaluate, when to evaluate them,
and whether to modify them before evaluation.

A macro always returns a value. If the macro needs the
result of a computation, it must explicitly call *eval*.
If *eval* is not used, the macro returns unevaluated syntax.

For anonymous macros, see [fn-macro](#f-fn-macro).

Examples:

```
; 1) simple macro: (inc2 x) → (+ x 2)

(define-macro (inc2 x)
  (+ (eval x) 2))

(inc2 10)
;-> 12


; 2) custom conditional form
;    (if-zero x a b) → (if (= x 0) a b)

(define-macro (if-zero x a b)
  (if (= (eval x) 0)
      (eval a)
      (eval b)))

(if-zero 0 "yes" "no")
;-> "yes"

(if-zero 5 "yes" "no")
;-> "no"


; 3) macro that defines a new function
;    (make-fn name (p1) body) → (define (name p1) body)

(define-macro (make-fn name params body)
  (define (name params) (eval body)))

(make-fn triple (x) (* x 3))
(triple 10)
;-> 30


; 4) macro with a default argument
;    (add2 x) → (+ x 2)

(define-macro (add2 (x 0))
  (+ (eval x) 2))

(add2 10)
;-> 12

(add2)
;-> 2
```

Notes:

- Macro arguments are not evaluated automatically.
- The macro controls evaluation explicitly using *eval*.
- A macro may return evaluated results or unevaluated syntax.
- Runtime macros allow creation of custom control forms.
- A macro with default parameters may receive fewer arguments
  than declared; unspecified parameters take their default
  values.

See: [fn-macro](#f-fn-macro), [macro](#f-macro), [fn](#f-fn)

---


<a name="f-delete"></a>
## delete [!]

```
syntax: (delete symbol [bool])
syntax: (delete sym-context [bool])
```

Description:

Removes a symbol from the symbol table or deletes all
symbols belonging to a context. After deletion, any
external references to the removed symbol become nil.
When deleting a context, all symbols inside the context
are removed and the context name is converted into a
normal symbol containing nil. A second delete can remove
the context symbol itself.

If bool evaluates to true, deletion is allowed only when
the symbol has no external references. If bool evaluates
to nil, deletion is unconditional and reference checks are
skipped. This unconditional mode must be used with care:
if a deleted symbol is still referenced somewhere, the
missing update to nil can lead to crashes or inconsistent
program state.

Protected symbols, built-in functions, nil, and true
cannot be deleted. delete returns true on successful
deletion, or nil if deletion was refused.

Examples:

```
(set 'lst '(a b xvar c d))
;-> (a b xvar c d)

(delete 'xvar)
;-> true

lst
;-> (a b nil c d)

(set 'lst '(a b xvar c d))
;-> (a b xvar c d)

(delete 'xvar true)
;-> nil

lst
;-> (a b xvar c d)
```

Deleting an entire context:

```
(set 'ctx1:x 123)
;-> 123

(set 'ctx1:y "hello")
;-> "hello"

(delete 'ctx1)
;-> true

ctx1
;-> nil
```

Notes:

- Deleting a symbol while code using that symbol is
  executing can lead to crashes or undefined behavior.
- Unconditional deletion (bool = nil) should only be used
  when no external references exist.
- After deleting a context, the context name survives as a
  normal symbol holding nil.

See: [constant](#f-constant), [context](#f-context),
[symbol?](#f-symbolp)

---


<a name="f-delete-file"></a>
## delete-file

```
syntax: (delete-file str-file-name)
```

Description:

Removes the file specified by str-file-name. Returns true
when the file is successfully deleted, or nil if the
operation fails. When deleting a local file, error details
can be retrieved using sys-error. When deleting a URL
resource, net-error provides additional information.

The file name may refer to a local path or to a URL using
the file:// or http:// schemes. When a URL is used,
additional parameters may be supported; see delete-url for
details.

Examples:

```
(delete-file "temp.txt")
;-> true or nil

(delete-file "http://example.com/sample.txt")
;-> true or nil

(delete-file "file://notes.txt")
;-> true or nil
```

Notes:

- For URL deletions, behavior depends on the underlying
  protocol handler.
- Use sys-error or net-error for diagnostics.

See: [delete-url](#f-delete-url), [sys-error](#f-sys-error),
[net-error](#f-net-error)

---


<a name="f-delete-url"></a>
## delete-url

```
syntax: (delete-url str-url)
```

Description:

Sends an HTTP DELETE request to the resource specified by
str-url. The remote server must support the DELETE method
and allow deletion of the target resource; otherwise the
call returns nil or an error string. Optional parameters
such as timeouts or custom headers may be supplied in the
same way as with get-url.

If str-url begins with file://, a file on the local
filesystem is removed instead. This behavior matches
delete-file when a URL is given.

Examples:

```
(delete-url "http://example.com/data.txt")
;-> true or nil

(delete-url "http://example.org:8080/page.html" 5000)
;-> true or nil

(delete-url "file:///home/user/remove.txt")
;-> true or nil
```

Notes:

- Remote deletion depends on server configuration and
  permissions.
- Additional options follow the same rules as get-url.
- Rebel-style server nodes accept DELETE requests
  unless started with -http-safe.

See: [delete-file](#f-delete-file), [get-url](#f-get-url),
[net-error](#f-net-error)

---


<a name="f-destroy"></a>
## destroy [!]

```
syntax: (destroy int-pid)
syntax: (destroy int-pid int-signal)
```

Description:

Sends a termination signal to the process identified by
int-pid. When only int-pid is given, destroy sends a
default fatal signal equivalent to SIGKILL. When
int-signal is supplied, the specified Unix signal is sent
instead. The function returns true on success or nil on
failure.

Process IDs are typically obtained from fork or process.
destroy operates exactly like the Unix kill utility and
obeys all standard semantics of process groups and user
permissions.

**CAUTION:**  
- If int-pid is 0, the signal is sent to all processes in
  the caller’s process group.  
- If int-pid is -1, the signal is sent to all processes
  owned by the current user.  
- Using these special values can terminate large numbers
  of processes and should be used with extreme care.

Examples:

```
; start an external command
(set 'pid (process "/usr/bin/bc" in out))
;-> <pid>

(destroy pid)
;-> true

; kill a forked background worker
(set 'pid (fork (dotimes (i 1000)
                 (println i)
                 (sleep 10))))
;-> <pid>

(sleep 100)
(destroy pid)
;-> true
```

Notes:

- Signals follow standard Unix behavior.
- destroy can terminate process groups when specific PID
  values (0 or -1) are used.

See: [process](#f-process), [fork](#f-fork), [wait-pid](#f-wait-pid)

---


<a name="f-det"></a>
## det

```
syntax: (det matrix [float-pivot])
```

Description:

Computes the determinant of a square matrix. The matrix
may be given as a nested list or as an array. If the
matrix is singular and no pivot override is supplied, det
returns nil.

The optional float-pivot argument specifies a substitute
value for zero pivots during LU decomposition. This is
useful when working with singular or near-singular
matrices. A value of 0.0 forces all zero pivots to be
treated as exact zeros; very small values allow controlled
fallback behavior and prevent premature singularity
detection.

Examples:

```
(set 'A '((-1 1 1)
          (1 4 -5)
          (1 -2 0)))
;-> ((-1 1 1) (1 4 -5) (1 -2 0))

(det A)
;-> -1

; singular matrices
(det '((2 -1)
       (4 -2)))
;-> nil

(det '((2 -1)
       (4 -2)) 0)
;-> -0

(det '((2 -1)
       (4 -2)) 1e-20)
;-> -4e-20
```

Notes:

- float-pivot influences LU decomposition when zero
  pivots occur.
- When omitted, singular matrices produce nil.
- Arrays may be used instead of nested lists.

See: [invert](#f-invert), [mat](#f-mat), [multiply](#f-multiply),
[transpose](#f-transpose)

---


<a name="f-device"></a>
## device

```
syntax: (device [int-io-handle])
```

Description:

Sets or retrieves the current I/O device. When called
without arguments, the function returns the handle of the
currently active device. When int-io-handle is supplied,
it becomes the new target for input and output.

The value 0 selects the default standard streams:

- 0 — stdin  
- 1 — stdout  
- 2 — stderr  

A device handle may also be a file descriptor previously
returned by open. When a file descriptor is active, all
high-level I/O functions (print, println, write,
write-line, read-char, read-line) operate on that file
instead of the console.

Any valid open handle can be used as the active I/O
channel.

Examples:

```
(device (open "myfile" "write"))
;-> 5

(print "This goes in myfile")
;-> "This goes in myfile"

(close (device))
;-> true
```

Notes:

- Closing the current device automatically resets the
  device to 0.
- All standard I/O functions respect the currently active
  device handle.

See: [open](#f-open), [close](#f-close),
[print](#f-print), [read-line](#f-read-line)

---


<a name="f-difference"></a>
## difference

```
syntax: (difference list-1 list-2)
syntax: (difference list-1 list-2 bool)
```

Description:

Computes the difference between two lists. In the first
form, the result contains all unique elements that appear
in list-1 but not in list-2. The input lists may contain
duplicates, but the result of this set operation is
unique.

In the second form, difference operates in list mode when
bool is true. All elements appearing in list-2 are removed
from list-1, while duplicates of remaining elements are
preserved.

Examples:

```
(difference '(2 5 6 0 3 5 0 2)
            '(1 2 3 3 2 1))
;-> (5 6 0)

(difference '(2 5 6 0 3 5 0 2)
            '(1 2 3 3 2 1)
            true)
;-> (5 6 0 5 0)
```

Notes:

- Set mode returns a unique list.
- List mode preserves duplicates except for those removed
  due to matches with list-2.

See: [intersect](#f-intersect), [unique](#f-unique),
[union](#f-union)

---


<a name="f-directory"></a>
## directory

```
syntax: (directory)
syntax: (directory str-path)
syntax: (directory str-path str-pattern [regex-option])
```

Description:

Returns a list of directory entry names for the path in
str-path. When str-path is omitted, entries from the
current working directory are returned. On failure, nil is
returned.

In the pattern form, only filenames matching the regular
expression in str-pattern are included. When present,
regex-option controls the matching mode. Matching follows
the same rules as in regex.

Examples:

```
(directory "/bin")

(directory "." "\\.c")
;-> ("file1.c" "file2.c")

(directory "." {\\.c})
;-> ("file1.c" "file2.c")

; show hidden entries
(directory "." "^[.]")
;-> ("." ".." ".conf" ".cache")
```

Notes:

- str-pattern uses standard regex syntax.
- regex-option enables specific regex evaluation modes.
- A nil return indicates that the directory could not be
  read.

See: [regex](#f-regex), [find](#f-find),
[find-all](#f-findall), [parse](#f-parse),
[replace](#f-replace), [search](#f-search)

---


<a name="f-directoryp"></a>
## directory?

```
syntax: (directory? str-path)
```

Description:

Checks whether str-path refers to an existing directory.
Returns true when the path is a directory, or nil when it
is not.

Examples:

```
(directory? "/etc")
;-> true

(directory? "/usr/local/bin/nvi2/")
;-> nil
```

Notes:

- The check follows the filesystem exactly; trailing
  slashes are allowed.
- Returns nil when the path does not exist or is not a
  directory.

See: [file?](#f-filep)

---


<a name="f-div"></a>
## div

```
syntax: (div num-1 num-2 [num-3 ...])
syntax: (div num-1)
```

Description:

Successively divides num-1 by each following argument.
Mixed numeric types are allowed, but the result is always
a floating-point number. Any operation involving NaN
propagates NaN.

When called with a single argument, div returns the
multiplicative inverse of num-1.

Examples:

```
(div 10 3)
;-> 3.333333333

(div 120 (sub 9.0 6) 100)
;-> 0.4

(div 10)
;-> 0.1
```

Notes:

- Always returns a floating-point result.
- Division by zero yields NaN.

See: [mul](#f-mul), [sub](#f-sub), [add](#f-add)

---


<a name="f-dountil"></a>
## do-until

```
syntax: (do-until exp-condition [body])
```

Description:

Evaluates the expressions in body first, then evaluates
exp-condition. If exp-condition evaluates to non-nil, the
loop terminates; otherwise the body is executed again.
Because the condition is checked only after the body,
do-until always performs at least one iteration.

The return value is the last result produced by body. If
body is empty, the final value of exp-condition is
returned. During iteration, the system iterator symbol
$idx is updated.

Examples:

```
(set 'x 1)
(do-until (> x 0) (inc x))
;-> 2

(set 'x 1)
(until (> x 0) (inc x))
;-> 1
```

Notes:

- do-until always runs the body at least once.
- until may execute the body zero times.
- $idx is incremented on each iteration.

See: [until](#f-until), [while](#f-while),
[do-while](#f-dowhile)

---


<a name="f-dowhile"></a>
## do-while

```
syntax: (do-while exp-condition body)
```

Description:

Evaluates the expressions in body first, then evaluates
exp-condition. If exp-condition evaluates to non-nil, the
loop continues; if it evaluates to nil, the loop stops.
Because the condition is checked only after executing the
body, do-while always performs at least one iteration.

The return value is the final result of evaluating body.
During iteration, the system iterator symbol $idx is
updated.

Examples:

```
(set 'x 10)
(do-while (< x 10) (inc x))
;-> 11

(set 'x 10)
(while (< x 10) (inc x))
;-> 10
```

Notes:

- do-while always runs the body at least once.
- while may execute the body zero times.
- $idx is incremented on each iteration.

See: [while](#f-while), [until](#f-until),
[do-until](#f-dountil)

---


<a name="f-doargs"></a>
## doargs

```
syntax: (doargs (sym [exp-break]) body)
```

Description:

Iterates over all arguments passed to the current
user-defined function or macro. The variable in sym is
bound to each argument in sequence. Iteration stops when
all arguments are processed or when exp-break evaluates to
a non-nil value. The return value is the final result of
evaluating body.

During iteration, the system iterator symbol $idx is
updated.

Examples:

```
(define (f)
  (doargs (i) (println i)))

(f 1 2 3 4)
;-> prints:
;   1
;   2
;   3
;   4

; stop early when an argument equals 'x
(define-macro (g)
  (doargs (i (= i 'x))
    (println i)))

(g a b x c d)
;-> prints:
;   a
;   b
;   true
```

Notes:

- exp-break is checked before each iteration step.
- $idx increments on each processed argument.
- Use args when the full argument list is required at once.

See: [args](#f-args)

---


<a name="f-dolist"></a>
## dolist

```
syntax: (dolist (sym list-1 [exp-break]) body)
```

Description:

Iterates over each element of list-1 (a list or array).
Before each iteration, sym is bound to the current element.
The binding is local to the loop and follows dynamic
scoping rules. The return value of dolist is the last value
produced by body, unless an early exit occurs.

If exp-break is present, it is evaluated before each
iteration step. When exp-break evaluates to a non-nil
value, the loop terminates immediately and returns that
value.

During iteration, the system variable $idx contains the
current index (starting at 0).

Examples:

```
(set 'x 123)

(dolist (i '(a b c d e f g))
  (print i))
;-> g
; console output:
;   abcdefg

; early exit when element equals 'e
(dolist (i '(a b c d e f g) (= i 'e))
  (print i))
;-> true
; console output:
;   abcd

; x outside the loop is unchanged
x
;-> 123

; show index and value
(dolist (i '(a b d e f g))
  (println $idx ":" i))
;-> g
; console output:
;   0:a
;   1:b
;   2:d
;   3:e
;   4:f
;   5:g
```

Notes:

- sym is local to the loop.
- exp-break is evaluated before each iteration.
- $idx increments on each step and cannot be modified.

See: [dotimes](#f-dotimes), [for](#f-for),
[map](#f-map)

---


<a name="f-dostring"></a>
## dostring

```
syntax: (dostring (sym string-1 [exp-break]) body)
```

Description:

Iterates over each character in string-1. Before every
iteration, sym is bound to the character’s integer code
point. The binding follows dynamic scoping rules and is
local to the loop.

If exp-break is present, it is evaluated before each
iteration step. When exp-break becomes non-nil, the loop
terminates immediately and returns its value. Otherwise,
body is evaluated and the loop continues.

The return value is the last evaluation of body. During
execution, the system iterator $idx contains the current
character index.

Examples:

```
; ASCII
(set 'str "abcdefg")
(dostring (c str)
  (println c " " (char c)))
;-> prints:
;   97 a
;   98 b
;   99 c
;   100 d
;   101 e
;   102 f
;   103 g

; UTF-8
(set 'txt "我能吞下玻璃而不伤身体。")
(dostring (c txt)
  (println c " " (char c)))
;-> prints codepoint and character for each element
```

Notes:

- Characters are iterated as full UTF-8 code points.
- Code points may exceed 255.
- $idx starts at zero and increments each iteration.

See: [dolist](#f-dolist), [dotimes](#f-dotimes),
[char](#f-char), [explode](#f-explode)

---


<a name="f-dotimes"></a>
## dotimes

```
syntax: (dotimes (sym-var int-count [exp-break]) body)
```

Description:

Executes body int-count times. Before each iteration,
sym-var is bound to the current loop index, starting at 0
and ending at int-count - 1. The binding is local to the
loop and follows dynamic scoping rules. The return value
is the last evaluation of body.

If exp-break is present, it is evaluated before each
iteration step. When exp-break evaluates to a non-nil
value, the loop terminates immediately and returns that
value.

After dotimes finishes, sym-var reverts to its previous
value.

Examples:

```
(dotimes (i 10)
  (print i))
;-> 9
; console output:
;   0123456789

; early exit example:
(dotimes (i 10 (= i 3))
  (print i))
;-> true
; console output:
;   012
```

Notes:

- sym-var is always an integer.
- exp-break is checked before body is evaluated.
- The final return value is either the last body result or
  the value of exp-break on early termination.

See: [dolist](#f-dolist), [for](#f-for),
[while](#f-while)

---


<a name="f-dotree"></a>
## dotree

```
syntax: (dotree (sym sym-context [bool]) body)
```

Description:

Iterates over all symbols stored in sym-context. Symbols
are visited in sorted order. Before each iteration, sym is
bound to the next symbol. The binding is local to the
loop and follows dynamic scoping rules. The return value
is the last evaluation of body.

If bool evaluates to non-nil, only symbols whose names
begin with an underscore (_) are included. This is useful
when a context contains internal keys or auxiliary data
under underscore-prefixed names.

During iteration, the system variable $idx contains the
current symbol index.

Examples:

```
; iterate through all symbols in a context
(dotree (s ctx)
  (print s " "))

; iterate only over symbols beginning with "_"
(dotree (s ctx true)
  (print s " "))
```

Notes:

- Symbol visitation order is lexicographic.
- sym is local to the loop.
- $idx increments on each step.
- dotree avoids the memory overhead of creating a list
  using symbols.

See: [symbols](#f-symbols), [dolist](#f-dolist),
[doargs](#f-doargs)

---


<a name="f-dump"></a>
## dump

```
syntax: (dump)
syntax: (dump exp)
```

Description:

Displays the raw binary fields of a cell. Without an
argument, dump prints information about all allocated
cells to the console. When exp is supplied, it is
evaluated and the internal representation of the resulting
cell is returned as a list of integers.

Each position in the list corresponds to a specific field
in the cell structure:

```
offset  description
0       memory address of the cell
1       cell->type (major/minor type)
2       cell->next pointer
3       cell->aux
        - string length + 1
        - low/high word of 64-bit integer
        - low word of IEEE 754 double
4       cell->contents
        - string or symbol address
        - high/low word of 64-bit integer
        - high word of IEEE 754 double
```

Examples:

```
(dump 'a)
;-> (9586996 5 9578692 9578692 9759280)

(dump 999)
;-> (9586996 130 9578692 9578692 999)
```

Notes:

- The exact numeric values are system-specific.
- This function is intended for inspecting or manipulating
  internal cell fields.
- Use with care; changing internal structures can cause
  instability.

See: [cpymem](#f-cpymem)

---


<a name="f-emptyp"></a>
## empty?

```
syntax: (empty? exp)
syntax: (empty? str)
```

Description:

Tests whether exp is an empty list or whether str is an
empty string. Returns true when the argument contains no
elements; otherwise returns nil.

Examples:

```
(set 'lst '())
(empty? lst)
;-> true

(empty? '(1 2 3 4))
;-> nil

(empty? "hello")
;-> nil

(empty? "")
;-> true
```

Notes:

- Works on lists and strings.
- Symbols evaluating to nil are not considered empty lists.

See: [nil?](#f-nilp), [list?](#f-listp), [string?](#f-stringp)

---


<a name="f-encrypt"></a>
## encrypt

```
syntax: (encrypt str-source str-pad)
```

Description:

Applies a one-time pad (OTP) transformation to str-source
using the pad string in str-pad. The operation is
symmetric: applying the same pad to the encrypted output
restores the original data.

Security depends entirely on the pad: it must be random,
kept secret, and never reused for the same message. When
these conditions are met and the pad length matches the
source length, the result is information-theoretically
secure.

The return value is a string containing the encrypted (or
decrypted) data.

Examples:

```
; encrypt
(set 'msg (encrypt "A secret message" "my secret key"))
;-> ",YS\022\006\017\023\017TM\014\022\n\012\030E"

; decrypt by applying the same pad
(encrypt msg "my secret key")
;-> "A secret message"

; encrypt a file
(write-file "file.enc"
  (encrypt (read-file "file.txt") "29kH67*"))
;-> true
```

Notes:

- Applying encrypt twice with the same pad restores the
  original data.
- For one-time–pad semantics, the pad must be random,
  secret, and at least as long as the source.
- Reusing the same pad for multiple messages weakens the
  transformation and should be avoided.
- Useful for lightweight obfuscation or exchanging data
  between cooperating processes (e.g., via net-eval), but
  does not replace modern authenticated encryption.
- Operates bytewise on the input strings.

See: [xor](#f-xorbit), [bits](#f-bits)

---


<a name="f-ends-with"></a>
## ends-with

```
syntax: (ends-with str-data str-key [num-option])
syntax: (ends-with list exp)
```

Description:

Checks whether str-data ends with str-key. When
num-option is omitted, str-key is treated as a literal
string. When num-option is present, str-key is interpreted
as a regular expression pattern, and num-option selects
the regex options. The function returns true or nil.

In the second form, ends-with tests whether a list ends
with the value in exp. exp may itself be a list.

Examples:

```
; string matching
(ends-with "RebelLang" "Lang")
;-> true

(ends-with "RebelLang" "lang")
;-> nil

; regular expression form
(ends-with "RebelLang" "lang|test" 1)
;-> true

; list matching
(ends-with '(1 2 3 4 5) 5)
;-> true

(ends-with '(a b c d e) 'b)
;-> nil

(ends-with '(a b c (+ 3 4)) '(+ 3 4))
;-> true
```

Notes:

- Regular expression matching uses the same option flags
  as regex.
- exp in the list form can be any expression, including a
  list.

See: [starts-with](#f-starts-with), [regex](#f-regex)

---


<a name="f-env"></a>
## env

```
syntax: (env)
syntax: (env var-str)
syntax: (env var-str value-str)
```

Description:

Accesses and modifies the process environment.

In the first form, env returns all environment variables
as an association list of key–value pairs.

In the second form, env retrieves the value of the
variable named in var-str, or nil if the variable is not
present.

In the third form, env sets or creates an environment
variable. When value-str is an empty string, the variable
is removed from the environment.

Examples:

```
; retrieve entire environment
(env)
;-> (("PATH" "/usr/bin:/bin") ("TERM" "xterm") ...)

; read a variable
(env "PATH")
;-> "/usr/bin:/bin:/usr/local/bin"

; set a variable
(env "REBELDIR" "/usr/local/share/rebel")
;-> true

; read it back
(env "REBELDIR")
;-> "/usr/local/share/rebel"

; remove the variable
(env "REBELDIR" "")
;-> true

(env "REBELDIR")
;-> nil
```

Notes:

- Variables are stored in the process environment and are
  inherited by child processes.
- Deletion is performed by assigning an empty string.

See: [process](#f-process), [exec](#f-exec)

---


<a name="f-erf"></a>
## erf

```
syntax: (erf num)
```

Description:

Computes the Gaussian error function of num. The function
is defined as:

erf(x) = 2/sqrt(pi) * ∫ from 0 to x of exp(-t^2) dt

The return value is a floating-point approximation of the
mathematical erf.

Examples:

```
(map erf (sequence 0.0 6.0 0.5))
;-> (0
;    0.5204998778
;    0.8427007929
;    0.9661051465
;    0.995322265
;    0.999593048
;    0.9999779095
;    0.9999992569
;    0.9999999846
;    0.9999999998
;    1
;    1
;    1)
```

Notes:

- Values approach 1 as num grows large.
- The implementation provides a numerical approximation.

See: [erfc](#f-erfc), [exp](#f-exp)

---


<a name="f-error-event"></a>
## error-event

```
syntax: (error-event sym-event-handler)
syntax: (error-event func-event-handler)
syntax: (error-event nil)
```

Description:

Installs a custom error handler. When an error occurs,
Rebel resets the current evaluation and invokes the
handler. The handler may be a quoted symbol referring to a
function or an inline lambda. Inside the handler,
last-error can be used to inspect the error number and
message.

Calling (error-event nil) removes the current handler.

Examples:

```
(define (my-handler)
  (print "error # " (first (last-error)) " occurred\n"))

(error-event 'my-handler)
;-> my-handler

; using the function directly
(error-event my-handler)
;-> $error-event

; using an inline function
(error-event
  (fn ()
    (print "error # " (first (last-error)) " occurred\n")))
;-> $error-event

; install exit as handler
(error-event exit)
;-> $error-event

; remove handler
(error-event nil)
;-> nil
```

See: [last-error](#f-last-error), [catch](#f-catch), [throw-error](#f-throw-error)

---


<a name="f-eval"></a>
## eval

```
syntax: (eval exp)
```

Description:

Evaluates exp in the current variable environment and
returns the resulting value. Symbols inside exp resolve to
their current bindings at the moment eval is executed.
Evaluation respects local scopes created by let and other
binding forms.

Examples:

```
; basic evaluation
(set 'expr '(+ 3 4))
expr
;-> (+ 3 4)

(eval expr)
;-> 7

(eval (list + 3 4))
;-> 7

(eval ''x)
;-> x

; evaluation uses current environment
(set 'x 3 'y 4)
(eval '(+ x y))
;-> 7

; local environment
(let ((x 33) (y 44))
  (eval '(+ x y)))
;-> 77

; after leaving the local environment
(eval '(+ x y))
;-> 7

; passing data by reference using quoted symbols
(define (change-list aList)
  (push 999 (eval aList)))

(set 'data '(1 2 3 4 5))

(change-list 'data)
;-> (999 1 2 3 4 5)
```

Notes:

- eval can operate on quoted symbols to modify the
  underlying value.
- Context-based references are safer in complex code.

See: [let](#f-let), [set](#f-set), [push](#f-push)

---


<a name="f-eval-string"></a>
## eval-string

```
syntax: (eval-string str-source [sym-context [exp-error [int-offset]]])
```

Description:

Compiles and evaluates the code contained in str-source.
If multiple expressions are present, the value of the last
expression is returned. When sym-context is supplied, all
symbols in the string resolve inside that context. If an
error occurs, exp-error is evaluated and its value is
returned. int-offset specifies an optional starting
position inside str-source.

Examples:

```
; basic evaluation
(eval-string "(+ 3 4)")
;-> 7

(set 'x 10)
(set 'y 20)
(eval-string "(+ x y)")
;-> 30

; evaluate in a different context
(context 'C)
(set 'C:x 2)
(set 'C:y 3)

(eval-string "(+ x y)" 'C)
;-> 5

; error handler expression
(eval-string "(+ 1 'a)"
             MAIN
             '"error")
;-> "error"

; start evaluating from an offset
(eval-string "(+ 1 2)(+ 3 4)" MAIN nil 7)
;-> 7
```

Notes:

- String contents are fully parsed before evaluation.
- exp-error is only evaluated when parsing or evaluation
  fails.
- int-offset allows evaluating a substring without
  extracting it manually.

See: [read-expr](#f-read-expr)

---


<a name="f-evenp"></a>
## even?

```
syntax: (even? int-number)
```

Description:

Checks whether int-number is evenly divisible by 2. If a
floating-point value is supplied, its fractional part is
discarded before testing. Returns true for even integers
and nil otherwise.

Examples:

```
(even? 123)
;-> nil

(even? 8)
;-> true

(even? 8.7)
;-> true
```

See: [odd?](#f-oddp)

---


<a name="f-exec"></a>
## exec

```
syntax: (exec str-process)
syntax: (exec str-process str-stdin)
```

Description:

Starts an external process given in str-process. In the
first form, all lines written to standard output are
returned as a list of strings. When the process cannot be
launched, the result is nil. If it launches but produces
no output, an empty list is returned.

In the second form, str-stdin is sent to the process as
standard input. Output from the process is not captured in
this form. The return value is true when the process
starts successfully and nil otherwise.

Examples:

```
; capture output of a command
(exec "ls *.c")
;-> ("rebel.c" "math.c" "string.c")

; send standard input to a Unix program that reads stdin
(set 'str "line1\nline2\n")
(exec "wc -l" str)
;-> true
```

See: [spawn](#f-spawn), [pipe](#f-pipe)

---


<a name="f-exists"></a>
## exists

```
syntax: (exists func-condition lst)
```

Description:

Applies func-condition to each element of lst in order and
returns the first element for which func-condition yields
true. When no element matches, the result is nil. The
function-condition must be a function or lambda that
accepts one argument.

Examples:

```
; first string in the list
(exists string? '(2 3 4 6 "hello" 7))
;-> "hello"

; no string present
(exists string? '(3 4 2 -7 3 0))
;-> nil

; find the first zero
(exists zero? '(3 4 2 -7 3 0))
;-> 0

; find the first negative number
(exists < '(3 4 2 -7 3 0))
;-> -7

; custom condition
(exists (fn (x) (> x 3)) '(3 4 2 -7 3 0))
;-> 4

; no match
(exists (fn (x) (= x 10)) '(3 4 2 -7 3 0))
;-> nil
```

Notes:

- When func-condition is nil?, a nil result becomes
  ambiguous. For such cases, index or find are clearer.
- Use for-all to test whether all elements satisfy a
  condition.

See: [for-all](#f-for-all), [index](#f-index), [find](#f-find)

---


<a name="f-exit"></a>
## exit

```
syntax: (exit)
syntax: (exit int)
```

Description:

Terminates the current Rebel process. When int is
supplied, it becomes the process exit code visible to the
host operating system. If no code is given, the exit code
defaults to 0.

Examples:

```
(exit 5)
;-> process terminates with exit code 5
```

Notes:

- When Rebel is running in a long-lived service wrapper,
  only the wrapper determines whether the process restarts.
- Exit codes follow normal Unix conventions.

See: [throw-error](#f-throw-error)

---


<a name="f-expand"></a>
## expand

```
syntax: (expand exp sym-1 [sym-2 ...])
syntax: (expand exp list-assoc [bool])
syntax: (expand exp)
```

Description:

Performs symbolic expansion inside exp. The expansion
rules differ based on the chosen syntax form.

In the first syntax, each symbol given in sym-1 ... is
replaced with its current value. Nested occurrences are
expanded recursively. The original structure is never
modified. This syntax is useful when building functions
dynamically or when composing rewrite-style macros.

In the second syntax, expansion uses list-assoc as a
temporary association list. Keys in list-assoc behave as
symbols and their values supply the replacement data.
When bool is true, the value expressions in list-assoc
are evaluated before substitution. No variable bindings
in the caller are changed.

In the third syntax, only symbols beginning with an
uppercase character are expanded. Symbols bound to nil
are skipped. This mode mirrors traditional logic
programming expansion of uppercase variables.

Examples:

```
; first syntax: expand bound variables
; ------------------------------------
(set 'x 2 'a '(d e))
(set 'val 'a)

(expand val 'a)
;-> (d e)

(expand '(a x b) 'x)
;-> (a 2 b)

(expand '(a x (b c x)) 'x 'a)
;-> ((d e) 2 (b c 2))

; dynamic function construction
(define (raise-to power)
  (expand (fn (base) (pow base power)) 'power))

(set 'square (raise-to 2))
(set 'cube   (raise-to 3))

(square 5)
;-> 25
(cube 5)
;-> 125

; second syntax: association list expansion
; -----------------------------------------
(expand '(a b c) '((a 1) (b 2)))
;-> (1 2 c)

(expand '(a b) '((a (+ 1 2)) (b (+ 3 4))))
;-> ((+ 1 2) (+ 3 4))

(expand '(a b) '((a (+ 1 2)) (b (+ 3 4))) true)
;-> (3 7)

; third syntax: uppercase-variable expansion
; ------------------------------------------
(set 'A 1 'Bvar 2 'C nil 'd 5)

(expand '(A (Bvar) C d))
;-> (1 (2) C d)

; simplified currying with uppercase variable
(define (raise-to Power)
  (expand (fn (base) (pow base Power))))

(set 'cube (raise-to 3))
(cube 4)
;-> 64
```

Notes:

- First syntax expands explicit symbol arguments.
- Second syntax provides temporary, non-destructive
  mappings, useful with unify in logic programming.
- Third syntax mirrors PROLOG-style variable expansion.

See: [letex](#f-letex), [unify](#f-unify)

---


<a name="f-explode"></a>
## explode [utf8]

```
syntax: (explode str [int-chunk [bool]])
syntax: (explode lst [int-chunk [bool]])
```

Description:

Splits a string or list into smaller pieces. The meaning of
int-chunk and bool depends on the argument type.

In the first syntax, str is split into a list of
single-character strings. If int-chunk is given, str is
split into chunks of that many characters. On UTF-8
systems, chunking respects character boundaries rather
than bytes. When bool is true, the final chunk is omitted
if it is shorter than int-chunk.

This form is especially useful when a string operation
needs to work on characters, but the underlying function
operates only on bytes. By exploding into characters first,
processing them, and joining the result afterwards, correct
UTF-8 workflow is preserved.

In the second syntax, lst is split into sublists of size
int-chunk. The default chunk size is 1. When bool is true,
the final sublist is omitted if it is shorter than
int-chunk.

Examples:

```
; string → characters
(explode "rebel")
;-> ("r" "e" "b" "e" "l")

; preserve original by joining back
(join (explode "keep it together"))
;-> "keep it together"

; string → fixed-width chunks
(explode "rebel" 2)
;-> ("re" "be" "l")

; omit incomplete chunk
(explode "rebel" 3 true)
;-> ("reb")

; binary handling on UTF-8 builds requires unpack
(set 'bytes "\001\002\003\004")
(unpack (dup "c" (length bytes)) bytes)
;-> (1 2 3 4)

; list → sublists
(explode '(a b c d e f g h))
;-> ((a) (b) (c) (d) (e) (f) (g) (h))

(explode '(a b c d e f g) 2)
;-> ((a b) (c d) (e f) (g))

; omit last short chunk
(explode '(a b c d e f g) 2 true)
;-> ((a b) (c d) (e f))

; works well with transpose
(transpose (explode '(a b c d e f g h) 2))
;-> ((a c e g) (b d f h))

; idiom: reverse a UTF-8 string safely
; reverse cannot operate on UTF-8 directly
(join (reverse (explode "žába")))
;-> "abáž"
```

Notes:

- UTF-8 mode splits by Unicode character, not by byte.
- Exploding first is required for functions that operate
  only on bytes but need correct UTF-8 behavior.
- For raw byte processing, use unpack for predictable
  behavior.
- join and append perform the inverse transformation.

See: [join](#f-join), [append](#f-append), [unpack](#f-unpack)

---



<a name="f-extend"></a>
## extend [!]

```
syntax: (extend list-1 [list-2 ... ])
syntax: (extend string-1 [string-2 ... ])
```

Description:

Destructively appends items to a list or string. The first
argument is modified in place, and the updated value is
returned. If list-1 or string-1 is an uninitialized symbol,
it becomes a new list or string during extension.

When used on lists, every list-2 is appended to list-1.
When used on strings, every string-2 is concatenated to
string-1. String inputs may contain binary zero bytes.

Examples:

```
; extending lists
(set 'lst '(a b))
(extend lst '(c d))
;-> (a b c d)

(extend lst '(e f g))
;-> (a b c d e f g)

lst
;-> (a b c d e f g)

; extending strings
(set 'str "ab")
(extend str "cd")
;-> "abcd"

(extend str "efg")
;-> "abcdefg"

str
;-> "abcdefg"

; extending nested elements in place
(set 'lst '(a b "CD" (e f)))

(extend (lst 2) "E")
lst
;-> (a b "CDE" (e f))

(extend (lst 3) '(g))
lst
;-> (a b "CDE" (e f g))
```

Notes:

- extend modifies data in place.  
  For a non-destructive alternative, use append.

See: [append](#f-append), [push](#f-push)

---


<a name="f-factor"></a>
## factor

```
syntax: (factor int)
```

Description:

Returns a list of the prime factors of int. If int is a
float, it is truncated before factoring. Values smaller
than 2 return nil. For floats larger than the maximum
64-bit integer, the largest 64-bit integer is factored.

Examples:

```
; basic factoring
(factor 123456789123456789)
;-> (3 3 7 11 13 19 3607 3803 52579)

; verify correctness
(= (apply * (factor 123456789123456789))
   123456789123456789)
;-> true

; factoring the largest signed 64-bit integer
(factor 9223372036854775807)
;-> (7 7 73 127 337 92737 649657)

; small prime generator
(define (primes n , p)
  (dotimes (e n)
    (if (= (length (factor e)) 1)
        (push e p -1)))
  p)

(primes 20)
;-> (2 3 5 7 11 13 17 19)
```

Notes:

- Returns nil for any number less than 2.
- For inputs beyond the representable integer range,
  the largest 64-bit integer is used.
- A number is prime when its factor list contains exactly
  one element.

See: [apply](#f-apply)

---


<a name="f-fft"></a>
## fft

```
syntax: (fft list-num)
```

Description:

Computes the discrete Fourier transform of list-num using
the Fast Fourier Transform algorithm. Each element of
list-num may be either:

- a real number, representing a complex value with
  imaginary part 0, or
- a two-element list (real imag) representing a complex
  number explicitly.

If list-num has a length that is not a power of two, the
input is padded with complex zeros until the next power of
two is reached.

The result is a list of complex numbers in the same
(real imag) structure. fft never modifies the input.

Examples:

```
; basic forward/inverse identity
(ifft (fft '((1 0) (2 0) (3 0) (4 0))))
;-> ((1 0) (2 0) (3 0) (4 0))

; imaginary part defaults to 0
(fft '(1 2 3 4))
;-> ((10 0) (-2 -2) (-2 0) (-2 2))

; mixing plain numbers and explicit complex pairs
(fft '(1 2 (3 0) 4))
;-> ((10 0) (-2 -2) (-2 0) (-2 2))
```

Notes:

- Input lengths not equal to a power of two are padded
  with (0 0).
- All results are returned as explicit complex pairs.
- The inverse transform is computed by ifft.

See: [ifft](#f-ifft)

---


<a name="f-file-info"></a>
## file-info

```
syntax: (file-info str-name [int-index [bool-flag]])
::-

Description:

Returns metadata about the file or directory named in
str-name. The result is a list of eight fields. When
int-index is supplied, only that field is returned.
Negative indices work from the end as usual.

When bool-flag is nil (or omitted), information describes
the link itself if str-name is a symbolic link. When
bool-flag is true, information describes the file that the
link points to.

Fields:

```
index   meaning
-----   ----------------------------------------
0       size in bytes
1       mode (permissions + type)
2       device mode
3       user ID  (owner)
4       group ID (owner group)
5       access time
6       modification time
7       status change time
```

Examples:

```
; full metadata list
(file-info ".bashrc")
;-> (124 33188 0 500 0 920951022 920951022 920953074)

; select only one field
(file-info ".bashrc" 0)
;-> 124

; human-readable timestamp
(date (file-info "/etc" -1))
;-> "Mon Mar 8 18:23:17 2005"
```

Notes:

- On symlinks, the mode field differs depending on whether
  bool-flag is nil (link) or true (target).
- All timestamps are returned as Unix epoch integers.
- Returned size refers to the target file except for the
  mode field.

See: [directory](#f-directory), [file?](#f-filep)

---


<a name="f-filep"></a>
## file?

```
syntax: (file? str-path-name)
syntax: (file? str-path-name bool)
```

Description:

Checks whether str-path-name exists in the filesystem.  
Returns true for both regular files and directories.

When bool is true, the path must be a **regular file**.
If it is a directory, the result is nil.  
When the path exists and is a regular file, the function
returns str-path-name.

Examples:

```
; exists (file or directory)
(file? "/usr/bin")
;-> true

; must be a regular file
(file? "/usr/bin/awk" true)
;-> "/usr/bin/awk"

(file? "/usr/bin" true)
;-> nil

; typical usage: prepare for opening a file
(if (file? "/tmp/data.txt")
    (println "file exists"))
```

Notes:

- Existence does not imply read/write permissions.
- Directories count as "existing" unless bool is true.

See: [directory?](#f-directoryp), [file-info](#f-file-info)

---


<a name="f-filter"></a>
## filter

```
syntax: (filter exp-predicate exp-list)
```

Description:

Returns a new list containing all elements of exp-list for
which exp-predicate evaluates to true. The predicate is
called once for each element. The original list is not
modified.

filter behaves like clean, but with the predicate
logically reversed: filter keeps elements that satisfy the
condition; clean removes them.

Examples:

```
; basic predicate
(filter symbol? '(1 2 d 4 f g 5 h))
;-> (d f g h)

; user-defined predicate
(define (big? x) (> x 5))
(filter big? '(1 10 3 6 4 5 11))
;-> (10 6 11)

; using a comparison functor
(set 'lst '((a 10 2 7) (b 5) (a 8 3) (c 8) (a 9)))

(filter (curry match '(a *)) lst)
;-> ((a 10 2 7) (a 8 3) (a 9))

(filter (curry match '(? ?)) lst)
;-> ((b 5) (c 8) (a 9))

(filter (curry match '(* 8 *)) lst)
;-> ((a 8 3) (c 8))
```

Notes:

- exp-predicate may be a built-in predicate, user-defined
  function, or fn expression.
- For list–list filtering, see difference or intersect.
- index returns the positions of matching elements.
- clean keeps elements for which the predicate is false.

See: [clean](#f-clean), [index](#f-index),  
[difference](#f-difference), [intersect](#f-intersect)

---


<a name="f-find"></a>
## find

```
syntax: (find exp-key lst [func-compare | regex-option])
syntax: (find str-key str-data [regex-option [int-offset]])
```

Description:

Searches for exp-key either inside a list or inside a
string. The return value is the zero-based index of the
first match, or nil if no match is found.

If the second argument is a list, each element is compared
either with = (default), or using a comparison function in
func-compare. When a regex option is supplied, exp-key must
be a string and the element comparison uses regular
expression rules.

If the second argument is a string, find searches for
str-key as a substring or regex pattern. Returned offsets
are byte offsets even on UTF-8 systems.

When a match is found, the system variable $0 contains the
matched element or substring. For regular expressions with
capture groups, $1, $2, … contain the submatches. See the
[regex](#f-regex) function for detailed behavior and
available options.

Examples:

```
; list search with plain comparison
(find '(1 2) '((1 4) 5 6 (1 2) (8 9)))
;-> 3

(find "world" '("hello" "world"))
;-> 1

(find "hi" '("hello" "world"))
;-> nil

; case-insensitive using regex option
(find "rebel" '("Rebel" "lang") "i")
;-> 0
$0
;-> "Rebel"

; comparison functor: find first element > key
(find 3 '(8 4 3 7 2 6) >)
;-> 4
$0
;-> 2

; custom comparison function
(define (has-last val item) (= val (last item)))
(find 22 '((a 3) (k 5) (z 22)) has-last)
;-> 2
$0
;-> (z 22)

; pattern match with match or unify
(find '(a ?) '((l 3) (a 10) (z 22)) match)
;-> 1
$0
;-> (a 10)

(find '(X X) '((a b) (c d) (e e)) unify)
;-> 2
$0
;-> (e e)

; string search
(find "world" "hello world")
;-> 6

(find "WORLD" "hello world")
;-> nil

; regex search (see regex for options)
(find "cat|dog" "I have a dog" 0)
;-> 9
$0
;-> "dog"

; search with offset
(find "cat|dog" "cat and dog" 0 5)
;-> 9
```

Notes:

- List comparisons default to = unless a functor or regex
  option is given.
- Regular expression options and capture behavior are
  documented in the [regex](#f-regex) function.
- $0 stores the last successful match, and $1, $2, …
  capture regex subexpressions.
- Offsets in strings are byte-based even under UTF-8.
- For nested or multidimensional list searches, use
  [ref](#f-ref) or [ref-all](#f-ref-all).

See:  
[regex](#f-regex), [find-all](#f-find-all),  
[search](#f-search), [replace](#f-replace),  
[ref](#f-ref), [ref-all](#f-ref-all)

---



<a name="f-find-all"></a>
## find-all

```
syntax: (find-all str-regex-pattern str-text [exp [regex-option]])
syntax: (find-all list-match-pattern lst [exp])
syntax: (find-all exp-key lst [exp [func-compare]])
::-

Description:

Collects all matches found in a string or list. The return
value is always a list. If no matches are found, the empty
list () is returned. After each call, the system variable
$count contains the number of matches.

In the first syntax, str-regex-pattern is a regular
expression applied to str-text. Each match is collected.
If exp is given, it is evaluated for every match with $0,
$1, … bound to the matched substring and its captures.
regex-option controls regular expression behavior; see
[regex](#f-regex).

In the second syntax, list-match-pattern is compared
against each element of lst using match. If exp is
supplied, it is evaluated for each match with $it bound to
the current element.

In the third syntax, exp-key is compared against elements
of lst using = (default), func-compare, or match when
exp-key is a list. If exp is given, it is evaluated for
each match with $it bound to the element.

Examples:

```
; string search using regex
;----------------------------------------
(find-all {\d+} "x12y300z5")
;-> ("12" "300" "5")
$count
;-> 3

; process captures in a custom expression
;----------------------------------------
(find-all {(n)(um)} "numNUM" (append $2 $1) "i")
;-> ("umn" "UMN")

; use $count in the processing expression
;----------------------------------------
(find-all "a" "ababa" (string $count $it))
;-> ("1a" "2a" "3a")

; list pattern search using match
;----------------------------------------
(find-all '(? 2) '((a 1) (b 2) (a 2) (c 4)))
;-> ((b 2) (a 2))
$count
;-> 2

; process matched sublists
;----------------------------------------
(find-all '(? 2) '((a 1) (b 2) (a 2) (c 4)) (first $it))
;-> (b a)

; list search using a comparison function
;----------------------------------------
(find-all 5 '(2 7 4 5 9 2 4 9 7 4 8) $it <)
;-> (7 9 9 7 8)
$count
;-> 5

; process found elements
;----------------------------------------
(find-all 5 '(2 7 4 5 9 2 4 9 7 4 8) (* 3 $it) <)
;-> (21 27 27 21 24)

; same example using explicit fn
;----------------------------------------
(find-all 5 '(2 7 4 5 9 2 4 9 7 4 8)
          (* 3 $it)
          (fn (x y) (< x y)))
;-> (21 27 27 21 24)

; any expression may produce the result
;----------------------------------------
(find-all 5 '(2 7 4 5 9 2 4 9 7 4 8)
          ("abcdefghijk" $it)
          <)
;-> ("h" "j" "j" "h" "i")

; using $count with list search
;----------------------------------------
(find-all 'a '(a b a b a b) (list $count $it))
;-> ((1 a) (2 a) (3 a))
::-

Notes:

- String searches always use regular expressions.
- List pattern searches always use match.
- When exp-key is a list and no comparator is provided,
  match is used automatically.
- In list modes, $it is bound to the current element.
- In string modes, $0, $1, … contain regex captures.
- $count reports the number of matches.

See:
[find](#f-find), [regex](#f-regex), [match](#f-match),
[unify](#f-unify), [search](#f-search), [replace](#f-replace)

---


<a name="f-first"></a>
## first [utf8]

```
syntax: (first lst)
syntax: (first arr)
syntax: (first str)
::-

Description:

Returns the first element of a list or array, or the first
character of a string. The argument is not modified. When
called on an empty list, an error is raised.

On UTF-8 systems, the string form returns the first
**character**, not the first byte.

Examples:

```
; lists
(first '(1 2 3 4 5))
;-> 1

(first '((a b) c d))
;-> (a b)

(set 'lst '(a b c d e))
(first lst)
;-> a

; arrays
(set 'arr (array 3 2 (sequence 1 6)))
;-> ((1 2) (3 4) (5 6))

(first arr)
;-> (1 2)

; empty list
; (first '())  -> error

; strings (UTF-8 safe)
(first "rebel")
;-> "r"

(first (rest "rebel"))
;-> "e"
```

Notes:

- Works on character boundaries for strings in UTF-8 mode.
- For the last element, use [last](#f-last).
- For all but the first element, use [rest](#f-rest).

See: [last](#f-last), [rest](#f-rest)

---


<a name="f-flat"></a>
## flat

```
syntax: (flat lst)
syntax: (flat lst int-level)
::-

Description:

Returns a flattened version of lst. By default, all nested
lists are recursively expanded into a single-level list.

When int-level is supplied, only that many levels are
flattened. A level of 0 means no flattening at all.

Examples:

```
; full flatten
(flat '(a (b (c d))))
;-> (a b c d)

; using flat to walk nested structures
(map (fn (x) (ref x lst))
     (flat '(a (b (c d)))))
;-> ((0) (1 0) (1 1 0) (1 1 1))

; partial flattening by level
;----------------------------------------
(flat '(a b (c d (e f)) (g h (i j))))
;-> (a b c d e f g h i j)

(flat '(a b (c d (e f)) (g h (i j))) 1)
;-> (a b c d (e f) g h (i j))

(flat '(a b (c d (e f)) (g h (i j))) 2)
;-> (a b c d e f g h i j)

; level 0 = no flattening
(flat '(a (b (c d))) 0)
;-> (a (b (c d)))
::-

Notes:

- Useful for iterating over arbitrarily nested structures.
- When int-level is omitted, complete flattening occurs.

See: [ref](#f-ref), [ref-all](#f-ref-all), [map](#f-map)

---



<a name="f-float"></a>
## float

```
syntax: (float exp)
syntax: (float exp exp-default)
::-

Description:

Converts exp to a floating-point number. If exp is already a
number or a numeric string, the corresponding float is
returned. If exp cannot be parsed as a float, the result is
nil unless exp-default is supplied, in which case the value
of exp-default is returned.

Valid string inputs must begin with a digit, +, −, or .
(period). Strings beginning with other characters cannot be
converted. Extremely large or small exponents produce +inf
or -inf.

Examples:

```
; basic parsing
(float "1.23")
;-> 1.23

(float " 1.23")
;-> 1.23

(float ".5")
;-> 0.5

(float "-1.23")
;-> -1.23

; invalid forms
(float "-.5")
;-> nil

(float "#1.23")
;-> nil

; default value
(float "#1.23" 0.0)
;-> 0.0

; non-numeric expressions
(float '(a b c))
;-> nil

(float '(a b c) 0)
;-> 0

(float nil 0)
;-> 0

; default value as non-numeric expression
(float "abc" "not a number")
;-> "not a number"

; overflow to infinity
(float "1e500")
;-> inf

(float "-1e500")
;-> -inf

; user input conversion
;----------------------------------------
; (print "enter a float: ")
; (set 'num (float (read-line)))
::-

Notes:

- Returns nil when conversion fails unless exp-default is
  given.
- Very large positive or negative exponents yield inf or
  -inf.
- For integer parsing, use [int](#f-int).

See: [int](#f-int), [float?](#f-floatp)

---



<a name="f-floatp"></a>
## float?

```
syntax: (float? exp)
::-

Description:

Returns true if exp evaluates to a floating-point number,
otherwise returns nil.

Examples:

```
(set 'num 1.23)
(float? num)
;-> true

(float? 123)
;-> nil

(float? "1.23")
;-> nil
::-

See: [float](#f-float), [int?](#f-intp), [number?](#f-numberp)

---



<a name="f-floor"></a>
## floor

```
syntax: (floor number)
::-

Description:

Returns the greatest integer less than or equal to number.
The result is always a floating-point value.

Examples:

```
(floor -1.5)
;-> -2

(floor 3.4)
;-> 3
::-

See: [ceil](#f-ceil), [int](#f-int)

---


<a name="f-flt"></a>
## flt

```
syntax: (flt number)
::-

Description:

Converts number into a 32-bit IEEE-754 floating value encoded
inside an integer. This is useful when an external C function
expects a 32-bit float rather than Rebel’s native 64-bit float.

The returned integer contains the raw 4-byte float bit pattern.

Examples:

```
(flt 1.23)
;-> 1067282596

;; same encoding shown manually
(get-int (pack "f" 1.23))
;-> 1067282596

;; verify decode
(unpack "f" (pack "ld" (flt 1.2345)))
;-> (1.234500051)
::-

Notes:

- Use when calling external code that requires 32-bit floats.
- The integer returned is a byte-accurate representation of
  the 32-bit float.

See: [pack](#f-pack), [unpack](#f-unpack), [import](#f-import)

---

<a name="f-fn"></a>
## fn

```
syntax: (fn (sym-1 [sym-2 ...]) body-1 [body-2 ...])
syntax: (fn (sym-1 [(sym-2 exp-default) ...]) body-1 [body-2 ...])
```

Description:

Constructs an anonymous function. The resulting function
value can be stored in a symbol, passed as an argument, or
returned from another function. For named functions, see
[define](#f-define).

When the function is invoked, each formal parameter is
bound to its corresponding argument. Extra arguments are
available through the system symbol $args or through the
function (args).

Parameters may specify default values using the form
(sym exp-default). When a call omits such an argument, the
default expression is evaluated and used.

The function returns the value of the last body expression.

Examples:

```
; basic anonymous function
(set 'f (fn (x) (+ x 1)))
(f 10)
;-> 11

; multiple parameters
(set 'add3 (fn (a b c) (+ a b c)))
(add3 1 2 3)
;-> 6

; default parameters
(set 'scale (fn (x (f 2)) (* x f)))
(scale 10)
;-> 20
(scale 10 5)
;-> 50

; variable arity using $args
(set 'sum-all (fn () (apply + $args)))
(sum-all 1 2 3 4 5)
;-> 15

; same using (args)
(set 'sum2 (fn () (apply + (args))))
(sum2 1 2 3 4 5)
;-> 15
```

Notes:

- fn constructs an anonymous function value.
- Extra arguments are available as $args or via (args).
- Parameters follow dynamic scoping rules.
- For named functions, see [define](#f-define).
- For macro functions, use [fn-macro](#f-fn-macro).

See: [define](#f-define), [apply](#f-apply), [fn-macro](#f-fn-macro)

---

<a name="f-fn-macro"></a>
## fn-macro

```
syntax: (fn-macro (sym-1 [sym-2 ...]) body-1 [body-2 ...])
syntax: (fn-macro ((sym-1 exp-default-1) (sym-2 exp-default-2) ...) body-1 [body-2 ...])
```

Description:

Constructs an anonymous runtime macro. When a macro is
called, its arguments arrive **unevaluated**. Inside the
macro you decide which arguments to evaluate, when to
evaluate them, and whether to modify them before
evaluation.

A macro always returns a value. If the macro needs the
result of a computation, it must explicitly call *eval*.
If *eval* is not used, the macro returns unevaluated syntax.

For named macros, see [define-macro](#f-define-macro).

Examples:

```
; 1) simple macro: (inc2 x) → (+ x 2)

(set 'inc2
  (fn-macro (x)
    (+ (eval x) 2)))

(inc2 10)
;-> 12


; 2) custom conditional form
;    (if-zero x a b) → (if (= x 0) a b)

(set 'if-zero
  (fn-macro (x a b)
    (if (= (eval x) 0)
        (eval a)
        (eval b))))

(if-zero 0 "yes" "no")
;-> "yes"

(if-zero 5 "yes" "no")
;-> "no"


; 3) macro that defines a new function
;    (make-fn name (p1) body) → (define (name p1) body)

(set 'make-fn
  (fn-macro (name params body)
    (define (name params) (eval body))))

(make-fn triple (x) (* x 3))
(triple 10)
;-> 30


; 4) macro with a default argument
;    (add2 x) → (+ x 2)

(set 'add2
  (fn-macro ((x 0))
    (+ (eval x) 2)))

(add2 10)
;-> 12

(add2)
;-> 2
```

Notes:

- Macro arguments are not evaluated automatically.
- The macro controls evaluation explicitly using *eval*.
- A macro may return evaluated results or unevaluated syntax.
- Runtime macros allow creation of custom control forms.
- The *make-fn* example cannot be implemented as a normal
  function. A function receives arguments already evaluated,
  but this macro needs the raw syntax of name, params and
  body in order to construct a new function definition.

See: [define-macro](#f-define-macro), [macro](#f-macro), [fn](#f-fn)

---


<a name="f-for"></a>
## for

```
syntax: (for (sym num-from num-to [num-step [exp-break]]) body)
```

Description:

Iterates sym over a numeric range from num-from to num-to
(inclusive), evaluating body on each step. When num-step is
omitted, the loop increments by 1. When num-step is present,
sym advances by that floating-point or integer step.

A break condition can be supplied in exp-break. When present,
the break expression is evaluated before each iteration, and
if it yields a non-nil value, the loop stops and returns that
value. If exp-break is used, num-step must also be specified.

The loop variable sym is dynamically scoped. Its previous
value (if any) is restored after the loop completes or exits.

Examples:

```
;; simple increasing loop
(for (x 1 10)
  (println x))
;-> prints 1 2 3 ... 10

;; custom step
(for (x 1 10 2)
  (println x))
;-> prints 1 3 5 7 9

;; decreasing with fractional step
(for (x 8 6 0.5)
  (println x))
;-> prints 8 7.5 7 6.5 6

;; break when a condition becomes true
(for (x 1 100 2 (> (* x x) 30))
  (println x))
;-> prints 1 3 5
;-> returns true

;; generate a countdown list
(collect 'lst
  (for (n 5 1)
    (push n lst -1)))
lst
;-> (5 4 3 2 1)
::-

Notes:

- Step size must be positive; decreasing ranges work because
  sym still moves toward num-to by the given step.
- exp-break is tested before body, allowing clean early exit.
- for is suitable for numeric iteration; for iterating lists
  see dotimes, dolist, or dotree.

See: [sequence](#f-sequence), [dotimes](#f-dotimes),
[dolist](#f-dolist), [dotree](#f-dotree)

---


<a name="f-for-all"></a>
## for-all

```
syntax: (for-all func-condition list)
::-

Description:

Evaluates func-condition on every element of list. If every
evaluation yields a non-nil value, for-all returns true.
If any element fails the condition, the result is nil.

This function is the logical "AND" counterpart to exists.

Examples:

```
(for-all number? '(2 3 4 6 7))
;-> true

(for-all number? '(2 3 4 6 "hello" 7))
;-> nil

(for-all (fn (x) (= x 10)) '(10 10 10 10 10))
;-> true

;; check if all strings are ASCII-only
(for-all (fn (str) (= (length str)
                      (length (explode str))))
         '("abc" "test" "X9"))
;-> true
```

Notes:

- func-condition may be any predicate: built-in, user-defined,
  or an fn-list.
- Evaluation stops early when a failure is detected.

See: [exists](#f-exists), [filter](#f-filter)

---


<a name="f-fork"></a>
## fork

```
syntax: (fork exp)
```

Description:

Creates a new Unix child process. The child begins execution
by evaluating exp, while the parent continues immediately. Both
processes start from the same memory snapshot, but afterwards
they are fully independent. Changes in one do not affect the
other. On success fork returns the child PID; on failure it
returns nil.

Examples:

```
; basic fork: parent and child diverge
;------------------------------------------------------------
(set 'x 0)

(fork
  (while (< x 5)
    (println (inc x))
    (sleep 1000)))
;-> PID-of-child

; child prints:
;-> 1
;-> 2
;-> 3
;-> 4
;-> 5

x
;-> 0    ; parent unchanged


; communication using a Unix pipe
;------------------------------------------------------------
(map set '(read-end write-end) (pipe))

(define (counter n out)
  (while (> n 0)
    (write-line out (string n))
    (dec n)))

(define (observer in)
  (let (line)
    (while (setq line (read-line in))
      (println "count " line))))

(set 'child-observer (fork (observer read-end)))
(set 'child-counter  (fork (counter 5 write-end)))

(wait-pid child-observer)
(wait-pid child-counter)


; destroying a long-running child
;------------------------------------------------------------
(define (demo)
  (set 'pid
       (fork
         (dotimes (i 1000)
           (println i)
           (sleep 20))))
  (sleep 100)
  (destroy pid))

(demo)
;-> prints numbers, child terminated
```

Notes:

- Parent and child begin with identical memory images but
  run independently.
- Use wait-pid to reap finished children.
- Use pipe for inter-process communication.
- Use destroy to stop a child explicitly.
- For automated parallel work with result aggregation,
  see spawn.

See: [wait-pid](#f-wait-pid), [destroy](#f-destroy),
[pipe](#f-pipe), [spawn](#f-spawn)

---


<a name="f-format"></a>
## format

```
syntax: (format str-format exp-data-1 [exp-data-2 ... ])
syntax: (format str-format list-data)
```

Description:

Builds a formatted string using str-format and the supplied
data values. The formatting rules follow the ANSI C printf
specification: each format specifier in str-format consumes
one data argument. When using the second syntax, all data
values must be provided inside list-data.

format validates the format string and ensures that the
number and type of arguments match the format specifiers.
Use int, float, or string when explicit conversion is
desired. A literal percent sign is written as %% inside
the format string.

The general form of a format specifier is:

```
"%w.pf"

w = optional width  
p = optional precision  
f = required type flag  
```

Examples:

```
(format ">>>%6.2f<<<" 1.2345)
;-> ">>>  1.23<<<"

(format "%e" 123456789)
;-> "1.234568e+08"

(format "Result = %05d" 2)
;-> "Result = 00002"

(format "%-15s" "hello")
;-> "hello          "

(format "%5.2s" "hello")
;-> "   he"

(format "%o" 80)
;-> "120"

(format "%x %X" -1 -1)
;-> "ffffffff FFFFFFFF"

(format "%c" 65)
;-> "A"
```

Format type flags:

```
s     text string  
c     character (1–255)  
d     decimal (32-bit)  
u     unsigned decimal (32-bit)  
x     hexadecimal lowercase  
X     hexadecimal uppercase  
o     octal  
f     floating point  
e     scientific notation (lowercase)  
E     scientific notation (uppercase)  
g     general floating point  
```

Extended integer formats:

These forms are available for working with full-width integers
used by Rebel. They accept and format 64-bit integers.

```
ld     signed decimal  
lu     unsigned decimal  
lx     hexadecimal  
lX     hexadecimal uppercase  

lld    signed decimal (long long)  
llu    unsigned decimal (long long)  
llx    hexadecimal (long long)  
llX    hexadecimal uppercase (long long)  
```

More examples:

```
(format "%14.2f" 12345678.12)
;-> "   12345678.12"

(format "%8d" 12345)
;-> "   12345"
```

Using a list as the data source:

```
(set 'lst '("hello" 123))
(format "%15s %d" lst)
;-> "          hello 123"
```

Automatic conversion if required by the format:

```
(format "%f" 123)
;-> "123.000000"

(format "%d" 123.456)
;-> 123
```

Notes:

- format is strict: mismatched specifiers and argument counts
  cause an error.
- All formatting rules operate exactly as defined by the
  printf family of functions.
- Width and precision fields work on all supported types.
- format is suitable both for human-readable strings and
  for constructing structured text output.

See: [string](#f-string), [int](#f-int), [float](#f-float)

---


<a name="f-fv"></a>
## fv

```
syntax: (fv num-rate num-nper num-pmt num-pv [int-type])
```

Description:

Computes the future value of a payment stream defined by
a constant interest rate, periodic fixed payments, and an
initial principal amount. num-rate is the interest rate
per period. num-nper is the number of periods. num-pmt is
the payment applied each period. num-pv is the principal
value at the beginning of the schedule.

When int-type is omitted or 0, payments occur at the end
of each period. When int-type is 1, payments occur at the
beginning of each period. The returned value represents
the balance after all interest and payments have been
applied.

Examples:

```
; end-of-period payment
(fv (div 0.07 12) 240 775.30 -100000)
;-> -0.5544645052

; beginning-of-period payment
(fv (div 0.07 12) 240 775.30 -100000 1)
;-> 54.093...
```

Notes:

- num-rate must be expressed as the rate per period
- int-type controls timing of payments
- signs of num-pv and num-pmt define cash direction

See: [irr](#f-irr), [nper](#f-nper), [npv](#f-npv),
[pmt](#f-pmt), [pv](#f-pv)

---


<a name="f-gammai"></a>
## gammai

```
syntax: (gammai num-a num-b)
```

Description:

Evaluates the normalized incomplete Gamma function for the
parameters num-a and num-b. The result is a floating-point
value in the range from 0 to 1. This function is commonly
used in probability calculations involving Chi-squared
distributions and related statistical measures.

The probability that a Chi-squared statistic exceeds a
given value can be expressed using gammai as:

```
Q(chi2 | df) = Q(df/2, chi2/2) = gammai(df/2, chi2/2)
```

Examples:

```
(gammai 4 5)
;-> 0.7349740847
```

Notes:

- Arguments must be positive real numbers.
- The result corresponds to the upper-tail probability of
  the Gamma distribution.

See: [prob-chi2](#f-prob-chi2)

---


<a name="f-gammaln"></a>
## gammaln

```
syntax: (gammaln num-x)
```

Description:

Computes the natural logarithm of the Gamma function for
num-x. The Gamma function generalizes factorials to real
numbers, and gammaln provides a numerically stable way to
evaluate log(gamma(x)) for a wide domain of inputs.

The relationship n! = gamma(n + 1) allows gammaln to be
used for factorial computations by exponentiating the
returned logarithmic value.

The log Gamma function is also related to the Beta
function. The Beta function can be written in terms of
gammaln as:

```
Beta(z, w) = Exp(gammaln(z) + gammaln(w) - gammaln(z + w))
```

Examples:

```
(exp (gammaln 6))
;-> 120
```

Notes:

- gammaln avoids overflow that may occur when evaluating
  gamma(x) directly.
- Input num-x must be positive for most practical uses.

See: [gammai](#f-gammai)

---


<a name="f-gcd"></a>
## gcd

```
syntax: (gcd int-1 [int-2 ...])
```

Description:

Computes the greatest common divisor of one or more
integers. The gcd of two integers that are not both zero
is the largest positive integer dividing both values
without remainder. When more than two integers are given,
the function reduces the sequence from left to right by
repeatedly applying the gcd operation.

If only one integer is supplied, that integer is returned.
If all arguments are zero, the result is zero. Negative
arguments are handled by absolute value before reduction.

Examples:

```
(gcd 0)
;-> 0

(gcd 0 0)
;-> 0

(gcd 10)
;-> 10

(gcd 12 36)
;-> 12

(gcd 15 36 6)
;-> 3
```

Notes:

- Arguments may be normal integers or big integers.
- gcd always returns a non-negative integer.
- Reduction is left-associative across all arguments.

See: [lcm](#f-lcm)

---

<a name="f-get-char"></a>
## get-char

```
syntax: (get-char int-address)
```

Description:

Reads an 8-bit unsigned value from the memory address in
int-address. This function is used with the FFI when a
foreign function returns a pointer to a byte buffer or to
a structure containing byte fields. The returned value is
an integer in the range 0 to 255. The pointer is taken
as-is. Invalid or stale pointers may terminate the process.

Examples:

```
; Step 1: Create a small C file returning a pointer to bytes
;-----------------------------------------------------------

unsigned char *
make_bytes(void)
{
  static unsigned char buf[] = "Hello";
  return buf;
}

; Step 2: Compile it as a shared object (my.so)
;-----------------------------------------------------------
;   cc -shared -fPIC -o my.so my.c

; Step 3: Import the function in Rebel
;-----------------------------------------------------------

(import "./my.so" "make_bytes")

; pointer returned by the C function
(make_bytes)
;-> 140737353912720    ; example address

; read bytes using get-char
(get-char (make_bytes))
;-> 72

(get-char (+ (make_bytes) 1))
;-> 101
```

Notes:

- Reads exactly one byte from memory.
- The caller is responsible for providing valid pointers.
- Intended for use with functions imported via the FFI.

See: [address](#f-address), [get-int](#f-get-int),
[get-long](#f-get-long), [get-float](#f-get-float),
[get-string](#f-get-string), [pack](#f-pack),
[unpack](#f-unpack)

---


<a name="f-get-float"></a>
## get-float

```
syntax: (get-float int-address)
```

Description:

Reads a 64-bit floating-point number in IEEE double
format from the memory address in int-address. This
function is used with the FFI when a foreign function
returns a pointer to a double value or to a structure
containing double fields. The address is taken as-is.
Invalid pointers may terminate the process.

Examples:

```
; Step 1: Create a small C file returning a pointer to a double
;-----------------------------------------------------------
double *
make_value(void)
{
  static double val = 123.456;
  return &val;
}

; Step 2: Compile it as a shared object (my.so)
;-----------------------------------------------------------
;   cc -shared -fPIC -o my.so my.c

; Step 3: Import the function in Rebel
;-----------------------------------------------------------
(import "./my.so" "make_value")

; pointer returned by the C function
(make_value)
;-> 140737353912720    ; example address

; read the floating-point value
(get-float (make_value))
;-> 123.456
```

Notes:

- Reads exactly one 64-bit IEEE double.
- Pointer validity is the caller's responsibility.
- Intended for pointers returned by imported functions.

See: [address](#f-address), [get-char](#f-get-char),
[get-int](#f-get-int), [get-long](#f-get-long),
[get-string](#f-get-string), [pack](#f-pack),
[unpack](#f-unpack)

---


<a name="f-get-int"></a>
## get-int

```
syntax: (get-int int-address)
```

Description:

Reads a 32-bit signed integer from the memory address in
int-address. This function is used with the FFI when a
foreign function returns a pointer to a 32-bit integer or
to a structure containing integer fields. The address is
taken as-is. Invalid pointers may terminate the process.

Examples:

```
; Step 1: Create a C file returning a pointer to a 32-bit integer
;-----------------------------------------------------------
int *
make_int(void)
{
  static int val = 123;
  return &val;
}

; Step 2: Compile it as a shared object (my.so)
;-----------------------------------------------------------
;   cc -shared -fPIC -o my.so my.c

; Step 3: Import the function in Rebel
;-----------------------------------------------------------
(import "./my.so" "make_int")

; pointer returned by the C function
(make_int)
;-> 140737353912720    ; example address

; read the integer value
(get-int (make_int))
;-> 123
```

Notes:

- Reads one 32-bit signed integer.
- Pointer validity is the caller's responsibility.
- Intended for pointers returned by imported functions.

See: [address](#f-address), [get-char](#f-get-char),
[get-float](#f-get-float), [get-long](#f-get-long),
[get-string](#f-get-string), [pack](#f-pack),
[unpack](#f-unpack)

---


<a name="f-get-long"></a>
## get-long

```
syntax: (get-long int-address)
```

Description:

Reads a 64-bit signed integer from the memory address in
int-address. This function is used with the FFI when a
foreign function returns a pointer to a long integer or to
a structure containing 64-bit integer fields. The address
is taken as-is. Invalid pointers may terminate the process.

Examples:

```
; Step 1: Create a C file returning a pointer to a 64-bit integer
;-----------------------------------------------------------
long *
make_long(void)
{
  static long val = 9876543210L;
  return &val;
}

; Step 2: Compile it as a shared object (my.so)
;-----------------------------------------------------------
;   cc -shared -fPIC -o my.so my.c

; Step 3: Import the function in Rebel
;-----------------------------------------------------------
(import "./my.so" "make_long")

; pointer returned by the C function
(make_long)
;-> 140737353912720    ; example address

; read the 64-bit integer value
(get-long (make_long))
;-> 9876543210
```

Notes:

- Reads one 64-bit signed integer from memory.
- Pointer validity is the caller's responsibility.
- Intended for pointers returned by imported functions.

See: [address](#f-address), [get-char](#f-get-char),
[get-int](#f-get-int), [get-float](#f-get-float),
[get-string](#f-get-string), [pack](#f-pack),
[unpack](#f-unpack)

---



<a name="f-get-string"></a>
## get-string

```
syntax: (get-string int-address [int-bytes [str-limit]])
```

Description:

Copies a sequence of bytes from the memory address in
int-address and returns a Rebel string. When only the
address is given, copying stops at the first zero byte.
This is suitable for reading standard zero-terminated
strings returned by foreign functions.

When int-bytes is specified, exactly int-bytes bytes are
copied regardless of zero bytes. When a str-limit string
is supplied, copying stops when this limit sequence is
found or when int-bytes bytes have been read.

The address is taken as-is. Invalid pointers may
terminate the process.

Examples:

```
; Step 1: Create a C file returning a pointer to a C string
;-----------------------------------------------------------
char *
make_bytes(void)
{
  static char buf[] = "ABCDEFG";
  return buf;
}

; Step 2: Compile it as a shared object (my.so)
;-----------------------------------------------------------
;   cc -shared -fPIC -o my.so my.c

; Step 3: Import the function in Rebel
;-----------------------------------------------------------
(import "./my.so" "make_bytes")

; basic string read: stops at first zero byte
(get-string (make_bytes))
;-> "ABCDEFG"

; Step 4: Reading from a Rebel buffer
;-----------------------------------------------------------
(set 'buff "ABC\000\000\000DEF")
;-> "ABC\000\000\000DEF"

(get-string buff)
;-> "ABC"

; offset into the buffer
(get-string (+ (address buff) 6))
;-> "DEF"

; Step 5: Using int-bytes to read a fixed-length block
;-----------------------------------------------------------
(get-string buff 9)
;-> "ABC\000\000\000DEF"

; Step 6: Using a string limit
;-----------------------------------------------------------
(set 'buff2 "ABC\000\000EFG\000DQW")

(get-string buff2 4 "FG")
;-> "ABC\000"

(get-string buff2 10)
;-> "ABC\000\000EFG\000D"

(get-string buff2 10 "FG")
;-> "ABC\000\000E"

; Step 7: UTF-32 buffer with explicit terminator limit
;-----------------------------------------------------------
(set 'utf32 (unicode "我能吞下玻璃而不伤身体。"))
(set 'addr (address utf32))

(get-string utf32 80 "\000\000\000\000")
;-> a raw UTF-32 byte sequence up to the terminator
```

Notes:

- Without int-bytes, copying stops at the first zero byte.
- With int-bytes, exactly that number of bytes is copied.
- str-limit stops copying when the limit string appears.
- Passing invalid pointers may crash the process.
- Passing a Rebel string automatically yields its address.

See: [get-char](#f-get-char), [get-int](#f-get-int),
[get-float](#f-get-float), [get-long](#f-get-long),
[pack](#f-pack), [unpack](#f-unpack)

---


<a name="f-get-url"></a>
## get-url

```
syntax: (get-url str-url [str-option] [int-timeout [str-header]])
```

Description:

Fetches data from the URL in str-url using the HTTP GET
method. Both http:// and file:// URLs are supported. When
no option is given, the function returns the body as a
string.

The "header" option returns only the HTTP header. The
"list" option returns a list containing header, body, and
the numeric status code. The "raw" option disables
automatic redirection. The "debug" option prints outgoing
request data. Options may be combined in the same string.

The int-timeout value specifies a timeout in milliseconds.
On timeout, get-url returns "ERR: timeout". Other errors
return a string starting with "ERR:" followed by a
description.

The str-header parameter may contain custom HTTP header
lines. Each must end with "\r\n". When str-header is
supplied, only the GET line, Host:, and Connection:
entries are generated automatically. All other fields come
from str-header.

HTTP redirects are followed unless the "raw" option is
used. Chunked Transfer-Encoding responses are unpacked.

Rebel does not support HTTPS requests. When HTTPS access
is required, use a proxy configured in the system.

Examples:

```
; basic GET request
;-----------------------------------------------------------
(get-url "http://example.com")

; GET with timeout
;-----------------------------------------------------------
(get-url "http://example.com" 3000)

; request only the HTTP header
;-----------------------------------------------------------
(get-url "http://example.com" "header")

; header + timeout
;-----------------------------------------------------------
(get-url "http://example.com" "header" 5000)

; return (header body status-code)
;-----------------------------------------------------------
(get-url "http://example.com" "list")

; reading a local file via file://
;-----------------------------------------------------------
(get-url "file:///home/user/data.txt")

; custom header supplied by str-header
;-----------------------------------------------------------
(get-url "http://example.com" 5000
"User-Agent: Rebel\r\nCookie: test=1\r\n")
```

Notes:

- "header" returns only the HTTP header.
- "list" returns (header body status-code).
- "raw" disables redirect handling.
- "debug" prints outgoing request data.
- file:// reads files from the local filesystem.
- Custom headers must end with "\r\n".

See: [put-url](#f-put-url), [post-url](#f-post-url),
[address](#f-address), [get-string](#f-get-string)

---


<a name="f-global"></a>
## global

```
syntax: (global sym-1 [sym-2 ...])
```

Description:

Marks one or more symbols as globally accessible outside
the MAIN context. Only symbols defined in MAIN can be
made global, and the call to global must also be executed
from MAIN. The function returns the last symbol processed.

A symbol made global becomes visible in all other
contexts and can be referenced or called without prefixing
it with MAIN:. This mechanism is typically used for
exporting functions or constants from the MAIN namespace.

Examples:

```
; making several MAIN symbols global
;-----------------------------------------------------------
(global 'a 'x 'y 'z)
;-> z

; exporting a function and protecting it
;-----------------------------------------------------------
(define (f x) (+ x 1))
(constant (global 'f))
```

Notes:

- Only symbols belonging to MAIN can be marked global.
- The call to global must be executed in MAIN.
- Returns the last symbol passed to the function.

See: [constant](#f-constant), [context](#f-context),
[define](#f-define)

---


<a name="f-globalp"></a>
## global?

```
syntax: (global? sym)
```

Description:

Checks whether the symbol in sym is marked as global.
Built-in functions, context symbols, and any symbol
declared global using the global function are considered
global. Returns true or nil.

Examples:

```
; built-in functions are always global
;-----------------------------------------------------------
(global? 'print)
;-> true

; marking a symbol as global
;-----------------------------------------------------------
(set 'val 123)
(global 'val)
(global? 'val)
;-> true

; exporting and protecting a symbol
;-----------------------------------------------------------
(set 'step 10)
(constant (global 'step))
(global? 'step)
;-> true
```

Notes:

- Returns true for all built-in functions.
- Returns true for symbols exported via global.
- Returns nil for non-global MAIN symbols.

See: [global](#f-global), [constant](#f-constant),
[context](#f-context)

---


<a name="f-history"></a>
## history

```
syntax: (history [bool-params])
```

Description:

Returns a list describing the call history of the current
function. Without bool-params, only the names of calling
functions are returned, starting with the current
function. When bool-params evaluates to true, each entry
includes both the function name and the argument pattern
used in the call.

Examples:

```
; track calling functions
;-----------------------------------------------------------
(define (f1 x y)
  (f2 (+ x 1) (* y 2)))

(define (f2 a b)
  (history))

(f1 1 2)
;-> (f2 f1)

; include argument patterns
;-----------------------------------------------------------
(define (f2 a b)
  (history true))

(f1 1 2)
;-> ((f2 (+ x 1) (* y 2)) (f1 1 2))
```

Notes:

- Without bool-params, only function names are returned.
- With bool-params true, argument patterns are included.
- The first element always describes the current function.

See: [define](#f-define), [apply](#f-apply),
[context](#f-context)

---


<a name="f-if"></a>
## if

```
syntax: (if exp-condition exp-1 [exp-2])
syntax: (if exp-cond-1 exp-1 exp-cond-2 exp-2 [ ... ])
```

Description:

Evaluates exp-condition. When the condition is neither nil
nor an empty list, exp-1 is evaluated and returned.
Otherwise exp-2 is evaluated and returned. If exp-2 is
omitted, the raw value of the condition is returned.

During evaluation, if binds the anaphoric variable $it to
the tested condition value. This allows using $it inside
the true branch.

When a branch must execute more than one expression, the
expressions must be grouped using begin. Without begin,
only the first expression would be part of that branch.
Both true and false branches follow this rule.

The second form accepts multiple condition–expression
pairs. Evaluation proceeds from left to right and returns
the expression associated with the first true condition.
If the final expression is omitted, the last condition’s
value is returned. This behaves like cond but requires no
parentheses around each pair.

Examples:

```
; basic conditional evaluation
;-----------------------------------------------------------
(set 'x 50)
(if (< x 100) "small" "big")
;-> "small"

(set 'x 1000)
(if (< x 100) "small" "big")
;-> "big"

; false branch absent: value of condition is returned
(if (> x 2000) "big")
;-> nil

; multiple expressions require begin
;-----------------------------------------------------------
(if (= x 10)
  (begin
    (println "true-branch")
    (println x))
  (begin
    (println "false-branch")
    (println x)))

; using the anaphoric variable $it
;-----------------------------------------------------------
(set 'lst '(A B C))
(if lst (println (last $it)))
;-> C

; multi-branch if
;-----------------------------------------------------------
(define (classify n)
  (if
    (< n 0)  "negative"
    (< n 10) "small"
    (< n 20) "medium"
    (>= n 30) "big"
    "n/a"))

(classify 15)
;-> "medium"

(classify 100)
;-> "big"

(classify 22)
;-> "n/a"

(classify -10)
;-> "negative"
```

Notes:

- begin is required whenever a branch must run more than
  one expression.
- $it is bound only during evaluation of the condition and
  the true branch.
- In the multi-branch form, evaluation stops at the first
  true condition.

See: [begin](#f-begin), [when](#f-when), [unless](#f-unless),
[cond](#f-cond)

---


<a name="f-ifft"></a>
## ifft

```
syntax: (ifft list-num)
```

Description:

Computes the inverse discrete Fourier transform of the
sequence in list-num using an FFT-based method. Each
complex number is represented as a two-element list
(real imag). When a value in the input list is a simple
number, it is treated as a complex value with an
imaginary part of 0.0.

If the number of elements in list-num is not an integer
power of two, the sequence is automatically padded with
zeros to the next power of two before transformation.

The result is always a list of complex pairs, each pair
containing the real and imaginary components of the
inverse transform.

Examples:

```
; inverse transform of a real-valued sequence
;-----------------------------------------------------------
(ifft (fft '((1 0) (2 0) (3 0) (4 0))))
;-> ((1 0) (2 0) (3 0) (4 0))

; imaginary part defaults to 0.0 when plain numbers are used
;-----------------------------------------------------------
(ifft (fft '(1 2 3 4)))
;-> ((1 0) (2 0) (3 0) (4 0))
```

Notes:

- Input is padded with zeros when its length is not a
  power of two.
- Plain numbers are promoted to complex values with
  imaginary part 0.0.
- The result always uses explicit complex pairs.

See: [fft](#f-fft)

---


<a name="f-import"></a>
## import

```
syntax: (import str-lib-name str-function-name)
syntax: (import str-lib-name str-function-name str-return-type [str-param-type ...])
syntax: (import str-lib-name)
```

Description:

Loads a foreign function from a shared library and makes it
callable from Rebel. The import call returns a callable
function object referencing the foreign symbol.

Rebel always uses the extended libffi import API on 64-bit
Unix systems. Argument and return types can be specified
explicitly using type labels. Imported symbols are
protected and can only be rebound using constant.

The simple two-argument syntax allows importing foreign
functions without specifying types. This is suitable for
functions returning integers, pointers, or performing
side effects. For fully typed interaction, use the second
syntax with explicit type labels.

The third syntax loads a library without importing any
symbol. This is useful when other imported functions
require internal dependencies provided by the library.

Strings are passed by address to zero-terminated buffers.
Returned pointers must be freed by the foreign library when
necessary. Rebel only manages memory it allocates itself.

Type Rules (LP64 model):

On all 64-bit Unix systems Rebel follows LP64: int is
32 bits, long and pointers are 64 bits, and double is
IEEE-754 64-bit. The table shows the valid type labels.

```
label                C type (LP64)                   Rebel type
---------------------------------------------------------------------------
"void"               void                            nil

"byte"               unsigned char (8 bit)           integer
"char"               signed char (8 bit)             integer

"unsigned short"     unsigned short (16 bit)         integer
"short"              short (16 bit)                  integer

"unsigned int"       unsigned int (32 bit)           integer
"int"                int (32 bit)                    integer

"long"               long (64 bit)                   integer
"long long"          long long (64 bit)              integer

"float"              float (32 bit)                  64-bit float (cut to 32)
"double"             double (64 bit)                 64-bit float

"char*"              char* pointer                   string buffer or return string
"void*"              void* pointer                   integer address
```

Notes on pointer types:

- "char*" returns a displayable C string. As an argument it
  expects a Rebel string buffer (dup "\000" n).
- "void*" accepts or returns any pointer as an integer
  address, suitable for get-string, get-char, unpack,
  struct, and other helper functions.

General FFI rules:

- Strings are always passed by address.
- Buffers are created using (dup "\000" size).
- Imported functions may take up to fourteen parameters.
- All 64-bit Rebel builds support only the cdecl ABI.
  No alternative calling conventions exist on Unix.

Examples:

```
; import printf with simple syntax
;-----------------------------------------------------------
(import "/usr/lib/libc.so" "printf")

(printf "hello %d\n" 123)
;-> 10

; import strcpy with explicit type labels
;-----------------------------------------------------------
(import "/usr/lib/libc.so" "strcpy" "char*" "char*" "char*")

(set 'src "hello world")
(set 'dst (dup "\000" (length src)))

(strcpy dst src)
;-> "hello world"

; load a library without importing symbols
;-----------------------------------------------------------
(import "/usr/local/lib/libm.so")

; function returning a double
;-----------------------------------------------------------
(import "/usr/lib/libc.so" "atof" "double" "char*")
(atof "3.141")
;-> 3.141

; returning a raw pointer (void*)
;-----------------------------------------------------------
(import "/usr/lib/libc.so" "strcpy" "void*" "char*" "char*")

(set 'buf (dup "\000" 32))
(strcpy buf "test")
;-> an address number

(get-string buf)
;-> "test"
```

See: [get-string](#f-get-string), [get-char](#f-get-char),
[pack](#f-pack), [unpack](#f-unpack), [address](#f-address)

---


<a name="f-inc"></a>
## inc

```
syntax: (inc place [num])
```

Description:

Increments the numeric value stored in place using
floating-point arithmetic. When num is omitted, the
value increases by 1.0. When num is supplied, it is
added after converting integers to floating-point
numbers.

place may refer to a symbol, a writable element inside
a list structure, or any expression yielding a number.
If place evaluates to nil, it is treated as 0.0 before
the increment. When place designates a writable
location, that location is updated in place. When place
is a computed expression with no writable target, the
incremented result is returned but not stored.

Examples:

```
(set 'x 0) ;-> 0
(inc x) ;-> 1
x ;-> 1

(inc x 0.25) ;-> 1.25
x ;-> 1.25
(inc x) ;-> 2.25

z ;-> nil
(inc z) ;-> 1

(set 'z nil)
(inc z 0.01) ;-> 0.01

(set 'l '(1 2 3 4))
(inc (l 3) 0.1) ;-> 4.1
(inc (first l)) ;-> 2
l ;-> (2 2 3 4.1)

(inc (+ 3 4)) ;-> 8
```

Notes:

- Performs floating-point arithmetic.
- Nil is treated as 0.0 when used as place.
- For integer increments, use ++.
- For floating-point decrements, use dec.

See: [++](#f-plusplus), [dec](#f-dec)

---


<a name="f-index"></a>
## index

```
syntax: (index exp-predicate exp-list)
```

Description:

Applies exp-predicate to every element of exp-list and
returns a list of all indices where the predicate
evaluates to true. Indexing begins at 0. The predicate
may be any test function: a built-in predicate, a
user-defined function, or a lambda expression.

exp-list is evaluated once before iteration. The order
of indices in the result list matches the order of
elements in exp-list.

Examples:

```
(index symbol? '(1 2 d 4 f g 5 h))
;-> (2 4 5 7)

(define (big? x) (> x 5))
;-> (lambda (x) (> x 5))

(index big? '(1 10 3 6 4 5 11))
;-> (1 3 6)
```

Notes:

- Returns indices, not the matching elements.
- Often combined with select to extract values at the
  returned indices.
- Use filter to obtain the elements themselves.

See: [filter](#f-filter), [clean](#f-clean), [select](#f-select)

---

<a name="f-infp"></a>
## inf?

```
syntax: (inf? float)
```

Description:

Checks whether float represents a floating-point infinity.
Returns true when the value is +inf or -inf, otherwise
returns nil. The argument is evaluated and must be a
floating-point number; non-float values always yield nil.

Floating-point infinities may arise from operations such
as dividing a floating value by zero. Integer division
by zero produces an error instead of an infinite value.

Examples:

```
(inf? (div 1 0))
;-> true

(div 0 0)
;-> NaN
```

Notes:

- Only floating-point infinities return true.
- Integer-mode division by zero (e.g. (/ 1 0)) signals an
  error and never produces infinity.
- Use nan? to check whether a floating-point value is not
  a valid number.

See: [nan?](#f-nanp), [div](#f-div)

---


<a name="f-int"></a>
## int

```
syntax: (int exp [exp-default [int-base]])
```

Description:

Converts exp into an integer. When exp evaluates to a
number or a string, its value is parsed and returned as
an integer. If conversion fails, the function returns
nil or, when exp-default is provided, the result of
evaluating exp-default.

Strings must begin with one of the following:

- optional spaces, then digits  
- optional spaces, then + or –  
- "0x" or "0X" for hexadecimal  
- "0" as a prefix for octal  
- "0b" or "0B" for binary  

int-base forces conversion using a specific base. When
int-base is present, prefix rules are ignored and the
string is interpreted strictly in that base.

Floating-point values convert by truncation toward zero.
Values exceeding the 64-bit integer range are truncated
to the nearest limit. NaN values convert to 0.

Examples:

```
(int "123") ;-> 123
(int " 123") ;-> 123
(int "a123" 0) ;-> 0
(int (trim " 123")) ;-> 123

(int "0xFF") ;-> 255
(int "0b11111") ;-> 31
(int "055") ;-> 45

(int "1.567") ;-> 1
(int 1.567) ;-> 1

(integer? 1.00) ;-> nil
(integer? (int 1.00)) ;-> true

(int "1111" 0 2) ;-> 15
(int "0FF" 0 16) ;-> 255

(int 'xyz) ;-> nil
(int 'xyz 0) ;-> 0
(int nil 123) ;-> 123

(int "abc" (throw-error "not a number"))
;-> ERR: user error : not a number

(print "Enter a num:")
(set 'num (int (read-line)))

(int (bits 12345) 0 2) ;-> 12345
```

Notes:

- Out-of-range integers are truncated to 64-bit limits.
- NaN converts to 0.
- Base-forced parsing bypasses prefix recognition.
- The inverse of int with base 2 is bits.
- Use float for floating-point conversion.

See: [float](#f-float), [bits](#f-bits), [integer?](#f-integerp)

---


<a name="f-integerp"></a>
## integer?

```
syntax: (integer? exp)
```

Description:

Checks whether exp evaluates to an integer. Returns true
only when the resulting value is an exact integer. All
other types, including floats, strings, symbols, lists,
and nil, yield nil. Floating-point values that happen to
print without a fractional part are not considered
integers unless explicitly converted.

Examples:

```
(set 'num 123) ;-> 123
(integer? num) ;-> true

(integer? 3.0) ;-> nil
(integer? (int 3.0)) ;-> true
```

Notes:

- Tests the evaluated value, not the literal form.
- Floating-point numbers are never integers unless
  explicitly converted.

See: [int](#f-int), [float](#f-float), [number?](#f-numberp)

---


<a name="f-intersect"></a>
## intersect

```
syntax: (intersect list-A list-B)
syntax: (intersect list-A list-B bool)
```

Description:

Computes the intersection of list-A and list-B. In the
first form, the result contains one copy of each value
that appears in both lists, preserving the order in
which those values first occur in list-A. Duplicates in
either input list are ignored in this mode.

In the second form, when bool evaluates to true (any
non-nil value), duplicates from list-A are preserved.
Every element of list-A that is also present in list-B
is included in the result in its original order, and no
deduplication occurs.

Membership tests follow standard list element equality.

Examples:

```
(intersect '(3 0 1 3 2 3 4 2 1) '(1 4 2 5))
;-> (2 4 1)

(intersect '(3 0 1 3 2 3 4 2 1) '(1 4 2 5) true)
;-> (1 2 4 2 1)
```

Notes:

- First form removes duplicates.
- Second form preserves duplicates from list-A.
- Order of results always follows list-A.

See: [difference](#f-difference), [unique](#f-unique), [union](#f-union)

---


<a name="f-invert"></a>
## invert

```
syntax: (invert matrix [float-pivot])
```

Description:

Computes the inverse of a square matrix. The matrix may
be given as a nested list or an array. The number of
columns is taken from the length of the first row; if a
row contains fewer elements, missing values are treated
as 0.0. The matrix must be non-singular unless a pivot
override is specified.

Inversion is performed using LU-decomposition. When
float-pivot is supplied, it is used to replace pivot
elements that would otherwise become zero during the
decomposition. This allows returning an approximate
inverse even for singular matrices; results depend on
the magnitude of float-pivot.

Without float-pivot, singular matrices cause invert to
return nil.

Examples:

```
(set 'm1 '((-1 1 1) (1 4 -5) (1 -2 0)))
(invert m1)
;-> ((10 2 9) (5 1 4) (6 1 5))

(invert (invert m1))
;-> ((-1 1 1) (1 4 -5) (1 -2 0))

; solve m1 * x = b
(multiply (invert m1) '((1) (2) (3)))
;-> ((41) (19) (23))

; singular matrix handling
(invert '((2 -1) (4 -2)))
;-> nil

(invert '((2 -1) (4 -2)) 0.0)
;-> ((inf -inf) (inf -inf))

(invert '((2 -1) (4 -2)) 1e-20)
;-> ((5e+19 -2.5e+19) (1e+20 -5e+19))
```

Notes:

- Matrix must be square.
- Missing elements in rows are treated as 0.0.
- Without float-pivot, singular matrices yield nil.
- With float-pivot, approximate inverses may be
  produced.

See: [det](#f-det), [mat](#f-mat), [multiply](#f-multiply), [transpose](#f-transpose)

---


<a name="f-irr"></a>
## irr

```
syntax: (irr list-amounts [list-times [num-guess]])
```

Description:

Computes the internal rate of return (IRR) for a cash
flow over time. list-amounts contains outgoing values
(positive numbers) and incoming values (negative
numbers). IRR is the discount rate that makes the net
present value of the entire cash flow equal to 0.0.

When list-times is omitted, each entry in list-amounts
is assumed to occur at times 1, 2, 3, ... in strictly
increasing order. When list-times is supplied, it must
provide explicit time points for each amount.

The computation is iterative. The initial guess defaults
to 0.5 unless num-guess is given. The algorithm stops
when the rate converges to within 0.000001. If no
convergence is reached within 50 iterations, the
function returns nil.

Examples:

```
(irr '(-1000 500 400 300 200 100))
;-> 0.2027

(npv 0.2027 '(500 400 300 200 100))
;-> 1000.033848

(irr '(-1000 500 400 300 200 100) '(0 3 4 5 6 7))
;-> 0.0998

(irr '(-5000 -2000 5000 6000) '(0 3 12 18))
;-> 0.0321
```

Notes:

- list-amounts must contain at least one negative and
  one positive value for IRR to be meaningful.
- list-times must have the same length as list-amounts
  when provided.
- Returns nil when the iteration fails to converge.

See: [fv](#f-fv), [nper](#f-nper), [npv](#f-npv), [pmt](#f-pmt), [pv](#f-pv)

---


<a name="f-json-error"></a>
## json-error

```
syntax: (json-error)
```

Description:

Retrieves information about the most recent JSON parsing
failure. When json-parse returns nil, this function
returns a two-element list containing an error message
string and the character position at which the parser
detected the problem. If no JSON error has occurred,
json-error returns nil.

Examples:

```
; failed parse returns nil
(json-parse "{\"address\" \"http://example.com\"}")
;-> nil

; inspect error info from previous failure
(json-error)
;-> ("missing : colon" 11)
```

Notes:

- Only meaningful immediately after a json-parse call
  that returned nil.
- The position is zero-based and refers to the input
  string passed to json-parse.
- Returns nil when no JSON parsing error is pending.

See: [json-parse](#f-json-parse), [json-format](#f-json-format)

---


<a name="f-join"></a>
## join

```
syntax: (join list-of-strings [str-joint [bool-trail-joint]])
```

Description:

Concatenates all strings in list-of-strings into a single
string. When str-joint is provided, it is placed between
each element. When bool-trail-joint evaluates to true,
the joint string is also appended after the final
element.

All elements of list-of-strings are converted to strings
before concatenation. An empty list produces an empty
string.

Examples:

```
(set 'lst '("this" "is" "a" "sentence"))
(join lst " ")
;-> "this is a sentence"

(join (map string (slice (now) 0 3)) "-")
;-> "2003-11-26"

(join (explode "keep it together"))
;-> "keep it together"

(join '("A" "B" "C") "-")
;-> "A-B-C"

(join '("A" "B" "C") "-" true)
;-> "A-B-C-"
```

Notes:

- str-joint defaults to an empty string.
- bool-trail-joint controls whether the joint string
  appears at the end.
- All elements are stringified before joining.

See: [append](#f-append), [string](#f-string),
     [explode](#f-explode)

---


<a name="f-kmeans-query"></a>
## kmeans-query

```
syntax: (kmeans-query list-data matrix-centroids)
syntax: (kmeans-query list-data matrix-data)
```

Description:

Computes Euclidean distances between the feature vector
in list-data and each row of the given matrix. In the
first form, matrix-centroids contains centroid vectors
from a previous kmeans-train call. In the second form,
matrix-data contains arbitrary data points (nested list
or 2-dimensional array).

list-data is an m-element vector. Each row of the
matrix must contain at least m feature values. When row
lengths differ, or when a row contains extra values
(e.g. cluster labels), only the smallest common length
is used. For each row, the Euclidean distance to
list-data is returned. The result is a list of numeric
distances in matrix row order.

Examples:

```
; centroids from previous kmeans-train
K:centroids
;-> ((6.39 7.188333333 5.935)
;->   (7.925714286 3.845714286 9.198571429)
;->   (2.207142857 2.881428571 0.8885714286))

(kmeans-query '(1 2 3) K:centroids)
;-> (8.036487279 9.475994267 2.58693657)

; smallest distance corresponds to centroid #3

; distances to all original data points
(kmeans-query '(1 2 3) data)
;-> (10.91671196 3.190626898 9.19723328 3.014415366 9.079763213
;->   6.83130295 8.533111976 9.624816881 6.444261013 2.013107051
;->   3.186549858 9.475199206 9.32936761 2.874786949 7.084638311
;->   10.96221237 10.50080473 3.162419959 2.423674896 9.526436899)

; filter distances by cluster membership

; cluster 1
(select (kmeans-query '(1 2 3) data) (K:clusters 0))
;-> (9.079763213 6.83130295 9.624816881 6.444261013 7.084638311 10.50080473)

; cluster 2
(select (kmeans-query '(1 2 3) data) (K:clusters 1))
;-> (10.91671196 9.19723328 8.533111976 9.475199206 9.32936761
;->   10.96221237 9.526436899)

; cluster 3
(select (kmeans-query '(1 2 3) data) (K:clusters 2))
;-> (3.190626898 3.014415366 2.013107051 3.186549858
;->   2.874786949 3.162419959 2.423674896)
```

Notes:

- Computes Euclidean distance row-by-row.
- Shorter feature length between list-data and row wins,
  allowing extra columns such as cluster labels.
- Works for both lists of lists and 2-D arrays.
- Useful for post-clustering inspection and for kNN
  analysis using the returned distance vector.

See: [kmeans-train](#f-kmeans-train),
     [select](#f-select),
     [sort](#f-sort)

---


<a name="f-last"></a>
## last

```
syntax: (last list)
syntax: (last array)
syntax: (last str)
```

Description:

Returns the last element of a list or array, or the last
character of a string. When applied to a list or array,
the final element is returned exactly as stored. When
applied to a string, the result is a one-character
string.

An empty list triggers an error. When using a UTF-8
build, last operates on character boundaries, not byte
boundaries.

Examples:

```
(last '(1 2 3 4 5))
;-> 5

(last '(a b (c d)))
;-> (c d)

(set 'A (array 3 2 (sequence 1 6)))
;-> ((1 2) (3 4) (5 6))

(last A)
;-> (5 6)

(last '())
;-> ERR: list is empty

(last "rebel")
;-> "l"
```

Notes:

- Returns a list element, array row, or a single
  character string.
- UTF-8 builds treat characters correctly regardless of
  byte length.

See: [first](#f-first), [rest](#f-rest), [nth](#f-nth)

---


<a name="f-last-error"></a>
## last-error

```
syntax: (last-error)
syntax: (last-error int-error)
```

Description:

Reports information about the most recent runtime error.
If no error has occurred since the session started,
last-error returns nil.

When called with no arguments, it returns either nil or
a two-element list containing the last error number and
its full message text.

When int-error is supplied, the function returns a list
containing int-error and its associated short error
description. If int-error is outside the known range,
the description defaults to "Unknown error".

Examples:

```
(last-error)
;-> nil

(abc)
;-> ERR: invalid function : (abc)

(last-error)
;-> (24 "ERR: invalid function : (abc)")

(last-error 24)
;-> (24 "invalid function")

(last-error 1)
;-> (1 "not enough memory")

(last-error 12345)
;-> (12345 "Unknown error")
```

Notes:

- Reports only errors generated by the evaluator.
- Can be combined with error-event and user-defined
  handlers for trapping exceptions.
- Networking errors use net-error.
- Operating-system errors use sys-error.

See: [net-error](#f-net-error), [sys-error](#f-sys-error),
     [error-event](#f-error-event)

---


<a name="f-legalp"></a>
## legal?

```
syntax: (legal? str)
```

Description:

Checks whether the string str represents a legal Rebel
symbol name. The test does not evaluate str; it inspects
its characters to verify whether the token conforms to
the rules for normal symbol names.

Strings that contain spaces, quotes, or other characters
not permitted in standard symbol syntax return nil.
Although such names are not legal tokens, symbols with
those names can still be created explicitly using sym.

Examples:

```
(symbol? (sym "one two"))
;-> true

(legal? "one two")
;-> nil

(set (sym "one two") 123)
;-> 123

(eval (sym "one two"))
;-> 123
```

Notes:

- legal? checks only whether the text is a valid symbol
  token, not whether a symbol with that name exists.
- Use sym to create symbols with arbitrary names.
- Illegal symbol names still behave like normal symbols
  once created via sym.

See: [sym](#f-sym), [symbol?](#f-symbolp), [eval](#f-eval)

---


<a name="f-length"></a>
## length

```
syntax: (length exp)
```

Description:

Returns the size of exp according to its type:

- For lists: the number of top-level elements.
- For arrays: the number of rows.
- For strings: the number of bytes.
- For symbols: the number of bytes in the symbol name.
- For integers (normal or big): the number of digits.
- For floats: the number of digits before the decimal
  point.

For all other types, the result is 0.

Big integers are counted by their full digit length.
Float digit counts ignore any fractional part.

Examples:

```
; list length
(length '(a b (c d) e))
;-> 4

(length '())
;-> 0

(set 'lst '(q w e r t y))
;-> (q w e r t y)
(length lst)
;-> 6

; array length (rows)
(set 'arr (array 2 4 '(0)))
;-> ((1 2) (3 4) (5 6) (7 8))   ; example filler changed by array rules
(length arr)
;-> 2

; byte length of strings
(length "Hello World")
;-> 11

(length "")
;-> 0

(length "\000\001\003")
;-> 3

; symbol name length in bytes
(length 'someVar)
;-> 7

; digit count of numbers
(length 0)
;-> 0

(length 123)
;-> 3

(length 1.23)
;-> 1

(length 1234567890123456789012345L)
;-> 25
```

Notes:

- length counts bytes, not UTF-8 characters, in strings.
- Use utf8len to count characters in UTF-8 encoded text.
- For floats, only the integer part contributes to the
  digit count.

See: [utf8len](#f-utf8len), [array](#f-array), [string](#f-string)

---


<a name="f-let"></a>
## let

```
syntax: (let ((sym1 [exp-init1]) [(sym2 [exp-init2]) ... ]) body)
syntax: (let (sym1 exp-init1 [sym2 exp-init2 ... ]) body)
```

Description:

Creates local variable bindings for the duration of
body. Each symbol is initialized with the corresponding
expression. In the fully parenthesized form, missing
initializers default to nil. In the flat form, each
symbol must be followed by its initializer.

Initializer expressions are evaluated in the environment
that existed before entering the let form. This means
that earlier local bindings created by let do not affect
the evaluation of later initializers. For incremental
initialization (each binding visible to the next), use
letn.

The body may contain one or more expressions evaluated
with the local bindings temporarily shadowing any outer
bindings. After body finishes, all local variables are
discarded.

Examples:

```
(define (sum-sq a b)
  (let ((x (* a a)) (y (* b b)))
    (+ x y)))
;-> (lambda (a b) ...)

(sum-sq 3 4)
;-> 25

; alternative syntax
(define (sum-sq a b)
  (let (x (* a a) y (* b b))
    (+ x y)))
;-> (lambda (a b) ...)
```

Notes:

- let evaluates all initializers in the outer scope.
- Variables default to nil only in the parenthesized
  form where the initializer is omitted.
- let expands conceptually into:
    ((lambda (sym1 sym2 ...) body) exp-init1 exp-init2 ...)
- Use letn when each initializer needs to see the
  bindings of previously initialized locals.
- Use local for automatic initialization to nil without
  writing initializer expressions.

See: [letn](#f-letn), [local](#f-local), [lambda](#f-lambda)

---


<a name="f-letex"></a>
## letex

```
syntax: (letex ((sym1 [exp-init1]) [(sym2 [exp-init2]) ... ]) body)
syntax: (letex (sym1 exp-init1 [sym2 exp-init2 ... ]) body)
```

Description:

Performs lexical substitution of symbols in body before
the resulting expression is evaluated. Each symbol in
the initializer list is replaced literally with its
initializer value. This expansion happens prior to
evaluation and produces a rewritten expression which is
then executed.

In the fully parenthesized form, initializers are
optional and missing values default to nil. In the flat
form, parentheses around (sym exp) pairs are omitted.

letex is useful for generating code where constants or
parameters must be substituted into an expression or a
lambda at definition time rather than at runtime. This
is a compile-time–style macro substitution, not dynamic
binding.

Examples:

```
(letex (x 1 y 2 z 3) '(x y z))
;-> (1 2 3)

(letex ((x 1) (y '(a b c)) (z "hello")) '(x y z))
;-> (1 (a b c) "hello")

; building a closure
(define (make-adder n)
  (letex (c n)
    (lambda (x) (+ x c))))

(define add3 (make-adder 3))
;-> (lambda (x) (+ x 3))

(add3 10)
;-> 13

; expansion into the same symbol
(define (make-adder n)
  (letex (n n)
    (lambda (x) (+ x n))))
```

Notes:

- Substitution is literal, performed before evaluation.
- All initializers are evaluated in the outer scope
  before substitution happens.
- letex produces rewritten code, not local bindings.
- Useful for code generation, closures, and templating.

See: [let](#f-let), [letn](#f-letn), [expand](#f-expand)

---


<a name="f-letn"></a>
## letn

```
syntax: (letn ((sym1 [exp-init1]) [(sym2 [exp-init2]) ... ]) body)
syntax: (letn (sym1 exp-init1 [sym2 exp-init2 ... ]) body)
```

Description:

Creates a sequence of nested let bindings, where each
initializer expression sees all bindings created earlier
in the same letn form. This differs from let, where all
initializers are evaluated in the outer scope before any
local bindings are introduced.

In the fully parenthesized form, missing initializers
default to nil. In the flat form, parentheses around
(sym exp) pairs may be omitted.

The semantics are equivalent to writing several let
forms wrapped inside each other in left-to-right order.

Examples:

```
(set 'x 10)

(let ((x 1) (y (+ x 1)))
  (list x y))
;-> (1 11)

(letn ((x 1) (y (+ x 1)))
  (list x y))
;-> (1 2)

(letn (x 1 y x)
  (+ x y))
;-> 2

; equivalent nested lets
(let (x 1)
  (let (y x)
    (+ x y)))
;-> 2
```

Notes:

- letn evaluates each initializer in an environment that
  includes all earlier bindings from the same letn.
- let evaluates all initializers in the outer scope.
- Conceptually identical to sequentially nested let
  forms.

See: [let](#f-let), [letex](#f-letex), [local](#f-local)

---


<a name="f-list"></a>
## list

```
syntax: (list exp-1 [exp-2 ... ])
```

Description:

Evaluates each expression in order and constructs a new
list from the resulting values. Arguments that are
arrays are converted to lists. Nested arrays become
nested lists. Lists created by list are fresh objects
and share no structure with their arguments unless the
arguments themselves were lists.

Examples:

```
(list 1 2 3 4 5)
;-> (1 2 3 4 5)

(list 'a '(b c) (+ 3 4) '() '*)
;-> (a (b c) 7 () *)
```

Notes:

- Each argument is evaluated before being inserted.
- Arrays are automatically converted into list form.
- Use cons or push to construct lists incrementally.

See: [cons](#f-cons), [push](#f-push)

---


<a name="f-listp"></a>
## list?

```
syntax: (list? exp)
```

Description:

Returns true only when exp evaluates to a list. All
other types yield nil. Lambda expressions and
lambda-macro expressions are also implemented as list
structures and therefore return true under list?.

Examples:

```
(set 'var '(1 2 3 4))
;-> (1 2 3 4)

(list? var)
;-> true

(define (double x) (+ x x))
;-> (lambda (x) (+ x x))

(list? double)
;-> true
```

Notes:

- list? reflects the internal representation: functions
  defined with lambda or define are stored as lists.
- For arrays, use array? rather than list?.

See: [array?](#f-arrayp), [lambda?](#f-lambdap), [list](#f-list)

---


<a name="f-load"></a>
## load

```
syntax: (load str-file-name-1 [str-file-name-2 ... ] [sym-context])
```

Description:

Loads and evaluates Rebel source code from one or more
files or URLs. Each file is read, translated into
expressions, and evaluated in sequence. The return value
is the result of the last expression in the last file.
If any file cannot be read or evaluated, load signals an
error.

When a sym-context is supplied, all top-level
expressions are evaluated in that context, unless the
file itself contains explicit context switches. If no
context argument is given, evaluation happens in MAIN
unless overridden within the file.

File names may be ordinary paths or URLs. The schemes
http:// and file:// are supported. URL and path entries
may be mixed in a single load call.

The current context after evaluating load is always the
same as before invoking it.

Examples:

```
(load "file.rbl")

(load "a.rbl" "b.rbl")

(load "file.rbl" "http://example.org/mod")

(load "http://192.168.0.21:6000//home/test/program.rbl")

; load into a specific context
(load "alpha.rbl" "beta.rbl" 'ctx)

; file:// form uses three slashes before the path
(load "file:///usr/local/share/rebel/module.rbl")
```

Notes:

- sym-context is created automatically when quoted.
- Context changes inside loaded files do not affect the
  caller’s context.
- HTTP mode performs a GET request and observes a
  built-in timeout.
- File paths and URLs may be combined freely.

See: [context](#f-context), [import](#f-import), [eval](#f-eval)

---


<a name="f-local"></a>
## local

```
syntax: (local (sym-1 [sym-2 ... ]) body)
```

Description:

Creates local variables initialized to nil, evaluates
body using these bindings, and returns the result of the
last expression. Unlike let or letn, local does not
accept initializer expressions; every listed symbol is
automatically bound to nil.

All local bindings shadow any outer bindings for the
duration of body. After evaluation, the original
bindings are restored.

Examples:

```
(local (a b c)
  (list a b c))
;-> (nil nil nil)

; localizing a variable that already has a value
(set 'x 42)
;-> 42

(local (x)
  (+ x 1))
;-> 1

x
;-> 42
```

Notes:

- All listed symbols initially hold nil.
- Useful when temporary variables are required without
  explicit initialization.
- Does not support initializer expressions; use let or
  letn when initialization is needed.

See: [let](#f-let), [letn](#f-letn), [letex](#f-letex)

---


<a name="f-log"></a>
## log

```
syntax: (log num)
syntax: (log num num-base)
```

Description:

Computes logarithms of numeric values. In the one-argument
form, log returns the natural logarithm of num. In the
two-argument form, num-base specifies the base of the
logarithm. Both arguments are evaluated before use.

The result is a floating-point number. Arguments must be
positive; non-positive values cause a math error.

Examples:

```
(log 1)
;-> 0

(log (exp 1))
;-> 1

(log 1024 2)
;-> 10

(log (exp 1) (exp 1))
;-> 1
```

Notes:

- log with one argument computes ln(num).
- log with two arguments computes:
    ln(num) / ln(num-base)
- Arguments must be strictly greater than 0.

See: [exp](#f-exp)

---


<a name="f-lookup"></a>
## lookup

```
syntax: (lookup exp-key list-assoc [int-index [exp-default]])
```

Description:

Searches list-assoc for the first association whose key
matches exp-key. An association is a list whose first
element is the key and whose remaining elements are its
values. When a match is found, lookup returns the element
at int-index of that association. If int-index is
omitted, the last element of the association is returned.

If no matching key is found, lookup returns exp-default
when provided, otherwise nil.

int-index may be negative, in which case elements are
indexed from the end of the association.

Examples:

```
(set 'params '(
  (name "Alice")
  (age 35)
  (role "admin")
  (balance 12.34)
))

(lookup 'age params)
;-> 35

; modify values using setf + lookup
(setf (lookup 'age params) 40)
;-> 40
(lookup 'age params)
;-> 40

; table of records
(set 'records '(
  ("Alice" 35 "admin" 12.34)
  ("Bob"   50 "user"  99.5)
))

(lookup "Bob" records 2)
;-> "user"

(lookup "Bob" records -3)
;-> 50

(lookup "Alice" records 1)
;-> 35

(lookup "Alice" records -2)
;-> "admin"

(lookup "Charlie" records 1 "N/A")
;-> "N/A"
```

Notes:

- lookup returns a specific element inside the found
  association.
- assoc returns the entire association; lookup extracts
  one element from it.
- Useful for quick table lookups and modifying values
  with setf.

See: [assoc](#f-assoc)

---


<a name="f-lower-case"></a>
## lower-case [utf8]

```
syntax: (lower-case str)
```

Description:

Returns a new string with all alphabetic characters in
str converted to lowercase. The original string is not
modified. When running a UTF-8–enabled build, lowercase
conversion operates on full Unicode characters rather
than raw bytes.

Examples:

```
(lower-case "HELLO WORLD")
;-> "hello world"

(set 'str "ABC")
;-> "ABC"

(lower-case str)
;-> "abc"

str
;-> "ABC"
```

Notes:

- Produces a new string; the input is unchanged.
- In UTF-8 builds, mapping follows Unicode lowercase
  rules.

See: [upper-case](#f-upper-case), [title-case](#f-title-case)

---


<a name="f-macro"></a>
## macro

```
syntax: (macro (sym-name [sym-param-1 ...]) [body-1 ...])
```

Description:

Creates a named **expansion macro**. Expansion macros run
during source reading, not at evaluation time. The macro
receives **unevaluated syntax** and transforms the call
into a new expression. The result of the transformation is
inserted into the code before evaluation.

Inside an expansion macro all parameters that should be
replaced in the generated syntax **must start with an
upper-case letter**. These parameters act as pattern
variables in the expanded expression.

Because macro definitions operate at read-time, they cannot
be redefined using define or define-macro. To redefine an
existing macro, use *constant*.

Examples:

```
; 1) simple expansion macro: (double X) → (+ X X)

(macro (double X)
  (+ X X))

(double 10)
;-> 20

(read-expr "(double 7)")
;-> (+ 7 7)


; 2) swap two variables by expanding the call into assignments

(macro (swap A B)
  (begin
    (set A B)
    (set B A)))

(set 'x 3 'y 9)
(swap x y)
(list x y)
;-> (9 3)


; 3) clamp value into a range using pure syntax expansion

(macro (clamp V LO HI)
  (if (< V LO) LO
      (if (> V HI) HI
          V)))

(clamp 15 0 10)
;-> 10

(clamp -3 0 10)
;-> 0

(clamp 7 0 10)
;-> 7
```

Notes:

- Expansion macros transform syntax **before** evaluation.
- Arguments are not evaluated; they are substituted directly
  into the expanded expression.
- All replaceable parameters must start with an upper-case
  letter.
- Expansion macros are not susceptible to variable capture.
- Expansion macros may cause unintended double evaluation
  if the argument expression appears more than once in the
  expansion.
- Macro definitions cannot be repeated. To redefine an
  existing macro, use *constant*. Redefinitions affect only
  future read operations.

See: [define-macro](#f-define-macro), [fn-macro](#f-fn-macro)

---


<a name="f-macrop"></a>
## macro?

```
syntax: (macro? exp)
```

Description:

Returns true if *exp* represents a macro. This includes:

- runtime macros created with fn-macro
- named runtime macros created with define-macro
- expansion macros created with macro

If *exp* is a symbol, the symbol’s value is inspected. If the
symbol contains a macro definition, true is returned.
In all other cases nil is returned.

Examples:

```
; runtime macro
(define-macro (mysetq lv rv)
  (set lv (eval rv)))

(macro? mysetq)
;-> true


; expansion macro
(macro (my-setq Lv Rv)
  (set Lv Rv))

(macro? my-setq)
;-> true

(macro? 'my-setq)
;-> true


; non-macro values
(macro? 123)
;-> nil

(macro? +)
;-> nil
```

Notes:

- macro? returns true for both runtime and expansion macros.
- When passing a symbol, its stored value is inspected.
- A symbol without a macro definition yields nil.

See: [fn-macro](#f-fn-macro), [define-macro](#f-define-macro), [macro](#f-macro)

---
