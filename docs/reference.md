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
## ! / run

```
syntax: (! str-shell-command)
```

Description:

Executes the external command contained in
`str-shell-command`. The call blocks until the invoked
program terminates. The return value is the exit status
reported by the operating system.

Use `exec` when capturing standard output or providing
standard input is required. Use `process` for non-blocking
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
through numeric indexing with this function. Using `($
int-idx)` enables programmatic access when the index is
not known in advance.

Examples:

```
(set txt "http://example.org:80")
(pos "http://(.*):(.*)" txt 0)  ; → 0

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
- Access via `($ int-idx)` is equivalent to referencing
  the variables `$0`–`$15` directly.

See: [pos](#f-pos), [find](#f-find),
[rx](#f-rx), [replace](#f-replace), [parse](#f-parse)

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

See: [-](#f-minus), [*](#f-star), [/](#f-slash),
[&#37;](#f-percent), [mod](#f-mod)

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

See: [+](#f-plus), [*](#f-star), [/](#f-slash),
[&#37;](#f-percent)

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

See: [+](#f-plus), [-](#f-minus), [/](#f-slash),
[&#37;](#f-percent)

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

See: [+](#f-plus), [-](#f-minus), [*](#f-star),
[&#37;](#f-percent)

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
replaced by `(mod result int-i)` for each argument. A
division-by-zero condition raises an error. When the
result exceeds the integer range, it wraps around.
Values evaluating to NaN are treated as zero and cause a
division-by-zero error.

Floating point numbers should be handled with the `mod`
function instead, which preserves fractional behavior.

Examples:

```
(% 10 3)   ; → 1
(% -10 3)  ; → -1
```

Notes:

- Floating point inputs are truncated toward zero.
- Division by zero raises an error.
- Overflow and underflow wrap around the integer limits.
- For floating point modulo, use `mod`.

See: [+](#f-plus), [-](#f-minus), [*](#f-star),
[/](#f-slash), [mod](#f-mod)

---


<a name="f-plusplus"></a>
## ++

```
syntax: (++ place [num ...])
```

Description:

Increments the value stored at `place` using integer
arithmetic. When no `num` argument is supplied, the value
is increased by `1`. When one or more arguments are given,
each value is truncated toward zero before being added
to the running total.

If `place` refers to a symbol containing `nil`, it is
treated as if it contained `0`. When `place` refers to a
list position, the element at that position is updated
in place. If `place` is an expression that yields a
number, the incremented result is returned but not
stored.

All calculations use integer limits. Values exceeding
`9,223,372,036,854,775,807` wrap around to negative
numbers. Values smaller than
`-9,223,372,036,854,775,808` wrap around to positive
numbers. Arguments evaluating to NaN are treated as `0`.

Examples:

```
(set n 1)
(++ n)                 ; → 2

(set n 3.8)
(++ n)                 ; → 4
(++ n 1.3)             ; → 5

(set lst '(1 2 3))
(++ (lst 1) 2)         ; → 4
lst                    ; → (1 4 3)
```

Notes:

- Floating point arguments are truncated toward zero.
- `nil` is treated as `0` when used as a `place`.
- Wrap-around occurs on integer overflow or underflow.
- For floating point incrementing, use `inc`.

See: [--](#f-minusminus), [inc](#f-inc)

---


<a name="f-minusminus"></a>
## --

```
syntax: (-- place [num ...])
```

Description:

Decrements the value stored at `place` using integer
arithmetic. When no `num` argument is supplied, the value
is decreased by 1. When additional arguments are given,
each value is truncated toward zero before being
subtracted from the running result.

If `place` refers to a symbol containing `nil`, it behaves
as if it contained `0`. When `place` selects an element in a
list, that element is updated in place. If `place` is an
expression that merely yields a number, the decremented
result is returned but not stored.

All operations use the full integer range. Values that
exceed `9,223,372,036,854,775,807` wrap around to negative
numbers. Values below `-9,223,372,036,854,775,808` wrap
around to positive numbers. Arguments evaluating to NaN
are treated as `0`.

Examples:

```
(set n 1)
(-- n)                 ; → 0

(set n 3.8)
(-- n)                 ; → 2
(-- n 1.3)             ; → 1

(set lst '(1 2 3))
(-- (lst 1) 2)         ; → 0
lst                    ; → (1 0 3)
```

Notes:

- All floating point arguments are truncated toward zero.
- `nil` is treated as 0 when used as a `place`.
- Integer overflow and underflow wrap around.
- For floating point decrementing, use `dec`.

See: [++](#f-plusplus), [dec](#f-dec)

---


<a name="f-lt"></a>
## < / lt

```
syntax: (< exp-1 [exp-2 ...])
```

Description:

Evaluates all expressions and compares each pair in
sequence. The result is true only if every comparison
satisfies the `<` relation. As soon as one comparison
fails, the function returns nil.

With a single argument, the operator compares the value
against `0`. This checks whether the number is negative.

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

- One argument means comparison against `0`.
- Lists compare recursively by element order.
- Mixed types use a fixed precedence:
  `nil < true < number < string < symbol < primitive
  < quoted list < list < fn < fn-macro`.

See: [>](#f-gt), [=](#f-eq), [<=](#f-le),
[>=](#f-ge), [!=](#f-ne)

---


<a name="f-gt"></a>
## > / gt

```
syntax: (> exp-1 [exp-2 ...])
```

Description:

Evaluates all expressions and compares them in order
using the `>` relation. The result is true only when every
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

- One argument compares the value against `0`.
- List comparison is recursive and length-sensitive.
- Mixed types follow type precedence:
  `nil < true < number < string < symbol < primitive
  < quoted list < list < fn < fn-macro`.

See: [<](#f-lt), [=](#f-eq), [<=](#f-le),
[>=](#f-ge), [!=](#f-ne)

---


<a name="f-eq"></a>
## = / eq

```
syntax: (= exp-1 [exp-2 ...])
```

Description:

Evaluates all expressions and compares them pairwise for
equality. The result is true only when each comparison
returns true. If any pair does not match, the function
returns `nil`.

When called with a single argument, the value is
compared against `0`, which allows checking for zero.

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

- One argument means comparison against `0`.
- Lists must match in both structure and length.
- Numeric values are truncated toward zero.
- Mixed-type equality applies only when types become
  equivalent under conversion rules.

See: [<](#f-lt), [>](#f-gt), [<=](#f-le),
[>=](#f-ge), [!=](#f-ne)

---


<a name="f-le"></a>
## <= / le

```
syntax: (<= exp-1 [exp-2 ...])
```

Description:

Evaluates all expressions and compares each consecutive
pair using the `<=` relation. The result is true only if
every comparison succeeds. When any comparison fails, the
function returns nil immediately.

With a single argument, the operator compares the value
against `0`, allowing a check for “less than or equal to
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

- One argument means comparison with `0`.
- List comparison is recursive and length-sensitive.
- Floating point values are truncated toward zero.
- Type precedence determines ordering for mixed types.

See: [<](#f-lt), [>](#f-gt), [=](#f-eq),
[>=](#f-ge), [!=](#f-ne)

---


<a name="f-ge"></a>
## >= / ge

```
syntax: (>= exp-1 [exp-2 ...])
```

Description:

Evaluates all expressions and compares each pair using
the `>=` relation. The result is true only when every
comparison succeeds. If any comparison fails, evaluation
stops and `nil` is returned.

With a single argument, the operator compares the value
against `0`. This makes it possible to test whether a
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
(>= abcd ab)                     ; → true
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

- One argument means comparison with `0`.
- Lists compare by structure and then length.
- Floating point values are truncated toward zero.
- Type precedence defines ordering for mixed types.

See: [<](#f-lt), [>](#f-gt), [=](#f-eq),
[<=](#f-le), [!=](#f-ne)

---


<a name="f-ne"></a>
## != / ne

```
syntax: (!= exp-1 [exp-2 ...])
```

Description:

Evaluates all expressions and checks that each
consecutive pair is not equal. The result is true only
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

- One argument means comparison with `0`.
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

Performs an arithmetic left shift on `int-1`. Each shift
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

Performs an arithmetic right shift on `int-1`. Each shift
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

Performs a bitwise AND operation. The value in `int-1` is
combined with `int-2` using AND, and the result is then
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

Performs a bitwise OR operation. The value in `int-1` is
combined with `int-2`, and the resulting value is then
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

Performs a bitwise XOR operation. The value in `int-1` is
combined with `int-2` using XOR, and the resulting value is
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

Performs a bitwise NOT operation on `int`. All bits in the
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


<a name="f-abort"></a>
## abort

```
syntax: (abort int-pid)
syntax: (abort)
```

Description:

Terminates child processes created with `spawn`. In the
first form, abort sends a termination request to the
specific child identified by `int-pid`. The process must
have been created using `spawn`. Processes started via
`fork` are not affected; Function `kill` must be used for 
those.

In the second form, abort terminates all child processes
spawned by the current process. The function returns
`true` when the requested terminations have been issued.

The `abort` function is part of the Cilk-style process
coordination interface used for structured parallel
execution.

Examples:

```
(abort 2245)        ; → true   ; abort one child
(abort)             ; → true   ; abort all spawned children
```

Notes:

- Only affects processes created by spawn.
- Processes created by `fork` require `kill`.
- Used for synchronization in Cilk-style parallel models.

See: [spawn](#f-spawn), [kill](#f-kill)

---


<a name="f-abs"></a>
## abs

```
syntax: (abs num)
```

Description:

Returns the absolute value of `num`. Integer arguments are
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

See: [sgn](#f-sgn)

---


<a name="f-acos"></a>
## acos

```
syntax: (acos num-radians)
```

Description:

Computes the arc cosine of `num-radians`. The argument is
interpreted as a floating point value. The result is the
angle whose cosine equals the input value. The output is
given in radians. Values outside the domain `[-1, 1]`
produce NaN.

Examples:

```
(acos 1)                 ; → 0
(cos (acos 1))           ; → 1
(acos 0)                 ; → 1.5707963268
```

Notes:

- The function returns NaN when the input is outside the
  valid range `[-1, 1]`.
- All calculations use floating point arithmetic.

See: [cos](#f-cos), [asin](#f-asin), [atan](#f-atan)

---


<a name="f-acosh"></a>
## acosh

```
syntax: (acosh num-radians)
```

Description:

Computes the inverse hyperbolic cosine of `num-radians`.
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

- Domain is `[1, +infinity)`. Values `< 1` yield NaN.
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

Returns the memory address of the value referenced by
`int`, `float`, or `str`. The returned value is an integer
representing the address of the underlying storage used
by the object.

This function is primarily used when passing pointers to
external C functions imported with `import`. The returned
address may also be used for pointer arithmetic.

When a string is passed directly as an argument to an
imported C function, its address is supplied implicitly
and an explicit call to address is not required.

The argument must refer to a value stored in a variable.
Addresses of temporary values produced by expressions
are not stable and must not be used.

Examples:

```
(set s "\001\002\003\004")
(charc (+ (address s) 3))   ; → 4

(set x 12345)
(longc (address x))         ; → 12345

(set x 1234567890)
(longc (address x))         ; → 1234567890
```

Notes:

- Returns an integer address suitable for use with C
  access functions.
- Intended for interaction with functions imported via
  `import`.
- Operates on values referenced by variables.

See: [charc](#f-charc), [intc](#f-intc),
[longc](#f-longc), [floatc](#f-floatc)

---


<a name="f-alarm"></a>
## alarm

```
syntax: (alarm sym-event-handler | func-event-handler num-seconds [int-option])
syntax: (alarm sym-event-handler | func-event-handler)
syntax: (alarm)
```

Description:

Starts or controls a one-shot `alarm` which invokes a
user-defined event handler after a specified delay.

When called with an event handler and `num-seconds`,
`alarm` schedules the handler to be executed after
`num-seconds` seconds have elapsed. The handler may be
given as a symbol naming a function or as an anonymous
function.

The `alarm` is implemented using Unix interval timers
and delivers one of the signals SIGALRM, SIGVTALRM, or
SIGPROF depending on the selected mode.

The optional `int-option` controls how time is measured:

0  real (wall-clock) time (default)  
1  process CPU time  
2  profiling time (real + CPU)

Seconds may be specified as floating point values,
allowing sub-second resolution.

Specifying `num-seconds` as 0 disables a running `alarm`
and prevents it from firing.

When called with only an event handler, `alarm` returns
the elapsed time of the currently running `alarm`. This
can be used to implement time lines or schedules.

When called without arguments, `alarm` returns the
symbol of the currently installed event handler.

Examples:

```
; periodic alarm using re-arming
;------------------------------------------------------------
(func (ticker)
  (println (date))
  (alarm 'ticker 1.0))

(ticker)
;-> ticker

; alarm events:
; Tue Apr 12 20:44:49 2005
; Tue Apr 12 20:44:50 2005
; Tue Apr 12 20:44:51 2005
```

Notes:

- The event handler is executed asynchronously.
- An `alarm` handler may re-arm the `alarm` to create
  periodic behavior.
- An `alarm` cannot interrupt a running built-in
  function; it runs between expression evaluations.
- For interrupting blocking I/O, polling patterns
  using `nselect` are required.

See: [nselect](#f-nselect)


---
<a name="f-and"></a>
## and

```
syntax: (and exp-1 [exp-2 ...])
```

Description:

Evaluates each expression from left to right and returns
the value of the last one. If any expression evaluates
to `nil` or to the empty list (), evaluation stops
immediately and that value is returned. When called with
no arguments, the result is `true`.

This operator provides short-circuit behavior: evaluation
continues only as long as all intermediate results are
non-nil and not ().

Examples:

```
(set x 10)                         ; → 10
(and (< x 100) (> x 2))            ; → true
(and (< x 100) (> x 2) "passed")   ; → "passed"
(and '())                          ; → ()
(and true)                         ; → true
(and)                              ; → true
```

Notes:

- Stops evaluation at the first `nil` or `()`.
- No arguments return `true`.
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

For linkage characters or custom separators, use `join`.
Use `string` to convert values and concatenate in one step.
To modify an existing list or string in place, use `extend`
or `push`.

Examples:

```
(append '(1 2 3) '(4 5 6) '(a b))
; → (1 2 3 4 5 6 a b)

(set lst '("hello" "world"))
(append lst '("here" "I am"))
; → ("hello" "world" "here" "I am")

(set A (array 3 2 (seq 1 6)))
; → ((1 2) (3 4) (5 6))

(set B (array 2 2 (seq 7 10)))
; → ((7 8) (9 10))

(append A B)
; → ((1 2) (3 4) (5 6) (7 8) (9 10))

(append B B B)
; → ((7 8) (9 10) (7 8) (9 10) (7 8) (9 10))

(set more " how are you")
(append "Hello " "world," more)
; → "Hello world, how are you"
```

Notes:

- Does not modify any of the input arguments.
- Preserves binary strings without truncation at zero
  bytes.
- For insertion into an existing list or string, use
  `extend` or `push`.

See: [join](#f-join), [extend](#f-extend), [push](#f-push)

---



<a name="f-apply"></a>
## apply

```
syntax: (apply function list [int-reduce])
syntax: (apply function)
```

Description:

Invokes `function` using the elements of `list` as its
arguments.  The function in `func` may be a primitive,
a user-defined function, or a `fn` expression.  Only
functions that evaluate all of their arguments may be
used; special forms that evaluate selectively (such as
`dolist` or `case`) will fail when used with `apply`.

In the second syntax form, apply simply calls
`function` without any arguments.

When `int-reduce` is supplied, it specifies how many
arguments `function` consumes.  The function is then
applied repeatedly in left-associative order: the
result of each application becomes the first argument
of the next application, and the remaining arguments
are taken successively from list.  This mechanism
allows a two-argument function to be extended into a
variadic one.

Examples:

```
(apply + '(1 2 3 4))
; → 10

(set lst '(3 4 5))
(apply * lst)
; → 60

(apply sqrt '(25))
; → 5

(apply (fn (x y) (* x y)) '(3 4))
; → 12

; reduce mode example

(func (gcd_ a b)
  (let (r (% b a))
    (if (= r 0) a (gcd_ r a))))

(mac (my-gcd)
  (apply gcd_ (map eval (args)) 2))

(my-gcd 12 18 6)     ; → 6
(my-gcd 12 18 6 4)   ; → 2
```

Notes:

- Only works with functions that evaluate all arguments.
- Special forms (`dolist`, `case`, etc.) cannot be used.
- `int-reduce` applies `function` repeatedly in
  left-associative order.

See: [map](#f-map), [fn](#f-fn)

---


<a name="f-args"></a>
## args

```
syntax: (args)
syntax: (args int-idx-1 [int-idx-2 ...])
```

Description:

Returns all unbound arguments of the currently
executing function or macro.  Only arguments not
already matched to formal parameters remain in this
list.  The function is useful for defining functions or
macros that accept a variable number of parameters.
For macros, `args` avoids accidental variable capture
because the returned values belong strictly to the
caller’s context.

When called with one or more indices, `args` returns
the corresponding elements from the unbound argument
list.  Indices may be nested-access indices, allowing
traversal inside structured data such as lists.

Examples:

```
(mac (print-line)
  (dolist (x (args))
    (print x "\n")))
(print-line "hello" "World")
; prints each argument on its own line

(mac (m)
  (print (args 2) (args 1) (args 0)))
(m x y z)
; → zyx

(func (f)
  (args 0 2 -1))
(f '(1 2 (3 4)))
; → 4

; bound vs unbound arguments
(func (f a b)
  (args))

(f 1 2)
; → ()

(f 1 2 3 4 5)
; → (3 4 5)
```

Notes:

- Returns only arguments not already bound to local
  variables of the active function or macro.
- Calling `(args)` inside a macro and passing it as an
  argument to *another* macro is invalid, because args
  would not be evaluated in the target macro’s
  environment.
- Safe to use as a parameter to functions.

See: [func](#f-func), [mac](#f-mac)

---


<a name="f-array"></a>
## array

```
syntax: (array int-n1 [int-n2 ...] [list-init])
```

Description:

Creates an array with `int-n1` elements.  Additional
integer dimensions define a multidimensional array,
supporting up to sixteen dimensions.  When `list-init`
is supplied, its elements initialize the array.  If
`list-init` has fewer elements than required, its
contents repeat until all array positions are filled.
Elements may be of any type.

Multidimensional arrays are stored as arrays of arrays.
Arrays provide efficient random indexing compared to
lists.  Most list operations work on arrays, but not
all; details depend on the specific function.
Replacing whole rows requires care to ensure the
replacement itself is an array and not a list.

Arrays can be converted to lists using `arraylist`, and
lists can be converted back by flattening them with
`flat` before constructing a new array.

Serialization using `save` or `source` preserves array
structure by writing the necessary array expression.

Examples:

```
(array 5)
; → (nil nil nil nil nil)

(array 5 (seq 1 5))
; → (1 2 3 4 5)

(array 10 '(1 2))
; → (1 2 1 2 1 2 1 2 1 2)

(set a (array 3 4 (seq 1 12)))
; → ((1 2 3 4) (5 6 7 8) (9 10 11 12))

(set (a 2 3) 99)
; → 99
a
; → ((1 2 3 4) (5 6 7 8) (9 10 11 99))

(set (a 1 1) "hello")
a
; → ((1 2 3 4) (5 "hello" 7 8) (9 10 11 99))

(set (a 1) '(a b c d))
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

(arraylist a)
; → ((1 2 3 4) (a b c d) (1 2 3 99))

(set lst '((1 2) (3 4)))
(set arr (array 2 2 (flat lst)))
; → ((1 2) (3 4))

(array? a)
; → true

(array? (arraylist a))
; → nil

; serialization example
(set a (array 3 4 (seq 1 12)))
(save "array.rbl" 'a)
;; saved file includes array expression
```

Notes:

- Up to sixteen dimensions are supported.
- `list-init` repeats until the array is fully initialized.
- Converting list rows requires care: use arrays, not
  lists, when replacing rows.
- `arraylist` converts arrays to lists; `flat` assists in
  reconstructing arrays from list data.

See: [arraylist](#f-arraylist), [array?](#f-arrayp),
[flat](#f-flat)

---


<a name="f-arraylist"></a>
## arraylist

```
syntax: (arraylist array)
```

Description:

Converts an `array` into a list structure. The conversion
is recursive: every row of a multidimensional array is
turned into a list, and nested arrays become nested
lists. The original array is left unchanged.

Examples:

```
(set a (array 3 4 (seq 1 12)))
; → ((1 2 3 4) (5 6 7 8) (9 10 11 12))

(set lst (arraylist a))
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

Checks whether `exp` is an array. The function returns true
when the expression is an array object and `nil` otherwise.
Nested or converted list structures do not qualify as
arrays, even when their shape resembles one.

Examples:

```
(set M (array 3 4 (seq 1 4)))
; → ((1 2 3 4) (1 2 3 4) (1 2 3 4))

(array? M)
; → true

(array? (arraylist M))
; → nil
```

Notes:

- Returns true only for actual array objects.
- Lists created from arrays using `arraylist` are not
  arrays.

See: [array](#f-array), [arraylist](#f-arraylist)

---


<a name="f-asin"></a>
## asin

```
syntax: (asin num-radians)
```

Description:

Computes the arcsine of `num-radians` and returns the
result as a floating point value in radians. The input
must lie within the domain `[-1, 1]`. Values outside this
range return NaN. The output is the angle whose sine
equals the argument.

Examples:

```
(asin 1)                 ; → 1.570796327
(sin (asin 1))           ; → 1
```

Notes:

- Domain: `[-1, 1]`. Outside values produce NaN.
- Result is expressed in radians.

See: [sin](#f-sin), [acos](#f-acos), [atan](#f-atan)

---


<a name="f-asinh"></a>
## asinh

```
syntax: (asinh num-radians)
```

Description:

Computes the inverse hyperbolic sine of `num-radians` and
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
- Result is the inverse of `sinh`.

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
element matches the given key.  In the first form,
`exp-key` is a single key expression.  The function
scans each member-list of `list-alist`; when the first
element equals `exp-key`, that member-list is returned.
If no match is found, the result is `nil`.

In the second form, `list-exp-key` is a list of keys
used to traverse nested association lists.  Each key
selects a sublist, allowing lookup inside multilevel
structures.

When `list-alist` is a context symbol, assoc operates
on its default functor, enabling large association
lists to be referenced without copying.

assoc may be combined with set to modify association
lists in place.

Examples:

```
(assoc 1 '((3 4) (1 2)))
; → (1 2)

(set db '((kiwi 12) (mango 77 5) (plum 9)))
(assoc 'mango db)
; → (mango 77 5)

(assoc 'papaya db)
; → nil

; modifying an association
(set (assoc 'plum db) '(plum 10))
db
; → ((kiwi 12) (mango 77 5) (plum 10))

; nested association lists
(set records '(
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
(set people:people '(
  (u001 (name "Nora")
        (address (country "Japan") (city "Kyoto")))
  (u002 (name "Liam")
        (address (country "Brazil") (city "Recife")))
))

(func (get-city db id)
  (last (assoc (list id 'address 'city) db)))

(get-city people 'u001)
; → "Kyoto"
```

Notes:

- `assoc` returns the full matching pair or sublist.
- `list-exp-key` allows multilevel lookup.
- When used with a context, `assoc` operates on the
  context's default functor.
- For modifications, combine `assoc` with `set`.
- `lookup` provides lookup + extraction in one step.

See: [lookup](#f-lookup), [set](#f-set)

---


<a name="f-atan"></a>
## atan

```
syntax: (atan num-radians)
```

Description:

Computes the arctangent of `num-radians` and returns the
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

Computes the inverse hyperbolic tangent of `num-radians`.
The result is the value whose hyperbolic tangent equals
the input. The valid domain is `[-1, 1]`. Inputs with an
absolute value greater than 1 produce NaN. An-value `±1`
returns positive or negative infinity, respectively.

Examples:

```
(atanh 0.5)              ; → 0.5493061443
(tanh (atanh 0.5))       ; → 0.5
(atanh 1.1)              ; → NaN
(atanh 1)                ; → inf
```

Notes:

- Domain: 
  |x| < 1.  
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

Checks whether `exp` evaluates to an atom. An expression is
considered an atom if it evaluates to one of the
following: nil, true, an integer, a float, a string, a
symbol, or a primitive. Structures such as lists,
`fn` expressions, `fn-macro` expressions, and quoted
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

- `atom?` tests the evaluated expression, not its literal
  form.
- Nested quoting yields non-atomic structures.

See: [list?](#f-listp), [symbol?](#f-symbolp)

---


<a name="f-b64dec"></a>
## b64dec

```
syntax: (b64dec str)
```

Description:

Decodes the BASE64-encoded string in `str` and returns the
decoded byte sequence as a string. The input is not
validated for correctness; invalid or malformed BASE64
data may yield unspecified output. The implementation is
compatible with RFC 4648 and is derived from routines
used in Unix curl.

Examples:

```
(b64dec "SGVsbG8gV29ybGQ=")
; → "Hello World"
```

Notes:

- Does not perform strict validation of BASE64 input.
- For encoding, use b64enc.

See: [b64enc](#f-b64enc)

---


<a name="f-b64enc"></a>
## b64enc

```
syntax: (b64enc str [bool-flag])
```

Description:

Encodes `str` into BASE64 format. BASE64 groups input bytes
in sets of three (24 bits) and produces four encoded
bytes (32 bits). Each 6-bit segment maps to one of 64
characters: `A–Z, a–z, 0–9, +, and /`. The character `=` is
used as padding when the input does not align to 3-byte
blocks. The result is a printable representation suitable
for embedding binary data in text-oriented formats.

When `bool-flag` is omitted or `nil`, the empty string ""
is encoded as "====". When `bool-flag` is true, the empty
string encodes to "". Both encodings decode back to ""
using `b64dec`.

The function returns the encoded string. No line breaks
are inserted; the result is a continuous BASE64 sequence.
The implementation conforms to RFC 4648 and is derived
from Unix curl routines.

Examples:

```
(b64enc "Hello World")
; → "SGVsbG8gV29ybGQ="

(b64enc "")
; → "===="

(b64enc "" true)
; → ""
```

Notes:

- Suitable for encoding binary data for text protocols
  such as XML-RPC.
- The empty-string behavior depends on `bool-flag`.
- No CR/LF insertion in long sequences.

See: [b64dec](#f-b64dec)

---


<a name="f-bigint"></a>
## bigint

```
syntax: (bigint number)
syntax: (bigint string)
```

Description:

Converts a `number` or a numeric `string` into a big integer.
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

(set n 567890)
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
`bool-eval` is omitted or `nil`, the values are assigned
exactly as they appear. When `bool-eval` is `true`, each
value is evaluated before assignment.

The return value of bind is the value of the last
association in the list.

`bind` is commonly used together with `unify`, which
produces association lists suitable for destructuring.

Examples:

```
(set lst '((a (+ 3 4)) (b "hello")))
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
(set structure '((one "two") 3 (four (x y z))))
(set pattern   '((A B)       C (D E)))

(bind (unify pattern structure))

A  ; → one
B  ; → "two"
C  ; → 3
D  ; → four
E  ; → (x y z)
```

Notes:

- Assigns all symbols appearing in the association list.
- With `bool-eval` = `true`, values are evaluated first.
- Often used with `unify` for de-structuring patterns.

See: [unify](#f-unify), [set](#f-set)

---


<a name="f-bits"></a>
## bits

```
syntax: (bits int [bool])
```

Description:

Converts an `int` into either a binary string or a list
of bit values. When bool is omitted or nil, the result is
a string of `'1'` and `'0'` characters, ordered from the
most significant bit to the least significant bit.

When `bool` evaluates to anything non-nil, `bits` returns a
list of booleans representing the binary form of the
integer, ordered from the lowest bit to the highest bit:
`true` for bit value 1 and `nil` for bit value 0. This
ordering allows direct indexing and use in flow control.

The function `int` with base 2 is the inverse of `bits`.

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

See: [int](#f-int), [&](#f-andbit), [|](#f-orbit)

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
Rebel function stored in `sym-function`. The mechanism
supports two independent modes. In the simple form the
first parameter selects one of sixteen static callback
slots numbered 0 to 15. Each slot contains a built-in
C function which transfers control into the runtime.

Assigning `sym-function` to a slot causes external C code
calling the slot pointer to enter the Rebel evaluator
and run the assigned function. This simple form accepts
up to eight parameters as raw integer or pointer
values. The slot pointer remains valid until reassigned
with a new `sym-function`.

The extended syntax uses libffi closures. A closure is
allocated dynamically and configured with a call
interface describing the return type and parameter
types. When external C code calls the closure address,
libffi performs type conversions, constructs a Rebel
argument list and invokes `sym-function`. After the
function finishes, libffi converts the returned value
back to the declared C type. This form has no slot
limits and can represent any combination of integer,
floating point or pointer parameters. Pointer values
arriving from C are represented as numbers and may be
inspected using `intc`, `longc` or `stringc` as
needed for buffer access.

The third syntax retrieves the previously created
callback pointer for `sym-function`. This avoids building
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
(func (hello)
  ;; External C code calling the slot pointer
  ;; will print this string.
  "hello world")

(set p0 (callback 0 'hello))
;; p0 now contains a C-callable pointer. Any C
;; function which receives p0 and calls it will
;; execute (hello).

;; Extended syntax with typed parameters
(func (sum a b)
  (+ a b))

(set ps (callback 'sum "int" "int" "int"))
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

See: [import](#f-import), [intc](#f-intc), [longc](#f-longc),
[pack](#f-pack), [unpack](#f-unpack)

---


<a name="f-cap"></a>
## cap

```
syntax: (cap list-A list-B)
syntax: (cap list-A list-B bool)
```

Description:

Computes the intersection of list-A and list-B. In the
first form, the result contains one copy of each value
that appears in both lists, preserving the order in
which those values first occur in `list-A`. Duplicates in
either input list are ignored in this mode.

In the second form, when `bool` evaluates to true (any
non-nil value), duplicates from `list-A` are preserved.
Every element of `list-A` that is also present in `list-B`
is included in the result in its original order, and no
deduplication occurs.

Membership tests follow standard list element equality.

Examples:

```
(cap '(3 0 1 3 2 3 4 2 1) '(1 4 2 5))
;-> (2 4 1)

(cap '(3 0 1 3 2 3 4 2 1) '(1 4 2 5) true)
;-> (1 2 4 2 1)
```

Notes:

- First form removes duplicates.
- Second form preserves duplicates from `list-A`.
- Order of results always follows `list-A`.

See: [diff](#f-diff), [uniq](#f-uniq), [union](#f-union)

---


<a name="f-case"></a>
## case

```
syntax: (case exp-switch (exp-1 body-1) [(exp-2 body-2) ... ])
```

Description:

Evaluates `exp-switch`, then compares its value against
each clause expression. Clause expressions are not
evaluated; they are matched literally. When a match is
found, all forms in the matching body are evaluated in
sequence, and the value of the final form becomes the
result of the case expression. If none of the clause
values match `exp-switch`, the body of the last clause is
executed and serves as a default branch.

Examples:

```
(func (translate n)
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

Evaluates `exp` under a handler capable of intercepting
throws and runtime errors. In the first syntax `catch`
returns a status flag and stores the outcome of `exp` in
`symbol`. When evaluation finishes normally, `catch`
returns `true` and `symbol` receives the value of `exp`. When
a runtime error occurs, `catch` returns `nil` and `symbol` is
set to the formatted error message. When a throw is
triggered during evaluation, `catch` returns `true` and
`symbol` receives the argument supplied to `throw`. This
form is used when errors or explicit throws are part of
normal program flow and must be handled without aborting
the surrounding computation.

In the second syntax `catch` returns the normal value of
`exp`, or if a `throw` is executed during evaluation—the
argument passed to `throw`. All intermediate evaluation
frames are discarded and control jumps directly to the
`catch` expression. This form is commonly used for
breaking out of nested iteration or forcing an early
return from functions or expression blocks.

Both forms install a temporary handler active only for
the dynamic extent of exp.

Examples:

```
(catch (repeat (x 1000) 
  (if (= x 500) (throw x))))  → 500

(func (f x)
   …
  (if condition (throw 123))
    …
  456)

(catch (f p))  → 123       ;; condition true
(catch (f p))  → 456       ;; condition false

(catch (f1 3 4) 'result)  → nil
result  
→ "ERR: invalid function (symbol) in function catch : (f1 3 4)"

(alias 'f1 +)
(catch (f1 3 4) 'result)  → true
result                      → 7

(catch (repeat (x 100) 
  (if (= x 50) (throw "fin"))) 'result)  → true
result  → "fin"
```

See: [throw](#f-throw), [throwerror](#f-throwerror),
[errorevent](#f-errorevent)

---


<a name="f-ceil"></a>
## ceil

```
syntax: (ceil number)
```

Description:

Returns the smallest integer value not less than `number`.
The result is returned as a floating-point value. When
`number` is already an integer value, it is returned
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


<a name="f-char"></a>
## char [utf8]

```
syntax: (char str [int-index [true]])
syntax: (char int)
```

Description:

Extracts a character from a string or constructs a
single-character string from an integer code. When the
first argument is a string, `char` selects the character
at `int-index` and returns its numeric code. If
`int-index` is omitted, index `0` is used. Negative indexes
address positions from the end of the string. On UTF-8
enabled builds the index normally refers to logical
UTF-8 characters. When the optional `true` flag is
supplied, the string is treated as a raw byte array and
the index refers to an 8-bit byte offset.

An empty string yields `nil`. Both `(char 0)` and 
`(char nil)` return the single byte string "\000".

When the first argument is an integer, `char` returns a
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

(map char (seq 1 255))

(char (& (char "生") (char "死"))) → 愛
```

See: [set](#f-set), [slice](#f-slice), [len](#f-len)

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
`int-chars` characters and returns the shortened copy. If
`int-chars` is omitted, one character is removed. The
original string is not modified. On UTF-8 enabled
builds character counting refers to logical UTF-8
characters.

When the first argument is a list, `chop` returns a
shallow copy of the list with `int-elements` removed from
the end. If `int-elements` is omitted, one element is
removed. The original list remains unchanged.

Examples:

```
(set str "Rebelion")  → "Rebelion"

(chop str)    → "Rebelio"
(chop str 2)  → "Rebeli"

str  → "Rebelion"

(set lst '(a b (c d) e))

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

Applies `exp-predicate` to each element of `list` and
returns a new list containing only those elements for
which the predicate evaluates to false. Elements for
which the predicate returns true are removed. The
behavior is equivalent to using `filter` with a negated
predicate. The predicate may be any built-in test,
user-defined function or `fn` expression. The order
of elements is preserved in the returned list.

Examples:

```
(clean symbol? '(1 2 d 4 f g 5 h))   → (1 2 4 5)

(filter symbol? '(1 2 d 4 f g 5 h))  → (d f g h)

(func (big? x) (> x 5))        → (fn (x) (> x 5))

(clean big? '(1 10 3 6 4 5 11))  → (1 3 4 5)

(clean <= '(3 4 -6 0 2 -3 0))  → (3 4 2)

(clean (curry match '(a *)) '((a 10) (b 5) (a 3) (c 8) (a 9)))
→ ((b 5) (c 8))
```

See: [filter](#f-filter), [index](#f-index),
[diff](#f-diff), [cap](#f-cap)

---


<a name="f-cline"></a>
## cline

```
syntax: (cline)
```

Description:

Returns the string most recently read by `readln`. This
value is also used implicitly by `writeln` when no
explicit string argument is supplied. `cline` is
useful for implementing streaming text filters that
process input line by line.

Examples:

```
; Displays all lines beginning with ";;" from the given
; file.

#!/usr/bin/env rebel

(set f (open (argv 2) "read"))

(while (readln f)
  (if (starts? (cline) ";;")
      (writeln)))

(exit)
```

Notes:


See: [readln](#f-readln), [writeln](#f-writeln),
[argv](#f-argv), [starts?](#f-startsp)

---


<a name="f-clone"></a>
## clone

```
syntax: (clone context-source sym-context-target [bool])
syntax: (clone context-source)
```

Description:

Copies all symbols and user-defined functions from
`context-source` into a target context.

When `sym-context-target` is provided, the context
referenced by `context-source` is copied into the
target context.  If the target context does not exist,
it is created with the same symbol names and
user-defined functions as in context-source.

If the target context already exists, symbols from
`context-source` are merged into it.  Existing symbols
are overwritten only when the optional `bool` argument
evaluates to anything other than nil.  When `bool` is nil
or omitted, existing symbols in the target context
remain unchanged.  This allows controlled mixins of
context objects.

`clone` returns the target context. The target context
must not be `MAIN`.

In the second syntax form, the context referenced by
`context-source` is copied into the current context,
which becomes the implicit target.

During copying, all symbol references that originally
pointed into the source context are rewritten so they point
into the target context.  As a result, all functions and
data structures continue to work unchanged, but now operate
entirely within the destination context.

Examples:

```
(context 'ctx1)
;-> ctx1

(context MAIN)
;-> MAIN

(clone ctx1 'ctx2)
;-> ctx2

; force overwrite of existing symbols
(context 'ctx3)
;-> ctx3
(clone ctx1 'ctx3 true)
;-> ctx3
```

The first example creates a new context `ctx2` with the
same structure as `ctx1`. The source context `ctx1` is not
quoted because contexts evaluate to themselves. The
target context `ctx2` must be quoted because it does not
exist yet.

The second example merges `ctx1 into an existing context
`ctx3`. Because `ctx3` already exists, the quote may be
omitted. All symbols of the same name in `ctx3` are
overwritten.

Contexts may also be referenced indirectly through
variables:

```
(set ctx1:x 123)
(set ctx2:y 999)

(set src ctx1)
(set dst ctx2)

(clone src dst)
;-> ctx2

ctx2:x
;-> 123

ctx2:y
;-> 999
```

In this example, the context referenced by `src` (ctx1)
is merged into the context referenced by `dst` (ctx2).

Notes:

- `clone` copies entire contexts; use `clonesym` to copy
  individual symbols only.
- All internal references are rewritten to point to
  the target context.
- Existing symbols are overwritten only when the
  optional `bool` argument evaluates to non-nil.

See: [clonesym](#f-clonesym), [context](#f-context)

---


<a name="f-clonesym"></a>
## clonesym

```
syntax: (clonesym sym-context-source [sym-context-target])
```

Description:

Creates a new symbol by copying the definition and
contents of `sym-context-source`. Only the referenced symbol 
is copied; the source context itself is not duplicated.

When `sym-context-target` is omitted, a symbol of the
same name is created in the current context.  In this
case, all symbol references belonging to the source
context are rewritten so they point into the current
context.  The current context must not be MAIN.

When `sym-context-target` is provided, the new symbol
is created in the context referenced by
`sym-context-target`.  Both the name and the
destination context may change.  Any symbol reference
that originally pointed into the source context is
rewritten to point into the target context.  `clonesym`
returns the newly created symbol.

This mechanism allows fine-grained copying of functions,
data structures, or context members, and enables creation
of isolated, statically scoped functions by assigning
each copy to its own namespace.

Examples:

```
(set ctx1:var '(ctx1:x ctx1:y))
;-> (ctx1:x ctx1:y)

(clonesym 'ctx1:var 'ctx2:myvar)
;-> ctx2:myvar

ctx2:myvar
;-> (ctx2:x ctx2:y)

; clonesym into current context
(context 'ctx3)
;-> ctx3

(clonesym 'ctx1:var)
;-> var

var
;-> (x y)
```

Creating a function with its own namespace:

```
(set temp (fn (x) (+ x x)))
;-> (fn (x) (+ x x))

(clonesym 'temp 'double:double)
;-> double:double

(double 10)
;-> 20

double:double
;-> (fn (double:x) (+ double:x double:x))
```

Building a helper for statically scoped accumulators:

```
(func (func-static s body)
  (clonesym 'body (sym s s)))

(func-static 'acc (fn (x)
  (inc sum x)))

(acc 1)
;-> 1
(acc 1)
;-> 2
(acc 8)
;-> 10
```

Notes:

- `clonesym` copies one symbol only; use `clone` to copy an
  entire context.
- All internal references pointing to the source
  context are rewritten to the target context.
- Useful for constructing context-local closures or
  configuring context objects piece by piece.

See: [clone](#f-clone), [context](#f-context), [sym](#f-sym),
[func](#f-func)

---


<a name="f-close"></a>
## close

```
syntax: (close int-file)
```

Description:

Closes the file or device associated with `int-file`. The
value in `int-file` must be a file handle previously
obtained from an `open` operation or from `device`. When
the `close` operation succeeds, the function returns
`true`; if the handle is invalid or the operation fails,
`nil` is returned. Closing the device handle resets it to
`0`, restoring the screen device as the active output
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

Repeatedly evaluates `exp` and accumulates each non-nil
result into a list. Evaluation continues until `exp`
returns `nil`, which terminates the collection, or until
`int-max-count` elements have been gathered when the
optional limit is specified. Each iteration reevaluates
`exp` from scratch, and all collected values are appended
to the result list in the order produced. If the first
evaluation of `exp` yields `nil`, an empty list is
returned.

Examples:

```
(set x 0)
(collect (if (<= (inc x) 10) x))
→ (1 2 3 4 5 6 7 8 9 10)

(set x 0)
(collect (if (<= (inc x) 10) x) 6)
→ (1 2 3 4 5 6)
```

See: [while](#f-while), [repeat](#f-repeat),
[for](#f-for)

---


<a name="f-commandevent"></a>
## commandevent

```
syntax: (commandevent sym-event-handler)
syntax: (commandevent func-event-handler)
syntax: (commandevent nil)
```

Description:

Installs a user-defined handler which receives each
command line before it is evaluated. The handler must
return a string. The returned string becomes the line
executed by the interpreter. Returning the empty string
prints a prompt without evaluating any expression.
Passing `nil` removes the currently installed handler.

The handler may be a symbol naming a function or a
`fn` expression. In interactive mode the handler can
rewrite, filter or suppress input lines before they
reach the evaluator, allowing full customization of
REPL input behavior. In http mode (enabled with the
`-http` option) the handler receives the raw HTTP request
line and may normalize, rewrite or block requests
before they are dispatched to the built-in HTTP
processor. Any non-string return value leaves the
original input line unchanged.

Examples:

```
;; treat plain words as shell commands
(commandevent
  (fn (s)
    (if (starts? s "[a-zA-Z]" 0)
        (append "!" s)
        s)))

;; set a prompt and add simple command translation
(promptevent
  (fn (ctx)
    (append (dirpath) "> ")))

(commandevent
  (fn (s)
    (if
        (starts? s "cd")
        (string " " (true? (dircd (last (parse s " ")))))

        (starts? s "[a-zA-Z]" 0)
        (append "!" s)

        true s)))
```

Examples (http mode):

```
;; httpd-conf.rbl
;; filter and rewrite HTTP request lines in -http mode
;; block access to *.ext resources

(commandevent
  (fn (s)
    (let (request s)
      (when (find "?" s)
        (set request (first (parse s "?")))
        (when (ends? request ".ext")
          (set request "GET /forbidden.html")) )
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

See: [promptevent](#f-promptevent)

---


<a name="f-cond"></a>
## cond

```
syntax: (cond (exp-condition-1 body-1) [(exp-condition-2 body-2) ... ])
```

Description:

Evaluates each `exp-condition` in order until one of them
yields a value other than `nil` or the empty list. When a
condition succeeds, its corresponding body is evaluated
and the resulting value becomes the value of the entire
`cond` expression. If a body is omitted, the value of the
successful `exp-condition` is returned. When no condition
succeeds, the value of the last evaluated conditional
expression is returned, which is typically `nil` or an
empty list.

`cond` is used for multi-branch conditional evaluation. It
behaves like a generalized form of `if`. When several
conditions must be tested in sequence, `cond` avoids
nested `if` forms and allows compact expression of
multiple branches. If used with several condition/body
pairs, the function if behaves similarly but requires no
extra parentheses around each pair.

Examples:

```
(func (classify x)
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

Creates a new list by combining the values of `exp-1` and
`exp-2`. When `exp-2` evaluates to a list, its elements are
prefixed with the value of `exp-1`. When `exp-2` evaluates
to anything other than a list, a two-element list is
constructed from the evaluated arguments. The function
never produces dotted pairs; combining two non-list
values always yields a proper list.

If `exp-2` is `nil`, the result is a list whose first element
is the value of `exp-1` and whose second element is `nil`.

When called with a single argument, `cons` produces a list
containing that argument. When called without arguments,
it returns an empty list. The operation is effectively
the inverse of extracting the head and tail of a list
using `first` and `rest`; when a list has exactly two
elements, `first` and `last` form the corresponding pair.

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


<a name="f-const"></a>
## const [!] / alias

```
syntax: (const sym-1 exp-1 [sym-2 exp-2] ...)
```

Description:

Assigns values to symbols and marks those symbols as
protected.  A protected symbol cannot be modified.  any
attempt to overwrite it raises an error.  A protected
symbol can only be changed by calling `const` again.
Only symbols in the current context may be protected,
preventing accidental changes to names defined in other
contexts.  The last initializer expression is optional.

Symbols created with `set`, `func` or `mac` can be
protected retroactively by using `const` on them. Since
a function definition is internally just an assignment
of a `fn` value to a `symbol`, protecting a function name
behaves the same way as protecting a variable.

The final value assigned by `const` is returned as the
result of the call.

Examples:

```
(const 'var 123)  → 123
(set var 999)
ERR: symbol is protected in function set: var

(func (double x) (+ x x))

(const 'double)

;; equivalent to

(const 'double (fn (x) (+ x x)))

(const 'squareroot sqrt)  → sqrt <406C2E>
(const '+ add)            → add <4068A6>
```

Notes:

- Protected symbols can only be reassigned by `const`.
- Only symbols in the current context may be protected.
- Renaming built-in functions using `const` incurs no
  performance penalty. The displayed hexadecimal address
  is the internal pointer to the function.

See: [set](#f-set), [def](#f-func), [mac](#f-mac)

---


<a name="f-context"></a>
## context

```
syntax: (context [sym-context])
syntax: (context sym-context str | sym [exp-value])
```

Description:

Switches the current namespace to `sym-context`. Any
symbols created by evaluating expressions, loading
source files or using `evalstr` are placed into the
active context. When the context does not yet exist, it
is created. Calling `context` without arguments returns
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

Contexts can be copied with `clone`. The copy behaves as an
independent namespace whose symbols can be accessed
directly or by a variable holding a reference to that
context.

In the second syntax, `context` creates or updates a
symbol inside `sym-context` without switching to it. This
form acts as namespace-based key/value storage. It does
not change the active context and should not be used as
a general hash mechanism.

Examples:

```
;; create and switch to GRAPH
(context 'GRAPH)

(func (draw x y) (+ x y))
(set value 123)

(symbols) → (draw value)

;; switch back to MAIN
(context MAIN)

GRAPH:value        → 123
(GRAPH:draw 2 3)   → 5
(set GRAPH:value 999)
GRAPH:value        → 999

;; implicit context creation
(set person:age 0)
(set person:address "")
person:age         → 0

;; copy a context
(clone person 'JohnDoe) → JohnDoe
(set JohnDoe:age 99)
JohnDoe:age            → 99

;; refer to a context through a variable
(set human JohnDoe)
human:age              → 99
(set human:address "1 Main Street")
JohnDoe:address        → "1 Main Street"

;; switching using an evaluated context
(context 'ctxA)
(context MAIN)
(set old ctxA)
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
  the prefix form (context:symbol).
- Two contexts may contain symbols with the same name
  without interfering with each other.
- If a plain symbol already exists, referring to it as a
  context redefines it as a context.
- The second syntax only stores data; it does not switch
  the active context.

See: [context?](#f-contextp), [clone](#f-clone), [symbols](#f-symbols)

---


<a name="f-contextp"></a>
## context?

```
syntax: (context? exp)
syntax: (context? exp str-sym)
```

Description:

Predicate for testing context values. In the first
syntax, `context?` returns true only when `exp` evaluates to
a context; otherwise it returns `nil`.

In the second syntax, `exp` must evaluate to a context.
The function then checks whether a symbol named `str-sym`
exists inside that context. The symbol name must be
given as a string. The result is `true` when the symbol is
present and `nil` when it is absent.

Examples:

```
;; test for context value
(context? MAIN)        → true

(set x 123)
(context? x)           → nil

;; implicit context creation and referencing
(set data:msg "hello")
(context? data)        → true
(set ctx data)
(context? ctx)         → true

;; test for symbol existence inside a context
(context? data "msg")  → true
(context? data "xy")   → nil
```

Notes:

- The second syntax does not evaluate `str-sym`; it must be
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
`exp`. Some functions modify their arguments in place,
changing the original list, array, or string. Wrapping
the argument with `copy` forces these functions to operate
on a duplicate, leaving the source data unchanged. The
returned value is the modified duplicate, while the
original stays intact.

A second form exists for low-level interop. When
`bool-flag` is true, `int-addr` is treated as a pointer to a
Rebel expression stored in memory. `copy` retrieves a full
duplicate of that expression. This mode is useful for
bridging external C routines that construct Rebel data in
native memory and expose only an address. The duplicate
behaves as a normal Rebel value and does not reference
the original memory block.

Examples:

```
(set lst '(a b c d e f)) ;-> (a b c d e f)

(replace 'c (copy lst)) ;-> (a b d e f)
lst ;-> (a b c d e f)

(set str "rebel-lang") ;-> "rebel-lang"
(rot (copy str)) ;-> "grebel-lan"
str ;-> "rebel-lang"

(set x "hello world") ;-> "hello world"
(copy x) ;-> "hello world"

(copy (first (dump x)) true) ;-> "hello world"
```

See: [replace](#f-replace), [rot](#f-rot), [dump](#f-dump)

---


<a name="f-cos"></a>
## cos

```
syntax: (cos num-radians)
```

Description:

Computes the cosine of `num-radians` and returns the
floating-point result. The argument is interpreted as an
angle in radians. The function accepts any real number
and produces a value in the range `-1 to 1`.

Examples:

```
(cos 1) ;-> 0.5403023059

(set pi (mul 2 (acos 0))) ;-> 3.141592654
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

Computes the hyperbolic cosine of `num-radians`. The
mathematical definition is `(exp x + exp -x) / 2`. The
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


<a name="f-crc32"></a>
## crc32

```
syntax: (crc32 str-data)
```

Description:

Computes a 32-bit CRC (Cyclic Redundancy Check) value for
the bytes contained in `str-data`. The calculation starts
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

See: [fread](#f-fread)

---


<a name="f-curry"></a>
## curry

```
syntax: (curry function exp)
```

Description:

Creates a new single-argument `function` by fixing the
first argument of a two-argument function. `curry` does not
evaluate `function` or `exp` when it is called. Instead, both are
captured and evaluated later when the resulting function
is invoked. The transformation turns a function `f(x,y)`
into a new function `g(y)` that behaves like `f(exp, y)`.

This mechanism is useful for building specialized
predicates, filters, and mapping functions without
creating explicit `fn` expressions.

Examples:

```
(set f (curry + 10))
;-> (fn ($x) (+ 10 $x))

(f 7)
;-> 17

(filter (curry match '(a *))
        '((a 10) (b 5) (a 3) (c 8) (a 9)))
;-> ((a 10) (a 3) (a 9))

(clean (curry match '(a *))
       '((a 10) (b 5) (a 3) (c 8) (a 9)))
;-> ((b 5) (c 8))

(map (curry list 'x) (seq 1 5))
;-> ((x 1) (x 2) (x 3) (x 4) (x 5))
```

See: [fn](#f-fn), [apply](#f-apply),
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
and time. When `int-secs` is supplied, it is interpreted as
a UTC-based timestamp and converted to local time. An
optional `int-offset`, in minutes, adjusts the timestamp
before conversion. Invalid `int-secs` produces `nil`. When a
custom `str-format` is given, formatting is performed by
the system `strftime` implementation. Invalid formats
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
;-> "Sat Jan  3 06:46:17 2026"

(date (time))
;-> "Sat Jan  3 06:46:17 2026"

> (date (time) 60)
;-> "Sat Jan  3 07:46:28 2026"

(date (time) 60 "%Y-%m-%d")
"2026-01-03"

> (date 0 0 "%Y-%m-%d")
"1970-01-01"
```

Notes:

- Exact output depends on system locale and C library.

See: [time](#f-time), [datelist](#f-datelist),
[datestamp](#f-datestamp), [timelist](#f-timelist)

---

<a name="f-dateiso"></a>
## dateiso

```
syntax: (dateiso)
```

Description:

Returns current date and time in UTC formatted as
YYYY-MM-DD HH:MM:SS.

Examples:

```
(dateiso)
;-> "2026-01-02 08:51:48" 
```

Notes:

- The format is fixed and cannot be customized.

See: [date](#f-date)


<a name="f-datelist"></a>
## datelist

```
syntax: (datelist int-seconds [int-index])
syntax: (datelist)
```

Description:

Converts a UTC timestamp into a list containing year,
month, day of month, hour, minute, second, day of year,
and day of week. The `int-seconds` is interpreted as the
number of seconds since January 1st, 1970 00:00:00 UTC.
The weekday value ranges from 1 to 7 for Monday through
Sunday. When called without arguments, `datelist` 
uses the current timestamp obtained from `time`.

If `int-index` is supplied, only the selected element of
the list is returned. Negative indices count from the end
of the list.


Format of the returned list:

```
(year month day hour minute second day-of-year day-of-week)
```

Examples:

```
(datelist)
;-> (2026 1 3 5 52 59 3 6)

(datelist 0)
;-> (1970 1 1 0 0 0 1 4)

(datelist (time))
;-> (2026 1 3 5 54 9 3 6)

(datelist 1767019672)
;-> (2025 12 29 14 47 52 363 1)

(datelist (time) 0)
;-> 2026
```

Notes:

- All values are interpreted as UTC.
- Supplying `int-index` returns a single element.

See: [time](#f-time), [date](#f-date),
[timelist](#f-timelist)

---


<a name="f-datestamp"></a>
## datestamp

```
syntax: (datestamp str-date str-format)
```

Description:

Parses a textual date according to strftime-style
directives in `str-format` and returns the corresponding
UTC timestamp as seconds since January 1st, 1970
00:00:00. The result ranges from `0` up to `2147472000`,
which corresponds to dates before the 2038 boundary.

The format directives are the same strftime-style
directives used by `date` for formatting timestamps.

This allows round-tripping between formatted strings and
timestamps when format and locale are consistent.

Examples:

```
(datestamp "2025.4.3" "%Y.%m.%d")
;-> 1743638400

(datestamp "October 31, 2024" "%B %d, %Y")
;-> 1730332800

(datelist (datestamp "October 31, 2024" "%B %d, %Y"))
;-> (2024 10 31 0 0 0 305 4)
```

Notes:

- The function always interprets the input as UTC and
  does not apply local time zone offsets.
- Invalid or incomplete date strings return nil.
- The supported timestamp range is limited to values
  representable before the year 2038.

See: [date](#f-date), [date-list](#f-datelist),

---


<a name="f-debug"></a>
## debug [!]

```
syntax: (debug function)
```

Description:

Enables tracing and evaluates `function`.  This is
shorthand for turning trace on, invoking the
expression, and then restoring the previous trace
state.  While trace is enabled, each function call and
return is printed, and errors do not abort execution.
Instead, the function that encounters an exception
returns `0` or `nil`, allowing program state and
variables to be inspected during debugging.

Examples:

```
; manual form
(trace true)
(function a b c)
(trace nil)

; equivalent shortcut
(debug (function a b c))
```

See: [trace](#f-trace)

---

<a name="f-dec"></a>
## dec [!]

```
syntax: (dec place [num])
```

Description:

Decrements the numeric value stored in `place` and returns
the updated result. The operation always uses floating-
point arithmetic. When `num` is omitted, the decrement is
1.0; otherwise `num` specifies the amount to subtract.
Integer values are converted to floating point before the
operation. `place` may be a symbol, a position inside a
list, or any writable location yielding a number.

If `place` evaluates to `nil`, it is treated as 0.0 before
the decrement. When `place` references a writable structure
element, that element is updated in place.

Examples:

```
(set x 10)
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

(set z nil)
(dec z 0.01)
;-> -0.01

(set l '(1 2 3 4))
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

- `dec` performs floating-point arithmetic.
- Use `--` for integer-mode decrement.
- Use `inc` for floating-point increment.

See: [--](#f-minusminus), [inc](#f-inc)

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
(set lst '(a b xvar c d))
;-> (a b xvar c d)

(delete 'xvar)
;-> true

lst
;-> (a b nil c d)

(set lst '(a b xvar c d))
;-> (a b xvar c d)

(delete 'xvar true)
;-> nil

lst
;-> (a b xvar c d)
```

Deleting an entire context:

```
(set ctx1:x 123)
;-> 123

(set ctx1:y "hello")
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
(set pid (process "/usr/bin/bc" in out))
;-> <pid>

(destroy pid)
;-> true

; kill a forked background worker
(set pid (fork (repeat (i 1000)
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
(set A '((-1 1 1)
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


<a name="f-dircd"></a>
## dircd

```
syntax: (dircd str-path)
```

Description:

Changes the current working directory to `str-path`. When
the directory change succeeds, the function returns
true. If the path does not exist or cannot be used as a
working directory, the function returns nil. The change
affects all subsequent file-system operations performed
by the current Rebel process.

Examples:

```
(dircd "/etc")
```

See: [dirpath](#f-dirpath)

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

<a name="f-do"></a>
## do

```
syntax: (do body)
```

Description:

Evaluates the expressions in `body` in sequence and returns
the value of the last one. `do` is used to group multiple
expressions where only a single expression is normally
allowed. It is commonly used inside conditional forms.

Many built-in control structures such as `cond`, `def`,
`doargs`, `dolist`, `dostring`, `when`, and `while`
already accept multiple expressions in their bodies, but
`do` is useful in forms like if, where only one `body`
expression is permitted.

The `silent` function behaves like `do` but suppresses
console output from the final result.

Examples:

```
(do
  (print "This is a block of 2 expressions\n")
  (print "================================"))
```

Notes:

- Returns the value of the last expression in `body`.
- Groups multiple expressions where only one is allowed.
- `silent` is the same as `do` but without printed output.

See: [silent](#f-silent), [if](#f-if), [cond](#f-cond)

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
(set x 1)
(do-until (> x 0) (inc x))
;-> 2

(set x 1)
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
(set x 10)
(do-while (< x 10) (inc x))
;-> 11

(set x 10)
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


<a name="f-forargs"></a>
## forargs

```
syntax: (forargs (sym [exp-break]) body)
```

Description:

Iterates over all arguments passed to the current
user-defined function or macro. The variable in `sym` is
bound to each argument in sequence. Iteration stops when
all arguments are processed or when `exp-break` evaluates to
a non-nil value. The return value is the final result of
evaluating `body`.

During iteration, the system iterator symbol `$idx` is
updated.

Examples:

```
(func (f)
  (forargs (i) (println i)))

(f 1 2 3 4)
;-> prints:
;   1
;   2
;   3
;   4

; stop early when an argument equals 'x
(mac (g)
  (forargs (i (= i 'x))
    (println i)))

(g a b x c d)
;-> prints:
;   a
;   b
;   true
```

Notes:

- `exp-break` is checked before each iteration step.
- `$idx` increments on each processed argument.
- Use `args` function or `$args` variable when the 
  full argument list is required at once.

See: [args](#f-args)

---


<a name="f-foreach"></a>
## foreach

```
syntax: (foreach (sym list-1|array-1 [exp-break]) body)
```

Description:

Iterates over each element of `list-1` or `array-1`.
Before each iteration, `sym` is bound to the current element.
The binding is local to the loop and follows dynamic
scoping rules. The return value of `foreach` is the last value
produced by `body`, unless an early exit occurs.

If `exp-break` is present, it is evaluated before each
iteration step. When `exp-break` evaluates to a non-nil
value, the loop terminates immediately and returns that
value.

During iteration, the system variable `$idx` contains the
current index (starting at `0`).

Examples:

```
(set x 123)

(foreach (x '(a b c d e f g))
  (print x))
;-> g
; console output:
;   abcdefg

; early exit when element equals 'e
(foreach (x '(a b c d e f g) (= x 'e))
  (print x))
;-> true
; console output:
;   abcd

; x outside the loop is unchanged
x
;-> 123

; show index and value
(foreach (x '(a b d e f g))
  (println $idx ":" x))
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

- `sym` is local to the loop.
- `exp-break` is evaluated before each iteration.
- `$idx` increments on each step and cannot be modified.

See: [repeat](#f-repeat), [for](#f-for),
[map](#f-map)

---


<a name="f-forchar"></a>
## forchar 

```
syntax: (forchar (sym string-1 [exp-break]) body)
```

Description:

Iterates over each character in `string-1`. Before every
iteration, `sym` is bound to the character’s integer code
point. The binding follows dynamic scoping rules and is
local to the loop.

If `exp-break` is present, it is evaluated before each
iteration step. When `exp-break` becomes non-nil, the loop
terminates immediately and returns its value. Otherwise,
`body` is evaluated and the loop continues.

The return value is the last evaluation of `body`. During
execution, the system iterator `$idx` contains the current
character index.

Examples:

```
; ASCII
(set str "abcdefg")
(forchar (c str)
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
(set txt "我能吞下玻璃而不伤身体。")
(forchar (c txt)
  (println c " " (char c)))
;-> prints codepoint and character for each element
```

Notes:

- Characters are iterated as full UTF-8 code points.
- Code points may exceed 255.
- `$idx` starts at zero and increments each iteration.

See: [foreach](#f-foreach), [repeat](#f-repeat),
[char](#f-char), [explode](#f-explode)

---


<a name="f-ls"></a>
## ls

```
syntax: (ls (sym sym-context [bool]) body)
```

Description:

`ls` loops over all symbols stored in `sym-context`.
Symbols are visited in sorted order.  Before each
iteration, `sym` is bound to the next symbol.  The
binding is local to the loop and follows dynamic
scoping rules.  The return value is the last evaluation
of `body`.

If `bool` evaluates to non-nil, only symbols whose names
begin with an underscore (_) are included. This is useful
when a context contains internal keys or auxiliary data
under underscore-prefixed names.

During iteration, the system variable `$idx` contains the
current symbol index.

Examples:

```
; iterate through all symbols in a context
(ls (s ctx)
  (print s " "))

; iterate only over symbols beginning with "_"
(ls (s ctx true)
  (print s " "))
```

Notes:

- Symbols order is lexicographic.
- `sym` is local to the loop.
- `$idx` increments on each step.
- `ls` avoids the memory overhead of creating a list
  using the `symbols` function.

See: [symbols](#f-symbols), [foreach](#f-foreach),
[forargs](#f-forargs)

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
(set lst '())
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
(set msg (encrypt "A secret message" "my secret key"))
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
function or an inline fn. Inside the handler,
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


<a name="f-etime"></a>
## etime

```
syntax: (etime exp [int-count])
```

Description:

Evaluates the expression `exp` and returns the time
spent on evaluation in floating point milliseconds.

When `int-count` is specified, `exp` is evaluated
`int-count` times and the total elapsed time is
returned.

Examples:

```
(etime (func x y z))
;-> 450.340

(etime (func x y z) 10)
;-> 4420.021
```

Notes:

- The returned value represents elapsed wall-clock
  time.
- When `int-count` is used, the reported time is the
  sum of all evaluations.
- `etime` evaluates its argument normally.

See: [mstime](#f-mstime)

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
(set expr '(+ 3 4))
expr
;-> (+ 3 4)

(eval expr)
;-> 7

(eval (list + 3 4))
;-> 7

(eval ''x)
;-> x

; evaluation uses current environment
(set x 3 'y 4)
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

(set data '(1 2 3 4 5))

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

(set x 10)
(set y 20)
(eval-string "(+ x y)")
;-> 30

; evaluate in a different context
(context 'C)
(set C:x 2)
(set C:y 3)

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
(set str "line1\nline2\n")
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
function-condition must be a function or fn that
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
(set x 2 'a '(d e))
(set val 'a)

(expand val 'a)
;-> (d e)

(expand '(a x b) 'x)
;-> (a 2 b)

(expand '(a x (b c x)) 'x 'a)
;-> ((d e) 2 (b c 2))

; dynamic function construction
(define (raise-to power)
  (expand (fn (base) (pow base power)) 'power))

(set square (raise-to 2))
(set cube   (raise-to 3))

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
(set A 1 'Bvar 2 'C nil 'd 5)

(expand '(A (Bvar) C d))
;-> (1 (2) C d)

; simplified currying with uppercase variable
(define (raise-to Power)
  (expand (fn (base) (pow base Power))))

(set cube (raise-to 3))
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
(set bytes "\001\002\003\004")
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
(set lst '(a b))
(extend lst '(c d))
;-> (a b c d)

(extend lst '(e f g))
;-> (a b c d e f g)

lst
;-> (a b c d e f g)

; extending strings
(set str "ab")
(extend str "cd")
;-> "abcd"

(extend str "efg")
;-> "abcdefg"

str
;-> "abcdefg"

; extending nested elements in place
(set lst '(a b "CD" (e f)))

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
  (repeat (e n)
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


<a name="f-fappend"></a>
## fappend

```
syntax: (fappend str-filename str-buffer)
```

Description:

Appends the content of `str-buffer` to the file specified
by `str-filename`. If the file does not yet exist, it is
created automatically and the function behaves like
`fwrite`. The return value is the number of bytes
written. On failure, the function returns `nil`.

When operating on local files, `syserr` can be queried
for additional information. When the target is a URL,
`nlasterr` provides extended diagnostics.

The function accepts both file paths and URLs with the
http:// or file:// prefix. When given an HTTP URL,
'`fappend`' behaves like '`hput`' with the
"Pragma: append\r\n" header automatically supplied. If
the remote file does not exist, it is created before the
append. This mechanism can also be used to transfer data
to remote Rebel nodes.

Examples:

```
(fwrite "myfile.txt" "ABC")
(fappend "myfile.txt" "DEF")
(fread "myfile.txt")
; → "ABCDEF"

(fappend "http://example.com/message.txt"
             "More message text.")
```

Notes:

- Returns the number of bytes written or `nil` on failure.
- Creates the file automatically if it does not exist.
- For HTTP URLs, uses an append-capable request similar
  to `hput`.
- For errors, use `syserr` (files) or `nlasterr` (URLs).

See: [fread](#f-fread), [fwrite](#f-fwrite)

---


<a name="f-fcopy"></a>
## fcopy

```
syntax: (copy-file str-from-name str-to-name)
```

Description:

Copies the file located at `str-from-name` into the path
specified by `str-to-name`. The function returns `true` when
the operation completes successfully. If the copy fails,
the result is `nil`. Failures may occur when the source
does not exist, the destination cannot be written, or an
I/O error interrupts the transfer.

Examples:

```
(fcopy "/home/me/rebel/data.rbl" "/tmp/data.rbl") ;-> true
```

See: [fmove](#f-fmove), [fdel](#f-fdel)

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
```

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
(set lst '((a 10 2 7) (b 5) (a 8 3) (c 8) (a 9)))

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
```

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
```

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
```

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

(set lst '(a b c d e))
(first lst)
;-> a

; arrays
(set arr (array 3 2 (sequence 1 6)))
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
```

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
```

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
```

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
; (set num (float (read-line)))
```

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
```

Description:

Returns true if exp evaluates to a floating-point number,
otherwise returns nil.

Examples:

```
(set num 1.23)
(float? num)
;-> true

(float? 123)
;-> nil

(float? "1.23")
;-> nil
```

See: [float](#f-float), [int?](#f-intp), [number?](#f-numberp)

---



<a name="f-floor"></a>
## floor

```
syntax: (floor number)
```

Description:

Returns the greatest integer less than or equal to number.
The result is always a floating-point value.

Examples:

```
(floor -1.5)
;-> -2

(floor 3.4)
;-> 3
```

See: [ceil](#f-ceil), [int](#f-int)

---


<a name="f-flt"></a>
## flt

```
syntax: (flt number)
```

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
```

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
(set f (fn (x) (+ x 1)))
(f 10)
;-> 11

; multiple parameters
(set add3 (fn (a b c) (+ a b c)))
(add3 1 2 3)
;-> 6

; default parameters
(set scale (fn (x (f 2)) (* x f)))
(scale 10)
;-> 20
(scale 10 5)
;-> 50

; variable arity using $args
(set sum-all (fn () (apply + $args)))
(sum-all 1 2 3 4 5)
;-> 15

; same using (args)
(set sum2 (fn () (apply + (args))))
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

(set inc2
  (fn-macro (x)
    (+ (eval x) 2)))

(inc2 10)
;-> 12


; 2) custom conditional form
;    (if-zero x a b) → (if (= x 0) a b)

(set if-zero
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

(set make-fn
  (fn-macro (name params body)
    (define (name params) (eval body))))

(make-fn triple (x) (* x 3))
(triple 10)
;-> 30


; 4) macro with a default argument
;    (add2 x) → (+ x 2)

(set add2
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
```

Notes:

- Step size must be positive; decreasing ranges work because
  sym still moves toward num-to by the given step.
- exp-break is tested before body, allowing clean early exit.
- for is suitable for numeric iteration; for iterating lists
  see dolist.

See: [sequence](#f-sequence), [repeat](#f-repeat),
[dolist](#f-dolist), [dotree](#f-dotree)

---


<a name="f-for-all"></a>
## for-all

```
syntax: (for-all func-condition list)
```

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
(set x 0)

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
(map with '(read-end write-end) (pipe))

(define (counter n out)
  (while (> n 0)
    (write-line out (string n))
    (dec n)))

(define (observer in)
  (let (line)
    (while (setq line (read-line in))
      (println "count " line))))

(set child-observer (fork (observer read-end)))
(set child-counter  (fork (counter 5 write-end)))

(wait-pid child-observer)
(wait-pid child-counter)


; destroying a long-running child
;------------------------------------------------------------
(define (demo)
  (set pid
       (fork
         (repeat (i 1000)
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
(set lst '("hello" 123))
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


<a name="f-freq"></a>
## freq

```
syntax: (freq list-1 list-2)
```

Description:

Computes the frequency of appearance of elements from
list-1 within list-2. The function returns a list of
frequency values aligned with the order of elements in
list-1.

list-1 should contain unique items. If list-1 contains
duplicates, only the first occurrence receives a
frequency value, and all following duplicates receive 0.

This function is often used to generate frequency tables.
Any type of comparable element may be counted, including
numbers, symbols, and strings.

Examples:

```
(freq '(1 2 3) '(3 2 1 4 2 3 1 1 2 2))
;-> (3 4 2)

(freq '(z a) '(z d z b a z y a))
;-> (3 2)

(set lst (explode (fread "myFile.txt")))
;-> list of characters

(set letter-counts (freq (uniq lst) lst))
;-> frequency data
```

See: [uniq](#f-uniq), [explode](#f-explode),
[fread](#f-fread)

---


<a name="f-func"></a>
## func

```
syntax: (func (sym-name [sym-1 ...]) body-1 [body-2 ...])
```

Description:

`func` creates a named function in the current context
and returns the created function value.  The returned
value is equivalent to an anonymous function created
with `fn`.

`sym-name` becomes a function that accepts the
specified parameters and evaluates the body expressions
in order.  The value of the last body expression is
returned.

Extra arguments passed to a function are available
through the system symbol `$args` or via the function
`(args)`.

Examples:

```
; basic named function
(func (increment x) (+ x 1))
(increment 10)
;-> 11

; multiple parameters
(func (add3 a b c) (+ a b c))
(add3 1 2 3)
;-> 6

; default parameters
(func (scale x (f 2)) (* x f))
(scale 10)
;-> 20
(scale 10 5)
;-> 50

; variable arity using $args
(func (sum-all) (apply + $args))
(sum-all 1 2 3 4 5)
;-> 15

; same using (args)
(func (sum2) (apply + (args)))
(sum2 1 2 3 4 5)
;-> 15
```

Notes:

- `func` creates a named function in the current context.
- Extra arguments are available as $args or via (args).
- Parameters follow dynamic scoping rules.
- For anonymous functions, see [fn](#f-fn).
- For macro definitions, use [mac](#f-mac) or 
  [macex](#f-macex).

See: [fn](#f-fn), [apply](#f-apply), [mac](#f-mac), 
     [macex](#f-macex)

---


<a name="f-functor"></a>
## functor

```
syntax: (functor context)
```

Description:

Returns the value stored in the default functor of the
given `context`. The default functor is the symbol whose
name is identical to the context name. Many expressions
implicitly fall back to the default functor when a bare
context name appears; `functor` provides explicit access to
that value.

Examples:

```
(set ctx:ctx 123)
;-> 123

(functor ctx)
;-> 123

(set (functor ctx) 456)
;-> 456

(set c ctx)
;-> ctx

(functor c)
;-> 456

ctx:ctx
;-> 456
```

Notes:

- The default functor is the symbol whose name matches
  the context name.
- Reading or writing the default functor is useful when
  an expression does not automatically resolve to it.
- Writable via `set` or `func`

See: [context](#f-context), [func](#f-func),
[set](#f-set)

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
(set buff "ABC\000\000\000DEF")
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
(set buff2 "ABC\000\000EFG\000DQW")

(get-string buff2 4 "FG")
;-> "ABC\000"

(get-string buff2 10)
;-> "ABC\000\000EFG\000D"

(get-string buff2 10 "FG")
;-> "ABC\000\000E"

; Step 7: UTF-32 buffer with explicit terminator limit
;-----------------------------------------------------------
(set utf32 (unicode "我能吞下玻璃而不伤身体。"))
(set addr (address utf32))

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
(set val 123)
(global 'val)
(global? 'val)
;-> true

; exporting and protecting a symbol
;-----------------------------------------------------------
(set step 10)
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
(set x 50)
(if (< x 100) "small" "big")
;-> "small"

(set x 1000)
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
(set lst '(A B C))
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

(set src "hello world")
(set dst (dup "\000" (length src)))

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

(set buf (dup "\000" 32))
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
(set x 0) ;-> 0
(inc x) ;-> 1
x ;-> 1

(inc x 0.25) ;-> 1.25
x ;-> 1.25
(inc x) ;-> 2.25

z ;-> nil
(inc z) ;-> 1

(set z nil)
(inc z 0.01) ;-> 0.01

(set l '(1 2 3 4))
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
user-defined function, or a fn expression.

exp-list is evaluated once before iteration. The order
of indices in the result list matches the order of
elements in exp-list.

Examples:

```
(index symbol? '(1 2 d 4 f g 5 h))
;-> (2 4 5 7)

(define (big? x) (> x 5))
;-> (fn (x) (> x 5))

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
(set num (int (read-line)))

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
(set num 123) ;-> 123
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
(set m1 '((-1 1 1) (1 4 -5) (1 -2 0)))
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
(set lst '("this" "is" "a" "sentence"))
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

(set A (array 3 2 (sequence 1 6)))
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

(set lst '(q w e r t y))
;-> (q w e r t y)
(length lst)
;-> 6

; array length (rows)
(set arr (array 2 4 '(0)))
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
;-> (fn (a b) ...)

(sum-sq 3 4)
;-> 25

; alternative syntax
(define (sum-sq a b)
  (let (x (* a a) y (* b b))
    (+ x y)))
;-> (fn (a b) ...)
```

Notes:

- let evaluates all initializers in the outer scope.
- Variables default to nil only in the parenthesized
  form where the initializer is omitted.
- let expands conceptually into:
    ((fn (sym1 sym2 ...) body) exp-init1 exp-init2 ...)
- Use letn when each initializer needs to see the
  bindings of previously initialized locals.
- Use local for automatic initialization to nil without
  writing initializer expressions.

See: [letn](#f-letn), [local](#f-local), [fn](#f-fn)

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
fn at definition time rather than at runtime. This
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
    (fn (x) (+ x c))))

(define add3 (make-adder 3))
;-> (fn (x) (+ x 3))

(add3 10)
;-> 13

; expansion into the same symbol
(define (make-adder n)
  (letex (n n)
    (fn (x) (+ x n))))
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
(set x 10)

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
other types yield nil. Fn expressions and
fn-macro expressions are also implemented as list
structures and therefore return true under list?.

Examples:

```
(set var '(1 2 3 4))
;-> (1 2 3 4)

(list? var)
;-> true

(define (double x) (+ x x))
;-> (fn (x) (+ x x))

(list? double)
;-> true
```

Notes:

- list? reflects the internal representation: functions
  defined with fn or define are stored as lists.
- For arrays, use array? rather than list?.

See: [array?](#f-arrayp), [fn?](#f-fnp), [list](#f-list)

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
(set x 42)
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
(set params '(
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
(set records '(
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

(set str "ABC")
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

(set x 3 'y 9)
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


<a name="f-make-dir"></a>
## make-dir

```
syntax: (make-dir str-dir-name [int-mode])
```

Description:

Creates a directory specified by str-dir-name.

When int-mode is given, it specifies the access mode of
the new directory. The function returns true on success
and nil on failure.

If no access mode is specified, the system default is
used (typically drwxr-xr-x).

The specified access mode is additionally masked by the
process umask. The effective permissions are the result
of:

requested mode AND (NOT umask)

The current umask can be inspected using the umask
command in the shell.

Examples:

```
; basic usage
;------------------------------------------------------------
(make-dir "adir")
;-> true

; explicit access mode (octal)
;------------------------------------------------------------
; leading zero denotes an octal number
(make-dir "secure-dir" 0750)
;-> true
```

Notes:

- Permission modes must be specified in octal form.
- The final permissions may differ due to the system
  umask (commonly 0022).
- If the directory already exists or cannot be created,
  nil is returned.

See: [remove-dir](#f-remove-dir), [change-dir](#f-change-dir)

---


<a name="f-map"></a>
## map

```
syntax: (map exp-functor list-args-1 [list-args-2 ... ])
```

Description:

Successively applies exp-functor to elements taken from
one or more argument lists and returns the collected
results as a list.

exp-functor can be a primitive function, a user-defined
function, or an anonymous function created with fn.

The number of arguments passed to exp-functor is
determined by the number of argument lists. Evaluation
stops when the first argument list is exhausted. Extra
elements in other lists are ignored.

The return value is always a list, even when arrays are
used as input.

Examples:

```
; basic mapping with multiple argument lists
;------------------------------------------------------------
(map + '(1 2 3) '(50 60 70))
;-> (51 62 73)

(map if '(true nil true nil true)
        '(1 2 3 4 5)
        '(6 7 8 9 10))
;-> (1 7 3 9 5)

(map (fn (x y) (* x y)) '(3 4) '(20 10))
;-> (60 40)

; generating a function dynamically
;------------------------------------------------------------
(define (make-adder n)
  (fn (x) (+ n x)))

(map (make-adder 2) '(1 2 3 4 5))
;-> (3 4 5 6 7)

(define (make-multiplier n)
  (fn (x) (* n x)))

(map (make-multiplier 3) '(1 2 3 4 5))
;-> (3 6 9 12 15)

; embedding map into a higher-level function
;------------------------------------------------------------
(define (map-with op p lst)
  (map (fn (x) (op p x)) lst))

(map-with + 2 '(1 2 3 4))
;-> (3 4 5 6)

(map-with * 1.5 '(1 2 3 4))
;-> (1.5 3 4.5 6)

; access to the internal index
;------------------------------------------------------------
(map (fn (x) (list $idx x)) '(a b c))
;-> ((0 a) (1 b) (2 c))
```

Notes:

- The iteration length is determined by the first
  argument list.
- Missing elements in other lists stop argument
  collection at that position.
- Special forms with syntactic evaluation rules
  (e.g. case) cannot be mapped.
- The variable $idx contains the zero-based index of
  the current iteration.

See: [apply](#f-apply), [fn](#f-fn)

---


<a name="f-mat"></a>
## mat

```
syntax: (mat + | - | * | / matrix-A matrix-B)
syntax: (mat + | - | * | / matrix-A number)
```

Description:

Performs element-wise arithmetic operations on
two-dimensional matrices.

The operation is specified by one of the arithmetic
operators +, -, *, or /. All computations are performed
using floating point arithmetic.

In the first syntax, the operation is applied pairwise
to corresponding elements of matrix-A and matrix-B.

In the second syntax, each element of matrix-A is
combined with the scalar value number.

Matrices are represented as two-dimensional lists or
arrays.

Examples:

```
; matrix-to-matrix operations
;------------------------------------------------------------
(set A '((1 2 3) (4 5 6)))
(set B A)

(mat + A B)
;-> ((2 4 6) (8 10 12))

(mat - A B)
;-> ((0 0 0) (0 0 0))

(mat * A B)
;-> ((1 4 9) (16 25 36))

(mat / A B)
;-> ((1 1 1) (1 1 1))

; operator supplied as a variable
;------------------------------------------------------------
(set op +)
(mat op A B)
;-> ((2 4 6) (8 10 12))

; matrix-to-scalar operations
;------------------------------------------------------------
(mat + A 5)
;-> ((6 7 8) (9 10 11))

(mat - A 2)
;-> ((-1 0 1) (2 3 4))

(mat * A 3)
;-> ((3 6 9) (12 15 18))

(mat / A 10)
;-> ((0.1 0.2 0.3) (0.4 0.5 0.6))
```

Notes:

- Operations are applied element-wise.
- All results are computed as floating point values.
- Matrix dimensions must match for matrix-to-matrix
  operations.

See: [det](#f-det), [invert](#f-invert), [multiply](#f-multiply), [transpose](#f-transpose)

---


<a name="f-match"></a>
## match

```
syntax: (match list-pattern list-match [bool])
```

Description:

Matches list-pattern against list-match and returns a
list of expressions captured by wildcard symbols in the
pattern.

The pattern may contain the wildcard symbols ?, +, and *:

- ? matches exactly one element and returns that element
- * matches zero or more elements and returns them as a list
- + matches one or more elements and returns them as a list

Wildcards may be nested. If the pattern cannot be
matched, match returns nil. If the pattern contains no
wildcards, an empty list is returned.

When the optional third argument evaluates to true, all
elements involved in the match are returned instead of
only the wildcard captures.

Examples:

```
; simple element capture
;------------------------------------------------------------
(match '(a ? c) '(a b c))
;-> (b)

(match '(a ? ?) '(a b c))
;-> (b c)

(match '(a ? c) '(a (x y z) c))
;-> ((x y z))

(match '(a ? c) '(a (x y z) c) true)
;-> (a (x y z) c)

(match '(a ? c) '(a x y z c))
;-> nil

; list capture with *
;------------------------------------------------------------
(match '(a * c) '(a x y z c))
;-> ((x y z))

(match '(a (b c ?) x y z) '(a (b c d) x y z))
;-> (d)

(match '(a (*) x ? z) '(a (b c d) x y z))
;-> ((b c d) y)

; behavior of + and *
;------------------------------------------------------------
(match '(+) '())
;-> nil

(match '(+) '(a))
;-> ((a))

(match '(+) '(a b))
;-> ((a b))

(match '(a (*) x ? z) '(a () x y z))
;-> (() y)

(match '(a (+) x ? z) '(a () x y z))
;-> nil

; binding matched values
;------------------------------------------------------------
(map with '(x y)
     (match '(a (? c) d *) '(a (b c) d e f)))

x
;-> b

y
;-> (e f)
```

Notes:

- The * wildcard matches the smallest possible sequence
  but may backtrack to allow a full match.
- The + wildcard behaves like *, but requires at least
  one element.
- match operates on lists only. String matching is not
  supported.

match is commonly used as a comparison functor in
[find](#f-find), [ref](#f-ref), [ref-all](#f-ref-all),
and [replace](#f-replace).

For logic-style pattern matching, see [unify](#f-unify).
For string matching, use [regex](#f-regex),
[find](#f-find), or [find-all](#f-find-all).

See: [unify](#f-unify), [find](#f-find), [replace](#f-replace)

---


<a name="f-max"></a>
## max

```
syntax: (max num-1 [num-2 ... ])
```

Description:

Evaluates the expressions num-1 and any following numbers
and returns the largest numeric value.

All arguments are evaluated before comparison. Integer and
floating-point numbers may be mixed. The return value uses
the numeric type required to represent the maximum value.

Examples:

```
(max 4 6 2 3.54 7.1)
;-> 7.1

(max 10 -5 3)
;-> 10

(max 2)
;-> 2
```

See: [min](#f-min)

---


<a name="f-member"></a>
## member

```
syntax: (member exp list)
syntax: (member str-key str [num-option])
```

Description:

In the first syntax, member searches for the element exp
in the list list. If exp is found, a new list is returned
starting with the matched element followed by the rest of
the original list. If exp is not found, nil is returned.

When num-option is specified, member performs a regular
expression search instead of a direct comparison.

In the second syntax, member searches for the substring
str-key in the string str. If found, the substring of str
starting at the match position is returned. If no match
is found, nil is returned.

Examples:

```
(set lst '(a b c d e f g h))
;-> (a b c d e f g h)

(member 'd lst)
;-> (d e f g h)

(member 55 lst)
;-> nil

(member "REBEL" "HelloREBEL")
;-> "REBEL"

(member "RE" "HelloREBEL")
;-> "REBEL"

(member "" "HelloREBEL")
;-> "HelloREBEL"

(member "xyz" "HelloREBEL")
;-> nil

(member "re" "HelloREBEL" 1)
;-> "REBEL"
```

See: [slice](#f-slice), [find](#f-find)

---


<a name="f-memcpy"></a>
## memcpy

```
syntax: (memcpy int-from-address int-to-address int-bytes)
```

Description:

Copies `int-bytes` of raw memory from `int-from-address` to
`int-to-address`.

The function operates directly on memory addresses and
does not perform any type checks, bounds checks, or
validation. Source and destination are treated as
untyped memory regions.

`memcpy` can read from or write to arbitrary memory
locations, including memory associated with Rebel
values and internal cells.

Examples:

```
; The example copies three bytes from a string literal
; into the memory backing an existing string value.

(set s "0123456789") ;-> "0123456789"
(memcpy "xxx" (+ (address s) 5) 3)
s ;-> "01234xxx89"
```

Notes:

- `memcpy` exposes raw memory manipulation.
- Behavior depends solely on the provided addresses and
  byte count.

See: [dump](#f-dump), [address](#f-address),
[pack](#f-pack), [unpack](#f-unpack)

---


<a name="f-min"></a>
## min

```
syntax: (min num-1 [num-2 ... ])
```

Description:

Evaluates the expressions num-1 and any following numbers
and returns the smallest numeric value.

All arguments are evaluated before comparison. Integer and
floating-point numbers may be mixed. The return value uses
the numeric type required to represent the minimum value.

Examples:

```
(min 4 6 2 3.54 7.1)
;-> 2

(min 10 -5 3)
;-> -5

(min 2)
;-> 2
```

See: [max](#f-max)

---


<a name="f-mod"></a>
## mod

```
syntax: (mod num-1 num-2 [num-3 ... ])
syntax: (mod num-1)
```

Description:

Calculates the modular value of the numbers in num-1 and
num-2. mod computes the remainder from the division of the
numerator num-i by the denominator num-i + 1.

The return value is computed as:

numerator - n * denominator

where n is the quotient of the numerator divided by the
denominator, rounded toward zero to an integer. The result
has the same sign as the numerator and its magnitude is
less than the magnitude of the denominator.

When multiple arguments are given, the operation is
applied successively from left to right.

In the second syntax, 1 is assumed for num-2 and the
result is the fractional part of num-1.

Examples:

```
(mod 10.5 3.3)
;-> 0.6

(mod -10.5 3.3)
;-> -0.6

(mod -10.5)
;-> -0.5
```

Use the [%](#f-percent) function when working with
integers only.

---


<a name="f-mstime"></a>
## mstime

```
syntax: (mstime)
```

Description:

Returns the time in milliseconds elapsed since the
start of the current day.

The value is reset at midnight and increases
monotonically during the day.

Examples:

```
(mstime)
;-> 45238765
```

Notes:

- The returned value represents milliseconds since
  00:00:00 local time.
- The value wraps at midnight.

See: [etime](#f-etime)

---


<a name="f-mul"></a>
## mul

```
syntax: (mul num-1 num-2 [num-3 ... ])
```

Description:

Evaluates all expressions num-1 and any following numbers,
calculating and returning their product.

mul performs mixed-type arithmetic but always returns a
floating-point value. If any argument is NaN, the result
is also NaN.

Examples:

```
(mul 1 2 3 4 5 1.1)
;-> 132

(mul 0.5 0.5)
;-> 0.25
```

---


<a name="f-multiply"></a>
## multiply

```
syntax: (multiply matrix-A matrix-B)
```

Description:

Returns the matrix multiplication of matrix-A and
matrix-B.

If matrix-A has dimensions n by m and matrix-B has
dimensions k by l, the values m and k must be equal.
The result is an n by l matrix.

multiply supports mixed-type arithmetic, but the
result always consists of double-precision floating
point values, even if all input elements are integers.

The dimensions of a matrix are determined by the
number of rows and the number of elements in the
first row. For missing elements in non-rectangular
matrices, 0.0 is assumed.

A matrix may be represented as either a nested list
or an array.

Examples:

```
(set A '((1 2 3) (4 5 6)))
(set B '((1 2) (1 2) (1 2)))

(multiply A B)
;-> ((6 12) (15 30))

(set v '(10 20 30))
(multiply A (transpose (list v)))
;-> ((140) (320))
```

When multiplying a matrix with a vector of n elements,
the vector must first be transformed into an n by 1
matrix using transpose.

All operations shown here on lists can also be
performed on arrays.

See: [det](#f-det), [invert](#f-invert), [mat](#f-mat), [transpose](#f-transpose)

---


<a name="f-nanp"></a>
## NaN?

```
syntax: (NaN? float)
```

Description:

Tests whether the result of a floating-point arithmetic
operation is a NaN value.

NaN stands for "Not a Number" and is a special IEEE 754
floating-point value produced by certain invalid or
undefined numeric operations.

All floating-point arithmetic operations involving a
NaN return NaN. All comparisons with NaN return nil.
Integer arithmetic treats NaN as the value 0.

Examples:

```
; floating-point operations on NaN yield NaN
(set x (sqrt -1))
;-> -nan

(NaN? x)
;-> true

(add x 123)
;-> -nan

(mul x 123)
;-> -nan

; integer operations treat NaN as zero
(+ x 123)
;-> 123

(* x 123)
;-> 0

; comparisons with NaN yield nil
(> x 0)
;-> nil

(<= x 0)
;-> nil

(= x x)
;-> nil

(set infinity (mul 1.0e200 1.0e200))
;-> inf

(NaN? (sub infinity infinity))
;-> true
```

See: [inf?](#f-infp)

---


<a name="f-net-accept"></a>
## net-accept

```
syntax: (net-accept int-socket)
```

Description:

Accepts an incoming connection on a socket that was
previously put into listening mode.

Returns a newly created socket handle that can be used
to receive and send data on the accepted connection.

Examples:

```
(set sock (net-listen 1234))
;-> <socket>

(net-accept sock)
;-> <socket>
```

Listening on ports below 1024 requires superuser
privileges.

See: [net-listen](#f-net-listen), [net-close](#f-net-close)

---


<a name="f-net-close"></a>
## net-close

```
syntax: (net-close int-socket [true])
```

Description:

Closes the network socket specified by int-socket.
The socket must have been previously created by
net-connect or net-accept.

Returns true on success and nil on failure.

When the optional true flag is given, immediate
shutdown is suppressed and the function waits for
any pending data transmissions to finish before
closing the socket.

Examples:

```
(net-close sock)
;-> true
```

See: [net-connect](#f-net-connect), [net-accept](#f-net-accept)

---


<a name="f-net-connect"></a>
## net-connect

```
syntax: (net-connect str-remote-host int-port [int-timeout-ms])
syntax: (net-connect str-remote-host int-port [str-mode [int-ttl]])
syntax: (net-connect str-file-path)
```

Description:

Creates a network socket and prepares it for communication.

In the first syntax, connects to remote host specified by
str-remote-host and port specified by int-port. On success,
returns a socket handle. On failure, returns nil.

Optional int-timeout-ms specifies connection timeout in
milliseconds. If omitted, a default timeout is used.

In the second syntax, str-mode selects UDP-related behavior.
Supported values:

- "udp" or "u"       create UDP socket
- "multi" or "m"     create UDP multicast socket
- "broadcast" or "b" create UDP broadcast socket

When using UDP modes, net-connect does not establish a
stream connection. The socket is prepared for sending and
receiving datagrams. Optional int-ttl specifies multicast
time-to-live value. Default is 3.

In the third syntax, connects to a local domain socket
specified by str-file-path. On success, returns a socket
handle. On failure, returns nil.

Examples:

```
; TCP
(set sock (net-connect "example.org" 80))
;-> <socket>

; timeout
(net-connect "example.org" 22 500)
;-> <socket>|nil

; UDP
(net-connect "226.0.0.1" 4096 "udp")
;-> <socket>

; UDP multicast
(net-connect "" 4096 "multi")
;-> <socket>

; UDP broadcast
(net-connect "192.168.2.255" 3000 "broadcast")
;-> <socket>

; local domain socket
(net-connect "/tmp/rebel.sock")
;-> <socket>
```

When using UDP modes, data transfer is performed using
net-send, net-send-to, net-receive, or net-receive-from.

See: [net-listen](#f-net-listen), [net-accept](#f-net-accept),
[net-send](#f-net-send), [net-receive](#f-net-receive)

---


<a name="f-net-error"></a>
## net-error

```
syntax: (net-error)
syntax: (net-error int-error)
```

Description:

Retrieves information about the last error produced by
network-related functions.

net-error reports errors from the following functions:
net-accept, net-connect, net-eval, net-listen,
net-lookup, net-receive, net-receive-udp, net-select,
net-send, net-send-udp, and net-service.

When one of these functions fails, it returns nil and
net-error can be used to obtain the error number and
message.

Each successful call to a net-* function clears the
stored error.

Functions that use sockets close the socket automatically
on failure and remove it from the net-sessions list.

When called without arguments, net-error returns nil if
no error is stored, or a list containing the last error
number and description.

When int-error is specified, net-error returns the error
number and description for that specific error code.

Error codes:

```
no   description
--   ------------------------------------------------
1    Cannot open socket
2    DNS resolution failed
3    Not a valid service
4    Connection failed
5    Accept failed
6    Connection closed
7    Connection broken
8    Socket send() failed
9    Socket recv() failed
10   Cannot bind socket
11   Too many sockets in net-select
12   Listen failed
13   Badly formed IP
14   Select failed
15   Peek failed
16   Not a valid socket
17   Cannot unblock socket
18   Operation timed out
19   HTTP badly formed URL
20   HTTP file operation failed
21   HTTP transfer failed
22   HTTP invalid response from server
23   HTTP no response from server
24   HTTP no content
25   HTTP error in header
26   HTTP error in chunked format
```

Examples:

```
(net-error)
;-> nil

(net-connect "jhghjgkjhg" 80)
;-> nil

(net-error)
;-> (2 "DNS resolution failed")

(net-error 10)
;-> (10 "Cannot bind socket")
```

See: [last-error](#f-last-error), [sys-error](#f-sys-error)

---


<a name="f-net-eval"></a>
## net-eval

```
syntax: (net-eval str-host int-port exp [int-timeout [func-handler]])
syntax: (net-eval '((str-host int-port exp) ... ) [int-timeout [func-handler]])
```

Description:

Evaluates source code remotely on one or more Rebel
nodes.

net-eval handles connection setup, transmission of
source code, evaluation on the remote node, and
collection of results.

The expression exp is evaluated in the environment of
the target node. exp may be:

- quoted expression
- string containing a single expression
- multiline program using [text] ... [/text]

When multiple expressions are provided in a string,
all are evaluated, but only the result of the first
expression is returned.

In the first syntax, net-eval sends exp to a single
remote node specified by str-host and int-port.

On success, the result of evaluating exp is returned.
On timeout or failure, nil is returned.

Optional int-timeout specifies timeout in milliseconds.
Default timeout is 60000 ms.

Examples:

```
(net-eval "192.168.1.94" 7306 '(+ 3 4))
;-> 7

(net-eval "192.168.1.94" 7306 "(+ 3 4)")
;-> 7

(net-eval "192.168.1.94" 7306 '(+ 3 4) 1)
;-> nil

(net-error)
;-> (18 "Operation timed out")

(net-eval "/tmp/rebel.sock" 0 '(+ 3 4))
;-> 7
```

In the second syntax, net-eval evaluates expressions on
multiple remote nodes.

Each element of the list specifies a target node and
expression. The function returns a list of results in
the same order. Nodes that time out return nil. Errors
are returned as (error-number error-text) lists.

Examples:

```
(net-eval '(
  ("192.168.1.94" 7306 '(+ 3 4))
  ("192.168.1.95" 7306 '(+ 5 6))
) 5000)
;-> (7 11)
```

When int-port is 0, str-host is interpreted as a local
domain socket path.

Optional func-handler specifies a handler function that
is called repeatedly while waiting and once for each
completed remote evaluation.

The handler is called with nil while waiting. When a
result is received, it is called with:

(str-host int-port result)

net-eval returns true if all evaluations complete
before timeout, or nil if the timeout expires.

Examples:

```
(define (handler msg)
  (if msg
      (println msg))
)

(net-eval '(
  ("192.168.1.94" 7306 '(+ 3 4))
  ("192.168.1.95" 7306 '(+ 5 6))
) 5000 handler)
```

See: [net-connect](#f-net-connect), [net-error](#f-net-error)

---


<a name="f-net-interface"></a>
## net-interface

```
syntax: (net-interface str-ip-addr)
syntax: (net-interface)
```

Description:

Sets or retrieves the default local interface address
used for network connections.

When str-ip-addr is specified, it becomes the default
local interface for subsequent network operations.
The address may be given as an IP address or a name.

When called without arguments, net-interface returns
the current default interface address. If no interface
has been set yet, the value "0.0.0.0" is returned. This
means the default address selected by the operating
system is used.

An explicitly set interface may be overridden by an
optional interface address provided to net-listen.

This function is useful on systems with multiple
network interfaces or multiple assigned IP addresses.
On systems with a single interface, network functions
select the interface automatically.

On error, the function returns nil. Use net-error to
retrieve error information.

Examples:

```
(net-interface "192.168.1.95")
;-> "192.168.1.95"

(net-interface "localhost")
;-> "127.0.0.1"

(net-interface)
;-> "127.0.0.1"
```

See: [net-listen](#f-net-listen), [net-error](#f-net-error)

---


<a name="f-net-ipv"></a>
## net-ipv

```
syntax: (net-ipv int-version)
syntax: (net-ipv)
```

Description:

Sets or retrieves the default Internet Protocol version
used for network operations.

int-version must be either 4 for IPv4 or 6 for IPv6.
When called without arguments, net-ipv returns the
current setting.

The default protocol version is IPv4.

Once a socket is created using net-connect or
net-listen, all subsequent operations on that socket
automatically use the protocol version selected at
creation time. Different sockets may use different
IPv4 or IPv6 settings at the same time.

The current protocol setting affects only sockets
created after the change.

Note:

net-packet operates in IPv4 mode regardless of the
current protocol setting.

Examples:

```
(net-ipv)
;-> 4

(net-ipv 6)
;-> 6

(net-ipv)
;-> 6
```

See: [net-connect](#f-net-connect), [net-listen](#f-net-listen)

---


<a name="f-net-listen"></a>
## net-listen

```
syntax: (net-listen int-port [str-ip-addr [str-mode]])
syntax: (net-listen str-file-path)
```

Description:

Creates a listening socket.

In the first syntax, binds to int-port and returns a
socket handle immediately. The returned socket is used
with net-accept to wait for an incoming connection.

After a connection is accepted, net-accept returns a
new socket handle that is used for communication with
the client.

Listening on ports below 1024 requires superuser
privileges.

When str-ip-addr is specified, listening is restricted
to that local interface address or name. If omitted,
the default interface is used.

Examples:

```
(set port 7306)
(set listen (net-listen port))

(unless listen
  (exit))

(set conn (net-accept listen))
(if conn
    (while (net-receive conn buff 1024 "\n")
      (print buff)
      (if (= buff "\r\n") (exit))))
```

In the second syntax, net-listen creates a local domain
socket specified by str-file-path. On success, returns
a socket handle usable with net-accept.

Examples:

```
(net-listen "/tmp/rebel.sock")
;-> <socket>

(net-accept <socket>)
;-> <socket>
```

After a connection is accepted, net-send, net-receive,
net-select, and net-peek can be used as with TCP
connections.

UDP mode:

When str-mode is "udp" or "u", net-listen creates a
UDP socket bound to the local address and port. In this
mode, net-accept is not used.

The socket can be used directly with net-receive-from,
net-send-to, net-select, or net-peek.

Examples:

```
(net-listen 10002 "" "udp")
;-> <socket>

(net-receive-from <socket> 1024)
```

In UDP mode, net-listen binds the socket but does not
establish a connection.

UDP multicast mode:

When str-mode is "multi" or "m", net-listen creates a
socket for multicast reception. str-ip-addr specifies
the multicast group address.

Examples:

```
(net-listen 4096 "226.0.0.1" "multi")
;-> <socket>

(net-receive-from <socket> 1024)
```

Packet divert mode:

When str-mode is "divert" or "d", net-listen creates a
divert socket bound to int-port. str-ip-addr is ignored.

This mode requires superuser privileges. The socket
receives raw packets diverted to the specified port
and may re-inject packets back into the kernel.

net-receive-from and net-send-to are used for reading
and writing packets on a divert socket.

See: [net-accept](#f-net-accept), [net-connect](#f-net-connect),
[net-receive](#f-net-receive), [net-receive-from](#f-net-receive-from),
[net-send-to](#f-net-send-to), [net-select](#f-net-select)

---


<a name="f-net-local"></a>
## net-local

```
syntax: (net-local int-socket)
```

Description:

Returns local address information for a network
connection associated with int-socket.

The return value is a list containing the local IP
address and port number used by the socket.

This is useful when the local address or port was
selected automatically by the operating system.

On error, the function returns nil. Use net-error
to retrieve error information.

Examples:

```
(net-local sock)
;-> ("204.179.131.73" 1689)
```

Use net-peer to retrieve the remote address and port
for the same connection.

See: [net-peer](#f-net-peer), [net-error](#f-net-error)

---


<a name="f-net-lookup"></a>
## net-lookup

```
syntax: (net-lookup str-ip-number)
syntax: (net-lookup str-hostname [bool])
```

Description:

Resolves host names and IP addresses.

In the first syntax, net-lookup returns the host name
associated with the IP address given in str-ip-number.

In the second syntax, net-lookup returns the IP address
associated with the host name given in str-hostname.

When the optional bool argument evaluates to true,
host-by-name lookup is forced even if str-hostname
starts with a numeric IP prefix.

On failure, the function returns nil. Use net-error
to retrieve error information.

Examples:

```
(net-lookup "example.com")
;-> "23.220.75.232"

(net-lookup "23.220.75.232")
;-> "a23-220-75-232.deploy.static.akamaitechnologies.com"
```

See: [net-error](#f-net-error)

---


<a name="f-net-packet"></a>
## net-packet

```
syntax: (net-packet str-packet)
```

Description:

Sends a custom network packet using a raw socket.

str-packet must contain a complete IPv4 packet starting
with an IP header, followed by a TCP, UDP, or ICMP header
and optional payload data.

The function requires superuser privileges.

Only IPv4 is supported.

On success, net-packet returns the number of bytes sent.
On failure, it returns nil. Use net-error and sys-error
to retrieve error information.

If checksum fields in the packet headers are zero, the
correct checksums are calculated and inserted
automatically. Existing checksums are left unchanged.

Examples:

```
; UDP packet with zeroed checksums (recalculated internally)

(set pkt (pack (dup "b" 39) '(
  0x45 0x00 0x00 0x27 0x4b 0x8f 0x00 0x00 0x40 0x11 0x00 0x00
  192 168 1 95
  192 168 1 92
  0xf2 0xc8 0x30 0x39 0x00 0x13 0x00 0x00
  0x48 0x65 0x6c 0x6c 0x6f 0x20 0x57 0x6f 0x72 0x6c 0x64
)))

(unless (net-packet pkt)
  (println (net-error))
  (println (sys-error)))
```

Warning:

Incorrect use can disrupt network devices. Use only on
isolated networks and only if you understand raw packet
processing.

See: [net-error](#f-net-error), [sys-error](#f-sys-error)

---


<a name="f-net-peek"></a>
## net-peek

```
syntax: (net-peek int-socket)
```

Description:

Returns the number of bytes ready for reading on the
network socket specified by int-socket.

If no data is available, 0 is returned. If an error
occurs or the connection is closed, nil is returned.

This function can be used to implement non-blocking
I/O loops.

Examples:

```
(set sock (net-connect "example.com" 7306))

(while (= (net-peek sock) 0)
  (do-something-else))

(net-receive sock buff 1024)
```

Use peek to check file descriptors and standard input.

See: [net-receive](#f-net-receive), [peek](#f-peek)

---


<a name="f-net-peer"></a>
## net-peer

```
syntax: (net-peer int-socket)
```

Description:

Returns remote address information for a network
connection associated with int-socket.

The return value is a list containing the remote IP
address and port number for the connection.

On error, the function returns nil. Use net-error
to retrieve error information.

Examples:

```
(net-peer sock)
;-> ("192.100.81.100" 13)
```

Use net-local to retrieve the local address and port
for the same connection.

See: [net-local](#f-net-local), [net-error](#f-net-error)

---


<a name="f-net-ping"></a>
## net-ping

```
syntax: (net-ping str-address [int-timeout [int-count bool]])
syntax: (net-ping list-addresses [int-timeout [int-count bool]])
```

Description:

Sends ICMP echo requests and collects reply information.

net-ping uses raw sockets and requires sufficient
privileges to create them. When executed without the
required privileges, the function returns nil and sets
an error retrievable with net-error.

In the first syntax, a single address is pinged.
str-address may be a host name, IP address, broadcast
address, wildcard, or range specification.

An optional int-timeout specifies the maximum wait
time in milliseconds. If omitted, the default timeout
is 1000 ms.

The return value is a list of (ip-address time-us)
pairs for each responding host. If no host responds,
an empty list is returned.

A return value of nil indicates an error. Use net-error
to retrieve error information.

Examples:

```
(net-ping "localhost")
;-> (("127.0.0.1" 222))

(net-ping "example.com" 3000)
;-> ()
```

In the second syntax, net-ping operates in batch mode.
Multiple addresses are pinged using a single socket.
Addresses may be specified as a list, wildcard (*),
or numeric range (-).

Packets are sent as fast as possible. Multiple replies
may be received.

Optional int-count limits the number of replies to
collect before returning. A value of 0 or omission
assumes all replies.

When bool evaluates to true, an error string is returned
instead of a response time for hosts that do not reply.

Examples:

```
(net-ping '("example.com" "192.168.1.255") 2000 20)
;-> (("23.220.75.232" 826420) ("192.168.1.1" 124))

(net-ping "192.168.1.*" 500)
;-> (("192.168.1.1" 120) ("192.168.1.2" 245))
```

Broadcast mode sends a single packet to a broadcast
address and may produce multiple replies. Batch mode
sends one packet per address.

Longer timeouts may be required when sending large
address lists. If the timeout expires before all
responses are received, net-ping may return an
incomplete list.

See: [net-error](#f-net-error)

---


<a name="f-net-receive"></a>
## net-receive

```
syntax: (net-receive int-socket sym-buffer int-max-bytes [wait-string])
```

Description:

Receives data from a network socket.

Reads up to int-max-bytes from int-socket and stores the
data in sym-buffer. The buffer size is adjusted to the
number of bytes actually received.

The return value is the number of bytes read. If the
connection is closed or an error occurs, nil is returned.
Use net-error to retrieve error information.

net-receive is a blocking call and waits until data
becomes available. Use net-peek or net-select to check
read readiness.

sym-buffer may be a symbol or a default functor used for
reference passing in user-defined functions.

When wait-string is specified, net-receive returns as
soon as the given string is received. The wait-string is
included in the contents of sym-buffer. In this mode,
data is read character-by-character and may be slower.

Examples:

```
(net-receive sock buf 1024)
;-> 128
```

Read until newline:

```
(define (net-receive-line sock buf)
  (net-receive sock buf 256 "\n"))

(net-receive-line sock line)
;-> 42
```

See: [net-peek](#f-net-peek), [net-select](#f-net-select),
[net-error](#f-net-error)

---


<a name="f-net-receive-from"></a>
## net-receive-from

```
syntax: (net-receive-from int-socket int-max-size)
```

Description:

Receives a UDP datagram from int-socket and returns
sender information.

The socket must have been created using net-listen or
net-connect with UDP mode enabled.

Up to int-max-size bytes are received. Excess bytes are
discarded.

On success, the function returns a list containing:

(data-string ip-address port)

On failure, nil is returned. Use net-error to retrieve
error information.

net-receive-from may be used in blocking mode or combined
with net-select or net-peek for non-blocking operation.

Examples:

```
(set sock (net-listen 1001 "" "udp"))
;-> <socket>

(while (not (net-select sock "r" 100000))
  (do-something))

(net-receive-from sock 20)
;-> ("hello" "192.168.0.5" 3240)

(net-send-to "192.168.0.5" 3240 "hello to you" sock)

(net-close sock)
```

In UDP communication, sender address and port are carried
in the packet itself. net-receive does not provide this
information and is therefore not suitable for replying
to UDP senders.

See: [net-listen](#f-net-listen), [net-connect](#f-net-connect),
[net-send-to](#f-net-send-to), [net-select](#f-net-select),
[net-error](#f-net-error)

---


<a name="f-net-receive-udp"></a>
## net-receive-udp

```
syntax: (net-receive-udp int-port int-maxsize [int-microsec [str-addr-if]])
```

Description:

Receives a UDP datagram on int-port.

Up to int-maxsize bytes are read. Bytes beyond this
limit are discarded.

The call blocks until a datagram arrives or until the
optional timeout int-microsec (microseconds) expires.

No prior setup using net-listen or net-connect is
required. When setting up a sender/receiver pair, the
receiver must be started first.

On success, net-receive-udp returns a list containing:

(data-string ip-address port)

On timeout or error, nil is returned. Use net-error to
retrieve error information.

When str-addr-if is specified, listening is restricted
to the given local interface address or name. When
using str-addr-if, a timeout must also be specified.

Examples:

```
; wait for datagram (max 20 bytes)
(net-receive-udp 10001 20)

; wait with timeout (5 seconds)
(net-receive-udp 10001 20 5000000)

; sender
(net-send-udp "example.com" 10001 "Hello")
;-> 5

; receiver result
;-> ("Hello" "203.0.113.10" 3312)

; binary payload
(net-send-udp "example.com" 2222 (pack "c c c c" 0 1 2 3))
;-> 4

(set buf (first (net-receive-udp 2222 10)))
(unpack "c c c c" buf)
;-> (0 1 2 3)
```

net-receive-udp is suitable for short, blocking UDP
transactions. For non-blocking UDP communication or
when replying to multiple senders, use net-listen or
net-connect with UDP mode together with net-receive-from
and net-send-to.

See: [net-send-udp](#f-net-send-udp), [net-receive-from](#f-net-receive-from),
[pack](#f-pack), [unpack](#f-unpack), [net-error](#f-net-error)

---


<a name="f-net-select"></a>
## net-select

```
syntax: (net-select int-socket str-mode int-micro-seconds)
syntax: (net-select list-sockets str-mode int-micro-seconds)
```

Description:

Checks readiness of one or more network sockets.

str-mode specifies the condition to test:

- "read" or "r"       ready for reading or accepting
- "write" or "w"      ready for writing
- "exception" or "e"  error condition

int-micro-seconds specifies the timeout in microseconds.
When set to -1, the call does not time out.

In the first syntax, net-select tests a single socket.
It returns true when the socket is ready, or nil on
timeout or error.

In the second syntax, net-select tests a list of sockets.
It returns a list of sockets that are ready. If no socket
becomes ready before timeout, an empty list is returned.

On error, net-error is set.

net-select enables non-blocking read, write, and accept
operations while allowing other parts of the program to
run.

Examples:

```
(set listen (net-listen 7306))

; wait for incoming connection
(while (not (net-select listen "r" 1000000))
  (if (net-error) (print (net-error))))

(set conn (net-accept listen))
(net-send conn "hello")

; wait for incoming data
(while (not (net-select conn "r" 1000000))
  (do-something))

(net-receive conn buf 1024)
```

Multiple sockets:

```
(set listen-list '(1001 1002))

(while (not (net-error))
  (dolist (s (net-select listen-list "r" 1000000))
    (accept-connection s)))
```

Supplying an invalid or closed socket causes an error
to be set in net-error.

See: [net-accept](#f-net-accept), [net-receive](#f-net-receive),
[net-send](#f-net-send), [net-error](#f-net-error)

---


<a name="f-net-send"></a>
## net-send

```
syntax: (net-send int-socket str-buffer [int-num-bytes])
```

Description:

Sends data on a network connection.

The contents of str-buffer are sent on the connection
specified by int-socket.

When int-num-bytes is specified, at most that many bytes
are sent from the beginning of str-buffer. When omitted,
the entire contents of str-buffer are sent.

The return value is the number of bytes actually sent.
On failure, nil is returned. Use net-error to retrieve
error information.

Examples:

```
(set buf "hello there")

(net-send sock buf)
;-> 11

(net-send sock buf 5)
;-> 5

(net-send sock "bye bye")
;-> 7
```

See: [net-receive](#f-net-receive), [net-send-to](#f-net-send-to),
[net-error](#f-net-error)

---


<a name="f-net-send-to"></a>
## net-send-to

```
syntax: (net-send-to str-remotehost int-remoteport str-buffer int-socket)
```

Description:

Sends data to a specified remote address using an
existing network socket.

The socket in int-socket must have been created using
net-connect or net-listen. The function can be used
with both TCP and UDP sockets.

When using UDP mode, net-connect or net-listen is used
only to create and bind the socket. No connection is
established.

When net-connect is used together with net-send-to,
only one of the two functions should specify the remote
address. The other must pass an empty string as the
address.

On success, the number of bytes sent is returned. On
failure, nil is returned. Use net-error to retrieve
error information.

Examples:

```
; UDP server
(set srv (net-listen 10001 "" "udp"))

(while (not (net-error))
  (set msg (net-receive-from srv 255))
  (net-send-to (nth 1 msg) (nth 2 msg)
               (upper-case (first msg)) srv))
```

```
; UDP client
(set cli (net-listen 10002 "" "udp"))

(net-send-to "127.0.0.1" 10001 "hello" cli)
(net-receive cli buf 255)
```

In UDP communication, the sender address and port are
taken from the received packet when replying.

For short blocking UDP transactions, use net-send-udp
and net-receive-udp.

See: [net-receive-from](#f-net-receive-from), [net-listen](#f-net-listen),
[net-send-udp](#f-net-send-udp), [net-error](#f-net-error)

---


<a name="f-net-send-udp"></a>
## net-send-udp

```
syntax: (net-send-udp str-remotehost int-remoteport str-buffer [bool])
```

Description:

Sends a UDP datagram to a remote host and port.

No prior setup using net-connect or net-listen is
required. The function sends the datagram and closes
the socket immediately.

The return value is the number of bytes sent. On
failure, nil is returned. Use net-error to retrieve
error information.

UDP does not guarantee delivery. If no receiver is
waiting, the datagram is lost. For reliable delivery,
a higher-level protocol must be implemented by the
application.

The practical maximum payload size is platform-
dependent. Sizes around 8 KB are generally safe.

Binary data can be sent using str-buffer.

Examples:

```
(net-send-udp "example.com" 3333 "Hello")
;-> 5
```

Broadcast mode:

When bool evaluates to true, the socket is put into
broadcast mode and the datagram is sent to all hosts
on the target network.

```
(net-send-udp "192.168.1.255" 2000 "Hello" true)
;-> 5
```

For non-blocking UDP communication or when maintaining
a socket for multiple sends, use net-connect or
net-listen with UDP mode together with net-send-to.

See: [net-receive-udp](#f-net-receive-udp),
[net-send-to](#f-net-send-to),
[net-connect](#f-net-connect),
[net-error](#f-net-error)

---


<a name="f-net-service"></a>
## net-service

```
syntax: (net-service str-service str-protocol)
syntax: (net-service int-port str-protocol)
```

Description:

Looks up service names and port numbers using the
system services database.

In the first syntax, net-service returns the standard
port number associated with str-service for the given
protocol.

In the second syntax, net-service returns the service
name associated with int-port for the given protocol.

str-protocol is typically "tcp" or "udp".

On failure, the function returns nil.

Examples:

```
(net-service "ftp" "tcp")
;-> 21

(net-service "http" "tcp")
;-> 80

(net-service 22 "tcp")
;-> "ssh"
```

See: [net-error](#f-net-error)

---


<a name="f-net-sessions"></a>
## net-sessions

```
syntax: (net-sessions)
```

Description:

Returns a list of active network sockets currently
managed by the runtime.

The list contains both listening sockets and active
connection sockets created by net-listen, net-connect,
or net-accept.

This function is useful for inspection, debugging,
and monitoring of open network resources.

If no sockets are active, an empty list is returned.

Examples:

```
(net-sessions)
;-> (3 4 7)
```

See: [net-listen](#f-net-listen), [net-connect](#f-net-connect),
[net-accept](#f-net-accept), [net-close](#f-net-close)

---


<a name="f-nilp"></a>
## nil?

```
syntax: (nil? exp)
```

Description:

Evaluates exp and returns true only if the result is
strictly nil. For any other value, including the empty
list (), nil? returns nil.

This predicate distinguishes between the logical value
nil and all other values that may be considered falsey
in different contexts.

Examples:

```
(map nil? '(x nil 1 nil "hi" ()))
;-> (nil true nil true nil nil)

(nil? nil)
;-> true

(nil? '())
;-> nil

; nil? means strictly nil
(nil? (not '()))
;-> nil
```

Notes:

- nil? returns true only for the value nil.
- The empty list () is not considered nil.
- nil? is useful when nil has a distinct semantic meaning
  from an empty list.

Note that nil? tests strictly for nil, while true?
matches any value that is neither nil nor the empty
list ().

See: [true?](#f-truep)

---


<a name="f-normal"></a>
## normal

```
syntax: (normal float-mean float-stdev int-n)
syntax: (normal float-mean float-stdev)
```

Description:

Generates normally distributed random floating point
numbers using the given mean and standard deviation.

In the first form, normal returns a list of length
int-n containing random values with a mean of
float-mean and a standard deviation of float-stdev.

In the second form, normal returns a single normally
distributed floating point number.

The internal pseudo-random number generator can be
seeded using the seed function.

Examples:

```
(normal 10 3 10)
;-> (7 6.563476562 11.93945312 6.153320312 9.98828125
;-> 7.984375 10.17871094 6.58984375 9.42578125
;-> 12.11230469)

(normal 1 0.2)
;-> 0.646875
```

When no parameters are given, normal assumes a mean of
0.0 and a standard deviation of 1.0.

Notes:

- normal produces continuously distributed values.
- Use random or rand for evenly distributed numbers.
- Use seed to control the pseudo-random sequence.

See: [random](#f-random), [rand](#f-rand),
[pick](#f-pick), [seed](#f-seed)

---


<a name="f-not"></a>
## not

```
syntax: (not exp)
```

Description:

Evaluates exp and returns true if the result is nil
or the empty list (). For any other value, not
returns nil.

not is a logical negation operator and treats both
nil and () as false.

Examples:

```
(not true)
;-> nil

(not nil)
;-> true

(not '())
;-> true

(not (< 1 10))
;-> nil

(not (not (< 1 10)))
;-> true
```

Notes:

- not returns true for both nil and ().
- Use nil? to test strictly for nil.

See: [nil?](#f-nilp), [true?](#f-truep)

---


<a name="f-now"></a>
## now

```
syntax: (now [int-minutes-offset [int-index]])
```

Description:

Returns information about the current date and time
as a list of integers.

An optional time-zone offset in minutes can be
specified in int-minutes-offset. This shifts the
time forward or backward before it is split into
individual date-time fields.

When int-index is provided, now returns only the
corresponding element from the result list.

Examples:

```
(now)
;-> (2002 2 27 18 21 30 140000 57 3 -300 0)

(now 0 -2)
;-> -300

(date-value (now))
;-> 1765683697
```

The result list contains the following fields:

```
index  field
-----  -------------------------------
0      year (Gregorian calendar)
1      month (1–12)
2      day (1–31)
3      hour (0–23, UTC)
4      minute (0–59)
5      second (0–59)
6      microsecond (0–999999)
7      day of year (Jan 1st is 1)
8      day of week (1–7, Monday = 1)
9      time zone offset (minutes west of GMT)
10     daylight savings time type
```

Notes:

- Hours are reported in UTC and are not adjusted
  for the local time zone.
- The resolution of the microseconds field depends
  on the platform; on some systems the last three
  digits are always 0.
- The day-of-week field follows ISO 8601, with
  Monday as day 1.
- On some platforms, the daylight savings time
  value may be 0 even during active DST.

Daylight savings time types:

```
value  meaning
-----  -----------------------------------------
0      not on daylight savings
1      USA style DST
2      Australian style DST
3      Western European DST
4      Middle European DST
5      Eastern European DST
6      Canada DST
```

See: [date](#f-date), [date-list](#f-date-list),
[date-parse](#f-date-parse), [date-value](#f-date-value),
[time](#f-time), [time-of-day](#f-time-of-day)

---


<a name="f-nper"></a>
## nper

```
syntax: (nper num-interest num-pmt num-pv [num-fv [int-type]])
```

Description:

Calculates the number of payment periods required to
repay a loan or investment.

num-interest specifies the interest rate per period,
num-pmt is the payment made each period, and num-pv
is the present value. An optional future value can
be given in num-fv.

When payments are made at the end of each period,
int-type is 0 or omitted. When payments are made at
the beginning of each period, int-type is 1.

Examples:

```
(nper (div 0.07 12) 775.30 -100000)
;-> 239.9992828
```

This example calculates the number of monthly payments
required to repay a loan of 100000 at a yearly interest
rate of 7 percent, with monthly payments of 775.30.

Notes:

- Interest rate is specified per period.
- A negative present value represents money borrowed.
- nper returns a floating point value.

See: [fv](#f-fv), [irr](#f-irr), [npv](#f-npv),
[pmt](#f-pm)

---


<a name="f-npv"></a>
## npv

```
syntax: (npv num-interest list-values)
```

Description:

Calculates the net present value of an investment
using a fixed interest rate.

num-interest specifies the interest rate per period.
list-values contains the cash flow values for each
period. Payments are represented by negative values,
while income is represented by positive values.

npv returns the total present value of all cash flows
discounted to the start of the first period.

Examples:

```
(npv 0.1 '(1000 1000 1000))
;-> 2486.851991

(npv 0.1 '(-2486.851991 1000 1000 1000))
;-> -1.434386832e-08
;-> ~ 0.0
```

In the example, an initial investment of 2486.85
allows for an income of 1000 at the end of the first,
second, and third periods.

Notes:

- Interest rate is specified per period.
- Negative values represent payments or investments.
- Positive values represent income.

See: [fv](#f-fv), [irr](#f-irr), [nper](#f-nper),
[pmt](#f-pmt), [pv](#f-pv)

---


<a name="f-nth"></a>
## nth

```
syntax: (nth int-index list)
syntax: (nth int-index array)
syntax: (nth int-index str)

syntax: (nth list-indices list)
syntax: (nth list-indices array)
```

Description:

Returns the element at the position specified by
int-index from a list, array, or string.

A single index accesses a single level. Multiple
indices may be used to recursively access elements
in nested lists or arrays. When multiple indices are
used, they must be provided as a list in list-indices.
If more indices are given than nesting levels, the
extra indices are ignored.

Negative indices count from the end. Out-of-bounds
indices cause an error.

Examples:

```
(set lst '(a b c))
(nth 0 lst)
;-> a

; implicit indexing
(lst 0)
;-> a

(set names '(adam bela cyril david))
;-> (adam bela cyril david)

(nth 2 names)
;-> cyril

(names -1)
;-> david
```

Multiple indices:

```
(set people '((adam 30) (bela 42) ((cyril nova) 17)))

(people 1 1)
;-> 42

(nth '(2 0 1) people)
;-> nova

; implicit form
(people 2 0 1)
;-> nova
```

Indices may be supplied via a list or vector:

```
(set idx '(2 0 1))

(people idx)
;-> nova

(nth idx people)
;-> nova
```

Negative indices:

```
(people -2 0)
;-> bela
```

Out-of-bounds indices cause an error:

```
(people 10)
;-> ERR: list index out of bounds

(people -5)
;-> ERR: list index out of bounds
```

Passing lists by reference using a context functor:

```
(set L:L '(a b c d e f g))

(define (second ctx)
  (nth 1 ctx))

(reverse L)
;-> (g f e d c b a)

L:L
;-> (g f e d c b a)

; by reference
(second L)
;-> b

; by value
(second L:L)
;-> b
```

Passing by reference is faster and uses less memory
for large lists and is recommended for lists with
hundreds of elements or more.

Arrays behave the same way as lists:

```
(set arr (array 2 3 '(a b c d e f)))
;-> ((a b c) (d e f))

(nth 1 arr)
;-> (d e f)

(arr 1)
;-> (d e f)

(nth '(1 0) arr)
;-> d

(arr 1 0)
;-> d

(arr '(1 0))
;-> d
```

String indexing:

In the string form, nth returns the character at
int-index as a string.

```
(nth 0 "Rebel")
;-> "R"

("Rebel" 0)
;-> "R"

("Rebel" -1)
;-> "l"
```

When UTF-8 support is enabled, nth operates on
character boundaries rather than byte boundaries.
To access single bytes in ASCII or binary buffers,
use slice.

Notes:

- nth supports implicit indexing by placing the
  container in the functor position.
- The implicit form is typically faster, while
  the explicit nth form may be more readable.

See: [setf](#f-setf), [push](#f-push), [pop](#f-pop),
[slice](#f-slice)

---


<a name="f-nullp"></a>
## null?

```
syntax: (null? exp)
```

Description:

Evaluates exp and returns true if the result is
considered null. The following values are treated
as null:

- nil
- the empty list ()
- the empty string ""
- NaN (not a number)
- 0 (zero)

For any other value, null? returns nil.

This predicate is useful when filtering or cleaning
results where multiple representations of “no value”
or “invalid value” may occur.

Examples:

```
(set x (sqrt -1))
;-> -nan

(null? x)
;-> true
```

```
(map null? '(1 0 0.0 2 "hello" "" (a b c) () true))
;-> (nil true true nil nil true nil true nil)
```

Using null? with filter:

```
(filter null? '(1 0 2 0.0 "hello" "" (a b c) () nil true))
;-> (0 0 "" () nil)
```

Using null? with clean:

```
(clean null? '(1 0 2 0.0 "hello" "" (a b c) () nil true))
;-> (1 2 "hello" (a b c) true)
```

Notes:

- null? matches multiple “empty” or invalid values.
- Use nil? to test strictly for nil.
- Use empty? to test empty lists or strings only.
- Use zero? to test numeric zero only.

See: [empty?](#f-emptyp), [nil?](#f-nilp), [zero?](#f-zerop)

---


<a name="f-numberp"></a>
## number?

```
syntax: (number? exp)
```

Description:

Evaluates exp and returns true if the result is a
number. This includes integers, floating point
numbers, and big integers. For any other value,
number? returns nil.

Examples:

```
(set x 1.23)
(set y 456)

(number? x)
;-> true

(number? y)
;-> true

(number? "678")
;-> nil
```

Notes:

- number? matches all numeric types, including
  big integers.
- Use float? or integer? to test for a specific
  numeric type.

See: [float?](#f-floatp), [integer?](#f-integerp)

---


<a name="f-oddp"></a>
## odd?

```
syntax: (odd? int-number)
```

Description:

Checks whether int-number has odd parity.

If int-number is not evenly divisible by 2, odd?
returns true; otherwise, it returns nil.

When a floating point number is provided, its
fractional part is discarded before the parity
check is performed. Big integers are supported.

Examples:

```
(odd? 123)
;-> true

(odd? 8)
;-> nil

(odd? 8.7)
;-> nil
```

Notes:

- odd? operates on integers and big integers.
- Floating point values are truncated to integers
  before testing.
- Use even? to test for even parity.

See: [even?](#f-evenp)

---


<a name="f-open"></a>
## open

```
syntax: (open str-path-file str-access-mode [str-option])
```

Description:

Opens a file specified by str-path-file using the
access mode given in str-access-mode and returns a
file handle as an integer. The returned handle is
used for subsequent read and write operations.

On failure, open returns nil.

When the access mode is "write" (or "w"), the file
is created if it does not exist, or truncated to
zero length if it already exists.

Access modes:

- "read"   or "r"  : read-only access
- "write"  or "w"  : write-only access
- "update" or "u"  : read/write access
- "append" or "a"  : append read/write access

Examples:

```
(device (open "file.txt" "write"))
;-> 5

(print "hello world\n")
;-> "hello world"

(close (device))
;-> 5
```

Reading from the same file:

```
(set f (open "file.txt" "read"))

(seek f 6)

(set c (read-char f))

(print c "\n")

(close f)
```

The first example sets the current output device to
the file handle returned by open and writes text
into the file. The second example reads a single
byte at offset 6 from the same file.

Calling close on (device) automatically resets the
current device to 0.

Options:

As an optional third argument, "non-block" or "n"
may be specified together with "read" or "write".
This enables non-blocking mode, which is useful
when working with named pipes.

Notes:

- open returns a numeric file handle.
- The behavior of non-blocking mode depends on the
  underlying I/O object.

See: [close](#f-close), [read-char](#f-read-char),
[seek](#f-seek), [print](#f-print), [device](#f-device)

---


<a name="f-or"></a>
## or

```
syntax: (or exp-1 [exp-2 ... ])
```

Description:

Evaluates expressions from left to right and returns
the first result that is neither nil nor the empty
list ().

If all expressions evaluate to nil or (), or returns
nil. When called with no arguments, or returns nil.

or performs short-circuit evaluation: once a
non-nil, non-empty value is found, remaining
expressions are not evaluated.

Examples:

```
(set x 10)

(or (> x 100) (= x 10))
;-> true

(or "hello" (> x 100) (= x 10))
;-> "hello"

(or '())
;-> ()

(or true)
;-> true

(or)
;-> nil
```

Notes:

- or returns the actual value of the first successful
  expression, not just true.
- Both nil and () are treated as false.
- Evaluation stops at the first successful result.

See: [and](#f-and), [not](#f-not), [nil?](#f-nilp)

---


<a name="f-pack"></a>
## pack

```
syntax: (pack str-format [exp-1 [exp-2 ... ]])
syntax: (pack str-format [list])

syntax: (pack struct [exp-1 [exp-2 ... ]])
syntax: (pack struct [list])
```

Description:

Packs values into a binary string according to a
format specification.

When the first argument is a string, str-format
describes the binary layout. The remaining expressions
or the values in list are packed sequentially and
returned as a binary string buffer.

The complementary function unpack is used to extract
values from such a binary buffer.

pack is typically used when working with binary files,
binary protocols, or foreign function interfaces.

When the first argument is a struct symbol, pack uses
the format defined by the struct. In this form, padding
bytes may be inserted automatically to satisfy data
alignment rules defined by the structure layout.

If no data expressions or list are provided, the
specified format or structure is filled with zero
bytes.

Format specifiers:

```
code  description
----  ----------------------------------------------
c     signed 8-bit integer
b     unsigned 8-bit integer
d     signed 16-bit integer
u     unsigned 16-bit integer
ld    signed 32-bit integer
lu    unsigned 32-bit integer
Ld    signed 64-bit integer
Lu    unsigned 64-bit integer
f     32-bit floating point number
lf    64-bit floating point number
sn    string of n null-padded ASCII characters
nn    n null bytes
>     switch to big-endian byte order
<     switch to little-endian byte order
```

Floating point values passed to integer formats are
converted to integers. Integer values passed to
floating point formats are converted to floats.

Examples:

```
(pack "c c c" 65 66 67)
;-> "ABC"

(unpack "c c c" "ABC")
;-> (65 66 67)

(pack "c c c" 0 1 2)
;-> "\000\001\002"

(unpack "c c c" "\000\001\002")
;-> (0 1 2)

(set s (pack "c d u" 10 12345 56789))
(unpack "c d u" s)
;-> (10 12345 56789)

(set s (pack "s10 f" "result" 1.23))
(unpack "s10 f" s)
;-> ("result\000\000\000\000" 1.230000019)

; Zero-filled formats:
;----------------------------------------------------------
(pack "n10")
;-> "\000\000\000\000\000\000\000\000\000\000"

(set lst '("A" "B" "C"))
(set buf (pack "lululu" lst))
(map get-string (unpack "lululu" buf))
;-> ("A" "B" "C")

; Byte order control:
;----------------------------------------------------------
(pack "d" 1)
;-> "\001\000"

(pack ">d" 1)
;-> "\000\001"

(pack ">u <u" 1 1)
;-> "\000\001\001\000"

(set txt "我能吞下玻璃而不伤身体。")
(set lst (unpack (dup "b" (length txt)) txt))
(pack (dup "b" (length lst)) lst)
;-> "我能吞下玻璃而不伤身体。"
```

Notes:

- pack returns a binary string buffer.
- The format determines size and byte order.
- pack used with struct may insert padding bytes.
- Spaces in the format string are ignored.

See: [unpack](#f-unpack), [struct](#f-struct),
[address](#f-address), [get-int](#f-get-int),
[get-long](#f-get-long), [get-char](#f-get-char),
[get-string](#f-get-string)

---


<a name="f-parse"></a>
## parse

```
syntax: (parse str-data [str-break [regex-option]])
```

Description:

Splits the string produced by evaluating str-data
into a list of string tokens.

When str-break is omitted, parse tokenizes the input
according to the language’s internal parsing rules.
This mode uses the same fast tokenizer as source
parsing and is intended for general-purpose token
splitting.

When str-break is provided, it specifies how the
input string is split. It may be a literal string
delimiter or a regular expression pattern. If
regex-option is given, str-break is treated as a
regular expression.

When no break specification is given, token sizes
are limited by the internal tokenizer. When a break
string or pattern is provided, there is no practical
limit on token length.

Examples:

```
; no break string specified
(parse "hello how are you")
;-> ("hello" "how" "are" "you")

; implicit tokenization on spaces, punctuation, and numbers
(parse "weight is 10lbs")
;-> ("weight" "is" "10" "lbs")

; specifying a break string
(parse "one:two:three" ":")
;-> ("one" "two" "three")

(parse "one--two--three" "--")
;-> ("one" "two" "three")

; regular expression break patterns
(parse "one-two--three---four" "-+" 0)
;-> ("one" "two" "three" "four")

(parse "hello regular   expression 1, 2, 3" {,\s*|\s+} 0)
;-> ("hello" "regular" "expression" "1" "2" "3")

; empty fields around separators are preserved
(parse "1,2,3," ",")
;-> ("1" "2" "3" "")

(parse "1,,,4" ",")
;-> ("1" "" "" "4")

(parse "," ",")
;-> ("" "")

; empty input always produces an empty list
(parse "")
;-> ()

(parse "" " ")
;-> ()
```

Notes:

- When str-break is omitted, parse uses a fast
  internal tokenizer suitable for source-like input.
- When a break string or pattern is provided, parse
  splits strictly at the specified delimiters.
- Empty fields between separators are returned as
  empty strings.
- Parsing an empty string always returns ().

See: [regex](#f-regex), [find](#f-find),
[find-all](#f-find-all), [replace](#f-replace),
[search](#f-search), [directory](#f-directory)

---


<a name="f-peek"></a>
## peek

```
syntax: (peek int-handle)
```

Description:

Returns the number of bytes ready to be read from
the file descriptor specified by int-handle.

If the file descriptor is invalid, peek returns nil.
peek does not read any data; it only reports how many
bytes can be read without blocking.

The value 0 may be used to check standard input.

Examples:

```
(peek 0)
;-> 12
```

This example checks how many bytes are currently
available on standard input.

Notes:

- peek operates on file descriptors.
- peek does not consume any input.
- A return value of 0 means no data is available.

For checking sockets or obtaining additional error
information, use net-peek.

See: [net-peek](#f-net-peek), [read](#f-read),
[read-char](#f-read-char)

---


<a name="f-pick"></a>
## pick 

```
syntax: (pick exp-1 [exp-2 ...])
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
(pick 'a 'b 'c 'd 'e)       ; → one of: a b c d e

(repeat (i 10)
  (print (pick 3 5 7)))     ; → 35777535755
```

Notes:

- One and only one expression result is selected.
- Uses the same internal generator as rand.
- Seed can be used to reproduce the random sequence.

See: [rand](#f-rand), [seed](#f-seed), [random](#f-random)

---


<a name="f-pipe"></a>
## pipe

```
syntax: (pipe)
```

Description:

Creates an inter-process communication pipe and
returns a list containing two file descriptors:
the read end and the write end of the pipe.

The returned handles can be passed to a child
process created with process or fork to enable
communication between processes.

Examples:

```
(pipe)
;-> (3 4)
```

In this example, descriptor 3 is used for reading
and descriptor 4 for writing.

Notes:

- Writing to a pipe does not block.
- Reading from a pipe blocks until data is available.
- read-line blocks until a newline character is read.
- read blocks if fewer bytes than requested are
  available and the write end is still open.
- Multiple pipes may be created as needed.

Named pipes may also be used. See open for details.

See: [process](#f-process), [fork](#f-fork),
[read](#f-read), [read-line](#f-read-line),
[open](#f-open)

---


<a name="f-pmt"></a>
## pmt

```
syntax: (pmt num-interest num-periods num-principal
           [num-future-value [int-type]])
```

Description:

Calculates the periodic payment required to repay
a loan or investment with a constant interest rate
over a fixed number of periods.

num-interest specifies the interest rate per period.
num-periods is the total number of payment periods.
num-principal is the present value of the loan.
An optional num-future-value specifies the value
remaining at the end of the periods (typically 0.0).

When payments are made at the end of each period,
int-type is 0 or omitted. When payments are made at
the beginning of each period, int-type is 1.

The returned value is typically negative, indicating
an outgoing payment.

Examples:

```
(pmt (div 0.07 12) 240 100000)
;-> -775.2989356
```

This example calculates the monthly payment for a
loan of 100000 at a yearly interest rate of 7 percent,
paid over 240 monthly periods.

Notes:

- Interest rate is specified per period.
- A negative result represents a payment.
- pmt returns a floating point value.

See: [fv](#f-fv), [irr](#f-irr), [nper](#f-nper),
[npv](#f-npv), [pv](#f-pv)

---


<a name="f-pop"></a>
## pop

```
syntax: (pop list [int-index-1 [int-index-2 ... ]])
syntax: (pop list [list-indexes])
syntax: (pop str [int-index [int-length]])
```

Description:

Removes and returns elements from lists or characters
from strings.

In the list forms, pop removes an element from the
list produced by evaluating list. When no index is
specified, the first element is removed. When one or
more indices are given, they are used to locate the
target element, similar to recursive indexing.

Indices may also be provided as a list in
list-indexes. This form is convenient when working
with functions such as ref or ref-all, which return
lists of indices.

pop modifies the target list in place and returns
the removed element.

In the string form, pop removes characters from the
string. With no index, the first character is
removed. With int-index and int-length, a substring
is removed and returned.

Examples:

```
(set lst '((f g) a b c "hello" d e 10))

(pop lst)
;-> (f g)

(pop lst)
;-> a

lst
;-> (b c "hello" d e 10)

(pop lst 3)
;-> d

(pop lst -1)
;-> 10

lst
;-> (b c "hello" e)

(pop lst -1)
;-> e

lst
;-> (b c "hello")

(pop lst -2)
;-> c

lst
;-> (b "hello")

; nested indices
;----------------------------------------------------------
(set lst '(a 2 (x y (p q) z)))

(pop lst -1 2 0)
;-> p

; using indices in a list
;----------------------------------------------------------
(set lst '(a b (c d () e)))

(push 'x lst '(2 2 0))
;-> (a b (c d (x) e))

lst
;-> (a b (c d (x) e))

(ref 'x lst)
;-> (2 2 0)

(pop lst '(2 2 0))
;-> x

; pop on strings (UTF-8 aware)
;----------------------------------------------------------
(set str "Rebel")

(pop str -2 2)
;-> "el"

str
;-> "Reb"

(pop str 1)
;-> "e"

str
;-> "Rb"

(set str "x")

(pop str)
;-> "x"

(pop str)
;-> ""
```

Notes:

- pop modifies the target list or string.
- Negative indices count from the end.
- pop on strings operates on character boundaries
  when UTF-8 support is enabled.
- Popping an empty string returns an empty string.

See: [push](#f-push), [ref](#f-ref), [ref-all](#f-ref-all)

---


<a name="f-pop-assoc"></a>
## pop-assoc

```
syntax: (pop-assoc exp-key list-assoc)
syntax: (pop-assoc list-keys list-assoc)
```

Description:

Removes and returns an association from an
association list.

In the first form, pop-assoc removes the association
whose key matches exp-key from list-assoc and returns
the removed association.

In the second form, list-keys specifies a path of
keys for accessing nested association lists. The
association at the specified path is removed and
returned.

pop-assoc modifies list-assoc in place.

Examples:

```
; simple associations
(set lst '((a 1) (b 2) (c 3)))

(pop-assoc 'b lst)
;-> (b 2)

lst
;-> ((a 1) (c 3))

;----------------------------------------------------------
; nested associations
(set lst '((a (b 1) (c (d 2)))))

(pop-assoc 'a lst)
;-> (a (b 1) (c (d 2)))

lst
;-> ()

(set lst '((a (b 1) (c (d 2)))))

(pop-assoc '(a b) lst)
;-> (b 1)

lst
;-> ((a (c (d 2))))

(set lst '((a (b 1) (c (d 2)))))

(pop-assoc '(a c) lst)
;-> (c (d 2))

lst
;-> ((a (b 1)))
```

Notes:

- pop-assoc removes exactly one association.
- Nested keys allow direct modification of
  hierarchical association lists.
- list-assoc is modified in place.

See: [assoc](#f-assoc), [setf](#f-setf)

---


<a name="f-post-url"></a>
## post-url

```
syntax: (post-url str-url str-content [str-content-type [str-option] [int-timeout [str-header]]])
```

Description:

Sends an HTTP POST request to the URL specified by
str-url and returns the response body as a string.

POST requests are commonly used to submit form data
or upload content to a server. post-url mimics the
behavior of a web client sending data to an HTTP
endpoint, but it may also be used to send arbitrary
content types, including binary data.

When an error occurs, post-url returns a string
starting with the prefix ERR:.

An optional timeout may be specified in
int-timeout, given in milliseconds. If no response
is received before the timeout expires, the string
ERR: timeout is returned.

When str-content-type is omitted, the default
content type is assumed to be:

```
application/x-www-form-urlencoded
```

Examples:

```
; specify content type
(post-url "https://example.com/form.cgi"
          "name=johnDoe&city=New%20York"
          "application/x-www-form-urlencoded")

; specify content type and timeout
(post-url "https://example.com/form.cgi"
          "name=johnDoe&city=New%20York"
          "application/x-www-form-urlencoded"
          nil
          8000)

; default content type, no timeout
(post-url "https://example.com/form.rbl"
          "name=johnDoe&city=New%20York")
```

Notes:

- post-url returns the server response as a string.
- On error, a string starting with ERR: is returned.
- int-timeout is specified in milliseconds.
- Additional options and custom headers may be
  provided using str-option and str-header.

Options:

When str-content-type is specified, str-option
accepts the same options as get-url for handling
the returned content. When a timeout is specified,
a custom HTTP header string may also be supplied.

See: [get-url](#f-get-url), [put-url](#f-put-url)

---


<a name="f-pow"></a>
## pow

```
syntax: (pow num-1 num-2 [num-3 ... ])
syntax: (pow num-1)
```

Description:

Raises num-1 to the power of num-2 and any following
exponents in sequence.

When more than two numbers are given, the result of
each exponentiation is used as the base for the next
one.

When only num-1 is provided, pow assumes an exponent
of 2.

Examples:

```
(pow 100 2)
;-> 10000

(pow 100 0.5)
;-> 10

(pow 100 0.5 3)
;-> 1000

(pow 3)
;-> 9
```

Notes:

- Exponents are applied from left to right.
- pow returns a numeric result.

See: [sqrt](#f-sqrt), [exp](#f-exp), [log](#f-log)

---


<a name="f-prefix"></a>
## prefix

```
syntax: (prefix sym)
```

Description:

Returns the context part of the symbol sym.

Examples:

```
(setf s 'ctx:var)
;-> ctx:var

(prefix s)
;-> ctx

(context? (prefix s))
;-> true

(term s)
;-> "var"

(= s (sym (term s) (prefix s)))
;-> true

(context (prefix s))
;-> ctx
```

Notes:

- prefix extracts the context of a symbol.
- The returned value is a context symbol.
- prefix is commonly used together with term.

See: [term](#f-term), [sym](#f-sym), [context](#f-context)

---


<a name="f-prefix"></a>
## prefix

```
syntax: (prefix sym)
```

Description:

Returns the context part of the symbol sym.

Examples:

```
(setf s 'ctx:var)
;-> ctx:var

(prefix s)
;-> ctx

(context? (prefix s))
;-> true

(term s)
;-> "var"

(= s (sym (term s) (prefix s)))
;-> true

(context (prefix s))
;-> ctx
```

Notes:

- prefix extracts the context of a symbol.
- The returned value is a context symbol.
- prefix is commonly used together with term.

See: [term](#f-term), [sym](#f-sym), [context](#f-context)

---


<a name="f-pretty-print"></a>
## pretty-print

```
syntax: (pretty-print [int-length [str-tab [str-fp-format]]])
```

Description:

Controls the formatting of expressions when printing,
saving, or displaying source in an interactive
console.

The optional parameters specify formatting settings:

- int-length sets the maximum line length.
- str-tab specifies the string used for indentation.
- str-fp-format defines the default format used for
  printing floating point numbers.

When called without arguments, pretty-print returns
the current formatting settings. When parameters are
provided, the settings are updated and the new values
are returned.

Examples:

```
(pretty-print)
;-> (80 " " "%1.15g")

(pretty-print 90 "\t")
;-> (90 "\t" "%1.15g")

(pretty-print 100)
;-> (100 "\t" "%1.15g")

(sin 1)
;-> 0.841470984807897

(pretty-print 80 " " "%1.3f")

(sin 1)
;-> 0.841

(set x 0.0)
x
;-> 0.000
```

The default settings use a maximum line length of 80,
a single space for indentation, and a high-precision
floating point format.

Changing the floating point format is useful when
printing values without fractional parts that should
remain distinguishable as floating point numbers.

All output operations that rely on automatic
formatting are affected by the current pretty-print
settings.

Notes:

- pretty-print cannot be used to suppress line
  breaking entirely.
- To output an expression without any formatting,
  convert it to a raw string first.

Examples without formatting:

```
(print (string my-expression))
```

See: [string](#f-string), [print](#f-print)

---


<a name="f-primitivep"></a>
## primitive?

```
syntax: (primitive? exp)
```

Description:

Evaluates exp and returns true if the result is a
primitive symbol. Otherwise, primitive? returns nil.

Primitive symbols include all built-in functions
and functions created using import.

Examples:

```
(set var define)

(primitive? var)
;-> true
```

Notes:

- primitive? matches built-in functions.
- Functions created with import are also primitives.
- User-defined functions are not primitives.

See: [import](#f-import), [define](#f-define)

---


<a name="f-print"></a>
## print

```
syntax: (print exp-1 [exp-2 ... ])
```

Description:

Evaluates each expression and prints the resulting
values to the current I/O device. By default, output
is sent to the console.

The current output device may be changed using the
device function.

When printing list expressions, elements are
indented according to the nesting level of their
opening parentheses.

Strings may contain escape sequences introduced by
the backslash character.

Escape sequences:

```
code   description
-----  -----------------------------------------
\n     line feed (ASCII 10)
\r     carriage return (ASCII 13)
\t     tab character (ASCII 9)
\nnn   decimal ASCII code (000–255)
\xnn   hexadecimal ASCII code (00–FF)
```

Examples:

```
(print (set res (+ 1 2 3)))
(print "the result is " res "\n")

"\065\066\067"
;-> "ABC"
```

Notes:

- print does not append a line feed automatically.
- Use println to terminate output with a line feed.
- Output is written to the current device.

See: [println](#f-println), [device](#f-device),
[string](#f-string)

---


<a name="f-println"></a>
## println

```
syntax: (println exp-1 [exp-2 ... ])
```

Description:

Evaluates each expression and prints the resulting
values to the current I/O device. By default, output
is sent to the console.

println works like print, but automatically appends
a line-feed character at the end of the output.

The current output device may be changed using the
device function.

Examples:

```
(println "hello world")
;-> "hello world\n"

(println "the result is " (+ 1 2 3))
;-> "the result is 6\n"
```

Notes:

- println always appends a line feed.
- Use print when no trailing line feed is desired.
- Output is written to the current device.

See: [print](#f-print), [write-line](#f-write-line),
[device](#f-device)

---


<a name="f-prob-chi2"></a>
## prob-chi2

```
syntax: (prob-chi2 num-chi2 int-df)
```

Description:

Returns the probability that an observed Chi-square
statistic num-chi2 is equal to or greater than the
given value under the null hypothesis, using
int-df degrees of freedom.

prob-chi2 is derived from the incomplete Gamma
function gammai.

Examples:

```
(prob-chi2 10 6)
;-> 0.1246520195
```

Notes:

- The returned value is a probability in the range
  0.0 to 1.0.
- Larger Chi-square values yield smaller
  probabilities for the same degrees of freedom.

See: [crit-chi2](#f-crit-chi2), [gammai](#f-gammai)

---


<a name="f-prob-f"></a>
## prob-f

```
syntax: (prob-f num-f int-df1 int-df2)
```

Description:

Returns the probability that an observed F statistic
num-f is equal to or greater than the given value
under the null hypothesis.

int-df1 and int-df2 specify the numerator and
denominator degrees of freedom.

Examples:

```
(prob-f 2.75 10 12)
;-> 0.0501990804
```

Notes:

- The returned value is a probability in the range
  0.0 to 1.0.
- Larger F values yield smaller probabilities for
  the same degrees of freedom.

See: [crit-f](#f-crit-f)

---


<a name="f-prob-t"></a>
## prob-t

```
syntax: (prob-t num-t int-df)
```

Description:

Returns the probability that an observed Student’s
t statistic num-t is equal to or greater than the
given value under the null hypothesis, using
int-df degrees of freedom.

Examples:

```
(prob-t 1.76 14)
;-> 0.05011454551
```

Notes:

- The returned value is a probability in the range
  0.0 to 1.0.
- Larger absolute t values yield smaller
  probabilities for the same degrees of freedom.

See: [crit-t](#f-crit-t)

---


<a name="f-prob-z"></a>
## prob-z

```
syntax: (prob-z num-z)
```

Description:

Returns the probability that a normally distributed
random variable with mean 0.0 and standard deviation
1.0 does not exceed the observed value num-z.

This corresponds to the cumulative distribution
function (CDF) of the standard normal distribution.

Examples:

```
(prob-z 0.0)
;-> 0.5
```

Notes:

- The returned value is a probability in the range
  0.0 to 1.0.
- Larger positive z values yield probabilities
  closer to 1.0, while negative values yield
  probabilities closer to 0.0.

See: [crit-z](#f-crit-z)

---


<a name="f-process"></a>
## process

```
syntax: (process str-command)
syntax: (process str-command int-pipe-in int-pipe-out)
syntax: (process str-command int-pipe-in int-pipe-out int-pipe-error)
```

Description:

Starts a new process executing the command specified
by str-command and returns its process identifier.

If the process cannot be created, process returns
nil.

The new process inherits the execution environment
of the parent process.

Command arguments are parsed from str-command using
spaces. Arguments containing spaces must be quoted
explicitly in the command string.

The returned process identifier may be used with
destroy to terminate the process if it does not
exit on its own.

Standard input, output, and error streams of the
child process may be redirected using pipe handles.

Examples:

```
(process "/usr/bin/true")
;-> 12345
```

Redirecting standard input and output using pipes:

```
(map with '(in out) (pipe))
(map with '(cin cout) (pipe))

(process "/usr/bin/bc" cin out)
;-> 7916

(write-line cout "3 + 4")
(read-line in)
;-> "7"

(destroy 7916)
```

Using an additional pipe for standard error:

```
(map with '(in out) (pipe))
(map with '(cin cout) (pipe))
(map with '(errin errout) (pipe))

(process "/usr/bin/bc" cin out errout)

(write-line cout "invalid")
```

Waiting for output or error data:

```
(while (and (= (peek in) 0)
            (= (peek errin) 0))
  (sleep 10))

(if (> (peek errin) 0)
  (println (read-line errin)))

(if (> (peek in) 0)
  (println (read-line in)))
```

If a stream is not required, specify 0 for the
corresponding pipe handle:

```
(process "app" 0 appout)
```

Notes:

- process starts the command asynchronously.
- Standard streams may be redirected independently.
- peek may be used to poll pipe handles.
- Not all programs support full interactive I/O
  redirection.

See: [pipe](#f-pipe), [peek](#f-peek),
[destroy](#f-destroy), [write](#f-write),
[write-line](#f-write-line), [read](#f-read),
[read-line](#f-read-line)

---


<a name="f-prompt-event"></a>
## prompt-event

```
syntax: (prompt-event sym-event-handler | func-event-handler)
syntax: (prompt-event nil)
```

Description:

Customizes the interactive prompt shown by the
interpreter.

The argument may be the symbol of a user-defined
function or an anonymous function. This function
is called each time the prompt is displayed.

To restore the default prompt, call prompt-event
with nil.

The current context is passed as the single
argument to the prompt handler function.

The handler must return a string of at most
63 characters. If no string is returned, the
prompt remains unchanged.

Examples:

```
(prompt-event (fn (ctx)
  (string ctx ":" (real-path) "$ ")))
```

After this call, the prompt shows the current
context, a colon, the current directory, and
a dollar sign.

Notes:

- The prompt handler is evaluated before each
  interactive input.
- The return value must be a string with a
  maximum length of 63 characters.
- Returning a non-string leaves the prompt
  unchanged.
- Use (prompt-event nil) to reset the prompt.

prompt-event can be combined with command-event
to build customized interactive shells or command
interpreters.

See: [command-event](#f-command-event),
[real-path](#f-real-path)

---


<a name="f-protectedp"></a>
## protected?

```
syntax: (protected? sym)
```

Description:

Checks whether the symbol sym is protected.

Protected symbols include all built-in functions,
context symbols, and symbols made constant using
the constant function.

If sym is protected, protected? returns true;
otherwise, it returns nil.

Examples:

```
(protected? 'println)
;-> true

(constant 'var 123)

(protected? 'var)
;-> true
```

Notes:

- Protected symbols cannot be redefined or modified.
- Use constant to explicitly protect a symbol.

See: [constant](#f-constant)

---


<a name="f-push"></a>
## push

```
syntax: (push exp list [int-index-1 [int-index-2 ... ]])
syntax: (push exp list [list-indexes])

syntax: (push str-1 str-2 [int-index])
```

Description:

Inserts the value of exp into a list or string.

In the list forms, push inserts exp into the list
referenced by list. When no index is given, the
value is inserted at index 0. When one or more
indices are provided, they are used to access a
nested list structure.

Indices may also be supplied as a list in
list-indexes. Improper indices are ignored.

push modifies the target list in place and returns
a reference to the modified list.

If list evaluates to nil, it is initialized to an
empty list before insertion.

Repeatedly inserting at index -1 is optimized and
can be used to efficiently append elements.

In the string form, push inserts characters into
the string. Indices refer to character positions,
not byte positions. UTF-8 characters are handled
as single characters.

Examples:

```
; inserting at the front
(set lst '(b c))
(push 'a lst)
;-> (a b c)

lst
;-> (a b c)

; insert at index
(push "hello" lst 2)
;-> (a b "hello" c)

; optimized appending at the end
(push 'z lst -1)
;-> (a b "hello" c z)

; inserting lists into lists
(push '(f g) lst)
;-> ((f g) a b "hello" c z)

; inserting at a negative index
(push 'x lst -3)
;-> ((f g) a b "hello" x c z)

; using multiple indices
(push 'h lst 0 -1)
;-> ((f g h) a b "hello" x c z)

; using indices in a list
;----------------------------------------------------------
(set lst '(a b (c d () e)))

(push 'x lst '(2 2 0))
;-> (a b (c d (x) e))

(ref 'x lst)
;-> (2 2 0)

(pop lst '(2 2 0))
;-> x

; place reference
;----------------------------------------------------------
(set lst '((a 1) (b 2) (c 3) (d)))

(push 4 (assoc 'd lst) -1)
;-> (d 4)

lst
;-> ((a 1) (b 2) (c 3) (d 4))

; push on uninitialized symbol
;----------------------------------------------------------
(var)
;-> nil

(push 999 var)
;-> (999)

var
;-> (999)

; using push and pop as a queue
;----------------------------------------------------------
(set q '(a b c d e))

(pop (push 'f q -1))
;-> a

(pop (push 'g q -1))
;-> b

q
;-> (c d e f g)

; push on strings (UTF-8 aware)
;----------------------------------------------------------
(set str "abcdefg")

(push "hijk" str -1)
;-> "abcdefghijk"

str
;-> "abcdefghijk"

(push "123" str)
;-> "123abcdefghijk"

(push "4" str 3)
;-> "1234abcdefghijk"

(set str "\u03b1\u03b2\u03b3")
;-> "αβγ"

(push "*" str 1)
;-> "α*βγ"

; push on a string reference
;----------------------------------------------------------
(set lst '("abc" "xyz"))

(push "x" (lst 0))
;-> "xabc"

lst
;-> ("xabc" "xyz")
```

Notes:

- push modifies the target list or string.
- Negative indices count from the end.
- Repeated appends using index -1 are optimized.
- push returns a reference to the modified value.

See: [pop](#f-pop), [ref](#f-ref), [ref-all](#f-ref-all)

---


<a name="f-put-url"></a>
## put-url

```
syntax: (put-url str-url str-content [str-option] [int-timeout [str-header]])
```

Description:

Sends an HTTP PUT request to the URL specified by
str-url, transferring the content in str-content
to the target resource.

HTTP PUT is typically used to upload or replace
the contents of a resource on a server. The server
must be configured to accept PUT requests.

If str-url starts with file://, the content is
written to the local file system.

On success, put-url returns the response body
returned by the server. On error, it returns a
string starting with ERR:.

An optional timeout may be specified in
int-timeout, given in milliseconds. If no response
is received before the timeout expires, the string
ERR: timeout is returned.

Examples:

```
; upload text to a remote resource
(put-url "https://example.com/data.txt" "Hello world")

; upload text with timeout
(put-url "https://example.com/data.txt" "Hello world" nil 2000)

; upload file contents
(put-url "https://example.com/page.html"
         (read-file "page.html"))

; write to local file system
(put-url "file:///home/user/file.txt" "Hello world")
```

Notes:

- The server must explicitly allow HTTP PUT.
- put-url replaces the target resource content.
- int-timeout is specified in milliseconds.
- On error, a string starting with ERR: is returned.

Options:

The optional str-option accepts the same options
as get-url for handling returned content. When a
timeout is specified, a custom HTTP header string
may also be supplied.

See: [get-url](#f-get-url), [post-url](#f-post-url)

---


<a name="f-pv"></a>
## pv

```
syntax: (pv num-int num-nper num-pmt [num-fv [int-type]])
```

Description:

Calculates the present value of a loan or investment
with a constant interest rate and constant payments.

num-int specifies the interest rate per period.
num-nper is the total number of payment periods.
num-pmt is the payment made each period.

The optional num-fv specifies the future value at
the end of all periods and defaults to 0.0.

When payments are made at the end of each period,
int-type is 0 or omitted. When payments are made at
the beginning of each period, int-type is 1.

The returned value is typically negative, indicating
an initial outgoing amount.

Examples:

```
(pv (div 0.07 12) 240 775.30)
;-> -100000.1373
```

This example computes the present value of a loan
paid off over 240 periods with constant payments
of 775.30 at a yearly interest rate of 7 percent.

Notes:

- Interest rate is specified per period.
- A negative result represents an initial payment.
- pv returns a floating point value.

See: [fv](#f-fv), [irr](#f-irr), [nper](#f-nper),
[npv](#f-npv), [pmt](#f-pmt)

---


<a name="f-quote"></a>
## quote

```
syntax: (quote exp)
```

Description:

Returns exp without evaluating it.

The same effect can be achieved by prepending a
single quote character ' to exp.

The function quote is resolved at runtime, while
the prefixed ' form is translated during code
reading into a quoted expression.

Examples:

```
(quote x)
;-> x

(quote 123)
;-> 123

(quote (a b c))
;-> (a b c)

(= (quote x) 'x)
;-> true
```

Notes:

- quote prevents evaluation of its argument.
- The ' shorthand produces the same result.
- The ' form is handled during code translation,
  while quote is a runtime function.

See: [eval](#f-eval), [list](#f-list)

---


<a name="f-rand"></a>
## rand

```
syntax: (rand int-range [int-N])
```

Description:

Generates random integer numbers using the internal
pseudo-random number generator.

rand returns a random integer in the range from
0 (zero) to int-range minus 1.

When int-range is 0 (zero), the random number
generator is initialized using the current time
value.

When the optional int-N parameter is specified,
rand returns a list of int-N random integers.

Examples:

```
(repeat (x 100)
  (print (rand 2)))
;-> 11100000110100111100111101...

(rand 3 100)
;-> (2 0 1 1 2 0 ...)
```

The first example prints a stream of equally
distributed 0 and 1 values. The second example
returns a list of 100 integers in the range
0 to 2.

Notes:

- rand generates integer values only.
- Use random or normal for floating point
  random numbers.
- Use seed to explicitly set the random seed.

See: [random](#f-random), [normal](#f-normal),
[seed](#f-seed)

---


<a name="f-random"></a>
## random

```
syntax: (random float-offset float-scale int-n)
syntax: (random float-offset float-scale)
```

Description:

Generates evenly distributed floating point random
numbers using the internal pseudo-random number
generator.

In the first form, random returns a list of int-n
floating point numbers. Each value is scaled by
float-scale and shifted by float-offset.

In the second form, random returns a single floating
point value scaled and shifted in the same way.

The sequence produced by random can be made
reproducible by initializing the generator with
seed.

Examples:

```
(random 0 1 10)
;-> (0.10898973 0.69823783 0.56434872 0.041507289 0.16516733
;    0.81540917 0.68553784 0.76471068 0.82314585 0.95924564)

(random 10 5)
;-> 11.0971
```

When no parameters are given, random assumes a
default offset of 0.0 and a scale of 1.0.

Notes:

- random generates floating point values.
- Values are evenly distributed.
- Use seed to control reproducibility.
- For normally distributed values, use normal.
- For integer random values, use rand.

See: [rand](#f-rand), [normal](#f-normal),
[seed](#f-seed)

---


<a name="f-randomize"></a>
## randomize

```
syntax: (randomize list [bool])
```

Description:

Rearranges the elements of list into a random order and
returns the resulting list.

By default, randomize guarantees that the returned
sequence differs from the previous result produced for
the same list length. To enforce this, the function may
internally generate multiple candidate permutations
until a different ordering is obtained. As a result,
execution time may vary between calls even when the
input list length is identical.

When the optional bool argument evaluates to not nil,
the result is allowed to be identical to the input
ordering. This disables the guarantee of producing a
different sequence and avoids the extra work required
to reject equal permutations.

randomize operates on lists only and does not modify
the original list.

The function relies on an internal pseudo-random number
generator that produces the same sequence of values
each time Rebel is started. Use the seed function to
initialize the generator with a different starting
value.

Examples:

```
(randomize '(a b c d e f g))
;-> (b a c g d e f)

(randomize (sequence 1 5))
;-> (3 5 4 1 2)

(randomize '(a b c d) true)
;-> (a b c d)
```

Notes:

- Without the optional bool argument, randomize will
  never return the same ordering twice in succession
  for lists of the same length.
- Allowing identical output may improve performance
  when repeated randomization is required.

See: [seed](#f-seed), [sequence](#f-sequence)

---


<a name="f-read"></a>
## read

```
syntax: (read int-file sym-buffer int-size [str-wait])
```

Description:

Reads up to int-size bytes from the file referenced by
int-file and stores the data into the symbol sym-buffer.

Any previous content referenced by sym-buffer is deleted
before reading. After the operation, sym-buffer contains
a string with the data read or nil if no new data was
available.

The file handle in int-file must originate from a
previous open call. The file position is advanced by
the number of bytes read.

sym-buffer may also be a default functor supplied by a
context symbol, allowing reference-style input and
output in user-defined functions.

read is a short form of read-buffer. The longer form
is deprecated and should not be used in new code.

When the optional str-wait argument is specified, read
stops early if the given string is encountered in the
input. The wait string is included in the returned
data. The string must not contain binary zero bytes.

If str-wait is specified and not found within the
int-size limit, read returns nil. In all cases, any
bytes read are still placed into sym-buffer.

Examples:

```
(set handle (open "file.txt" "read"))

(read handle buf 200)
;-> 200

buf
;-> "..."

(read handle buf 1000 "password:")
;-> 143

buf
;-> "...password:"
```

Notes:

- sym-buffer is always updated when data is read,
  even if the function returns nil.
- When no new bytes are read, sym-buffer contains nil.
- Binary zero bytes are not allowed in str-wait.

See: [write](#f-write), [seek](#f-seek), [open](#f-open)

---


<a name="f-read-char"></a>
## read-char

```
syntax: (read-char [int-file])
```

Description:

Reads a single byte from the file referenced by
int-file or from the current I/O device when no
file handle is specified.

The file handle in int-file must originate from a
previous open call. Each invocation of read-char
advances the file position by one byte.

When the end of the file or device is reached,
read-char returns nil.

read-char is intended for byte-wise processing.
For line-oriented input, use read-line together
with device.

Examples:

```
(define (copy-bytes from-file to-file)
  (set in-file (open from-file "read"))
  (set out-file (open to-file "write"))
  (while (set chr (read-char in-file))
    (write-char out-file chr))
  (close in-file)
  (close out-file)
  "finished")
```

Notes:

- read-char always returns a number representing
  the byte value, or nil at end of input.
- The file pointer is advanced by one byte on
  each successful read.

See: [write-char](#f-write-char), [read-line](#f-read-line),
[device](#f-device), [copy-file](#f-copy-file)

---


<a name="f-read-expr"></a>
## read-expr

```
syntax: (read-expr str-source [sym-context [exp-error [int-offset]]])
```

Description:

Parses the first expression found in str-source and
returns the translated expression without evaluating
it.

The optional sym-context argument specifies the context
in which the expression is translated. This controls
symbol resolution during parsing but does not trigger
evaluation.

After a successful call, the system variable $count
contains the number of characters consumed from
str-source.

If an error occurs while translating the source text,
the expression in exp-error is evaluated and its result
is returned instead.

The optional int-offset argument specifies the starting
position inside str-source. This is typically used when
calling read-expr repeatedly, advancing the offset by
the value stored in $count.

read-expr behaves like eval-string without executing
the resulting expression.

Examples:

```
(set code "; a statement
(define (double x) (+ x x))")

(read-expr code)
;-> (define (double x) (+ x x))

$count
;-> 41

(read-expr "(+ 3 4)")
;-> (+ 3 4)

(eval-string "(+ 3 4)")
;-> 7
```

Notes:

- read-expr never evaluates the returned expression.
- $count always reflects the number of characters
  processed during the last call.
- int-offset allows incremental parsing of a larger
  source string.

See: [eval-string](#f-eval-string), [reader-event](#f-reader-event)

---


<a name="f-read-file"></a>
## read-file

```
syntax: (read-file str-file-name)
```

Description:

Reads the entire contents of the file specified by
str-file-name and returns it as a string.

On failure, read-file returns nil. When operating on
local files, error details can be retrieved using
sys-error. When used with network resources, additional
information is available through net-error.

The function reads the file in one operation and does
not modify the file position of any open handle.

read-file also accepts URLs. When str-file-name starts
with http:// or file://, the function behaves like
get-url and supports the same optional parameters.

Examples:

```
(write-file "file.enc"
  (encrypt (read-file "file.txt") "secret"))
```

The file file.txt is read, encrypted using the password
"secret", and written to a new file named file.enc.

```
(read-file "http://example.com/file.tgz" 10000)
```

The remote file file.tgz is retrieved via HTTP. The
transfer times out after 10 seconds if not completed.

Notes:

- read-file always returns a string or nil.
- The entire file is loaded into memory at once.
- URL handling follows the same rules as get-url.

See: [write-file](#f-write-file), [append-file](#f-append-file),
[get-url](#f-get-url)

---


<a name="f-read-key"></a>
## read-key

```
syntax: (read-key [true])
```

Description:

Reads a key from the keyboard and returns its numeric
value.

For keys representing ASCII characters, the returned
value corresponds to the ASCII code. For navigation
keys and control sequences, multiple read-key calls may
be required to read the complete sequence.

When the optional true flag is specified, read-key
operates in non-blocking mode and returns 0 when no key
is available. Without this flag, the call blocks until
a key is pressed.

read-key reads from standard input and requires an
interactive terminal.

Examples:

```
(read-key)
;-> 97    ; after pressing the A key

(read-key)
;-> 65    ; after pressing shifted A

(read-key true)
;-> 0     ; when no key has been pressed

(while (!= (set c (read-key)) 1)
  (println c))
```

The loop above can be used to inspect key and control
sequences. To terminate the loop, press Ctrl-A.

Notes:

- ASCII character keys return consistent values.
- Navigation and function keys may emit multi-byte
  sequences and require multiple reads.
- Non-blocking mode returns 0 when no input is present.

See: [read-char](#f-read-char), [device](#f-device)

---


<a name="f-read-line"></a>
## read-line

```
syntax: (read-line [int-file])
```

Description:

Reads a line of text terminated by a line-feed
character (ASCII 10) and returns it as a string.

The terminating line-feed is not included in the
returned value. A carriage return (ASCII 13) breaks
the line only when followed by a line-feed, in which
case both characters are discarded. A lone carriage
return breaks the line only if it is the final
character in the input stream.

By default, input is read from the current I/O device.
A different device can be selected using device, or a
file handle obtained from open may be supplied in
int-file.

There is no intrinsic limit on line length when
reading from files or pipes. When reading from
standard input interactively, line length is limited
and input is optimized for speed.

The contents of the most recent read-line operation
are stored internally and can be retrieved using
current-line.

When input is exhausted, read-line returns nil.

Examples:

```
(print "Enter a num:")
(set num (int (read-line)))
```

```
(set in-file (open "file.txt" "read"))
(while (read-line in-file)
  (write-line))
(close in-file)
```

In the second example, write-line is called without
arguments and outputs the contents of the internal
line buffer filled by the last read-line call.

Notes:

- The returned string never includes line terminators.
- current-line provides access to the last line read.
- nil signals end of input.

See: [current-line](#f-current-line), [write-line](#f-write-line),
[device](#f-device), [open](#f-open)

---


<a name="f-read-utf8"></a>
## read-utf8

```
syntax: (read-utf8 int-file)
```

Description:

Reads a single UTF-8 encoded character from the file
referenced by int-file and returns its Unicode code
point as an integer.

The file handle in int-file must originate from a
previous open call. Each invocation of read-utf8
advances the file position by the number of bytes
used by the UTF-8 character.

When the end of the file is reached, read-utf8
returns nil.

The returned integer value can be converted to a
displayable UTF-8 character string using char.

Examples:

```
(set in-file (open "utf8.txt" "read"))
(while (set chr (read-utf8 in-file))
  (print (char chr)))
(close in-file)
```

The example reads UTF-8 encoded text from a file and
writes it to the terminal.

Notes:

- read-utf8 reads logical characters, not bytes.
- The file position advances by the UTF-8 byte length.
- nil signals end of input.

See: [char](#f-char), [read-char](#f-read-char), [open](#f-open)

---


<a name="f-reader-event"></a>
## reader-event

```
syntax: (reader-event [sym-event-handler | func-event-handler])
syntax: (reader-event nil)
```

Description:

Installs or removes a reader event handler that is
invoked between the parsing and evaluation stages of
expression processing.

When a handler is set, it is called after an expression
has been read and translated, but before it is
evaluated. The handler receives the translated
expression and may return a modified one, which is then
used for evaluation.

Specifying nil removes the handler and restores the
default behavior.

The handler may be specified as a symbol referring to a
function or as an inline function value.

reader-event affects expressions read by load and
eval-string.

Examples:

```
(reader-event (fn (ex)
  (print " => " ex)
  ex))

(+ 1 2 3)
; prints:  => (+ 1 2 3)
;-> 6
```

The example installs a handler that traces expressions
before evaluation while leaving them unchanged.

Notes:

- The handler runs after parsing and before evaluation.
- Returning the original expression preserves behavior.
- Returning a modified expression alters evaluation.
- Setting reader-event to nil disables interception.

See: [read-expr](#f-read-expr), [eval-string](#f-eval-string),
[load](#f-load)

---


<a name="f-real-path"></a>
## real-path

```
syntax: (real-path [str-path])
syntax: (real-path str-exec-name true)
```

Description:

Returns an absolute path resolved from a relative
filesystem path or from an executable name.

In the first form, real-path resolves str-path to its
absolute form. When str-path is omitted, the current
directory "." is used.

In the second form, real-path resolves the full path
of an executable specified by str-exec-name. The
lookup is performed using the search path defined
by the environment.

If resolution fails, nil is returned.

The returned path length is limited by the maximum
path length supported by the system.

Examples:

```
(real-path)
;-> "/home/user"

(real-path "./file.txt")
;-> "/home/user/file.txt"

(real-path "make" true)
;-> "/usr/bin/make"
```

Notes:

- Relative paths are resolved to absolute paths.
- Executable lookup uses the environment search path.
- nil indicates that the path could not be resolved.

See: [env](#f-env), [which](#f-which)

---


<a name="f-receive"></a>
## receive

```
syntax: (receive int-pid sym-message)
syntax: (receive)
```

Description:

Receives messages sent from child processes created
with spawn.

In the first form, receive reads one message from the
message queue of the child process identified by
int-pid. The received message replaces the contents
of sym-message.

Each call reads at most one message. When the message
queue is empty, receive returns nil.

Examples:

```
; sending process
(send spid "hello")
;-> true

; receiving process
(receive pid msg)
;-> true

msg
;-> "hello"
```

To wait until a message arrives, receive can be used
in a loop:

```
(until (receive pid msg))
```

In the second form, receive returns a list of process
IDs for all child processes that currently have
pending messages for the parent process.

Examples:

```
(dolist (pid (receive))
  (receive pid msg)
  (println "received message: " msg " from:" pid))
```

The list returned by (receive) contains only process
IDs with unread messages. Calling (receive pid msg)
for these processes is guaranteed to succeed without
blocking.

Notes:

- Each receive call retrieves a single queued message.
- nil indicates that no message is available.
- Message queues are maintained per child process.

See: [send](#f-send)

---



<a name="f-ref"></a>
## ref

```
syntax: (ref exp-key list [func-compare [true]])
```

Description:

Searches for the expression exp-key in list and
returns an index vector identifying the position
of the first matching element in a nested list.

If no match is found, ref returns nil.

By default, ref compares expressions using equality.
When func-compare is supplied, it is used as a
comparison function. The comparison function always
receives two arguments, even if only one is used.

When the optional true argument is present, the
matched element itself is returned instead of the
index vector.

The index vector returned by ref can be used directly
to index into the original list.

Examples:

```
(set lst '(a b (c d (x) e)))

(ref 'x lst)
;-> (2 2 0)

(ref '(x) lst)
;-> (2 2)

(set p '(c d (x) e))
(ref p lst)
;-> (2)

(set v (ref '(x) lst))
(lst v)
;-> (x)

(ref 'foo lst)
;-> nil
```

Using comparison functions:

```
(set lst '(a b (c d (e) f)))

(ref 'e lst)
;-> (2 2 0)

(ref 'e lst =)
;-> (2 2 0)

(ref 'e lst >)
;-> (0)

(ref 'e lst > true)
;-> a
```

Using an anonymous comparison function:

```
(ref 'e lst (fn (x y) (or (= x y) (= y 'd))))
;-> (2 1)

(ref 'e lst (fn (x y) (or (= x y) (= y 'd))) true)
;-> d
```

Using match and unify as comparison functions:

```
(set lst '((l 3) (a 12) (k 5) (a 10) (z 22)))

(ref '(a ?) lst match)
;-> (1)

(set lst '(((a b) (c d)) ((e e) (f g))))

(ref '(X X) lst unify)
;-> (1 0)

(ref '(X g) lst unify)
;-> (1 1)

(ref '(X g) lst unify true)
;-> (f g)
```

Passing the list by reference using a context:

```
(set ctx:ctx '(a b (c d) e f))

(ref 'd ctx)
;-> (2 1)
```

Notes:

- ref returns the first match only.
- The returned index vector can be used directly
  for list indexing.
- Quoted patterns are required when using unify
  or match to prevent evaluation.

See: [ref-all](#f-ref-all), [match](#f-match), [unify](#f-unify)

---


<a name="f-ref-all"></a>
## ref-all

```
syntax: (ref-all exp-key list [func-compare [true]])
```

Description:

Works like ref, but returns a list of all index
vectors found for exp-key in list.

When no match is found, an empty list is returned.

By default, ref-all compares expressions using
equality. When func-compare is supplied, it is used
as a comparison function. The comparison function
always receives two arguments, even if only one is
used.

When the optional true argument is present, the
matched elements are returned instead of the index
vectors.

After execution, the system variable $count contains
the number of matches found.

Examples:

```
; basic usage
;----------------------------------------------------------
(set lst '(a b c (d a f (a h a)) (k a (m n a) (x))))

(ref-all 'a lst)
;-> ((0) (3 1) (3 3 0) (3 3 2) (4 1) (4 2 2))

$count
;-> 6

(lst '(3 1))
;-> a

(map 'lst (ref-all 'a lst))
;-> (a a a a a a)

; comparison functions
;----------------------------------------------------------
(set lst '(a b c (d f (h l a)) (k a (m n) (x))))

(ref-all 'c lst)
;-> ((2))

(ref-all 'c lst =)
;-> ((2))

(ref-all 'c lst >)
;-> ((0) (1) (3 2 2) (4 1))

(ref-all 'c lst > true)
;-> (a b a a)

; Using an anonymous comparison function:
;----------------------------------------------------------
(ref-all 'a lst (fn (x y) (or (= x y) (= y 'k))))
;-> ((0) (3 2 2) (4 0) (4 1))

; comparison ignoring the key
;----------------------------------------------------------
(ref-all nil lst (fn (x y) (> (length y) 2)))
;-> ((3) (3 2) (4))

; named comparison functions
;----------------------------------------------------------
(define (is-long? x y)
  (> (length y) 2))

(ref-all nil lst is-long?)
;-> ((3) (3 2) (4))

(define (is-it-or-d x y)
  (or (= x y) (= y 'd)))

(set lst '(a b (c d (e) f)))

(ref-all 'e lst is-it-or-d)
;-> ((2 1) (2 2 0))

; using match and unify
;----------------------------------------------------------
(set lst '((l 3) (a 12) (k 5) (a 10) (z 22)))

(ref-all '(a ?) lst match)
;-> ((1) (3))

(ref-all '(a ?) lst match true)
;-> ((a 12) (a 10))

(set lst '(((a b) (c d)) ((e e) (f g)) ((z) (z))))

(ref-all '(X X) lst unify)
;-> ((1 0) (2))

(ref-all '(X X) lst unify true)
;-> ((e e) ((z) (z)))

(set lst '(((x y z) g) ((a b) (c d)) ((e e) (f g))))

(ref-all '(X g) lst unify)
;-> ((0) (2 1))

(ref-all '(X g) lst unify true)
;-> (((x y z) g) (f g))
```

Notes:

- ref-all returns all matches, not just the first.
- The returned index vectors can be used directly
  for list indexing.
- Quoted patterns are required when using unify
  or match to prevent evaluation.

See: [ref](#f-ref)

---


<a name="f-regex"></a>
## regex

```
syntax: (regex str-pattern str-text [regex-option [int-offset]])
```

Description:

Performs a Perl Compatible Regular Expression (PCRE)
search on str-text using the pattern in str-pattern.

regex returns a list containing the matched string,
followed by the offset and length of the match, and
optionally any captured subexpressions with their
offsets and lengths. If no match is found, nil is
returned.

The optional regex-option argument controls matching
behavior. Options can be specified as numbers or as
letters in a string. Multiple options may be combined.

The optional int-offset argument specifies the starting
position in str-text where matching begins.

After a successful match, the system variables $0,
$1, $2, ... contain the full match and captured
subexpressions. These variables are not reset when
no match is found.

When no UTF-8 option is specified, offsets and lengths
are reported in bytes. When PCRE_UTF8 is enabled,
offsets and lengths are reported in UTF-8 characters.

Examples:

```
; basic matching
;----------------------------------------------------------
(regex "b+" "aaaabbbaaaa")
;-> ("bbb" 4 3)

(regex "b+" "AAAABBBAAAA" 1)
;-> ("BBB" 4 3)

(regex "b+" "AAAABBBAAAA" "i")
;-> ("BBB" 4 3)

(regex "[bB]+" "AAAABbBAAAA")
;-> ("BbB" 4 3)

; subexpressions
;----------------------------------------------------------
(regex "http://(.*):(.*)" "http://example.com:80")
;-> ("http://example.com:80" 0 22 "example.com" 7 11 "80" 19 2)

$0
;-> "http://example.com:80"

$1
;-> "example.com"

$2
;-> "80"

; escaping rules
;----------------------------------------------------------
(regex "\\(abc\\)" "xyz(abc)xyz")
;-> ("(abc)" 3 5)

(regex "\\d{1,3}" "qwerty567asdfg")
;-> ("567" 6 3)

(regex "\"" "abc\"def")
;-> ("\"" 3 1)

(regex {\(abc\)} "xyz(abc)xyz")
;-> ("(abc)" 3 5)

(regex {"} "abc\"def")
;-> ("\"" 3 1)

(regex [text]\(abc\)[/text] "xyz(abc)xyz")
;-> ("(abc)" 3 5)

; balanced brackets
;----------------------------------------------------------
(regex {\d{1,3}} "qwerty567asdfg")
;-> ("567" 6 3)
```

Regex options:

```
no        letter    description
--------  --------  -----------------------------------------
1         i         case-insensitive matching
2         m         multiline mode
4         s         dot matches newline
8         x         ignore whitespace in pattern
16        A         anchor at start of string
32        D         $ matches end of string only
64                  extra functionality (unused)
128                 ^ does not match start of string
256                 $ does not match end of string
512       U         ungreedy quantifiers
1024                empty string considered invalid
2048      u         UTF-8 mode
0x8000              replace once (replace only)
0x10000   p         precompiled pattern
```

Notes:

- Captured subexpressions are returned in order.
- Offsets and lengths are useful for incremental
  processing.
- Regular expression syntax follows PCRE rules.

See: [regex-comp](#f-regex-comp), [find](#f-find),
[find-all](#f-find-all), [replace](#f-replace),
[search](#f-search)

---


<a name="f-regex-comp"></a>
## regex-comp

```
syntax: (regex-comp str-pattern [int-option])
```

Description:

Pre-compiles a regular expression pattern for reuse
in subsequent regular expression operations.

Rebel automatically compiles regular expression
patterns and caches the most recent compilation.
This is sufficient when the same pattern is used
repeatedly. When multiple different patterns are
used in a loop, the cache is not effective.

regex-comp allows patterns to be compiled once and
reused efficiently. The compiled pattern is returned
and can be passed to functions that accept regular
expressions.

Normal pattern options are specified during
pre-compilation using int-option. When a pre-compiled
pattern is later used, the option value 0x10000 must
be supplied to signal that the pattern is already
compiled.

The option 0x10000 may only be combined with 0x8000,
which limits replace to a single substitution.

Examples:

```
; slower without pre-compilation
;----------------------------------------------------------
(dolist (line page)
  (replace pattern-str1 line repl1 0)
  (replace pattern-str2 line repl2 512))

; faster with pre-compilation
;----------------------------------------------------------
(set p1 (regex-comp pattern-str1))
(set p2 (regex-comp pattern-str2 512))

(dolist (line page)
  (replace p1 line repl1 0x10000)
  (replace p2 line repl2 0x10000))
```

Notes:

- Regular expression options are identical to those
  used by the [regex](#f-regex) function.
- Pre-compiled patterns improve performance when
  multiple different patterns are reused.
- The option 0x10000 signals use of a compiled pattern.
- The option 0x10000 can only be combined with 0x8000.
- The function ends-with must not be used with
  compiled patterns.

See: [regex](#f-regex), [replace](#f-replace),
[ends-with](#f-ends-with)

---


<a name="f-remove-dir"></a>
## remove-dir

```
syntax: (remove-dir str-path)
```

Description:

Removes the directory specified by str-path.

The directory must be empty for remove-dir to
succeed. If the directory cannot be removed,
nil is returned.

Examples:

```
(remove-dir "tmp")
;-> true
```

Notes:

- The directory must be empty.
- The operation affects the filesystem immediately.
- nil indicates failure.

---


<a name="f-rename-file"></a>
## rename-file

```
syntax: (rename-file str-path-old str-path-new)
```

Description:

Renames a file or directory entry from str-path-old
to str-path-new.

If the operation succeeds, true is returned.
On failure, rename-file returns nil.

Examples:

```
(rename-file "data.txt" "data.backup")
;-> true
```

Notes:

- Both files and directories can be renamed.
- The target path must not violate filesystem rules.
- nil indicates that the rename operation failed.

---


<a name="f-repeat"></a>
## repeat

```
syntax: (repeat (sym-var int-count [exp-break]) body)
```

Description:

Executes body int-count times. Before each iteration,
`sym-var` is bound to the current loop index, starting at `0`
and ending at `int-count - 1`. The binding is local to the
loop and follows dynamic scoping rules. The return value
is the last evaluation of body.

If `exp-break` is present, it is evaluated before each
iteration step. When `exp-break` evaluates to a non-nil
value, the loop terminates immediately and returns that
value.

After `repeat` finishes, `sym-var` reverts to its previous
value.

Examples:

```
(repeat (i 10)
  (print i))
;-> 9
; console output:
;   0123456789

; early exit example:
(repeat (i 10 (= i 3))
  (print i))
;-> true
; console output:
;   012
```

Notes:

- `sym-var` is always an integer.
- `exp-break` is checked before body is evaluated.
- The final return value is either the last body result or
  the value of `exp-break` on early termination.

See: [dolist](#f-dolist), [for](#f-for),
[while](#f-while)

---


<a name="f-replace"></a>
## replace

```
syntax: (replace exp-key list exp-replacement [func-compare])
syntax: (replace exp-key list)

syntax: (replace str-key str-data exp-replacement)
syntax: (replace str-pattern str-data exp-replacement regex-option)
```

Description:

Performs replacement operations on lists or strings.

replace is a destructive function. When operating on
lists, it modifies the list in place and also returns
the modified list. When operating on strings, a new
string is returned.

The number of replacements performed is stored in the
system variable $count.

During replacement, the anaphoric system variable
$it is bound to the current element or match being
replaced.

List replacement:

When the second argument is a list, all elements
matching exp-key are replaced with exp-replacement.
If exp-replacement is omitted, all matching elements
are removed from the list.

By default, equality is used for comparison. An
optional func-compare argument may specify a
comparison operator or user-defined function. The
comparison function always receives two arguments.

String replacement:

When the first two arguments are strings, replace
substitutes all occurrences of str-key in str-data
with the evaluated exp-replacement.

Regular expression replacement:

When a fourth argument is present, str-pattern is
interpreted as a regular expression and regex-option
controls the matching behavior. Replacement proceeds
from left to right. By default, all matches are
replaced.

Examples:

```
; list replacement
;----------------------------------------------------------
(set lst '(a b c d e a b c d))

(replace 'b lst 'B)
;-> (a B c d e a B c d)

lst
;-> (a B c d e a B c d)

$count
;-> 2

; list replacement with comparison function
;----------------------------------------------------------
(set lst '(1 4 22 5 6 89 2 3 24))

(replace 10 lst 10 <)
;-> (1 4 10 5 6 10 2 3 10)

$count
;-> 3

(replace 10 lst 10 (fn (x y) (< x y)))
;-> (1 4 10 5 6 10 2 3 10)

; dynamic replacement using $it and $count
;----------------------------------------------------------
(replace 'a '(a b a b a b) (list $count $it) =)
;-> ((1 a) b (2 a) b (3 a) b)

; using match and unify on lists
;----------------------------------------------------------
(set lst '((john 5 6 4) (mary 3 4 7) (bob 4 2 7 9) (jane 3)))

(replace '(mary *) lst
         (list 'mary (apply + (rest $it)))
         match)
;-> ((john 5 6 4) (mary 14) (bob 4 2 7 9) (jane 3))

(set lst '((john 5 6 4) (mary 3 4 7) (bob 4 2 7 9) (jane 3)))

(replace '(*) lst
         (list ($it 0) (apply + (rest $it)))
         match)
;-> ((john 15) (mary 14) (bob 22) (jane 3))

(replace '(X X)
         '((3 10) (2 5) (4 4) (6 7) (8 8))
         (list ($it 0) 'double ($it 1))
         unify)
;-> ((3 10) (2 5) (4 double 4) (6 7) (8 double 8))

; list removal
;----------------------------------------------------------
(set lst '(a b a a c d a f g))

(replace 'a lst)
;-> (b c d f g)

lst
;-> (b c d f g)

$count
;-> 4

; string replacement without regex
;----------------------------------------------------------
(set str "this isa sentence")

(replace "isa" str "is a")
;-> "this is a sentence"

$count
;-> 1

; regular expression replacement
;----------------------------------------------------------
(set str "ZZZZZxZZZZyy")

(replace "x|y" str "PP" 0)
;-> "ZZZZZPPZZZZPPPP"

(set str "---axb---ayb---")

(replace "(a)(.)(b)" str (append $3 $2 $1) 0)
;-> "---bxa---bya---"

(replace "a" "aaa" "X" 0)
;-> "XXX"

(replace "a" "aaa" "X" 0x8000)
;-> "Xaa"

; dynamic regex replacement
;----------------------------------------------------------
(set str "xxx%41xxx%42")

(replace "%([0-9A-F][0-9A-F])" str
         (char (int (append "0x" $1)))
         1)

str
;-> "xxxAxxxB"

$count
;-> 2
```

Notes:

- replace is destructive when operating on lists.
- The system variable $count contains the number of
  replacements performed.
- The anaphoric variable $it refers to the current
  match or element.
- Regular expression options are identical to those
  used by the [regex](#f-regex) function.
- The regex options table is documented only once
  with regex to avoid duplication.

See: [regex](#f-regex), [find](#f-find),
[find-all](#f-find-all), [parse](#f-parse),
[search](#f-search)

---


<a name="f-reset"></a>
## reset

```
syntax: (reset)
syntax: (reset true)
syntax: (reset int-max-cells)
```

Description:

Resets the execution environment.

In the first form, reset returns execution to the
top-level evaluator, disables trace mode, and switches
to the MAIN context. Variable environments are restored
from the saved environments on the stack.

reset raises the error condition
"user reset - no error", which can be intercepted by
user-defined error handlers. It also interrupts
command-line parameter processing and is invoked
automatically after an error condition.

During execution, reset walks the entire cell space,
which may take noticeable time in a heavily loaded
system.

In the second form, reset terminates the current
process and starts a new clean process using the same
command-line parameters.

In the third form, reset changes the maximum number of
cells available to the runtime system. The new limit
is reported by sys-info. Cell memory does not include
string storage referenced by cells.

The minimum allowed cell count is 4095. Values below
this threshold are clamped to 4095. The program exits
when attempting to allocate more cells than permitted.

Examples:

```
(sys-info)
;-> (437 576460752303423488 409 1 0 2048 0 60391 10602 1411)

; allocate approximately 1 MB of cell memory
(reset 32768)
;-> true

(sys-info)
;-> (437 32768 409 1 0 2048 0 60392 10602 1411)
```

Notes:

- reset restores the top-level evaluation state.
- reset is automatically triggered after errors.
- Changing the maximum cell count does not restart
  the system.
- Cell memory is allocated in blocks of 4095 cells.

See: [sys-info](#f-sys-info)

---



<a name="f-rest"></a>
## rest

```
syntax: (rest list)
syntax: (rest array)
syntax: (rest str)
```

Description:

Returns all elements of a list or array except the
first one.

When applied to a string, rest returns a new string
containing all characters except the first character.

For empty lists, rest returns an empty list.

rest operates on character boundaries when UTF-8
support is enabled.

Examples:

```
; list usage
;----------------------------------------------------------
(rest '(1 2 3 4))
;-> (2 3 4)

(rest '((a b) c d))
;-> (c d)

(set lst '(a b c d e))

(rest lst)
;-> (b c d e)

(first (rest lst))
;-> b

(rest (rest lst))
;-> (d e)

(rest (first '((a b) c d)))
;-> (b)

(rest '())
;-> ()

; array usage
;----------------------------------------------------------
(set A (array 2 3 (sequence 1 6)))
;-> ((1 2) (3 4) (5 6))

(rest A)
;-> ((3 4) (5 6))

; string usage
;----------------------------------------------------------
(rest "Rebel")
;-> "ebel"

(first (rest "Rebel"))
;-> "e"
```

Notes:

- rest returns a new value and does not modify the
  original list, array, or string.
- For UTF-8 strings, rest advances by characters,
  not bytes.
- An implicit rest is available for lists; see the
  chapter on implicit rest and slice.

See: [first](#f-first), [last](#f-last)

---


<a name="f-reverse"></a>
## reverse

```
syntax: (reverse list)
syntax: (reverse array)
syntax: (reverse str)
```

Description:

Reverses the order of elements in a list or array,
or the order of characters in a string.

When applied to a list or array, reverse is
destructive: it modifies the original object and
also returns it.

When applied to a string, reverse returns a new
string with characters in reverse order.

For UTF-8 strings, reversing operates on character
boundaries when combined with explode.

Examples:

```
; reverse a list
;----------------------------------------------------------
(set lst '(a b c d e f))

(reverse lst)
;-> (f e d c b a)

lst
;-> (f e d c b a)

; reverse an array
;----------------------------------------------------------
(set arr (array 3 2 '(1 2 3 4 5 6)))
;-> ((1 2) (3 4) (5 6))

(reverse arr)
;-> ((5 6) (3 4) (1 2))

arr
;-> ((5 6) (3 4) (1 2))

; reverse a byte string
;----------------------------------------------------------
(set str "Rebel")

(reverse str)
;-> "lebeR"

str
;-> "lebeR"

; reverse a UTF-8 string
;----------------------------------------------------------
(join (reverse (explode "ΑΒΓΔΕΖΗΘ")))
;-> "ΘΗΖΕΔΓΒΑ"
```

Notes:

- reverse is destructive for lists and arrays.
- Strings are not modified in place.
- For UTF-8 text, use explode to reverse by
  characters instead of bytes.

See: [sort](#f-sort)

---


<a name="f-rotate"></a>
## rotate

```
syntax: (rotate list [int-count])
syntax: (rotate str  [int-count])
```

Description:

Rotates the elements of a list or the bytes of a
string and returns the result.

rotate is destructive: it modifies the original
list or string and also returns it.

When int-count is positive, rotation is performed
to the right. When int-count is negative, rotation
is performed to the left. If int-count is omitted,
a rotation of one position to the right is applied.

Examples:

```
; list rotation
;----------------------------------------------------------
(set lst '(1 2 3 4 5 6 7 8 9))

(rotate lst)
;-> (9 1 2 3 4 5 6 7 8)

(rotate lst 2)
;-> (7 8 9 1 2 3 4 5 6)

lst
;-> (7 8 9 1 2 3 4 5 6)

(rotate lst -3)
;-> (1 2 3 4 5 6 7 8 9)

; string rotation (byte-based)
;----------------------------------------------------------
(set str "Rebel")

(rotate str)
;-> "lRebe"

(rotate str 2)
;-> "elReb"

(rotate str -2)
;-> "Rebel"

; UTF-8 character rotation using explode
;----------------------------------------------------------
(join (rotate (explode "ΑΒΓΔΕΖΗΘ")))
;-> "ΘΑΒΓΔΕΖΗ"
```

Notes:

- rotate modifies the original list or string.
- String rotation operates on byte boundaries.
- For UTF-8 text, use explode to rotate by
  character boundaries.

---


<a name="f-round"></a>
## round

```
syntax: (round number [int-digits])
```

Description:

Rounds number to the precision specified by
int-digits.

When int-digits is positive, rounding is applied
to the integer part of the number. When int-digits
is negative, rounding is applied to decimal places.

If int-digits is omitted, rounding is performed
to zero decimal digits.

Examples:

```
; integer rounding
;----------------------------------------------------------
(round 123.49 2)
;-> 100

(round 123.49 1)
;-> 120

(round 123.49 0)
;-> 123

(round 123.49)
;-> 123

; decimal rounding
;----------------------------------------------------------
(round 123.49 -1)
;-> 123.5

(round 123.49 -2)
;-> 123.49
```

Notes:

- Positive int-digits round the integer magnitude.
- Negative int-digits round fractional digits.
- For formatting numbers for display, use format.

See: [format](#f-format)

---


<a name="f-save"></a>
## save

```
syntax: (save str-file)
syntax: (save str-file sym-1 [sym-2 ... ])
```

Description:

Writes symbols and contexts to a file in textual
form.

In the first form, save writes the entire workspace
to str-file. Loading this file later restores the
workspace to the same state as when save was invoked.

System symbols, built-in function symbols, symbols
starting with the $ character, and symbols containing
nil are not saved automatically.

In the second form, one or more symbols may be
specified. When a symbol is given, only that symbol
is saved. When a context symbol is given, all symbols
contained in that context are saved. Symbols and
contexts may be mixed.

System symbols are saved only when specified
explicitly.

Symbols are serialized using set statements, or
using define / define-macro when the symbol contains
a function or macro definition.

save always serializes symbols as if the current
context were MAIN, ensuring consistent output
regardless of the active context.

On successful completion, save returns true.

Examples:

```
; save entire workspace
;----------------------------------------------------------
(save "workspace.txt")
;-> true

; save a single function
;----------------------------------------------------------
(save "fn.txt" 'fn)

; save a context
;----------------------------------------------------------
(save "ctx.txt" 'ctx:ctx)

; save multiple symbols and contexts
;----------------------------------------------------------
(save "stuff.txt" 'ctx:ctx 'fn '$main-args)

; save to a remote location (HTTP PUT)
;----------------------------------------------------------
(save "http://example.com/source.txt" 'fn)
```

Notes:

- Saving the MAIN context saves all contexts.
- System symbols and $-prefixed symbols are excluded
  unless explicitly listed.
- Symbols incompatible with standard syntax are
  serialized using sym instead of set.
- Output is deterministic and independent of the
  current context.

See: [load](#f-load), [source](#f-source)

---


<a name="f-search"></a>
## search

```
syntax: (search int-file str-search [bool-flag [regex-option]])
```

Description:

Searches a file referenced by int-file for the
string or pattern specified in str-search.

The file handle in int-file must originate from a
previous open call. After the search, the file
position is updated as follows:

- by default, the file pointer is positioned at the
  beginning of the matched string
- when bool-flag evaluates to true, the file pointer
  is positioned at the end of the matched string
- when nothing is found, the file pointer is placed
  at the end of the file and nil is returned

When regex-option is not specified, a fast plain
string search is performed.

When regex-option is specified, str-search is treated
as a regular expression and matching follows PCRE
rules. In this case, captured subexpressions are
stored in the system variables $0 to $15.

On success, search returns the new file position.
On failure, nil is returned.

Examples:

```
; plain string search
;----------------------------------------------------------
(set file (open "file.txt" "read"))

(search file "define")
(print (read-line file) "\n")

(close file)

; regular expression search
;----------------------------------------------------------
(set file (open "program.c" "read"))

(while (search file "#define (.*)" true 0)
  (println $1))

(close file)
```

Notes:

- search operates on open file handles only.
- When regex options are used, match data is
  available in $0 to $15.
- Regular expression options are identical to those
  used by the [regex](#f-regex) function.

See: [regex](#f-regex), [find](#f-find),
[find-all](#f-find-all), [parse](#f-parse),
[replace](#f-replace)

---


<a name="f-seed"></a>
## seed

```
syntax: (seed int-seed)
syntax: (seed int-seed true [int-pre-N])
syntax: (seed)
```

Description:

Initializes the internal random number generator used by
amb, normal, rand, random, and randomize.

When called with a single argument, seed initializes the
generator using the standard C library random source.
All subsequent calls to random-related functions follow
the sequence defined by this generator.

When the second argument evaluates to true, seed switches
to an internal deterministic generator that is independent
of the C library implementation. This generator produces
reproducible sequences across builds and executions when
initialized with the same seed value.

The optional int-pre-N parameter specifies how many random
values are generated internally during initialization.
This advances the generator state before it is used.
When omitted, int-pre-N defaults to 50.

Only the lower 32 bits of int-seed are used to initialize
the generator.

When called without arguments, seed returns the current
internal seed state as an integer. This value can later be
used to restore the generator to the same state.

Examples:

```
(seed 12345)
;-> 12345

(seed (time-of-day))
;-> int

; deterministic generator with reproducible sequence
;------------------------------------------------------------
(seed 123 true)
;-> 123

(random)
;-> 0.2788576787704871

(random)
;-> 0.7610070955758016

(random)
;-> 0.2462553424976092

(random)
;-> 0.8135413573186572

; save current generator state
(set state (seed))
;-> int

(random)
;-> 0.1895924546707387

(random)
;-> 0.4803856511043318

; restore previous generator state
(seed state true 0)
;-> int

(random)
;-> 0.1895924546707387

(random)
;-> 0.4803856511043318
```

Notes:

- Reusing the same seed value produces the same sequence
  of random numbers, which is useful for debugging and
  testing.
- Using a time-based seed ensures a different sequence
  on each program start.

See: [random](#f-random), [rand](#f-rand),
[normal](#f-normal), [pick](#f-pick)

---


<a name="f-self"></a>
## self

```
syntax: (self [int-index ... ])
```

Description:

Accesses the target object of a FOOP method call.
The object referenced by self is automatically set
by the : colon operator when invoking a method.

When called without indices, self refers to the
entire object. One or more int-index arguments may
be used to access individual object members.

Objects accessed through self are mutable. Any
modification performed on self or its indexed
members directly affects the original object.

self is only valid inside a FOOP method. Outside
of a method call, self is not defined.

Examples:

```
(new Class 'Circle)

(define (Circle:move dx dy)
  (inc (self 1) dx)
  (inc (self 2) dy))

(set Circle1 (Circle 1 2 3))

(:move Circle1 10 20)

Circle1
;-> (Circle 11 22 3)

; objects can be anonymous
;------------------------------------------------------------
(set circles '((Circle 1 2 3) (Circle 4 5 6)))

(:move (circles 0) 10 20)
(:move (circles 1) 10 20)

circles
;-> ((Circle 11 22 3) (Circle 14 25 6))
```

Notes:

- self is implicitly bound by the : operator.
- Indexed access using int-index follows standard
  list indexing rules.
- Mutating self modifies the original object, not
  a copy.

See: [:](#f-colon), [new](#f-new), [context](#f-context)

---


<a name="f-seek"></a>
## seek

```
syntax: (seek int-file [int-position])
```

Description:

Sets the file position for the open file identified
by int-file.

When int-position is specified, the file pointer is
moved to that absolute position counted from the
beginning of the file. A value of 0 refers to the
start of the file.

When called without int-position, seek returns the
current file position.

When int-position is -1, the file pointer is moved
to the end of the file.

If int-file is 0, seek returns the number of bytes
written to standard output.

On failure, seek returns nil.

seek may position the file pointer beyond the
current end of the file. Writing at that position
extends the file and fills the gap with zeros.
Such files are sparse files; unused blocks are
not physically allocated on disk.

Examples:

```
(set file (open "file.dat" "read"))
;-> int

(seek file 100)
;-> 100

(seek file)
;-> 100

(seek file -1)        ; seek to end of file
;-> int

(set big (open "large-file" "read"))
(seek big 30000000000)
;-> 30000000000
```

Notes:

- File positions are absolute offsets from the
  beginning of the file.
- seek supports 64-bit file offsets.
- Seeking past EOF does not allocate disk space
  until data is written.

See: [open](#f-open), [close](#f-close),
[read](#f-read), [write](#f-write)

---



<a name="f-select"></a>
## select [utf8]

```
syntax: (select list list-selection)
syntax: (select list [int-index_i ... ])

syntax: (select string list-selection)
syntax: (select string [int-index_i ... ])
```

Description:

Selects one or more elements from a list or one or
more characters from a string using index values.

When operating on a list, select returns a new list
containing the elements located at the indices
specified in list-selection or by the individual
int-index_i arguments.

When operating on a string, select returns a new
string composed of the characters located at the
specified indices.

Indices may be positive or negative. Negative
indices count from the end, where -1 refers to the
last element or character.

Selected elements may be repeated and do not need
to appear in ascending order. Reordering indices
reorders the result accordingly.

Examples:

```
; list selection
;------------------------------------------------------------
(set lst '(a b c d e f g))

(select lst '(0 3 2 5 3))
;-> (a d c f d)

(select lst '(-2 -1 0))
;-> (f g a)

(select lst -2 -1 0)
;-> (f g a)

; string selection (UTF-8 aware)
;------------------------------------------------------------
(set str "abcdefg")

(select str '(0 3 2 5 3))
;-> "adcfd"

(select str '(-2 -1 0))
;-> "fga"

(select str -2 -1 0)
;-> "fga"
```

Notes:

- Negative indices are resolved relative to the end
  of the list or string.
- Duplicate indices produce duplicate elements in
  the result.
- Index order defines result order.
- When applied to strings, indexing is UTF-8 aware.

See: [slice](#f-slice), [nth](#f-nth),
[length](#f-length)

---


<a name="f-semaphore"></a>
## semaphore

```
syntax: (semaphore)
syntax: (semaphore int-id)
syntax: (semaphore int-id int-wait)
syntax: (semaphore int-id int-signal)
syntax: (semaphore int-id 0)
```

Description:

Creates and controls a system semaphore used for
interprocess synchronization.

A semaphore maintains a counter value between zero
and a system-defined maximum. A value greater than
zero represents the signaled state. A value of zero
represents the non-signaled (blocking) state.

Calling semaphore without arguments creates a new
semaphore and returns its integer identifier. The
initial semaphore value is zero.

Calling semaphore with int-id only returns the
current value of the semaphore.

Calling semaphore with int-id and a positive or
negative integer attempts to adjust the semaphore
value by that amount:

- A negative value attempts to decrement the
  semaphore. If this would reduce the value below
  zero, the calling process blocks until another
  process increments the semaphore.
- A positive value increments the semaphore and
  may unblock one or more waiting processes.

The semaphore value is never allowed to become
negative. Blocking occurs automatically when this
would happen.

Calling semaphore with int-id and a final argument
of 0 releases the semaphore and frees all associated
system resources. Any blocked processes waiting on
this semaphore are released.

On failure, semaphore returns nil. Use sys-error
to retrieve the underlying system error.

Examples:

```
; create semaphore
;------------------------------------------------------------
(set sid (semaphore))
;-> int

(semaphore sid)
;-> 0

; wait (block until signaled)
;------------------------------------------------------------
(semaphore sid -1)

; signal once
;------------------------------------------------------------
(semaphore sid 1)

; signal multiple times
;------------------------------------------------------------
(semaphore sid 3)

; release semaphore
;------------------------------------------------------------
(semaphore sid 0)
```

Example with a child process:

```
(define (counter n)
  (println "counter started")
  (repeat (x n)
    (semaphore sid -1)
    (println x)))

(set sid (semaphore))

(fork (counter 5))

(semaphore sid 1)
(semaphore sid 3)
(semaphore sid 1)
```

Notes:

- Semaphores synchronize independent Unix processes.
- A blocked semaphore call resumes automatically
  when sufficient signals are received.
- The maximum number of semaphores is limited by
  system-wide kernel settings.
- semaphores are commonly used together with
  fork and share.

See: [fork](#f-fork), [share](#f-share),
[sys-error](#f-sys-error)

---


<a name="f-send"></a>
## send

```
syntax: (send int-pid exp)
syntax: (send)
```

Description:

Sends a message between a parent process and its
child processes created with spawn.

send implements asynchronous message passing using
paired send and receive queues. No locks, semaphores,
or shared memory are required.

Each process owns its own receive queue. send places
a message into the target process receive queue.

Only processes started with spawn can use send and
receive. Processes created with fork or process
must use other IPC mechanisms.

When called with int-pid and exp, send attempts to
enqueue exp into the receive queue of the target
process identified by int-pid. The target may be
either the parent process or one of its spawned
child processes.

When called without arguments, send returns a list
of child process IDs whose receive queues can
currently accept messages. This form is used only
by the parent process.

If the target receive queue is full, send returns
nil. The message is not queued.

Messages may contain any valid expression: numbers,
strings, symbols, or list expressions. Message size
is not limited.

Examples:

```
; child sending message to parent
;------------------------------------------------------------
(set ppid (sys-info -4))
(send ppid "hello")

; parent receiving message
;------------------------------------------------------------
(receive child-pid msg)
msg
;-> "hello"

; blocking send and receive
;------------------------------------------------------------
(until (send pid msg))
(until (receive pid msg))

; parent dispatching messages from multiple children
;------------------------------------------------------------
(define (child)
  (set ppid (sys-info -4))
  (while true
    (until (send ppid (rand 100)))))

(repeat (i 5)
  (spawn 'result (child) true))

(for (i 1 3)
  (dolist (cpid (sync))
    (until (receive cpid msg))
    (print "pid:" cpid "->" msg " "))
  (println))
```

Notes:

- send and receive work only with spawn.
- Communication is bidirectional between parent and
  child processes.
- Message passing is non-blocking by default.
- Blocking behavior can be implemented using until.
- Messages may contain code for evaluation by the
  receiver using eval.
- Symbols in received expressions are evaluated in
  the receiver’s environment.

See: [receive](#f-receive), [spawn](#f-spawn),
[sync](#f-sync), [eval](#f-eval)

---


<a name="f-sequence"></a>
## sequence

```
syntax: (sequence num-start num-end [num-step])
```

Description:

Generates a numeric sequence starting at num-start
and ending at num-end.

When num-step is omitted, a step size of 1 is used
and the generated values are integers.

When num-step is specified, it must be a positive
number. In this case, the generated values are
floating-point numbers.

The sequence direction is determined automatically.
If num-start is greater than num-end, the sequence
counts downward. If num-start is less than num-end,
the sequence counts upward.

The end value num-end is included if it lies exactly
on the generated sequence.

Examples:

```
(sequence 10 5)
;-> (10 9 8 7 6 5)

(sequence 0 1 0.2)
;-> (0 0.2 0.4 0.6 0.8 1)

(sequence 2 0 0.3)
;-> (2 1.7 1.4 1.1 0.8 0.5 0.2)
```

Notes:

- num-step must always be positive, even when
  generating a descending sequence.
- Without num-step, results are integers.
- With num-step, results are floating-point numbers.

Use [series](#f-series) to generate geometric
sequences.

See: [series](#f-series), [map](#f-map),
[range](#f-range)

---


<a name="f-series"></a>
## series

```
syntax: (series num-start num-factor num-count)
syntax: (series exp-start function num-count)
```

Description:

Generates a sequence of values derived from a
starting value.

In the first syntax, series generates a geometric
sequence of num-count elements starting at
num-start. Each subsequent element is multiplied
by num-factor. The generated values are always
floating-point numbers.

If num-count is less than 1, an empty list is
returned.

In the second syntax, series generates a sequence
by repeatedly applying function to the previous value.
The first element is exp-start, and each following
element is the result of calling function with the
previous element.

The internal index variable $idx is updated for
each generated element and can be referenced
inside function.

Examples:

```
; geometric sequences
;------------------------------------------------------------
(series 2 2 5)
;-> (2 4 8 16 32)

(series 1 1.2 6)
;-> (1 1.2 1.44 1.728 2.0736 2.48832)

(series 10 0.9 4)
;-> (10 9 8.1 7.29)

(series 0 0 10)
;-> (0 0 0 0 0 0 0 0 0 0)

(series 99 1 5)
;-> (99 99 99 99 99)

; functional series
;------------------------------------------------------------
(series 1 (fn (x) (div (add 1 x))) 20)
;-> (1 0.5 0.6666666 0.6 0.625 0.6153846 0.619047 0.6176470
;    0.6181818 0.6179775 0.6180555 0.6180257 0.6180371
;    0.6180327 0.6180344 0.6180338 0.6180340 0.6180339
;    0.6180339 0.6180339)

(define (oscillate x)
  (if (< x)
    (+ (- x) 1)
    (- (+ x 1))))

(series 1 oscillate 20)
;-> (1 -2 3 -4 5 -6 7 -8 9 -10 11 -12 13 -14 15 -16 17 -18
;    19 -20)

; non-numeric data
;------------------------------------------------------------
(series "a" (fn (c) (char (inc (char c)))) 5)
;-> ("a" "b" "c" "d" "e")

; dependency on previous values
;------------------------------------------------------------
(let (x 1)
  (series x (fn (y) (+ x (swap y x))) 10))
;-> (1 2 3 5 8 13 21 34 55 89)
```

Notes:

- In the geometric form, values are always floats.
- In the functional form, any data type may be
  used as the start value.
- function is evaluated in the caller environment.
- The $idx variable reflects the current index.

Use [sequence](#f-sequence) to generate arithmetic
sequences.

See: [sequence](#f-sequence), [map](#f-map),
[fold](#f-fold)

---


<a name="f-set"></a>
## set

```
syntax: (set sym-1 exp-1 [sym-2 exp-2 ... ])
```

Description:

Evaluates both arguments and assigns the result of
each exp to the symbol specified by sym.

The assignment is performed by copying the contents
of the right-hand side into the symbol. The previous
contents of the symbol are discarded.

The set expression returns the result of the last
assignment performed.

An error is raised when attempting to modify the
symbols nil, true, or a context symbol.

set accepts multiple symbol/expression pairs and
processes them from left to right.

Examples:

```
; basic assignment
;------------------------------------------------------------
(set x 123)
;-> 123

(set x 'y)
;-> y

(set x "hello")
;-> "hello"

y
;-> "hello"

(set lst '(1 2 3))
;-> (1 2 3)

; multiple assignments
;------------------------------------------------------------
(set x 1 'y "hello")
;-> "hello"

x
;-> 1

y
;-> "hello"

; symbol computed by expression
;------------------------------------------------------------
(set lst '(x y z))
;-> (x y z)

(set (first lst) 123)
;-> 123

x
;-> 123

; expressions may depend on earlier assignments
;------------------------------------------------------------
(set a 10 'b (+ a a))

a
;-> 10

b
;-> 20

; assigning functions
;------------------------------------------------------------
(set double (fn (x) (+ x x)))
;-> (fn (x) (+ x x))

; equivalent to define
(define (double x) (+ x x))
;-> (fn (x) (+ x x))
```

Notes:

- set evaluates both the symbol expression and the
  value expression.
- Assignments overwrite previous contents.
- Multiple assignments are evaluated sequentially.
- To protect a symbol from reassignment, use
  constant.

See: [define](#f-define), [constant](#f-constant),
[first](#f-first)

---


<a name="f-set-locale"></a>
## set-locale

```
syntax: (set-locale [str-locale [int-category]])
```

Description:

Reports or changes the process locale.

When called without arguments, set-locale returns
the currently active locale and the decimal point
character as a list.

When str-locale is specified, the locale is changed.
An empty string selects the default locale of the
current system. When int-category is omitted, all
locale categories are affected (LC_ALL).

When int-category is specified, only that category
is changed. Category values correspond to those
defined by the system C library.

On success, set-locale returns a list containing
the active locale string and the decimal separator.
On failure, it returns nil.

Examples:

```
; report current locale
;------------------------------------------------------------
(set-locale)
;-> ("C" ".")

; set system default locale
;------------------------------------------------------------
(set-locale "")
;-> (str-locale str-decimal)

; change numeric formatting only (decimal separator)
;------------------------------------------------------------
(set-locale "de_DE.UTF-8" 4)
;-> ("de_DE.UTF-8" ",")
```

Locale categories:

```
LC_ALL      0
LC_COLLATE  1
LC_CTYPE    2
LC_MONETARY 3
LC_NUMERIC  4
LC_TIME     5
```

Notes:

- The default C locale uses a decimal dot.
- Most non-C locales use a decimal comma.
- Locale settings affect numeric formatting,
  collation, and time formatting.
- Regular expression behavior is not affected
  by set-locale.
- Category values are defined by the system
  locale.h header.

See: [format](#f-format), [sys-info](#f-sys-info)

---


<a name="f-set-ref"></a>
## set-ref

```
syntax: (set-ref exp-key list exp-replacement [func-compare])
```

Description:

Searches for exp-key in list and replaces the first
matching element with exp-replacement.

The search descends recursively into nested lists.
When a match is found, the element is replaced and
the modified list is returned.

During replacement, the system variable $it is
bound to the matched expression and may be used
inside exp-replacement.

If function-compare is specified, it is used to compare
exp-key with list elements. Without function-compare,
default equality comparison is used.

The original list is modified when passed by
reference.

Examples:

```
; simple replacement in nested list
;------------------------------------------------------------
(set data '(fruits (apples 123 44) (oranges 1 5 3)))

(set-ref 'apples data 'Apples)
;-> (fruits (Apples 123 44) (oranges 1 5 3))

data
;-> (fruits (Apples 123 44) (oranges 1 5 3))

; passing list by reference via context
;------------------------------------------------------------
(set db:db '(fruits (apples 123 44) (oranges 1 5 3)))

(define (update ct key value)
  (set-ref key ct value))

(update db 'apples 'Apples)
;-> (fruits (Apples 123 44) (oranges 1 5 3))

(update db 'oranges 'Oranges)
;-> (fruits (Apples 123 44) (Oranges 1 5 3))

db:db
;-> (fruits (Apples 123 44) (Oranges 1 5 3))
```

Notes:

- Only the first matching element is replaced.
- The search is recursive across nested lists.
- $it contains the matched element during
  replacement.
- For replacing all occurrences, use set-ref-all.

See: [set-ref-all](#f-set-ref-all), [replace](#f-replace),
[ref](#f-ref)

---


<a name="f-set-ref-all"></a>
## set-ref-all

```
syntax: (set-ref-all exp-key list exp-replacement [func-compare])
```

Description:

Searches for exp-key in list and replaces all
matching elements with exp-replacement.

The search descends recursively into nested lists.
Each matching element is replaced and the modified
list is returned.

During replacement, the system variable $it is
bound to the currently matched expression and may
be used inside exp-replacement.

After completion, the system variable $count
contains the number of replacements performed.

If function-compare is specified, it is used to compare
exp-key with list elements. Without function-compare,
default equality comparison is used.

The original list is modified when passed by
reference.

Examples:

```
; replace all matching elements
;------------------------------------------------------------
(set data
 '((monday (apples 20 30) (oranges 2 4 9))
   (tuesday (apples 5) (oranges 32 1))))

(set-ref-all 'apples data "Apples")
;-> ((monday ("Apples" 20 30) (oranges 2 4 9))
;    (tuesday ("Apples" 5) (oranges 32 1)))

$count
;-> 2

; passing list by reference via context
;------------------------------------------------------------
(set db:db
 '((monday (apples 20 30) (oranges 2 4 9))
   (tuesday (apples 5) (oranges 32 1))))

(define (foo ctx)
  (set-ref-all 'apples ctx "Apples"))

(foo db)
;-> ((monday ("Apples" 20 30) (oranges 2 4 9))
;    (tuesday ("Apples" 5) (oranges 32 1)))

db:db
;-> ((monday ("Apples" 20 30) (oranges 2 4 9))
;    (tuesday ("Apples" 5) (oranges 32 1)))

; custom comparison using match
;------------------------------------------------------------
(set data
 '((monday (apples 20 30) (oranges 2 4 9))
   (tuesday (apples 5) (oranges 32 1))))

(set-ref-all '(oranges *) data
  (list (first $it) (apply + (rest $it)))
  match)
;-> (... (oranges 15) ... (oranges 33) ...)
```

Notes:

- All matching elements are replaced.
- The search is recursive across nested lists.
- $it contains the currently matched element.
- $count contains the number of replacements.
- For replacing only the first match, use set-ref.

See: [set-ref](#f-set-ref), [replace](#f-replace),
[ref-all](#f-ref-all), [match](#f-match)

---


<a name="f-setq-setf"></a>
## setq, setf

```
syntax: (setq place-1 exp-1 [place-2 exp-2 ... ])
syntax: (setf place-1 exp-1 [place-2 exp-2 ... ])
```

Description:

setq and setf assign values to places.

Both forms are implemented by the same built-in
function and behave identically. The distinction
is conventional and used throughout this manual:

- setq is used when assigning to a symbol
- setf is used when assigning to list, array, or
  string place references

Assignments overwrite the previous contents of the
target place. Multiple place/expression pairs may
be specified and are evaluated from left to right.
The return value is the result of the last
assignment.

Examples:

```
; symbol assignment
;------------------------------------------------------------
(setq x 123)
;-> 123

(setq x 1 y 2 z 3)
;-> 3

x
;-> 1
y
;-> 2
z
;-> 3

; list assignment using implicit indices
;------------------------------------------------------------
(setq lst '(a b (c d) e f g))

(setf (lst 1) 'B)
;-> B
lst
;-> (a B (c d) e f g)

(setf (nth 1 lst) 'B)
;-> B

(setf (lst 2 0) 'C)
;-> C
lst
;-> (a B (C d) e f g)

(setf (lst 2) 'X)
;-> X
lst
;-> (a B X e f g)

; assignment using assoc
;------------------------------------------------------------
(setq lst '((a 1) (b 2)))

(setf (assoc 'b lst) '(b 3))
;-> (b 3)
lst
;-> ((a 1) (b 3))

; assignment using lookup
;------------------------------------------------------------
(setf (lookup 'b lst) 30)
;-> 30
lst
;-> ((a 1) (b 30))

; nested accessors
;------------------------------------------------------------
(setq lst '((a 1) (b 2)))

(push 'b (setf (assoc 'b lst) '(b 4)))
;-> b
lst
;-> ((a 1) (b b 4))

; string modification
;------------------------------------------------------------
(set s "Example")

(setf (s 0) "e")
;-> "e"
s
;-> "example"

(setf (s 3) "XYZ")
;-> "XYZ"
s
;-> "exaXYZle"

; using $it for value-dependent updates
;------------------------------------------------------------
(setq lst '((apples 4) (oranges 1)))

(setf (lst 1 1) (+ $it 1))
;-> 2
lst
;-> ((apples 4) (oranges 2))

(set s "Sample")

(setf (s 0) (lower-case $it))
;-> "s"
s
;-> "sample"
```

Notes:

- setq and setf are aliases of the same primitive.
- The use of setq vs. setf is a documentation
  convention, not a semantic difference.
- $it refers to the previous value of the place
  being modified.
- Multiple assignments are evaluated sequentially.

See: [set](#f-set), [assoc](#f-assoc),
[lookup](#f-lookup), [nth](#f-nth)

---


<a name="f-sgn"></a>
## sgn

```
syntax: (sgn num)
syntax: (sgn num exp-1 [exp-2 [exp-3]])
```

Description:

Evaluates the sign of num.

In the first syntax, sgn returns an integer value
according to the sign of num:

- num > 0  →  1
- num < 0  → -1
- num = 0  →  0

In the second syntax, sgn returns the result of
evaluating one of the optional expressions instead
of the numeric sign values.

The expressions correspond to the following cases:

- exp-1 is evaluated when num < 0
- exp-2 is evaluated when num = 0
- exp-3 is evaluated when num > 0

If the expression corresponding to the triggered
case is omitted, sgn returns nil.

Examples:

```
; numeric sign
;------------------------------------------------------------
(sgn -3.5)
;-> -1

(sgn 0)
;-> 0

(sgn 123)
;-> 1

; conditional expressions
;------------------------------------------------------------
(sgn x -1 0 1)
;-> same behavior as (sgn x)

(sgn x -1 1 1)
;-> -1 for negative x, 1 otherwise

(sgn x nil true true)
;-> nil for negative x, true otherwise

(sgn x (abs x) 0)
;-> (abs x) for x < 0, 0 for x = 0, nil otherwise
```

Notes:

- Any expression or constant may be used for
  exp-1, exp-2, and exp-3.
- Expressions are evaluated only for the case
  that matches the sign of num.
- sgn can be used as a compact conditional
  selector based on numeric sign.

See: [abs](#f-abs), [if](#f-if), [cond](#f-cond)

---


<a name="f-share"></a>
## share

```
syntax: (share)
syntax: (share int-address)
syntax: (share int-address exp-value)
syntax: (share nil int-address)
```

Description:

Provides access to shared memory for communication
between parent and child processes.

When called without arguments, share requests a
shared memory region from the operating system and
returns an integer address that can be stored for
later use.

When called with int-address and exp-value, the
value of exp-value is written into the shared memory
region. The value written is also returned.

When called with int-address only, share returns
the value currently stored in the shared memory.
If no value has been written yet, nil is returned.

When called with nil and int-address, the shared
memory region is unmapped and released. Using a
shared address after unmapping results in undefined
behavior.

Shared memory can be accessed only between a parent
process and its child processes.

Examples:

```
; allocate shared memory
;------------------------------------------------------------
(set mem (share))
;-> int-address

; write and read values
;------------------------------------------------------------
(share mem 123)
;-> 123

(share mem)
;-> 123

(share mem "hello world")
;-> "hello world"

(share mem)
;-> "hello world"

(share mem true)
;-> true

(share mem)
;-> true

; store and evaluate expressions
;------------------------------------------------------------
(share mem '(+ 1 2 3 4))
;-> (+ 1 2 3 4)

(eval (share mem))
;-> 10

; unmap shared memory
;------------------------------------------------------------
(share nil mem)
;-> true
```

Notes:

- Memory can be shared only between parent and
  child processes.
- Access to shared memory must be synchronized.
  Concurrent unsynchronized access can crash the
  process.
- Use semaphore to coordinate access between
  processes.
- Large objects may be backed internally by
  temporary files.

See: [semaphore](#f-semaphore), [fork](#f-fork),
[eval](#f-eval)

---


<a name="f-signal"></a>
## signal

```
syntax: (signal int-signal sym-event-handler | function-event-handler)
syntax: (signal int-signal "ignore" | "default" | "reset")
syntax: (signal int-signal)
```

Description:

Installs, modifies, or queries a signal handler for
the signal specified by int-signal.

When a symbol or function expression is supplied,
it becomes the user-defined handler for the signal.
The handler argument is not evaluated at setup time.

When one of the string options is supplied, the
signal handler is set as follows:

- "ignore"   ignores the signal
- "default"  restores the system default handler
- "reset"    restores the handler state from
              interpreter startup

The option strings are case-insensitive and may be
abbreviated to their first letter.

When called with only int-signal, signal returns
the currently installed handler symbol or nil if
no handler is defined.

Examples:

```
; define and install a handler
;------------------------------------------------------------
(constant 'SIGINT 2)

(define (ctrlC-handler)
  (println "ctrl-C has been pressed"))

(signal SIGINT 'ctrlC-handler)

; pressing ctrl-C now invokes the handler
;------------------------------------------------------------
ctrl-C has been pressed

; reset handler to startup state
;------------------------------------------------------------
(signal SIGINT "reset")
```

Handler as inline function:

```
(signal SIGINT
  (fn (s)
    (println "signal " s " occurred")))
```

Query current handler:

```
(signal SIGINT)
;-> sym | nil
```

Setting multiple signals to one handler:

```
(define (signal-handler sig)
  (println "received signal: " sig))

(for (s 1 8)
  (signal s 'signal-handler))
```

Notes:

- Signal numbers and availability are defined by
  the underlying system headers.
- Some signals cannot be intercepted and always
  terminate the process.
- Signal handlers may be invoked asynchronously,
  even while another function is executing.
- The signal number may be passed as a parameter
  to the handler function.

See: [constant](#f-constant), [destroy](#f-destroy),
[sleep](#f-sleep)

---


<a name="f-silent"></a>
## silent

```
syntax: (silent [exp-1 [exp-2 ... ]])
```

Description:

Evaluates one or more expressions while suppressing
console output of the return value and the prompt.

silent behaves like begin, but discards any output
that would normally be printed after evaluation.
It is typically used when the return value is not
needed, such as when Rebel is controlled by another
application.

Silent mode is automatically reset when execution
returns to the prompt.

When called without arguments, silent enables silent
mode for the next expression.

Examples:

```
; suppress output of a single expression
;------------------------------------------------------------
(silent (my-func))

; equivalent form
;------------------------------------------------------------
(silent)
(my-func)
```

Notes:

- silent suppresses only console output, not side
  effects of evaluated expressions.
- Silent mode ends automatically when control
  returns to the prompt.
- In interactive mode, press [enter] twice after
  using silent to restore the prompt.

See: [begin](#f-begin), [eval](#f-eval)

---


<a name="f-sin"></a>
## sin

```
syntax: (sin num-radians)
```

Description:

Calculates the sine of num-radians and returns the
result as a floating-point number.

The argument is interpreted as an angle measured
in radians.

Examples:

```
(sin 1)
;-> 0.8414709838

(set pi (mul 2 (acos 0)))
;-> 3.141592654

(sin (div pi 2))
;-> 1
```

Notes:

- The argument is specified in radians, not degrees.
- The return value is a floating-point number.

See: [cos](#f-cos), [tan](#f-tan), [acos](#f-acos)

---


<a name="f-sinh"></a>
## sinh

```
syntax: (sinh num-radians)
```

Description:

Calculates the hyperbolic sine of num-radians and
returns the result as a floating-point number.

The hyperbolic sine is defined as:

  (exp x - exp (-x)) / 2

Large values of num-radians may cause the result to
overflow to inf.

Examples:

```
(sinh 1)
;-> 1.175201194

(sinh 10)
;-> 11013.23287

(sinh 1000)
;-> inf

(sub (tanh 1) (div (sinh 1) (cosh 1)))
;-> 0
```

Notes:

- The argument is specified in radians.
- The return value is a floating-point number.
- Very large arguments may overflow.

See: [cosh](#f-cosh), [tanh](#f-tanh),
[exp](#f-exp)

---


<a name="f-slice"></a>
## slice

```
syntax: (slice list int-index [int-length])
syntax: (slice array int-index [int-length])
syntax: (slice str int-index [int-length])
```

Description:

Extracts a portion of a list, array, or string and
returns it as a new value. The original object is
left unchanged.

For lists and arrays, slice copies elements starting
at int-index. When int-length is specified, that
number of elements is copied.

If int-length is negative, it is treated as an
offset counted from the end, and elements are copied
up to (but not including) that offset.

If int-length is omitted, all elements from
int-index to the end are copied.

For strings, slice extracts a substring starting at
int-index with length int-length. When int-length
is omitted, everything to the end of the string is
copied.

slice operates on byte boundaries, not character
boundaries.

Examples:

```
; list slicing
;------------------------------------------------------------
(slice '(a b c d e f) 3 2)
;-> (d e)

(slice '(a b c d e f) 2 -2)
;-> (c d)

(slice '(a b c d e f) 2)
;-> (c d e f)

(slice '(a b c d e f) -4 3)
;-> (c d e)

; array slicing
;------------------------------------------------------------
(set arr (array 3 2 (sequence 1 6)))
;-> ((1 2) (3 4) (5 6))

(slice arr 1 2)
;-> ((3 4) (5 6))

; string slicing (byte-based)
;------------------------------------------------------------
(slice "Hello World" 6 2)
;-> "Wo"

(slice "Hello World" 0 5)
;-> "Hello"

(slice "Hello World" 6)
;-> "World"

(slice "Example" -4 2)
;-> "pl"

; UTF-8 strings: operate on bytes
;------------------------------------------------------------
(join
  (slice
    (explode "ΩΨΧΦΥΤΣΣΡΠΟΞΝΜΛΚΙΘΗΖΕΔΓΒΑ")
    3 5))
;-> "ΦΥΤΣΣ"
```

Notes:

- slice always operates on 8-bit byte boundaries.
- For UTF-8 strings, character-aware slicing
  requires converting the string to a list first.
- Negative indices and lengths are supported.
- slice does not modify the original object.

See: [explode](#f-explode), [join](#f-join),
[select](#f-select), [nth](#f-nth)

---


<a name="f-slice"></a>
## slice

```
syntax: (slice list int-index [int-length])
syntax: (slice array int-index [int-length])
syntax: (slice str int-index [int-length])
```

Description:

Extracts a portion of a list, array, or string and
returns it as a new value. The original object is
left unchanged.

For lists and arrays, slice copies elements starting
at int-index. When int-length is specified, that
number of elements is copied.

If int-length is negative, it is treated as an
offset counted from the end, and elements are copied
up to (but not including) that offset.

If int-length is omitted, all elements from
int-index to the end are copied.

For strings, slice extracts a substring starting at
int-index with length int-length. When int-length
is omitted, everything to the end of the string is
copied.

slice operates on byte boundaries, not character
boundaries.

Examples:

```
; list slicing
;------------------------------------------------------------
(slice '(a b c d e f) 3 2)
;-> (d e)

(slice '(a b c d e f) 2 -2)
;-> (c d)

(slice '(a b c d e f) 2)
;-> (c d e f)

(slice '(a b c d e f) -4 3)
;-> (c d e)

; array slicing
;------------------------------------------------------------
(set A (array 3 2 (sequence 1 6)))
;-> ((1 2) (3 4) (5 6))

(slice A 1 2)
;-> ((3 4) (5 6))

; string slicing (byte-based)
;------------------------------------------------------------
(slice "Hello World" 6 2)
;-> "Wo"

(slice "Hello World" 0 5)
;-> "Hello"

(slice "Hello World" 6)
;-> "World"

(slice "Example" -4 2)
;-> "pl"

; UTF-8 strings: operate on bytes
;------------------------------------------------------------
(join
  (slice
    (explode "ΩΨΧΦΥΤΣΣΡΠΟΞΝΜΛΚΙΘΗΖΕΔΓΒΑ")
    3 5))
;-> "ΦΥΤΣΣ"
```

Notes:

- slice always operates on 8-bit byte boundaries.
- For UTF-8 strings, character-aware slicing
  requires converting the string to a list first.
- Negative indices and lengths are supported.
- slice does not modify the original object.

See: [explode](#f-explode), [join](#f-join),
[select](#f-select), [nth](#f-nth)

---


<a name="f-sort"></a>
## sort

```
syntax: (sort list [func-compare])
syntax: (sort array [func-compare])
```

Description:

Sorts the elements of list or array in ascending
order and returns the sorted object.

The operation is destructive: the original list or
array is reordered in place.

Elements of any type may be sorted. When elements
are themselves lists or arrays, comparison is
performed recursively. When values of different
types are compared, ordering follows this hierarchy:

Atoms:
  nil, true, integer/float, string, symbol, primitive

Lists:
  quoted expression, list, fn, macro

The sorting algorithm is a stable binary merge sort
with approximately O(n log2 n) performance. Adjacent
elements that compare equal preserve their order.

An optional comparison operator or function may be
supplied. When provided, it must be compatible with
<= or >= semantics to preserve stability.

Examples:

```
; basic sorting
;------------------------------------------------------------
(sort '(v f r t h n m j))
;-> (f h j m n r t v)

(sort '((3 4) (2 1) (1 10)))
;-> ((1 10) (2 1) (3 4))

(sort '((3 4) "hi" 2.8 8 b))
;-> (2.8 8 "hi" b (3 4))

; destructive behavior
;------------------------------------------------------------
(set s '(k a l s))
(sort s)
;-> (a k l s)

s
;-> (a k l s)

; custom order
;------------------------------------------------------------
(sort '(v f r t h n m j) >)
;-> (v t r n m j h f)

(sort s <)
;-> (a k l s)

(sort s >)
;-> (s l k a)

s
;-> (s l k a)

; user-defined comparison
;------------------------------------------------------------
(define (comp x y)
  (>= (last x) (last y)))

(set db '((a 3) (g 2) (c 5)))

(sort db comp)
;-> ((c 5) (a 3) (g 2))

; anonymous comparison function
;------------------------------------------------------------
(sort db (fn (x y) (>= (last x) (last y))))
```

Notes:

- sort modifies the original list or array.
- The algorithm is stable.
- Custom comparison functions must implement a
  consistent ordering.

See: [last](#f-last), [map](#f-map),
[select](#f-select)

---


<a name="f-source"></a>
## source

```
syntax: (source)
syntax: (source sym-1 [sym-2 ... ])
```

Description:

Serializes symbols, definitions, and contexts into
a string representation.

source behaves like save, but writes the serialized
output to a string instead of a file.

When called without arguments, source serializes
the entire workspace.

When one or more symbols are specified, only those
symbols are serialized. Context symbols cause all
symbols contained in that context to be serialized
as well.

Symbols with a value of nil are not serialized.
System symbols beginning with the $ character are
serialized only when explicitly specified.

Symbols not belonging to the current context are
written with their context prefix.

Examples:

```
(define (double x)
  (+ x x))

(source 'double)
;-> "(define (double x)\n  (+ x x))\n\n"
```

Notes:

- source returns a string containing valid source
  code.
- Formatting (line breaks and indentation) can be
  controlled using pretty-print.
- Context contents are serialized recursively.

See: [save](#f-save), [pretty-print](#f-pretty-print),
[context](#f-context)

---


<a name="f-spawn"></a>
## spawn

```
syntax: (spawn sym exp [true])
```

Description:

Starts the evaluation of exp in a new child process
and returns immediately.

The symbol sym is quoted and will receive the result
of the evaluation when sync is executed. spawn is
used to evaluate expressions concurrently in
separate processes.

spawn always creates a new process. It does not
limit the number of spawned processes and does not
take the number of CPU cores into account.

Actual parallel execution is determined entirely by
the operating system scheduler.

When the optional third argument evaluates to true,
the spawned process is enabled for communication
using send and receive.

After successfully starting a child process, spawn
returns the process ID of the child.

Examples:

```
; parallel computation
;------------------------------------------------------------
(define (primes from to)
  (local (plist)
    (for (i from to)
      (if (= 1 (length (factor i)))
        (push i plist -1)))
    plist))

(set start (time-of-day))

(spawn 'p1 (primes 1 1000000))
(spawn 'p2 (primes 1000001 2000000))
(spawn 'p3 (primes 2000001 3000000))
(spawn 'p4 (primes 3000001 4000000))

(sync 60000)

(println "time spawn: " (- (time-of-day) start))

; Waiting and polling:
;------------------------------------------------------------
; poll every 2 seconds
(until (sync 2000)
  (println "."))

; list pending child process IDs
(until (sync 300)
  (println (sync)))

; Abort unfinished tasks:
;------------------------------------------------------------
(if (not (sync 60000))
  (begin
    (println "aborting unfinished: " (sync))
    (abort))
  (println "all finished successfully"))

; Recursive spawning:
;------------------------------------------------------------
(define (fibo n)
  (local (f1 f2)
    (if (< n 2)
      1
      (begin
        (spawn 'f1 (fibo (- n 1)))
        (spawn 'f2 (fibo (- n 2)))
        (sync 10000)
        (+ f1 f2)))))

(fibo 7)
;-> 21
```

Notes:

- spawn always creates a new process.
- The number of processes spawned is not limited by
  the number of CPU cores.
- At any moment, only as many processes can run in
  parallel as there are available CPU cores.
- Additional processes are scheduled by the
  operating system and may increase overhead due
  to context switching.
- For CPU-bound workloads, spawning significantly
  more processes than available cores may reduce
  performance.
- For workloads involving blocking operations,
  additional spawned processes may still be useful.
- Results from spawned processes are collected
  using sync.

See: [sync](#f-sync), [abort](#f-abort),
[send](#f-send), [receive](#f-receive),
[fork](#f-fork)

---


<a name="f-sqrt"></a>
## sqrt

```
syntax: (sqrt num)
```

Description:

Calculates the square root of num and returns the
result as a floating-point number.

Examples:

```
(sqrt 10)
;-> 3.16227766

(sqrt 25)
;-> 5
```

Notes:

- The return value is a floating-point number.

See: [pow](#f-pow), [exp](#f-exp), [log](#f-log)

---


<a name="f-ssq"></a>
## ssq

```
syntax: (ssq list-vector | array-vector)
```

Description:

Calculates the sum of squares of the numeric elements
in list-vector or array-vector and returns the result.

Examples:

```
(set vector (sequence 1 10))
(ssq vector)
;-> 385

(set vector (array 10 (sequence 1 10)))
(ssq vector)
;-> 385
```

Notes:

- Elements must be numeric.
- The input may be either a list or an array.

See: [sequence](#f-sequence), [pow](#f-pow),
[sum](#f-sum)

---


<a name="f-starts-with"></a>
## starts-with

```
syntax: (starts-with str str-key [num-option])
syntax: (starts-with list exp)
```

Description:

Checks whether a string or list begins with a given
value.

In the first form, starts-with tests whether the
string str begins with the key string str-key. The
function returns true on success or nil otherwise.

When num-option is specified, str-key is treated as
a regular expression pattern. The value of
num-option controls regular expression options.

In the second form, starts-with tests whether list
begins with the element exp. The comparison is done
on the first element of the list.

Examples:

```
; string prefix
;------------------------------------------------------------
(starts-with "this is useful" "this")
;-> true

(starts-with "this is useful" "THIS")
;-> nil

; regular expression matching
;------------------------------------------------------------
(starts-with "this is useful" "THIS" 1)
;-> true

(starts-with "this is useful" "this|that" 0)
;-> true

; list prefix
;------------------------------------------------------------
(starts-with '(1 2 3 4 5) 1)
;-> true

(starts-with '(a b c d e) 'b)
;-> nil

(starts-with '((+ 3 4) b c d) '(+ 3 4))
;-> true
```

Notes:

- For strings, comparison is case-sensitive unless
  regular expression options specify otherwise.
- For lists, only the first element is tested.
- The function returns true or nil.

See: [ends-with](#f-ends-with), [regex](#f-regex)

---


<a name="f-stats"></a>
## stats

```
syntax: (stats list-vector)
```

Description:

Calculates statistical values describing central
tendency and distribution moments of the numeric
values in list-vector.

The function returns a list containing the following
values in this order:

N
  Number of values

mean
  Mean of values

avdev
  Average deviation from the mean

sdev
  Standard deviation (population estimate)

var
  Variance (population estimate)

skew
  Skewness of the distribution

kurt
  Kurtosis of the distribution

Examples:

```
(set data '(90 100 130 150 180 200 220 300 350 400))

(println
  (format [text]
    N        = %5d
    mean     = %8.2f
    avdev    = %8.2f
    sdev     = %8.2f
    var      = %8.2f
    skew     = %8.2f
    kurt     = %8.2f
[/text]
  (stats data)))
```

Output:

```
N        =    10
mean     =   212.00
avdev    =    84.40
sdev     =   106.12
var      = 11262.22
skew     =     0.49
kurt     =    -1.34
```

Notes:

- All values are computed as population estimates.
- Input values must be numeric.
- The return value is a plain list in fixed order.

See: [ssq](#f-ssq), [mean](#f-mean),
[format](#f-format)

---


<a name="f-string"></a>
## string

```
syntax: (string exp-1 [exp-2 ... ])
```

Description:

Converts the result of evaluating one or more
expressions into a string.

When multiple expressions are specified, each
expression is converted to a string and the results
are concatenated in order.

Examples:

```
(string 'hello)
;-> "hello"

(string 1234)
;-> "1234"

(string '(+ 3 4))
;-> "(+ 3 4)"

(string (+ 3 4) 8)
;-> "78"

(string 'hello " " 123)
;-> "hello 123"
```

Zero bytes:

If a buffer passed to string contains zero bytes
(\000), copying stops at the first terminating
zero.

```
(set buff "ABC\000\000\000")
;-> "ABC\000\000\000"

(length buff)
;-> 6

(string buff)
;-> "ABC"

(length (string buff))
;-> 3
```

Notes:

- string stops copying at the first zero byte.
- For binary-safe concatenation, use append or join.
- string converts expressions to their textual
  representation.
- To convert a function into its source form, use
  source.

See: [append](#f-append), [join](#f-join),
[source](#f-source)

---


<a name="f-stringp"></a>
## string?

```
syntax: (string? exp)
```

Description:

Evaluates exp and tests whether the result is a
string.

Returns true if the evaluated expression is a
string, otherwise nil.

Examples:

```
(set var "hello")

(string? var)
;-> true
```

See: [list?](#f-listp), [symbol?](#f-symbolp),
[number?](#f-numberp)

---


<a name="f-struct"></a>
## struct

```
syntax: (struct symbol [str-data-type ... ])
```

Description:

Defines an aggregate data type for use with foreign
functions imported via libffi.

struct is used together with import, pack, and
unpack to describe C-compatible structure layouts.
This enables calling C functions that accept struct
values or pointers to structs.

The symbol names the structure type. Each
str-data-type specifies one field in declaration
order and must match the corresponding C type and
platform ABI.

struct returns the symbol naming the defined
structure.

Examples:

```
; import C functions working with struct tm
;------------------------------------------------------------
(import "libc.so" "asctime"   "char*" "void*")
(import "libc.so" "localtime" "void*" "void*")

; define struct tm (Unix 64-bit layout)
;------------------------------------------------------------
(struct 'tm
  "int"    ; tm_sec
  "int"    ; tm_min
  "int"    ; tm_hour
  "int"    ; tm_mday
  "int"    ; tm_mon
  "int"    ; tm_year
  "int"    ; tm_wday
  "int"    ; tm_yday
  "int"    ; tm_isdst
  "long"   ; tm_gmtoff
  "char*"  ; tm_zone
)

; obtain current time
;------------------------------------------------------------
(set today (date-value))

; localtime returns a pointer to struct tm
(set ptr (localtime (address today)))

; unpack struct fields into a list
(unpack tm ptr)
;-> (13 15 7 17 11 111 6 350 0 -28800 "PST")

; format using C library
(asctime ptr)
;-> "Sat Dec 17 07:15:13 2011\n"

; equivalent one-liner
(asctime (localtime (address today)))

; Nested structures:
;------------------------------------------------------------
; define a simple pair
(struct 'pair "char" "char")
;-> pair

; define a structure containing another struct
(struct 'comp "pair" "int")
;-> comp

; pack and unpack nested structures
(pack comp (pack pair 1 2) 3)
;-> "\001\002\000\000\003\000\000\000"

(unpack comp "\001\002\000\000\003\000\000\000")
;-> ((1 2) 3)
```

Notes:

- struct definitions must match the exact C layout
  used by the target system and compiler ABI.
- Field order, size, and alignment are critical.
- struct definitions may be nested and are unpacked
  recursively.
- Pointers to structs are represented as void*.
- Passing invalid addresses to imported functions
  or unpack can crash the interpreter.
- struct requires libffi support.

See: [import](#f-import), [pack](#f-pack),
[unpack](#f-unpack), [address](#f-address)

---


<a name="f-sub"></a>
## sub

```
syntax: (sub num-1 [num-2 ... ])
```

Description:

Subtracts the values in num-2 and following arguments
successively from num-1.

The function performs mixed-type arithmetic and accepts
integers and floating point numbers as arguments. The
result is always returned as a floating point number,
even when all arguments are integers.

When called with only one argument, sub returns the
negated value of num-1.

If any argument involved in the calculation is NaN,
the result is NaN.

Examples:

```
(sub 10 8 0.25)
;-> 1.75

(sub 123)
;-> -123
```

See: [-](#f-minus), [add](#f-add), [mul](#f-mul), [div](#f-div)

---


<a name="f-swap"></a>
## swap

```
syntax: (swap place-1 place-2)
```

Description:

Swaps the contents of place-1 and place-2.

A place can refer to the contents of an unquoted
symbol or to an addressable location inside a list
or array. Valid places include references created
with first, last, nth, implicit indexing, and places
returned by assoc or lookup.

swap is a destructive operation. It modifies the
lists, arrays, or symbols involved directly and does
not create copies of the exchanged values.

Any two places may be swapped, whether they belong
to the same object or to different objects.

Examples:

```
; swapping elements in a list
;------------------------------------------------------------
(set lst '(a b c d e f))

(swap (first lst) (last lst))
;-> a

lst
;-> (f b c d e a)

; swapping between two lists
;------------------------------------------------------------
(set lst-b '(x y z))

(swap (lst 0) (lst-b -1))
;-> f

lst
;-> (z b c d e a)

lst-b
;-> (x y f)

; swapping array rows
;------------------------------------------------------------
(set A (array 2 3 (sequence 1 6)))

(swap (A 0) (A 1))
;-> (1 2 3)

A
;-> ((4 5 6) (1 2 3))

; swapping symbol values
;------------------------------------------------------------
(set x 1 'y 2)

(swap x y)
;-> 1

x
;-> 2

y
;-> 1

; swapping via assoc and lookup
;------------------------------------------------------------
(set lst '((a 1 2 3) (b 10 20 30)))

(swap (lookup 'a lst -1) (lookup 'b lst 1))

lst
;-> ((a 1 2 10) (b 3 20 30))

(swap (assoc 'a lst) (assoc 'b lst))

lst
;-> ((b 3 20 30) (a 1 2 10))
```

Notes:

- swap operates on places, not on values.
- Both places must be assignable locations.
- The operation is performed in constant time
  relative to the size of the data structures.

See: [set](#f-set), [setf](#f-setf), [first](#f-first),
     [last](#f-last), [nth](#f-nth), [assoc](#f-assoc),
     [lookup](#f-lookup)

---


<a name="f-sym"></a>
## sym

```
syntax: (sym string [sym-context [nil-flag]])
syntax: (sym number [sym-context [nil-flag]])
syntax: (sym symbol [sym-context [nil-flag]])
```

Description:

Translates the first argument into a symbol and returns
it. The first argument may be a string, number, or
symbol.

If sym-context is omitted, the current context is used
for symbol lookup or creation. When sym-context is
specified, lookup and creation take place in that
context. If the context does not exist and sym-context
is given as a quoted symbol, the context is created.
If sym-context is unquoted, it is interpreted as an
existing context or a variable containing a context.

By default, symbols are created when they do not already
exist. When the optional third argument nil-flag
evaluates to nil, symbol creation is suppressed. In
that case, sym returns nil if the symbol is not found.
This form can be used to test for the existence of a
symbol.

sym can create symbols that are not legal identifiers
in source code, such as numeric names or names
containing special characters. This makes sym suitable
for associative memory access similar to hash tables
in other languages.

When the first argument is a symbol, its name is
extracted and used as the symbol name in the target
context.

Examples:

```
; basic symbol creation
;------------------------------------------------------------
(sym "some")
;-> some

(set (sym "var1") 345)
;-> 345

var1
;-> 345

; specifying contexts
;------------------------------------------------------------
(sym "s" 'ctx)
;-> ctx:s

(sym "s" ctx)
;-> ctx:s

; suppressing symbol creation
;------------------------------------------------------------
(sym "var2" ctx nil)
;-> nil

(sym "var2" ctx)
;-> var2

(sym "var2" ctx nil)
;-> var2

; using sym as associative memory
;------------------------------------------------------------
(set (sym "alpha" 'db) 1.234)
(set (sym "(" 'db) "parenthesis open")
(set (sym 12 'db) "twelve")

(eval (sym "alpha" 'db))
;-> 1.234

(eval (sym "(" 'db))
;-> "parenthesis open"

(eval (sym 12 'db))
;-> "twelve"

; deleting a dynamically created symbol
;------------------------------------------------------------
(delete (sym "alpha" 'db))
;-> true

; using symbols as name sources
;------------------------------------------------------------
(sym 'var1 'ctx)
;-> ctx:var1

; dynamic context population
;------------------------------------------------------------
(define-macro (def-context)
  (dolist (s (rest (args)))
    (sym s (first (args)))))

(def-context ctx x y z)

(symbols ctx)
;-> (ctx:x ctx:y ctx:z)
```

Notes:

- sym returns the symbol that was found or created.
- Symbols created by sym may not be representable as
  source identifiers.
- When nil-flag is used, sym performs lookup only and
  does not modify the symbol table.

See: [context](#f-context), [set](#f-set), [setf](#f-setf),
     [delete](#f-delete), [symbols](#f-symbols)

---


<a name="f-symbolp"></a>
## symbol?

```
syntax: (symbol? exp)
```

Description:

Evaluates the expression exp and returns true if the
resulting value is a symbol. Otherwise, nil is returned.

symbol? tests the value produced by evaluation, not
the syntactic form of the expression.

Examples:

```
(set x 'y)
;-> y

(symbol? x)
;-> true

(symbol? 123)
;-> nil

(symbol? (first '(var x y z)))
;-> true
```

Notes:

- symbol? evaluates its argument before testing.
- Quoted symbols evaluate to symbols.
- Non-symbol values always return nil.

See: [symbol](#f-symbol), [context](#f-context),
     [symbols](#f-symbols)

---


<a name="f-symbols"></a>
## symbols

```
syntax: (symbols [context])
```

Description:

Returns a sorted list of all symbols defined in a
context.

When called without arguments, symbols returns the
symbols defined in the current context. When a context
is specified, symbols defined in that context are
returned.

The context may be given as a quoted symbol, an
unquoted context name, or a variable containing a
context. Contexts evaluate to themselves, so quoting
is optional.

Examples:

```
; symbols in the current context
;------------------------------------------------------------
(symbols)

; symbols in a specific context
;------------------------------------------------------------
(symbols 'ctx)
(symbols ctx)

(set ct ctx)
(symbols ct)
```

Notes:

- The returned list is sorted alphabetically.
- Only symbols directly defined in the specified
  context are returned.

See: [symbol?](#f-symbolp), [symbol](#f-symbol),
     [context](#f-context)

---


<a name="f-sync"></a>
## sync

```
syntax: (sync int-timeout [func-inlet])
syntax: (sync)
```

Description:

Synchronizes child processes launched with spawn.

When called with int-timeout specified in milliseconds,
sync waits for spawned child processes to finish. As
each child process completes, the evaluation result of
the spawned subtask is assigned to the symbol specified
in the corresponding spawn statement.

The function returns true when all child processes have
been processed. If the timeout is reached while child
processes are still pending, sync returns nil.

If an optional inlet function func-inlet is provided,
it is called with the child process identifier (PID)
as its argument whenever a child process finishes.
func-inlet may be a function symbol or an anonymous
function.

When called without arguments, sync returns a list of
PIDs for child processes whose results have not yet
been processed.

Examples:

```
; waiting for child processes
;------------------------------------------------------------
; wait for 10 seconds and process finished children
(sync 10000)

; wait for the maximum time
(sync -1)

; using an inlet function
;------------------------------------------------------------
(define (report pid)
  (println "process: " pid " has returned"))

; wait for up to 10 seconds and report finished children
(sync 10000 report)

; querying pending child processes
;------------------------------------------------------------
(sync)
;-> (245 246 247 248)

; periodic polling while waiting
;------------------------------------------------------------
(until (true? (sync 10 report))
  (println (time-of-day)))
```

Notes:

- sync blocks only when a timeout value is specified.
- Without arguments, sync returns immediately.
- A negative timeout waits indefinitely.
- The inlet function is invoked once per completed
  child process.
- sync operates only on processes created by spawn.

sync is part of the Cilk API for process
synchronization and parallel execution.

See: [spawn](#f-spawn), [until](#f-until),
     [true?](#f-truep)

---


<a name="f-sys-error"></a>
## sys-error

```
syntax: (sys-error)
syntax: (sys-error int-error)
syntax: (sys-error 0)
```

Description:

Reports the last error generated by the underlying
operating system.

When called without arguments, sys-error returns a
list containing the system error number and the
corresponding error message text. If no system error
has occurred, nil is returned.

When int-error is greater than zero, sys-error returns
the error number and its associated error text without
changing the current error state.

Specifying 0 as int-error resets the stored system
error. After resetting, a subsequent call to
sys-error returns nil.

System errors are typically set when functions that
access system resources return nil. Common causes
include nonexistent files, insufficient permissions,
or exhaustion of system resources such as file
descriptors.

The exact error numbers and messages depend on the
platform C library.

Examples:

```
; opening a nonexistent file
;------------------------------------------------------------
(open "xyz" "r")
;-> nil

(sys-error)
;-> (2 "No such file or directory")

; resetting the system error
;------------------------------------------------------------
(sys-error 0)
;-> (0 "Unknown error: 0")

(sys-error)
;-> nil
```

Notes:

- sys-error reflects the last OS-level error.
- Error values originate from the platform C library.
- The error state persists until explicitly reset.

See: [last-error](#f-last-error), [net-error](#f-net-error)

---


<a name="f-sys-info"></a>
## sys-info

```
syntax: (sys-info [int-idx])
```

Description:

Returns internal runtime and system information.

When called without arguments, sys-info returns a list
of integers describing internal resource usage and
runtime state.

When int-idx is specified, only the element at that
index is returned. Negative indices may be used to
access elements from the end of the list.

The returned list contains the following entries:

```
offset  description
0       Number of allocated cells
1       Maximum number of cells (constant)
2       Number of symbols
3       Current evaluation / recursion level
4       Environment stack level
5       Maximum call stack depth (constant)
6       Parent process PID or 0
7       Current process PID
8       Version number (integer)
9       Operating system and feature flags
```

The operating system value uses the following base
constants:

```
1 = Linux  
2 = BSD  
3 = macOS  
```

Additional feature flags may be ORed into this value:

```
+1024  FFI support enabled  
+512   IPv6 support enabled  
+256   64-bit runtime  
+128   UTF-8 support enabled  
+64    Library build  
```

To preserve compatibility with future extensions, it
is recommended to access fixed fields using offsets
0 through 5, and to use negative offsets for entries
near the end of the list.

Examples:

```
(sys-info)
;-> (429 268435456 402 1 0 2048 0 19453 10406 1155)

(sys-info 3)
;-> 1

(sys-info -2)
;-> 10406
```

Notes:

- The maximum number of cells can be adjusted using
  the -m command-line option.
- The maximum call stack depth can be adjusted using
  the -s command-line option.
- The exact contents of the list may grow in future
  versions, but existing offsets remain stable.

See: [sys-error](#f-sys-error), [last-error](#f-last-error)

---


<a name="f-t-test"></a>
## t-test

```
syntax: (t-test list-vector number-value)
syntax: (t-test list-vector-A list-vector-B [true])
syntax: (t-test list-vector-A list-vector-B float-probability)
```

Description:

Performs Student’s t-tests for comparing mean values.

In the first syntax, t-test performs a one-sample
Student’s t-test comparing the mean of list-vector
against number-value.

The result is returned as a list with the following
elements:

```
index  name        description
-----  ----------  ----------------------------------------
0      mean        mean of data in vector
1      value       value to compare
2      sdev        standard deviation of data
3      mean-error  standard error of the mean
4      t           t statistic
5      df          degrees of freedom
6      p           two-tailed probability
```

Examples:

```
; one-sample t-test
;------------------------------------------------------------
(t-test '(3 5 4 2 5 7 4 3) 2.5)
;-> (4.125 2.5 1.552 0.549 2.960 7 0.021)
```

In the second syntax, t-test compares the means of
two data vectors using a two-sample Student’s t-test.

When the optional true flag is omitted or nil, the
samples are treated as independent and may differ
in length.

When the flag is true, a paired t-test is performed,
assuming both vectors contain measurements from the
same subjects taken under different conditions.

The returned result list contains:

```
index  name    description
-----  ------  -----------------------------------------
0      mean-a  mean of group A
1      mean-b  mean of group B
2      sdev-a  standard deviation of group A
3      sdev-b  standard deviation of group B
4      t       t statistic
5      df      degrees of freedom
6      p       two-tailed probability
```

Examples:

```
; two independent samples
;------------------------------------------------------------
(set hours-sleep-8 '(5 7 5 3 5 3 3 9))
(set hours-sleep-4 '(8 1 4 6 6 4 1 2))

(t-test hours-sleep-8 hours-sleep-4)
;-> (5 4 2.138 2.563 0.847 14 0.411)

; two related samples
;------------------------------------------------------------
(set mood-pre '(3 0 6 7 4 3 2 1 4))
(set mood-post '(5 1 5 7 10 9 7 11 8))

(t-test mood-pre mood-post true)
;-> (3.333 7 2.236 3.041 -3.143 8 0.0137)
```

In the third syntax, Welch’s t-test is used when the
variances of the two samples differ significantly.

When float-probability is specified, t-test first
performs an F-test on the variances. If the probability
of the F-ratio is below float-probability, Welch’s
t-test is applied. Specifying 1.0 forces the Welch
method unconditionally.

Examples:

```
; Welch t-test forced
;------------------------------------------------------------
(t-test '(10 4 7 1 1 6 1 8 2 4)
        '(4 6 9 4 6 8 9 3)
        1.0)
;-> (4.4 6.125 3.239 2.357 -1.307 15 0.211)

; standard t-test
;------------------------------------------------------------
(t-test '(10 4 7 1 1 6 1 8 2 4)
        '(4 6 9 4 6 8 9 3))
;-> (4.4 6.125 3.239 2.357 -1.260 16 0.226)
```

Notes:

- Smaller float-probability values restrict the use
  of Welch’s t-test to cases with stronger variance
  differences.
- Probability values p express two-tailed significance.
- Results assume normally distributed samples.

See: [mean](#f-mean), [sdev](#f-sdev), [variance](#f-variance)

---


<a name="f-tan"></a>
## tan

```
syntax: (tan num-radians)
```

Description:

Calculates the tangent of the angle specified by
num-radians and returns the result.

The argument is interpreted as an angle in radians.
The result is returned as a floating point number.

Examples:

```
(tan 1)
;-> 1.557407725

(set pi (mul 2 (asin 1)))
;-> 3.141592654

(tan (div pi 4))
;-> 1
```

Notes:

- The argument is given in radians, not degrees.
- tan returns a floating point value.
- Results near odd multiples of pi/2 grow rapidly.

See: [sin](#f-sin), [cos](#f-cos), [atan](#f-atan)

---


<a name="f-tanh"></a>
## tanh

```
syntax: (tanh num-radians)
```

Description:

Calculates the hyperbolic tangent of num-radians.

The hyperbolic tangent is defined as the ratio of
the hyperbolic sine to the hyperbolic cosine:

tanh(x) = sinh(x) / cosh(x)

The argument is interpreted as a real number. The
result is returned as a floating point value.

Examples:

```
(tanh 1)
;-> 0.761594156

(tanh 10)
;-> 0.9999999959

(tanh 1000)
;-> 1

(= (tanh 1) (div (sinh 1) (cosh 1)))
;-> true
```

Notes:

- tanh approaches 1 as the absolute value of the
  argument increases.
- The function operates on real numbers.
- Results are returned as floating point values.

See: [sinh](#f-sinh), [cosh](#f-cosh), [atanh](#f-atanh)

---


<a name="f-term"></a>
## term

```
syntax: (term symbol)
```

Description:

Returns the term part of symbol as a string, without
the context prefix.

The term is the symbol name following the context
separator. The returned value is always a string.

Examples:

```
(set ctx:var 123)
(set sm 'ctx:var)

(string sm)
;-> "ctx:var"

(term sm)
;-> "var"

; reconstructing the original symbol
;------------------------------------------------------------
(set s 'ctx:var)

(= s (sym (term s) (prefix s)))
;-> true
```

Notes:

- term operates on symbols only.
- The context prefix is not included in the result.
- The return value is a string, not a symbol.

See: [prefix](#f-prefix), [symbol](#f-symbol),
     [sym](#f-sym)

---


<a name="f-throw"></a>
## throw

```
syntax: (throw exp)
```

Description:

Transfers control to the nearest enclosing catch
expression and supplies exp as the result value.

throw works together with catch. When throw is
executed, evaluation stops at that point and the
corresponding catch returns immediately.

The value of exp is assigned to the result symbol
specified in the catch call, or returned directly
when catch is used without a result symbol.

throw does not signal an error condition. It is a
controlled, non-error flow transfer mechanism.

Examples:

```
; basic throw and catch
;------------------------------------------------------------
(define (throw-test)
  (repeat (x 1000)
    (if (= x 500) (throw "interrupted"))))

(catch (throw-test) 'result)
;-> true

result
;-> "interrupted"

; short form returning the thrown value
;------------------------------------------------------------
(catch (throw-test))
;-> "interrupted"

; early exit from an expression block
;------------------------------------------------------------
(catch (begin
  ...
  (if (foo X) (throw X) Y)
  ...
))
```

Notes:

- throw exits only the dynamic extent of the
  nearest enclosing catch.
- throw does not raise an error or exception.
- Use throw-error to signal user-defined errors.

See: [catch](#f-catch), [throw-error](#f-throw-error)

---


<a name="f-throw-error"></a>
## throw-error

```
syntax: (throw-error exp)
```

Description:

Signals a user-defined error exception with the
message produced by evaluating exp.

throw-error aborts normal evaluation and raises an
error condition. The error message is reported as a
user error and can be handled like any other runtime
error using error handlers, error-event, or the
catch form that captures error exceptions.

Unlike throw, throw-error represents an actual error
state and is intended for invalid arguments or
illegal conditions detected by user code.

Examples:

```
; raising a user-defined error
;------------------------------------------------------------
(define (func x y)
  (if (= x 0)
      (throw-error "first argument cannot be 0"))
  (+ x y))

(func 1 2)
;-> 3

(func 0 2)
;-> ERR: user error : first argument cannot be 0
;-> called from user-defined function func
```

Notes:

- throw-error raises an error exception.
- Evaluation stops immediately when the error is
  raised.
- The error can be intercepted using error handlers
  or catch forms that handle error exceptions.
- Use throw for controlled, non-error flow transfer.

See: [throw](#f-throw), [catch](#f-catch),
     [error-event](#f-error-event), [last-error](#f-last-error)

---


<a name="f-time"></a>
## time

```
syntax: (time int-year int-month int-day [int-hour int-min int-sec])
syntax: (time list-date-time)
syntax: (time)
```

Description:

Converts a given UTC date and time into the number of
seconds since January 1st, 1970 00:00:00. The first form
accepts separate numeric fields for year, month, and day,
with hour, minute, and second optional. 

The second form accepts a list containing the same fields in
the same order.  Missing time fields default to zero.  

In the third form, `time` returns the timestamp for the
current system time.

The conversion always uses UTC and does not apply the
local time zone. 

Examples:

```
(time 2025 7 16)
;-> 1752624000

(time '(2025 7 16))
;-> 1752624000

(time 1970 1 1 0 0 0)
;-> 0

(datelist (time))
;-> (2026 1 3 6 41 26 3 6)
```

See: [date](#f-date), 
[datelist](#f-datelist),
[datestamp](#f-datestamp), 
[timelist](#f-timelist)

---


<a name="f-title-case"></a>
## title-case [utf8]

```
syntax: (title-case str [bool])
```

Description:

Returns a copy of the string in str with its first
character converted to uppercase.

When the optional bool argument evaluates to a
non-nil value, the remainder of the string is
converted to lowercase. When bool is nil or omitted,
only the first character is affected.

The original string is not modified.

Examples:

```
(title-case "hello")
;-> "Hello"

(title-case "hELLO" true)
;-> "Hello"

(title-case "hELLO")
;-> "HELLO"
```

Notes:

- title-case operates on UTF-8 encoded strings.
- A new string is always returned.

See: [lower-case](#f-lower-case),
     [upper-case](#f-upper-case)

---


<a name="f-trace"></a>
## trace

```
syntax: (trace int-device)
syntax: (trace true)
syntax: (trace nil)
syntax: (trace)
```

Description:

Controls tracing and interactive debugging of
expression evaluation.

When called with int-device, trace writes a continuous
log of all expression entries and exits to the given
device. The device is typically a file descriptor
returned by open. When int-device is 1, output is
written to standard output.

When called with a value evaluating to true, trace
enables interactive debugger mode. In this mode,
execution stops at each expression entry and exit
and waits for user input.

Expressions are highlighted by enclosing them in
the trace highlight character (default '#'). This
character can be changed using trace-highlight.

When called with nil, trace disables tracing and
debugger mode and closes any open trace output.

When called without arguments, trace returns the
current tracing or debugging mode.

Examples:

```
; tracing to a file
;------------------------------------------------------------
(trace (open "trace.txt"))

(f1 x y)
(f2 x)

(trace nil)

; interactive debugger
;------------------------------------------------------------
(trace true)
;-> true

(f1 a b c)

(trace nil)
;-> nil

; setting breakpoints
;------------------------------------------------------------
(define (test-func x)
  (trace true)
  (+ x 1))

(test-func 10)

; using debug shortcut
;------------------------------------------------------------
(debug (f1 a b c))
```

Notes:

- Trace output records both entry into and exit from
  expressions.
- In debugger mode, execution waits for commands at
  each step.
- Supported debugger commands include stepping,
  continuing, and quitting.
- Arbitrary expressions can be evaluated at the
  debugger prompt to inspect or modify state.
- trace nil disables both tracing and debugging.

See: [debug](#f-debug), [trace-highlight](#f-trace-highlight)

---


<a name="f-trace-highlight"></a>
## trace-highlight

```
syntax: (trace-highlight str-pre str-post [str-header str-footer])
```

Description:

Configures how expressions are highlighted during
trace and interactive debugging.

The strings str-pre and str-post define the markers
used to enclose the currently active expression.
By default, the number sign '#' is used. The marker
strings may be up to seven characters long.

When the terminal supports control sequences, the
highlight strings may contain terminal control codes
to change color, intensity, or other attributes.

Optional str-header and str-footer strings control
the separator and debugger prompt. The header may be
up to 15 characters long and the footer up to 31
characters long.

Examples:

```
; replace default markers with >> and <<
;------------------------------------------------------------
(trace-highlight ">>" "<<")

; use terminal control sequences for emphasis
;------------------------------------------------------------
(trace-highlight "\027[1m" "\027[0m")
```

Notes:

- Highlighting applies only when trace or debugger
  mode is active.
- Control sequences depend on terminal capabilities.
- Excessively long marker strings are truncated.

See: [trace](#f-trace), [debug](#f-debug)

---


<a name="f-transpose"></a>
## transpose

```
syntax: (transpose matrix)
```

Description:

Transposes a matrix by reversing its rows and columns.

The argument matrix may be a nested list or an array.
The matrix may contain values of any data type.

Matrix dimensions are determined by the number of rows
and by the number of elements in the first row. During
transposition, matrices are made rectangular by
assuming nil for missing elements, ignoring superfluous
elements, or expanding non-list rows into repeated
elements.

Examples:

```
; basic list matrix
;------------------------------------------------------------
(set mtx '((1 2 3) (4 5 6)))

(transpose mtx)
;-> ((1 4) (2 5) (3 6))

; single-row matrix
;------------------------------------------------------------
(transpose (list (sequence 1 5)))
;-> ((1) (2) (3) (4) (5))

; mixed data types
;------------------------------------------------------------
(transpose '((a b) (c d) (e f)))
;-> ((a c e) (b d f))

; array matrix
;------------------------------------------------------------
(set arr (array 2 3 (sequence 1 6)))
(set mtx (transpose arr))

mtx
;-> ((1 4) (2 5) (3 6))

; uneven rows
;------------------------------------------------------------
(set mtx '((1 2 3) (4 5) (7 8 9)))

(transpose mtx)
;-> ((1 4 7) (2 5 8) (3 nil 9))

; non-list row expansion
;------------------------------------------------------------
(set mtx '((1 2 3) X (7 8 9)))

(transpose mtx)
;-> ((1 X 7) (2 X 8) (3 X 9))
```

Notes:

- The number of columns is defined by the length of
  the first row.
- Missing elements are treated as nil.
- Extra elements in rows beyond the first row length
  are ignored.
- Non-list rows are expanded across all columns.
- The result is always returned as a list matrix.

See: [det](#f-det), [invert](#f-invert),
     [mat](#f-mat), [multiply](#f-multiply)

---


<a name="f-trim"></a>
## trim [utf8]

```
syntax: (trim str)
syntax: (trim str str-char)
syntax: (trim str str-left-char str-right-char)
```

Description:

Removes leading and trailing characters from the
string str and returns a new string.

With the first syntax, all leading and trailing
whitespace characters are removed.

With the second syntax, characters specified in
str-char are stripped from both sides. When
str-char is an empty string, the space character
is assumed.

With the third syntax, different characters may be
trimmed from the left and right sides. Specifying
an empty string for one side disables trimming on
that side.

Examples:

```
(trim "   hello \n ")
;-> "hello"

(trim "   h e l l o   ")
;-> "h e l l o"

(trim "----hello-----" "-")
;-> "hello"

(trim "00012340" "0" "")
;-> "12340"

(trim "1234000" "" "0")
;-> "1234"

(trim "----hello=====" "-" "=")
;-> "hello"
```

Notes:

- trim operates on UTF-8 encoded strings.
- The original string is not modified.
- For complex pattern-based stripping, use replace.
- When possible, trim is preferred for performance.

See: [replace](#f-replace),
     [lower-case](#f-lower-case),
     [upper-case](#f-upper-case)

---


<a name="f-truep"></a>
## true?

```
syntax: (true? exp)
```

Description:

Evaluates the expression exp and returns true if the
result is neither nil nor the empty list (). Otherwise,
nil is returned.

true? uses the same truth semantics as if. Both nil
and the empty list () are considered false.

Examples:

```
(map true? '(x 1 "hi" (a b c) nil ()))
;-> (true true true true nil nil)

(true? nil)
;-> nil

(true? '())
;-> nil
```

Notes:

- true? evaluates its argument before testing.
- Both nil and () are treated as false.
- All other values are treated as true.

See: [if](#f-if), [not](#f-not), [nil?](#f-nilp)

---


<a name="f-unify"></a>
## unify

```
syntax: (unify exp-1 exp-2 [list-env])
```

Description:

Evaluates and attempts to unify exp-1 and exp-2.

Two expressions unify if they are equal, or if one of
them is an unbound variable which can be consistently
bound to the other expression. When both expressions
are lists, unification proceeds recursively by
comparing corresponding subexpressions.

Unbound variables are symbols starting with an
uppercase character. On success, unify returns an
association list of variable bindings. When no
variables are bound but the match succeeds, an empty
list is returned. When unification fails, nil is
returned.

The underscore symbol '_' matches any atom, list, or
unbound variable and never binds.

An optional association list list-env may be supplied
to pre-bind variables. This is useful when chaining
multiple unification steps.

unify implements a unification algorithm with a
correctly applied occurs check, preventing infinite
or circular bindings.

Examples:

```
; simple matches
;------------------------------------------------------------
(unify 'A 'A)
;-> ()

(unify 'A 123)
;-> ((A 123))

(unify '(A B) '(x y))
;-> ((A x) (B y))

(unify 'abc 'xyz)
;-> nil

; aliasing and structure
;------------------------------------------------------------
(unify '(A B) '(B abc))
;-> ((A abc) (B abc))

(unify '(f A) '(f B))
;-> ((A B))

(unify '(f A) '(g B))
;-> nil

(unify '(f A) 'A)
;-> nil

; nested structures
;------------------------------------------------------------
(unify '(f (g A)) '(f B))
;-> ((B (g A)))

(unify '(f (g A) A) '(f B xyz))
;-> ((B (g xyz)) (A xyz))

; wildcard _
;------------------------------------------------------------
(unify '(A b _) '(x G z))
;-> ((A x) (G b))

(unify '(A b c _) '(x G _ z))
;-> ((A x) (G b))

(unify '(A b _) '(x G (x y z)))
;-> ((A x) (G b))

; using an environment
;------------------------------------------------------------
(unify '(f X) '(f 123))
;-> ((X 123))

(unify '(A B) '(X A) '((X 123)))
;-> ((X 123) (A 123) (B 123))
```

Notes:

- unify does not assign variables globally.
  It returns logical bindings as a list.
- The occurs check prevents infinite bindings.
- The '_' symbol matches but never binds.
- unify is commonly used as a comparison functor
  in search and replacement functions.

Use unify with expand to substitute bound variables:

```
(set bindings (unify '(f (g A) A) '(f B xyz)))
;-> ((B (g xyz)) (A xyz))

(expand '(f (g A) A) bindings)
;-> (f (g xyz) xyz)
```

Use unify with bind to destructure data:

```
(bind (unify '((A B) C) '((one "two") 3)))

A
;-> one

B
;-> "two"

C
;-> 3
```

See: [match](#f-match), [expand](#f-expand),
     [bind](#f-bind), [find](#f-find),
     [ref](#f-ref), [replace](#f-replace)

---


<a name="f-union"></a>
## union

```
syntax: (union list-1 list-2 [list-3 ... ])
```

Description:

Returns a list containing the unique elements found
in two or more lists.

Elements are collected from the input lists in order
of appearance. Each element appears only once in the
result, even if it occurs multiple times in the input
lists.

Examples:

```
(union '(1 3 1 4 4 3) '(2 1 5 6 4))
;-> (1 3 4 2 5 6)
```

Notes:

- union preserves the order of first occurrence.
- Duplicate elements are removed.
- Comparison is performed using equal semantics.

See: [difference](#f-difference), [intersect](#f-intersect),
     [unique](#f-unique)

---


<a name="f-unique"></a>
## unique

```
syntax: (unique list)
```

Description:

Returns a list containing the elements of list with
all duplicate entries removed.

The order of elements is preserved based on their
first occurrence in the input list.

Examples:

```
(unique '(2 3 4 4 6 7 8 7))
;-> (2 3 4 6 7 8)
```

Notes:

- The input list does not need to be sorted.
- unique performs faster on already sorted lists.
- Element comparison uses equal semantics.

See: [difference](#f-difference), [intersect](#f-intersect),
     [union](#f-union)

---


<a name="f-unless"></a>
## unless

```
syntax: (unless exp-condition body)
```

Description:

Evaluates the expressions in body only when
exp-condition evaluates to nil or the empty list ().

If body is executed, the result of its last
expression is returned. If body is not executed,
the return value of exp-condition is returned.

Because unless has no else branch, multiple
expressions in body do not need to be grouped
using begin.

Examples:

```
(unless (starts-with (read-line) "quit")
  (process (current-line))
  ...
  (finish))
```

Notes:

- unless uses the same truth semantics as if.
- The empty list () is treated as false.
- unless is the logical inverse of when.

See: [when](#f-when), [if](#f-if)

---


<a name="f-unpack"></a>
## unpack

```
syntax: (unpack str-format str-addr-packed)
syntax: (unpack str-format num-addr-packed)

syntax: (unpack struct num-addr-packed)
syntax: (unpack struct str-addr-packed)
```

Description:

Unpacks binary data into values according to a
specified format or a struct definition.

When the first argument is a format string,
unpack decodes the binary data found in
str-addr-packed or at the memory address
num-addr-packed. This is the reverse operation
of pack.

Using a numeric address allows unpacking data
returned from imported shared library functions.

When the first argument is a struct symbol,
unpack uses the layout defined by the struct.
In this mode, alignment padding is handled
automatically according to data types and CPU
architecture.

If num-addr-packed does not point to valid memory,
a bus error or segmentation fault may occur.

When unpacking structures containing NULL pointers,
conversion to strings will raise an error. Use
void* in struct definitions when NULL pointers
are expected.

Format specifiers:

```
code  description
----  -------------------------------------------------
c     signed 8-bit integer
b     unsigned 8-bit integer
d     signed 16-bit integer
u     unsigned 16-bit integer
ld    signed 32-bit integer
lu    unsigned 32-bit integer
Ld    signed 64-bit integer
Lu    unsigned 64-bit integer
f     32-bit float
lf    64-bit double float
sn    string of n null-padded ASCII characters
nn    n null bytes
>     switch to big-endian byte order
<     switch to little-endian byte order
```

Examples:

```
; basic packing and unpacking
;------------------------------------------------------------
(pack "c c c" 65 66 67)
;-> "ABC"

(unpack "c c c" "ABC")
;-> (65 66 67)

(set buf (pack "c d u" 10 12345 56789))
(unpack "c d u" buf)
;-> (10 12345 56789)

; strings and floats
;------------------------------------------------------------
(set buf (pack "s10 f" "result" 1.23))
(unpack "s10 f" buf)
;-> ("result\000\000\000\000" 1.230000019)

(set buf (pack "s3 lf" "result" 1.23))
(unpack "s3 f" buf)
;-> ("res" 1.23)

; null padding
;------------------------------------------------------------
(set buf (pack "c n7 c" 11 22))
(unpack "c n7 c" buf)
;-> (11 22)

; byte order control
;------------------------------------------------------------
(set buf (pack "d" 1))
(unpack "d" buf)
;-> (1)

(unpack ">d" buf)
;-> (256)

; different pack and unpack formats
;------------------------------------------------------------
(set buf (pack "s3" "ABC"))
(unpack "c c c" buf)
;-> (65 66 67)
```

Notes:

- unpack is a low-level operation.
- Invalid memory addresses may crash the process.
- Byte order switches affect all multi-byte numbers.
- The format string may include spaces for readability.
- If the buffer is smaller than specified, trailing
  format elements may be ignored.

See: [pack](#f-pack), [struct](#f-struct),
     [address](#f-address), [get-int](#f-get-int),
     [get-long](#f-get-long), [get-char](#f-get-char),
     [get-string](#f-get-string)

---


<a name="f-until"></a>
## until

```
syntax: (until exp-condition [body])
```

Description:

Repeatedly evaluates body while exp-condition
evaluates to nil or the empty list ().

Evaluation stops when exp-condition evaluates to
any value other than nil or (). The return value
of until is the result of the last expression
evaluated in body.

If body is omitted, until repeatedly evaluates
exp-condition and returns its last value.

until is functionally equivalent to:

(while (not exp-condition) ...)

During execution, until updates the system
iterator symbol $idx.

Examples:

```
(device (open "somefile.txt" "read"))
(set line-count 0)

(until (not (read-line))
  (inc line-count))

(close (device))
(print "the file has " line-count " lines\n")
```

Notes:

- until tests the condition before each iteration.
- Both nil and () are treated as false.
- The iterator symbol $idx is updated automatically.
- Use do-until to test the condition after executing
  the body expressions.

See: [do-until](#f-do-until), [while](#f-while),
     [not](#f-not)

---


<a name="f-upper-case"></a>
## upper-case [utf8]

```
syntax: (upper-case str)
```

Description:

Returns a copy of the string in str with all
characters converted to uppercase.

International (UTF-8) characters are converted
correctly. The original string is not modified.

Examples:

```
(upper-case "hello world")
;-> "HELLO WORLD"
```

Notes:

- upper-case operates on UTF-8 encoded strings.
- A new string is always returned.

See: [lower-case](#f-lower-case),
     [title-case](#f-title-case)

---


<a name="f-utf8"></a>
## utf8

```
syntax: (utf8 str-unicode)
```

Description:

Converts a UCS-4 (4-byte) Unicode-encoded string
into UTF-8.

utf8 is the inverse operation of unicode. It is
only meaningful when UCS-4 encoded strings are in
use. On systems using UTF-8 natively, this function
is typically not required.

When evaluated as a symbol, utf8 can also be used
to test whether UTF-8 support is available.

On big-endian architectures, byte order is handled
accordingly during conversion.

Examples:

```
(unicode "new")
;-> "n\000\000\000e\000\000\000w\000\000\000\000\000\000\000"

(utf8 (unicode "new"))
;-> "new"

; testing for UTF-8 support
;------------------------------------------------------------
(if utf8
  (do-utf8-version-of-code)
  (do-ascii-version-of-code))
```

Notes:

- utf8 converts from UCS-4 to UTF-8.
- utf8 and unicode are inverse operations.
- Most systems use UTF-8 natively.
- The function is available only when UTF-8
  support is enabled.

See: [unicode](#f-unicode),
     [lower-case](#f-lower-case),
     [upper-case](#f-upper-case)

---


<a name="f-utf8len"></a>
## utf8len

```
syntax: (utf8len str)
```

Description:

Returns the number of characters in a UTF-8 encoded
string.

UTF-8 characters may consist of multiple bytes.
utf8len counts Unicode characters, not bytes. This
differs from length, which returns the number of
bytes in the string.

This function is available only when UTF-8 support
is enabled.

Examples:

```
(utf8len "我能吞下玻璃而不伤身体。")
;-> 12

(length "我能吞下玻璃而不伤身体。")
;-> 36
```

Notes:

- utf8len counts characters, not bytes.
- length counts raw bytes.
- On UTF-8 systems, utf8len should be used when
  character count matters.

See: [length](#f-length), [unicode](#f-unicode),
     [utf8](#f-utf8)

---


<a name="f-uuid"></a>
## uuid

```
syntax: (uuid [str-node])
```

Description:

Constructs and returns a UUID (Universally Unique
Identifier).

When called without arguments, uuid returns a
randomly generated type 4 UUID.

When the optional str-node argument is specified,
uuid returns a type 1 UUID derived from a timestamp
and node identifier. The node identifier may be a
valid MAC address of a network adapter or a randomly
generated node ID.

When a random node ID is used, the least significant
bit of the first node byte should be set to 1 to
avoid collisions with real MAC addresses.

UUID generation follows RFC 4122.

Examples:

```
; type 4 UUID (random)
;------------------------------------------------------------
(uuid)
;-> "493AAD61-266F-48A9-B99A-33941BEE3607"

; type 1 UUID using a node ID
;------------------------------------------------------------
; node ID for MAC 00:14:51:0a:e0:bc
(set id (pack "cccccc" 0x00 0x14 0x51 0x0a 0xe0 0xbc))

(uuid id)
;-> "0749161C-2EC2-11DB-BBB2-0014510AE0BC"
```

Notes:

- Each call to uuid returns a new identifier.
- Type 4 UUIDs are randomly generated.
- Type 1 UUIDs are time-based and include a node ID.
- No system-wide shared store is required.
- For distributed systems, type 1 UUIDs should use
  distinct node IDs per node.
- Multiple processes on the same node can safely
  generate UUIDs concurrently.

See: [pack](#f-pack), [time](#f-time)

---


<a name="f-wait-pid"></a>
## wait-pid

```
syntax: (wait-pid int-pid [int-options | nil])
```

Description:

Waits for a child process to terminate and returns
its process identifier and termination status.

The child process must have been created previously
using process or fork. When the specified child
terminates, wait-pid returns a list containing the
pid and a status value describing the reason for
termination.

When int-pid is:

- a positive value: waits for the specified child
- -1: waits for any child process of the caller
- 0: waits for children in the same process group
- another negative value: waits for the process
  group specified by the absolute value

The interpretation of the returned status value is
platform-specific. Consult the Unix documentation
for waitpid for details.

An optional int-options argument may be supplied to
control behavior. When nil is specified instead of
int-options, wait-pid operates in non-blocking mode
and returns immediately. In this case, a pid value
of 0 indicates that no child process has terminated.

Examples:

```
; wait for a specific child
;------------------------------------------------------------
(set pid (fork (my-process)))

(set ret (wait-pid pid))
;-> (8596 0)

(println "process: " pid
         " has finished with status: "
         (last ret))

; non-blocking polling for any child
;------------------------------------------------------------
(until (not (first (wait-pid -1 nil))))
```

Notes:

- wait-pid blocks unless a non-blocking option is
  specified.
- Status values depend on the Unix implementation.
- Use non-blocking mode for event loops or polling.
- Only available on Unix-like systems.

See: [process](#f-process), [fork](#f-fork),
     [sync](#f-sync), [exit](#f-exit)

---


<a name="f-when"></a>
## when

```
syntax: (when exp-condition body)
```

Description:

Evaluates the expressions in body only when
exp-condition evaluates to a value other than
nil or the empty list ().

If body is executed, the result of its last
expression is returned. If body is not executed,
nil or () is returned.

Because when has no else branch, multiple
expressions in body do not need to be grouped
using begin.

Examples:

```
(when (read-line)
  (set result (analyze (current-line)))
  (report result)
  (finish))
```

Notes:

- when uses the same truth semantics as if.
- The empty list () is treated as false.
- when is the logical complement of unless.

See: [unless](#f-unless), [if](#f-if)

---


<a name="f-while"></a>
## while

```
syntax: (while exp-condition body)
```

Description:

Repeatedly evaluates body while exp-condition
evaluates to a value other than nil or the empty
list ().

Evaluation stops when exp-condition evaluates to
nil or (). The return value of while is the result
of the last expression evaluated in body.

During execution, while updates the system
iterator symbol $idx.

Examples:

```
(device (open "somefile.txt" "read"))
(set line-count 0)

(while (read-line)
  (inc line-count))

(close (device))
(print "the file has " line-count " lines\n")
```

Notes:

- while tests the condition before each iteration.
- Both nil and () are treated as false.
- The iterator symbol $idx is updated automatically.
- Use do-while to test the condition after executing
  the body expressions.

See: [do-while](#f-do-while), [until](#f-until),
     [not](#f-not)

---


<a name="f-write"></a>
## write

```
syntax: (write)
syntax: (write int-file str-buffer [int-size])
syntax: (write str str-buffer [int-size])
```

Description:

Writes data to a file, standard output, or appends
destructively to a string.

When called with int-file, write outputs int-size
bytes from str-buffer to the file descriptor obtained
from open. If int-size is omitted, all data in
str-buffer is written. The function returns the
number of bytes written, or nil on failure.

When called with a string as the first argument,
write appends data destructively to that string.

When called without arguments, write writes the
contents of the last read-line to standard output
(STDOUT).

write is a shorter form of write-buffer. The longer
name is deprecated and should be avoided in new code.

Examples:

```
; writing to a file
;------------------------------------------------------------
(set handle (open "file.ext" "write"))

(write handle data 100)
(write handle "a quick message\n")

; destructive string append
;------------------------------------------------------------
(set str "")
(write str "hello world")

str
;-> "hello world"

; write last read line to stdout
;------------------------------------------------------------
(write)
```

Notes:

- write operates on raw bytes.
- When writing to a string, the operation is
  destructive.
- write-buffer is deprecated in favor of write.

See: [read](#f-read), [open](#f-open),
     [close](#f-close)

---


<a name="f-write-char"></a>
## write-char

```
syntax: (write-char int-file int-byte1 [int-byte2 ... ])
```

Description:

Writes one or more bytes to a file specified by
the file handle int-file.

Each int-byte argument specifies an 8-bit byte
value to be written. The file handle must have
been obtained from a prior open operation.

Each call to write-char advances the file pointer
by one byte per value written. The function returns
the number of bytes written.

Examples:

```
; slow byte-by-byte file copy
;------------------------------------------------------------
(define (slow-file-copy from-file to-file)
  (set in-file (open from-file "read"))
  (set out-file (open to-file "write"))

  (while (set chr (read-char in-file))
    (write-char out-file chr))

  (close in-file)
  (close out-file)
  "finished")
```

Notes:

- write-char writes raw 8-bit byte values.
- Writing data byte-by-byte is slow and should be
  avoided for large transfers.
- Use write, print, or copy-file for efficient
  bulk output.

See: [read-char](#f-read-char), [write](#f-write),
     [copy-file](#f-copy-file), [open](#f-open)

---


<a name="f-write-file"></a>
## write-file

```
syntax: (write-file str-file-name str-buffer)
```

Description:

Writes the contents of str-buffer to a file specified
by str-file-name in a single operation.

On success, write-file returns the number of bytes
written. On failure, nil is returned.

When operating on local files, error details can be
retrieved using sys-error. When operating on URLs,
net-error provides additional error information.

write-file can also write to URLs. When the filename
starts with http://, the function behaves like
put-url and supports the same additional parameters.

Examples:

```
; write encrypted file
;------------------------------------------------------------
(write-file "file.enc"
  (encrypt (read-file "/home/user/file") "secret"))

; write to a remote URL
;------------------------------------------------------------
(write-file "http://example.com/message.txt"
  "This is a message")
```

Notes:

- write-file writes the entire buffer in one step.
- On failure, nil is returned.
- For local file errors, use sys-error.
- For URL operations, use net-error.
- HTTP writes behave like put-url.
- write-file can be used to transfer files to
  remote newLISP server nodes.

See: [read-file](#f-read-file),
     [append-file](#f-append-file),
     [put-url](#f-put-url),
     [sys-error](#f-sys-error),
     [net-error](#f-net-error)

---


<a name="f-write-line"></a>
## write-line

```
syntax: (write-line [int-file [str]])
syntax: (write-line str-out [str])
```

Description:

Writes a line to a device or appends a line to a
string.

When int-file is specified, the string in str and
a line-termination character are written to the
file handle obtained from open. If str is omitted,
the contents of the last read-line are written.

When both arguments are omitted, write-line writes
the last read-line to standard output (STDOUT) or
to the device currently set by device.

When the first argument is a string, write-line
appends the string and a line terminator
destructively to that string.

write-line returns the number of bytes written.

Examples:

```
; write a line to a file
;------------------------------------------------------------
(set out (open "file.txt" "write"))
(write-line out "hello there")
(close out)

; write last read line to stdout
;------------------------------------------------------------
(set in (open "init.txt" "read"))
(while (read-line in)
  (write-line))
(close in)

; append lines to a string
;------------------------------------------------------------
(set str "")
(write-line str "hello")
(write-line str "world")

str
;-> "hello\nworld\n"
```

Notes:

- write-line always appends a line terminator.
- When writing to a string, the operation is
  destructive.
- Use write to output data without a line
  terminator.

See: [write](#f-write), [read-line](#f-read-line),
     [open](#f-open), [close](#f-close)

---


<a name="f-xfer-event"></a>
## xfer-event

```
syntax: (xfer-event sym-event-handler | func-event-handler)
syntax: (xfer-event nil)
```

Description:

Registers a user-defined function to monitor byte
transfers during HTTP operations.

The event handler is called whenever a block of data
is transferred by functions such as get-url,
post-url, put-url, or by file functions that operate
on URLs, including load, save, read-file, write-file,
and append-file.

For each transferred data block, the handler function
is called with a single argument specifying the
number of bytes transferred.

When xfer-event is called with nil, the transfer
event handler is reset to its default state.

Examples:

```
; monitor download progress
;------------------------------------------------------------
(xfer-event (fn (n)
  (println "->" n)))

(length (get-url "http://example.com"))

; sample output:
;-> 73
;-> 799
;-> 1452
;-> 351
;-> 1093
;-> 352
;-> 211
;-> 885
;-> 564
;-> 884
;-> 561
;-> 75
;-> 812
;-> 638
;-> 1452
;-> 801
;-> 5
;-> 927
;-> 11935

; using a named handler
;------------------------------------------------------------
(define (report n)
  (println "->" n))

(xfer-event 'report)
```

Notes:

- The handler function is called once per data block.
- The argument passed is the size of the transferred
  block in bytes.
- Useful for monitoring progress of long-running
  uploads or downloads.
- HTTP-related functions only.

See: [get-url](#f-get-url), [post-url](#f-post-url),
     [put-url](#f-put-url), [read-file](#f-read-file),
     [write-file](#f-write-file)

---


<a name="f-xml-error"></a>
## xml-error

```
syntax: (xml-error)
```

Description:

Returns error information from the last xml-parse
operation.

If an error occurred during parsing, xml-error
returns a list containing two elements:

- an error message describing the problem
- the scan position in the source XML text,
  starting at index 0

If no error occurred, xml-error returns nil.

Examples:

```
(xml-parse "<tag1>hello</tag1><fin")
;-> nil

(xml-error)
;-> ("expected closing tag: >" 18)
```

Notes:

- xml-error reports only the most recent
  xml-parse error.
- The scan position refers to the original
  XML input string.
- When xml-parse succeeds, xml-error returns nil.

See: [xml-parse](#f-xml-parse)

---


<a name="f-xml-parse"></a>
## xml-parse

```
syntax: (xml-parse string-xml [int-options [sym-context [func-callback]]])
```

Description:

Parses a string containing XML 1.0 compliant,
well-formed XML and returns a list structure.

xml-parse does not perform DTD validation. Document
type declarations and processing instructions are
skipped. Nodes of type ELEMENT, TEXT, CDATA, and
COMMENT are parsed.

If an element has no attributes or child nodes, an
empty list is returned for that element. Attributes
are returned as association lists and can be accessed
using assoc.

When parsing fails due to malformed XML, xml-parse
returns nil and xml-error can be used to retrieve
error information.

Examples:

```
(set xml
  "<person name='John Doe' tel='555-1212'>nice guy</person>")

(xml-parse xml)
;-> (("ELEMENT" "person"
;     (("name" "John Doe") ("tel" "555-1212"))
;     (("TEXT" "nice guy"))))
```

Options:

The optional int-options argument controls how XML
input is translated. The following option values
can be combined by addition:

```
value  description
-----  ---------------------------------------------
1      suppress whitespace TEXT nodes
2      suppress empty attribute lists
4      suppress COMMENT nodes
8      translate string tags into symbols
16     add SXML attribute tags (@ ...)
```

Examples:

```
; parse without options
;------------------------------------------------------------
(xml-parse (read-file "example.xml"))

; suppress whitespace, empty attributes, and comments
;------------------------------------------------------------
(xml-parse (read-file "example.xml") (+ 1 2 4))
```

Tag Translation:

When option 8 is specified, XML tags are translated
from strings into symbols. The optional sym-context
parameter specifies the target context for symbol
creation. If omitted, the current context is used.

If the specified context does not exist, it will be
created automatically.

Examples:

```
(xml-type-tags nil nil nil nil)
(xml-parse "<msg>Hello World</msg>" (+ 1 2 4 8 16) 'CTX)
;-> ((CTX:msg "Hello World"))
```

SXML Output:

When xml-type-tags is configured to suppress XML
type tags and options 1, 2, 4, 8, and 16 are used,
xml-parse produces SXML-style output.

Examples:

```
(xml-type-tags nil nil nil nil)
(xml-parse (read-file "example.xml") (+ 1 2 4 8 16))
;-> ((DATABASE (@ (name "example.xml"))
;     (FRUIT (NAME "apple") (COLOR "red") (PRICE "0.80"))
;     (FRUIT (NAME "orange") (COLOR "orange") (PRICE "1.00"))
;     (FRUIT (NAME "banana") (COLOR "yellow") (PRICE "0.60"))))
```

Callback Processing:

When func-callback is specified, xml-parse calls the
callback function after each tag is closed. The
callback receives three arguments:

- the generated S-expression
- the start position in the source XML
- the length of the source fragment

Examples:

```
(define (xml-callback s-expr start size)
  (when (or (= (s-expr 0) 'NAME)
            (= (s-expr 0) 'COLOR)
            (= (s-expr 0) 'PRICE))
    (print "parsed expression:" s-expr)
    (println ", source:" (start size example-xml))))

(xml-type-tags nil 'cdata '!-- nil)
(xml-parse (read-file "example.xml") (+ 1 2 8) MAIN xml-callback)
```

Notes:

- XML tag names are case-sensitive and preserved.
- Namespace separators ':' are translated to '.'
  when tags are converted to symbols.
- xml-parse blocks until parsing is complete,
  unless a callback function is used.

See: [xml-error](#f-xml-error),
     [xml-type-tags](#f-xml-type-tags)

---


<a name="f-xml-type-tags"></a>
## xml-type-tags

```
syntax: (xml-type-tags [exp-text-tag exp-cdata-tag exp-comment-tag exp-element-tag])
```

Description:

Controls suppression or translation of XML type tags
used by xml-parse.

The XML type tags affected are:

- "TEXT"
- "CDATA"
- "COMMENT"
- "ELEMENT"

Each parameter specifies how the corresponding tag
is handled:

- nil        suppresses the tag completely
- a symbol   replaces the tag with that symbol
- a string  replaces the tag with that string

xml-type-tags modifies only the tag names. It does
not suppress or alter the associated content. To
remove whitespace text nodes, empty attribute lists,
or comments, use option numbers in xml-parse.

When called without arguments, xml-type-tags returns
the currently active type tags.

Examples:

```
; show current XML type tags
;------------------------------------------------------------
(xml-type-tags)
;-> ("TEXT" "CDATA" "COMMENT" "ELEMENT")

; suppress TEXT and ELEMENT, rename CDATA and COMMENT
;------------------------------------------------------------
(xml-type-tags nil 'cdata '!-- nil)
```

Notes:

- xml-type-tags affects subsequent xml-parse calls.
- Only the tag labels are modified or suppressed.
- Content handling is controlled by xml-parse options.

See: [xml-parse](#f-xml-parse)

---


<a name="f-zerop"></a>
## zero? [bigint]

```
syntax: (zero? exp)
```

Description:

Evaluates exp and returns true if the result is
the numeric value 0 (zero). Otherwise, nil is
returned.

Both integer and floating-point zero are accepted.
For all non-numeric data types, zero? returns nil.

Examples:

```
(set value 1.2)
(set var 0)

(zero? value)
;-> nil

(zero? var)
;-> true

(map zero? '(0 0.0 3.4 4))
;-> (true true nil nil)

(map zero? '(nil true 0 0.0 "" ()))
;-> (nil nil true true nil nil)
```

Notes:

- zero? operates only on numeric values.
- Both integer and floating-point zero are treated
  as zero.
- Non-numeric values always return nil.

See: [number?](#f-numberp), [true?](#f-truep),
     [nil?](#f-nilp)

---


<!-- ------------ NON CORE abc order ------------ -->


<a name="f-fin-fv"></a>
## fin-fv

```
syntax: (fin-fv num-rate num-nper num-pmt num-pv [int-type])
```

Description:

Computes the future value of a payment stream defined by
a constant interest rate, periodic fixed payments, and an
initial principal amount. `num-rate` is the interest rate
per period. `num-nper` is the number of periods. `num-pmt` is
the payment applied each period. `num-pv` is the principal
value at the beginning of the schedule.

When `int-type` is omitted or `0`, payments occur at the end
of each period. When `int-type` is `1`, payments occur at the
beginning of each period. The returned value represents
the balance after all interest and payments have been
applied.

Examples:

```
; end-of-period payment
(fin-fv (div 0.07 12) 240 775.30 -100000)
;-> -0.5544645052

; beginning-of-period payment
(fin-fv (div 0.07 12) 240 775.30 -100000 1)
;-> 54.093...
```

Notes:

- `num-rate` must be expressed as the rate per period
- `int-type` controls timing of payments
- signs of `num-pv` and `num-pmt` define cash direction

See: [fin-irr](#f-fin-irr), [fin-nper](#f-fih-nper), 
     [fin-npv](#f-fin-npv), [fin-pmt](#f-fin-pmt), 
     [fin-pv](#f-fin-pv)

---


<a name="f-sta-bayes-query"></a>
## sta-bayes-query

```
syntax: (sta-bayes-query list-L context-D [bool-chain [bool-probs]])
```

Description:

Computes category probabilities for the tokens in `list-L`
using the trained dictionary `context-D`. The dictionary
contains token frequencies or probability values for one
or more categories. The function returns one probability
per category.

Two compounding modes exist:

1. Inverse Chi2 mode (default):
   Frequencies are combined using the inverse Chi2
   method. Zero-frequency tokens reduce a category but do
   not remove it.
2. Chain Bayesian mode (`bool-chain` = true):
   Posterior values become priors for the next token.
   Zero-frequency tokens eliminate the affected category
   from subsequent steps.

When `bool-probs` is true, dictionary entries are treated
as probabilities instead of raw counts.

The returned list contains probability values in the same
order as the categories stored in `context-D`. A higher
value means a higher estimated probability that the
tokens in `list-L` belong to that category. Values are real
numbers in [0.0, 1.0]. In Chain Bayesian mode, eliminated
categories may prevent the list from summing to 1.0.


Examples:

**1. Classification using two short sample texts**

- Category A text: sunny day bright sky
- Category B text: storm rain dark clouds

Token frequencies:

```
(set Sample:A '(
  (sunny  1)
  (day    1)
  (bright 1)
  (sky    1)
))

(set Sample:B '(
  (storm  1)
  (rain   1)
  (dark   1)
  (clouds 1)
))

(set Sample:total '(
  (A-total 4)
  (B-total 4)
))
```

Tokens similar to category A:

```
(sta-bayes-query '(bright sky) Sample)
;-> (0.8571428571 0.1428571428)

; first value = probability for category A
; second value = probability for category B
```

Tokens similar to category B:

```
(sta-bayes-query '(dark rain) Sample)
;-> (0.1428571428 0.8571428571)

; higher value indicates the more likely category
```

**2. Chain Bayesian mode**

```
(set C:pos '(8 18))
(set C:neg '(2 72))
(set C:total '(10 90))
```

```
(sta-bayes-query '(test-positive) C true)
;-> (0.3076923077 0.6923076923)

(sta-bayes-query '(test-positive test-positive) C true)
;-> (0.64 0.36)
```

**3. Using direct probabilities**

```
(set P:pos '(0.8 0.2))
(set P:neg '(0.2 0.8))
(set P:total '(0.1 0.9))
```

```
(sta-bayes-query '(test-positive) P true true)
;-> (0.3076923077 0.6923076923)

(sta-bayes-query '(test-positive test-positive) P true true)
;-> (0.64 0.36)

(sta-bayes-query '(test-positive test-positive test-positive) P true true)
;-> (0.8767123288 0.1232876712)
```

Notes:

- Inverse Chi2 mode tolerates missing tokens.
- Chain Bayesian mode eliminates categories with
  zero-frequency tokens for the observed items.
- `list-L` should refer to known tokens unless category
  elimination is intended.

See: [sta-bayes-train](#f-sta-bayes-train), [parse](#f-parse)

---


<a name="f-sta-bayes-train"></a>
## sta-bayes-train

```
syntax: (sta-bayes-train list-M1 [list-M2 ... ] sym-context-D)
```

Description:

Builds or updates a Bayesian dictionary inside the context
`sym-context-D`. Each `list-Mi` represents a category. The
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

`sta-bayes-train` constructs the frequency model that
`sta-bayes-query` evaluates. The minimal text-classification
example shown in `sta-bayes-query` can be reproduced entirely
using `sta-bayes-train`. This provides a complete training and
classification workflow inside Rebel.

**Example: two small “documents” forming two categories**

- Category A text: sunny day bright sky 
- Category B text: storm rain dark clouds

These two lists define the categories A and B. Each token
will be counted and stored inside the dictionary Weather.

```
(sta-bayes-train
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
`sta-bayes-query` in the sample classification examples.


**Using the trained dictionary for classification**

The same model can now classify short texts:

Tokens typical for category A:

```
(sta-bayes-query '(bright sky) Weather)
;-> (0.8571428571 0.1428571428)

; first value = probability for category A
; second value = probability for category B
```

Tokens typical for category B:

```
(sta-bayes-query '(dark rain) Weather)
;-> (0.1428571428 0.8571428571)

; higher value indicates the more likely category
```

This demonstrates the complete cycle:

- `sta-bayes-train` builds the model
- `sta-bayes-query` evaluates new data against that model

**Example: training with strings**

String tokens become symbols by prepending "_". The
following example shows three categories, trained
simultaneously:

```
(sta-bayes-train
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

**Explicit model construction without `sta-bayes-train`**

When the data set is small and token counts are known
directly, the model may be created manually without
training:

```
(set Data:tested-positive '(8 18))
(set Data:tested-negative '(2 72))
(set Data:total '(10 90))
```

Such data sets can still be evaluated using `sta-bayes-query`.

**Multi-stage training**

Large corpora can be trained in several batches:

```
(sta-bayes-train  docA-part1 '() 'Model)
(sta-bayes-train  docA-part2 '() 'Model)
(sta-bayes-train  '() docB-part1 'Model)
(sta-bayes-train  '() docB-part2 'Model)
```

`sta-bayes-train` will update all counts and maintain correct
totals. All batches must provide the same number of
categories as the original model.

Notes:

- A category list may be empty to indicate missing
  training data for that category.
- Token order inside lists is irrelevant; only frequency
  matters.
- The total symbol cannot be used as a token name inside
  the training lists.
- Contexts used for training must be created in `MAIN` context 
  when training from inside another context.

See: [sta-bayes-query](#f-sta-bayes-query), [context](#f-context)

---


<a name="f-sta-beta"></a>
## sta-beta

```
syntax: (sta-beta cum-a num-b)
```

Description:

Computes the Beta function of two arguments `a` and `b`,
derived from the log Gamma function `sta-gammaln`. The value
is calculated using the relation

```
beta(a, b) = exp(gammaln(a) + gammaln(b) - gammaln(a + b))
```

and returned as a floating point number. The function is
typically used with positive real parameters.

Examples:

```
(sta-beta 1 2)
; → 0.5
```

Notes:

- Implemented via `sta-gammaln` to improve numerical stability
  for a wide range of arguments.
- Commonly used in probability and statistics, especially
  in conjunction with the Beta distribution.

See: [sta-gammaln](#f-sta-gammaln)

---


<a name="f-sta-betai"></a>
## sta-betai

```
syntax: (sta-betai num-x num-a num-b)
```

Description:

Computes the incomplete Beta function at point `x` using
parameters `a` and `b`. The result corresponds to the
cumulative probability of the Beta distribution at `x`.
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
(sta-betai 0.5 3 8)
; → 0.9453125
```

Notes:

- Useful for computing binomial tail probabilities and
  for statistical distributions derived from Beta.
- Parameters `a` and `b` must be positive.
- Related to the standard Beta function and the Gamma
  function.

See: [sta-binomial](#f-sta-binomial), [sta-beta](#f-sta-beta)

---


<a name="f-sta-binomial"></a>
## sta-binomial

```
syntax: (sta-binomial int-n int-k float-p)
```

Description:

Computes the probability that an event with probability `p`
occurs exactly `k` times in `n` independent trials. The
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
(sta-binomial 10 3 0.5)
; → 0.1171875
```

Notes:

- Models the probability of exactly k successes in n
  Bernoulli trials.
- For cumulative binomial probability, use betai.

See: [sta-betai](#f-sta-betai), [sta-beta](#f-sta-beta)

---


<a name="f-sta-corr"></a>
## sta-corr

```
syntax: (sta-corr list-vector-X list-vector-Y)
```

Description:

Computes the Pearson product-moment correlation between
two numerical vectors. `list-vector-X` and `list-vector-Y`
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
(set study-time '(90 100 130 150 180 200 220 300 350 400))
;-> (90 100 130 150 180 200 220 300 350 400)

(set test-errors '(25 28 20 20 15 12 13 10 8 6))
;-> (25 28 20 20 15 12 13 10 8 6)

(sta-corr study-time test-errors)
;-> (-0.926 29.241 -0.064 -6.944 8 0.0001190)
```

See: [stats](#f-sta-stats), [pow](#f-pow)

---


<a name="f-sta-crit-chi2"></a>
## sta-crit-chi2

```
syntax: (sta-crit-chi2 num-probability int-df)
```

Description:

Computes the critical Chi^2 value for a given confidence
probability and degrees of freedom. The returned number
is the threshold above which an observed Chi^2 statistic
would be considered significant under the null
hypothesis. `num-probability` is the confidence level, and
`int-df` is the number of degrees of freedom.

This function is typically used in hypothesis testing to
find the cutoff point for Chi^2 distributions. A higher
probability yields a larger critical value.

Examples:

```
(sta-crit-chi2 0.01 4)
;-> 13.27670443
```

See: [sta-prob-chi2](#f-sta-prob-chi2)

---


<a name="f-sta-crit-f"></a>
## sta-crit-f

```
syntax: (sta-crit-f num-probability int-df1 int-df2)
```

Description:

Computes the critical value of the F distribution for a
given confidence probability. `num-probability` is the
desired confidence level, and `int-df1` and `int-df2` are the
numerator and denominator degrees of freedom. The
returned number is the cutoff point above which an
observed F statistic would be considered significant
under the null hypothesis.

This function is used in variance comparisons and general
F-tests where the shape of the distribution depends on
two separate degrees of freedom.

Examples:

```
(sta-crit-f 0.05 10 12)
;-> 2.753386727
```

See: [sta-prob-f](#f-sta-prob-f)

---


<a name="f-sta-crit-t"></a>
## sta-crit-t

```
syntax: (sta-crit-t num-probability int-df)
```

Description:

Computes the critical value of the Student t distribution
for a given confidence probability and degrees of
freedom. `num-probability` specifies the desired confidence
level, and `int-df` is the number of degrees of freedom.
The returned value is the threshold above which an
observed t statistic would be judged significant under
the null hypothesis.

This function is commonly used in one-sample and
two-sample t-tests to determine whether the magnitude of
a t statistic is large enough to reject the null
hypothesis.

Examples:

```
(sta-crit-t 0.05 14)
;-> 1.761310142
```

See: [sta-prob-t](#f-sta-prob-t)

---


<a name="f-sta-crit-z"></a>
## sta-crit-z

```
syntax: (sta-crit-z num-probability)
```

Description:

Computes the critical value of the standard normal
distribution for a given cumulative probability.
`num-probability` specifies the point on the Z curve where
the cumulative area reaches that probability. The
returned value is used when evaluating statistical
significance or constructing confidence intervals based
on normal theory.

Examples:

```
(sta-crit-z 0.999)
;-> 3.090232372
```

See: [sta-prob-z](#f-sta-prob-z)

---


<a name="f-sta-gammai"></a>
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


<a name="f-sta-gammaln"></a>
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


