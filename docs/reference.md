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
syntax: (! str [int])
```

Description:

Executes the operating system command given in `str`. The
command is run through the system shell as a subprocess. The
return value depends on the shell and executed program. When
an optional integer flag is supplied, it may alter execution
behavior in implementation-defined ways.

Returns nil on failure. Use `sys-error` for diagnostics.

Examples:

```
(! "echo Hello Rebel")
(! "ls -l")
```

Notes:

- Executes external commands through the system shell.
- Returns nil when execution fails.
- Use `sys-error` for detailed error information.

See: [exec](#f-exec), [process](#f-process)

---

<a name="f-not-equal"></a>
## !=

```
syntax: (!= val [val ...])
```

Description:

Returns true when all arguments differ from each other. If any
two values compare as equal, the function stops evaluation and
returns nil.

Examples:

```
(!= 1 2 3)       -> true
(!= 1 1)         -> nil
(!= "a" "b" "a") -> nil
```

Notes:

- Implements inequality for multiple arguments.
- Stops at the first repetition.

See: [=](#f-equal), [<](#f-less), [>](#f-greater)

---

<a name="f-dollar"></a>
## $

```
syntax: ($ int)
```

Description:

Returns the value of the capture group at index `int` from the
most recent regex match. Capture groups start at index 1.

Returns nil if the index is invalid or if no match is available.

Examples:

```
(regex "([A-Z]+)-([0-9]+)" "REBEL-42")
($ 1) -> "REBEL"
($ 2) -> "42"
```

Notes:

- Depends on the previous successful regex match.
- Indexing starts at 1.

See: [regex](#f-regex), [find](#f-find), [find-all](#f-find-all)

---

<a name="f-mod-op"></a>
## %

```
syntax: (% int [int ...])
```

Description:

Computes the modulo of the first integer by each subsequent
integer in sequence. All arguments must be integers.

Examples:

```
(% 10 3)     -> 1
(% 20 3 2)   -> 0
```

Notes:

- Performs left-associative modulo.
- All arguments must be integers.

See: [div](#f-div), [mul](#f-mul)

---

<a name="f-amp"></a>
## &

```
syntax: (& int int [int ...])
```

Description:

Computes the bitwise AND of all integer arguments. Returns an
integer where each bit is set if it is set in all arguments.

Examples:

```
(& 7 3)     -> 3
(& 15 1 7)  -> 1
```

Notes:

- Bitwise operation on integers.
- All arguments must be integers.

See: [|](#f-or-bit), [^](#f-xor)

---

<a name="f-mul"></a>
## *

```
syntax: (* int [int ...])
```

Description:

Returns the product of all integer arguments. With one
argument, returns the argument unchanged.

Examples:

```
(* 3 4)     -> 12
(* 2 3 5)   -> 30
(* 7)       -> 7
```

Notes:

- Pure arithmetic multiplication.
- At least one argument required.

See: [+](#f-add), [-](#f-sub)

---

<a name="f-add"></a>
## +

```
syntax: (+ int [int ...])
```

Description:

Returns the sum of all integer arguments. With one argument,
returns the argument unchanged.

Examples:

```
(+ 1 2 3)  -> 6
(+ 10)     -> 10
```

Notes:

- Standard arithmetic addition.
- Accepts any number of arguments.

See: [*](#f-mul), [-](#f-sub)

---

<a name="f-inc"></a>
## ++

```
syntax: (++ val [int])
```

Description:

Increments the value stored in `val` by the given integer.
Modifies the target in place and returns the new value.

With no integer argument, increments by 1.

Examples:

```
(set 'x 10)
(++ x)     -> 11
(++ x 5)   -> 16
```

Notes:

- Destructive: modifies its argument.
- Works only on mutable storage locations.

See: [--](#f-dec), [inc](#f-inc-fn)

---

<a name="f-sub"></a>
## -

```
syntax: (- int [int ...])
```

Description:

Subtracts all subsequent integers from the first one. With one
argument, returns the negated value.

Examples:

```
(- 10 3)    -> 7
(- 10 3 2)  -> 5
(- 7)       -> -7
```

Notes:

- Left-associative subtraction.
- Unary form negates the argument.

See: [+](#f-add), [*](#f-mul)

---

<a name="f-dec"></a>
## --

```
syntax: (-- val [int])
```

Description:

Decrements the value stored in `val` by the given integer.
Modifies the target in place and returns the new value.

With no integer argument, decrements by 1.

Examples:

```
(set 'x 10)
(-- x)     -> 9
(-- x 3)   -> 6
```

Notes:

- Destructive: modifies its argument.
- Works only on mutable storage locations.

See: [++](#f-inc), [dec](#f-dec-fn)

---

<a name="f-div"></a>
## /

```
syntax: (/ int [int ...])
```

Description:

Performs integer division. With one argument, returns the
reciprocal as a floating-point number.

With multiple arguments, divides left to right.

Examples:

```
(/ 10 2)     -> 5
(/ 20 2 5)   -> 2
(/ 4)        -> 0.25
```

Notes:

- Integer division for multiple arguments.
- Single-argument form returns 1/arg as float.

See: [%](#f-mod-op), [*](#f-mul)

---

<a name="f-colon"></a>
## :

```
syntax: (: val lst [val ...])
```

Description:

Applies a method or operation to an object-like structure.
This operator delegates evaluation to the underlying type and
is used internally by several sequence and context operations.

Examples:

```
; Usage depends on context and data types.
```

Notes:

- Internal dispatch operator.
- Behavior depends on the operand type.

See: [apply](#f-apply), [map](#f-map)

---

<a name="f-less"></a>
## <

```
syntax: (< val [val ...])
```

Description:

Returns true if each argument is strictly less than the next.
All comparisons are evaluated left to right.

With one argument, compares the value to 0.

Examples:

```
(< 1 2 3)    -> true
(< 3 2)      -> nil
(< 5)        -> false   ; 5 < 0
```

Notes:

- Multi-argument chained comparison.
- Unary form compares to zero.

See: [>](#f-greater), [<=](#f-leq)

---

<a name="f-shl"></a>
## <<

```
syntax: (<< int int [int ...])
```

Description:

Performs a left bit shift on integers. With multiple arguments,
shifts left sequentially using each shift amount.

Examples:

```
(<< 1 3)      -> 8
(<< 2 1 2)    -> 16
```

Notes:

- Bitwise left shift.
- All arguments must be integers.

See: [>>](#f-shr), [&](#f-amp)

---

<a name="f-leq"></a>
## <=

```
syntax: (<= val [val ...])
```

Description:

Returns true if each argument is less than or equal to the
next. Evaluates left to right.

With one argument, compares the value to 0.

Examples:

```
(<= 1 2 2)  -> true
(<= 3 2)    -> nil
(<= -1)     -> true
```

Notes:

- Multi-argument chained comparison.
- Unary form compares to zero.

See: [<](#f-less), [>=](#f-geq)

---

<a name="f-equal"></a>
## =

```
syntax: (= val [val ...])
```

Description:

Checks whether all arguments compare equal. Returns true when
all values match; returns nil as soon as a mismatch is found.

Examples:

```
(= 1 1 1)        -> true
(= 1 2)          -> nil
(= "a" "a")      -> true
```

Notes:

- Compares values using Rebel’s equality rules.
- Stops at the first mismatch.

See: [!=](#f-not-equal), [eq?](#f-eq)

---

<a name="f-greater"></a>
## >

```
syntax: (> val [val ...])
```

Description:

Returns true if each argument is strictly greater than the
next. Evaluated left to right.

With one argument, compares the value to 0.

Examples:

```
(> 3 2 1)   -> true
(> 1 2)     -> nil
(> -1)      -> false
```

Notes:

- Multi-argument chained comparison.
- Unary form compares to zero.

See: [<](#f-less), [>=](#f-geq)

---

<a name="f-geq"></a>
## >=

```
syntax: (>= val [val ...])
```

Description:

Returns true if each argument is greater than or equal to the
next. Evaluated left to right.

With one argument, compares the value to 0.

Examples:

```
(>= 3 3 1)  -> true
(>= 1 2)    -> nil
(>= 0)      -> true
```

Notes:

- Multi-argument chained comparison.
- Unary form compares to zero.

See: [>](#f-greater), [<=](#f-leq)

---

<a name="f-shr"></a>
## >>

```
syntax: (>> int int [int ...])
```

Description:

Performs a right bit shift on integers. With multiple shift
amounts, shifts sequentially from left to right.

Examples:

```
(>> 16 2)      -> 4
(>> 32 1 3)    -> 2
```

Notes:

- Bitwise right shift.
- All arguments must be integers.

See: [<<](#f-shl), [&](#f-amp)

---
<a name="f-nanp"></a>
## NaN?

```
syntax: (NaN? float)
```

Description:

Returns true if the given floating-point value is Not-a-Number
(NaN). Returns nil for any valid numeric value.

Examples:

```
(NaN? 0.0)          -> nil
(NaN? (/ 0.0 0.0))  -> true
```

Notes:

- Only checks floating-point NaN values.
- Integers never produce NaN.

See: [inf?](#f-infp), [float?](#f-floatp)

---

<a name="f-xor"></a>
## ^

```
syntax: (^ int int [int ...])
```

Description:

Computes the bitwise exclusive OR of all integer arguments.
Evaluation proceeds from left to right.

Examples:

```
(^ 1 3)       -> 2
(^ 7 1 2)     -> 4
```

Notes:

- Bitwise XOR for integers.
- All arguments must be integers.

See: [&](#f-amp), [|](#f-orbit)

---

<a name="f-abort"></a>
## abort

```
syntax: (abort [int])
```

Description:

Terminates the current process immediately.  
If an integer code is given, it is used as the exit status.
If omitted, a default nonzero exit status is used.

Examples:

```
(abort)
(abort 3)
```

Notes:

- Does not run cleanup handlers.
- Intended for abnormal termination.

See: [exit](#f-exit), [throw](#f-throw)

---

<a name="f-abs"></a>
## abs

```
syntax: (abs num)
```

Description:

Returns the absolute value of `num`. Negative numbers are
returned as their positive counterparts; zero and positive
values are returned unchanged.

Examples:

```
(abs -10)   -> 10
(abs 3.5)   -> 3.5
```

Notes:

- Works on both integers and floats.
- Non-numeric values return nil.

See: [sqrt](#f-sqrt), [sgn](#f-sgn)

---

<a name="f-acos"></a>
## acos

```
syntax: (acos float)
```

Description:

Returns the arc cosine of the given floating-point value.
The result is expressed in radians.

Examples:

```
(acos 1.0)     -> 0.0
(acos 0.0)     -> 1.570796
```

Notes:

- Argument must be in the range −1..1.
- Returns NaN outside the valid domain.

See: [asin](#f-asin), [atan](#f-atan)

---

<a name="f-acosh"></a>
## acosh

```
syntax: (acosh float)
```

Description:

Returns the inverse hyperbolic cosine of the given value.
The result is expressed in radians.

Examples:

```
(acosh 1.0)    -> 0.0
(acosh 2.0)    -> 1.316957
```

Notes:

- Domain is ≥ 1.0.
- Returns NaN for invalid inputs.

See: [asinh](#f-asinh), [atanh](#f-atanh)

---

<a name="f-add"></a>
## add

```
syntax: (add float [float ...])
```

Description:

Returns the sum of all floating-point arguments.  
Accepts any number of arguments. With one argument, returns it
unchanged.

Examples:

```
(add 1.5 2.5)     -> 4.0
(add 10.0)        -> 10.0
```

Notes:

- Similar to `+`, but intended for numeric float operations.
- Returns float even when arguments appear integer-like.

See: [+](#f-add), [sub](#f-sub)

---

<a name="f-address"></a>
## address

```
syntax: (address val)
```

Description:

Returns the memory address of the given value in the Rebel
runtime. This is primarily intended for FFI operations.

Examples:

```
(address "Rebelion")  -> integer address
```

Notes:

- Address values are platform-dependent.
- Should be used only with FFI-related functions.

See: [get-int](#f-get-int), [cpymem](#f-cpymem)

---

<a name="f-amb"></a>
## amb

```
syntax: (amb val [val ...])
```

Description:

Returns one of the provided arguments nondeterministically.
Each evaluation may return a different argument.

Examples:

```
(amb 1 2 3)   -> 1 or 2 or 3
```

Notes:

- Used for exploring nondeterministic branches.
- Do not rely on deterministic results.

See: [case](#f-case), [cond](#f-cond)

---

<a name="f-andfn"></a>
## and

```
syntax: (and exp-1 [exp-2 ...])
```

Description:

Evaluates expressions from left to right. If any expression
evaluates to nil or the empty list, evaluation stops and that
value is returned. If all expressions succeed, returns the
value of the last expression.

Examples:

```
(and (< 10 20) (> 5 1))    -> true
(and true "ok")            -> "ok"
(and '())                  -> ()
(and)                     -> true
```

Notes:

- Implements short-circuit logical AND.
- Returns last truthy value or nil/().

See: [or](#f-or), [not](#f-not)

---

<a name="f-append"></a>
## append

```
syntax: (append val [val ...])
```

Description:

Appends all arguments to form a new sequence.  
The type of the result matches the type of the first argument:

- list   → all arguments must be lists  
- array  → all arguments must be arrays  
- string → all arguments are converted to strings

This operation is nondestructive.

Examples:

```
(append '(1 2) '(3 4))       -> (1 2 3 4)
(append "Rebel " "Lang")     -> "Rebel Lang"
```

Notes:

- Does not modify input values.
- Safe for binary strings.

See: [extend](#f-extend), [push](#f-push)

---

<a name="f-append-file"></a>
## append-file

```
syntax: (append-file str-filename str-buffer)
```

Description:

Appends the contents of `str-buffer` to the file or URL
specified in `str-filename`. If the file does not exist, it is
created. Returns the number of bytes written, or nil on error.

When operating on files, use `sys-error` for detailed error
information. When operating on URLs, use `net-error`.

For HTTP targets, the request behaves like `put-url` with
"Pragma: append" automatically supplied.

Examples:

```
(append-file "output.txt" "More data\n")
(append-file "http://example.com/log.txt" "Entry")
```

Notes:

- Works with `file://` and `http://` URLs.
- Suitable for appending to remote Rebel nodes.

See: [write-file](#f-write-file), [read-file](#f-read-file)

---

<a name="f-apply"></a>
## apply

```
syntax: (apply func lst [int])
```

Description:

Invokes the function `func` using the list `lst` as its
arguments. The optional integer reduces nested lists during
application.

With no arguments, `(apply func)` calls the function without
arguments.

Examples:

```
(apply + '(1 2 3))    -> 6
(apply list '(a b))   -> (a b)
```

Notes:

- Useful for variadic function expansion.
- Arguments in `lst` are evaluated normally.

See: [map](#f-map), [fn](#f-fn)

---

<a name="f-args"></a>
## args

```
syntax: (args [int ...])
```

Description:

Returns command-line arguments passed to the current Rebel
process. Without arguments, returns the full argument list.
With indices, returns the specified argument positions.

Examples:

```
(args)        -> entire argument list
(args 0)      -> program name
(args 1 2)    -> selected args
```

Notes:

- Returns a list.
- Out-of-range indices produce nil.

See: [main-args](#f-main-args), [env](#f-env)

---

<a name="f-array"></a>
## array

```
syntax: (array int [int ...] [lst])
```

Description:

Creates a new array with the specified dimensions. If an
optional list is provided, it is used to initialize the array
contents in row-major order.

Examples:

```
(array 2 3)                 -> 2×3 array
(array 2 2 '(1 2 3 4))      -> [[1 2] [3 4]]
```

Notes:

- Dimensions must be positive integers.
- Excess initializer elements are ignored.

See: [array-list](#f-array-list), [first](#f-first)

---

<a name="f-array-list"></a>
## array-list

```
syntax: (array-list arr)
```

Description:

Converts an array into a nested list structure. Each row of the
array becomes a sublist.

Examples:

```
(array-list (array 2 2 '(1 2 3 4)))
-> ((1 2) (3 4))
```

Notes:

- Useful for list-based processing of array data.

See: [array](#f-array), [flat](#f-flat)

---

<a name="f-arrayp"></a>
## array?

```
syntax: (array? val)
```

Description:

Returns true if `val` is an array. Returns nil for all other
types.

Examples:

```
(array? (array 2 2))   -> true
(array? '(1 2 3))      -> nil
```

Notes:

- Strict type check.

See: [list?](#f-listp), [string?](#f-stringp)

---

<a name="f-asin"></a>
## asin

```
syntax: (asin float)
```

Description:

Returns the arc sine of the given floating-point value, in
radians.

Examples:

```
(asin 0.0)     -> 0.0
(asin 1.0)     -> 1.570796
```

Notes:

- Argument must be in the range −1..1.
- Returns NaN for invalid inputs.

See: [acos](#f-acos), [atan](#f-atan)

---

<a name="f-asinh"></a>
## asinh

```
syntax: (asinh float)
```

Description:

Returns the inverse hyperbolic sine of the given value.

Examples:

```
(asinh 0.0)    -> 0.0
(asinh 2.0)    -> 1.443635
```

Notes:

- Accepts any real number.
- Returns float.

See: [acosh](#f-acosh), [atanh](#f-atanh)

---
<a name="f-assoc"></a>
## assoc

```
syntax: (assoc val lst)
```

Description:

Searches the association list `lst` for a sublist whose first
element matches `val`. If found, the matching sublist is
returned. If no such entry exists, returns nil.

`assoc` does not search recursively; only the top-level pairs
are examined.

Examples:

```
(assoc 'a '((a 1) (b 2)))    -> (a 1)
(assoc 'z '((a 1) (b 2)))    -> nil
```

Notes:

- Looks only at the first element of each sublist.
- Suitable for simple key → value pairs.

See: [lookup](#f-lookup), [pop-assoc](#f-pop-assoc)

---

<a name="f-atan"></a>
## atan

```
syntax: (atan float)
```

Description:

Returns the arc tangent of the given floating-point value.
The result is expressed in radians.

Examples:

```
(atan 1.0)     -> 0.785398
(atan 0.0)     -> 0.0
```

Notes:

- Maps all real inputs to the range −pi/2..pi/2.

See: [atan2](#f-atan2), [asin](#f-asin)

---

<a name="f-atan2"></a>
## atan2

```
syntax: (atan2 float float)
```

Description:

Returns the arc tangent of `y/x` using the signs of both
arguments to determine the correct quadrant. The result is in
radians.

Examples:

```
(atan2 1.0 1.0)    -> 0.785398
(atan2 1.0 0.0)    -> 1.570796
```

Notes:

- Provides a full 2-argument arctangent.
- Handles all quadrants correctly.

See: [atan](#f-atan)

---

<a name="f-atanh"></a>
## atanh

```
syntax: (atanh float)
```

Description:

Returns the inverse hyperbolic tangent of the given value.

Examples:

```
(atanh 0.0)     -> 0.0
(atanh 0.5)     -> 0.549306
```

Notes:

- Domain is −1 < x < 1.
- Returns NaN for inputs outside the valid range.

See: [asinh](#f-asinh), [acosh](#f-acosh)

---

<a name="f-atomp"></a>
## atom?

```
syntax: (atom? val)
```

Description:

Returns true if `val` is an atom (i.e., not a list). Lists and
arrays return nil; all other values return true.

Examples:

```
(atom? 10)         -> true
(atom? "Rebel")    -> true
(atom? '(1 2))     -> nil
```

Notes:

- Tests for “non-listness”.
- Arrays are not atoms.

See: [list?](#f-listp), [nil?](#f-nilp)

---

<a name="f-base64-dec"></a>
## base64-dec

```
syntax: (base64-dec str)
```

Description:

Decodes the Base64-encoded string `str` and returns the raw
decoded data as a string. Returns nil on invalid input.

Examples:

```
(base64-dec "QUJD")    -> "ABC"
```

Notes:

- Result may contain binary data.
- Returns nil for malformed encoding.

See: [base64-enc](#f-base64-enc)

---

<a name="f-base64-enc"></a>
## base64-enc

```
syntax: (base64-enc str [bool])
```

Description:

Encodes the string `str` into Base64. When the optional boolean
flag is true, line breaks may be inserted for readability.

Examples:

```
(base64-enc "ABC")     -> "QUJD"
```

Notes:

- Produces an ASCII-safe representation.
- Optional flag controls line wrapping.

See: [base64-dec](#f-base64-dec)

---

<a name="f-bayes-query"></a>
## bayes-query

```
syntax: (bayes-query lst context [bool bool])
```

Description:

Queries a Bayesian classifier previously trained with
`bayes-train`. The list contains feature values; the context
represents the classifier. Optional boolean flags enable
chained evaluation and probability output.

Examples:

```
(bayes-query '(rain wet) BayesCtx)
```

Notes:

- Requires prior training with `bayes-train`.
- Context stores the classifier data.

See: [bayes-train](#f-bayes-train)

---

<a name="f-bayes-train"></a>
## bayes-train

```
syntax: (bayes-train lst [lst ...] context)
```

Description:

Trains a Bayesian classifier using one or more lists of feature
→ outcome pairs. The results are stored in the given context.

Examples:

```
(bayes-train '((rain yes) (dry no)) BayesCtx)
```

Notes:

- Context accumulates feature statistics.
- Used together with `bayes-query`.

See: [bayes-query](#f-bayes-query)

---

<a name="f-begin"></a>
## begin

```
syntax: (begin val ...)
```

Description:

Evaluates its arguments in order and returns the value of the
last expression. Useful for grouping multiple expressions into
a single block.

Examples:

```
(begin (set 'x 10) (+ x 5))    -> 15
```

Notes:

- Equivalent to a sequence block.
- Last expression determines the result.

See: [when](#f-when), [while](#f-while)

---

<a name="f-beta"></a>
## beta

```
syntax: (beta float float)
```

Description:

Computes the beta function for the given parameters. Returns a
floating-point value.

Examples:

```
(beta 2.0 3.0)
```

Notes:

- Used in probability and statistics.

See: [betai](#f-betai)

---

<a name="f-betai"></a>
## betai

```
syntax: (betai float float float)
```

Description:

Computes the incomplete beta function for the given arguments.

Examples:

```
(betai 0.5 2.0 3.0)
```

Notes:

- Generalization of the beta function.

See: [beta](#f-beta)

---

<a name="f-bigint"></a>
## bigint

```
syntax: (bigint number)
```

Description:

Converts the given number into an arbitrary-precision integer.
Suitable for high-precision computations.

Examples:

```
(bigint 12345678901234567890)
```

Notes:

- Stores values beyond native integer range.

See: [bigint?](#f-bigintp)

---

<a name="f-bigint-str"></a>
## bigint (string)

```
syntax: (bigint string)
```

Description:

Parses the given string as an arbitrary-precision integer.
Leading signs are recognized.

Examples:

```
(bigint "1234567890")
```

Notes:

- Accepts decimal string input.

See: [bigint?](#f-bigintp)

---

<a name="f-bigintp"></a>
## bigint?

```
syntax: (bigint? val)
```

Description:

Returns true if `val` is an arbitrary-precision integer.

Examples:

```
(bigint? (bigint 10))    -> true
(bigint? 10)             -> nil
```

Notes:

- Checks precise integer type only.

See: [integer?](#f-integerp)

---

<a name="f-bind"></a>
## bind

```
syntax: (bind lst [bool])
```

Description:

Temporarily binds variables to values based on a list of
symbol/value pairs. When evaluation finishes, all bindings are
restored to their previous values.

Examples:

```
(bind '((x 10) (y 20)) (+ x y))   -> 30
```

Notes:

- Provides dynamic variable binding.
- Optional boolean controls evaluation of values.

See: [let](#f-let), [local](#f-local)

---

<a name="f-binomial"></a>
## binomial

```
syntax: (binomial int int float)
```

Description:

Computes the probability of `k` successes in `n` trials using a
binomial distribution with probability `p`.

Examples:

```
(binomial 10 3 0.5)
```

Notes:

- n choose k combined with probability p.

See: [normal](#f-normal)

---

<a name="f-bits"></a>
## bits

```
syntax: (bits int [bool])
```

Description:

Returns a string representing the bit pattern of the given
integer. When the optional boolean is true, leading zeros may
be included.

Examples:

```
(bits 7)        -> "111"
```

Notes:

- Produces textual bit representation.

See: [|](#f-orbit), [&](#f-amp)

---

<a name="f-callback"></a>
## callback

```
syntax: (callback func str [str ...])
```

Description:

Registers a Rebel function as a callback that can be invoked
from foreign code via the FFI. The return type and parameter
types are given as strings, following FFI conventions.

Examples:

```
(callback 'handler "int" "int" "string")
```

Notes:

- Used with dynamic libraries.
- Requires correct C-level type strings.

See: [import](#f-import), [struct](#f-struct)

---

<a name="f-case"></a>
## case

```
syntax: (case val (val body) [(val body) ...])
```

Description:

Evaluates `val` and compares it to each case key in order.
When a matching key is found, the corresponding body is
evaluated and its result returned. If no case matches and a
default clause is present, the default body is used.

Examples:

```
(case 'b
  ('a "A")
  ('b "B")
  ('c "C"))
-> "B"
```

Notes:

- Keys are compared using Rebel’s equality rules.
- Lacks fall-through; only one branch executes.

See: [cond](#f-cond), [if](#f-if)

---

<a name="f-catch"></a>
## catch

```
syntax: (catch val [sym])
```

Description:

Evaluates `val` within a protected context. If a `throw` is
encountered, the thrown value is returned instead of aborting
execution. When a symbol is provided, the thrown value is bound
to that symbol.

Examples:

```
(catch (throw "err"))       -> "err"
(catch (throw 42) x)        -> 42
```

Notes:

- Provides controlled non-local exits.
- Works together with `throw`.

See: [throw](#f-throw), [throw-error](#f-throw-error)

---

<a name="f-ceil"></a>
## ceil

```
syntax: (ceil num)
```

Description:

Rounds `num` upward to the nearest integer, returning the
ceiling value as a float.

Examples:

```
(ceil 3.1)    -> 4
(ceil -2.8)   -> -2
```

Notes:

- Always rounds toward positive infinity.
- Returns a floating-point value.

See: [floor](#f-floor), [round](#f-round)

---

<a name="f-change-dir"></a>
## change-dir

```
syntax: (change-dir str)
```

Description:

Changes the current working directory to the path given in
`str`. Returns the new directory path on success, or nil on
failure.

Examples:

```
(change-dir "/tmp")
```

Notes:

- Path must exist and be accessible.
- Use `sys-error` for failure details.

See: [real-path](#f-real-path), [directory](#f-directory)

---

<a name="f-char"></a>
## char

```
syntax: (char int)
syntax: (char str [int [true]])
```

Description:

Converts an integer to a single-character string, or extracts a
character from a string. When extracting, an optional index
selects the character, and the optional true flag returns the
numeric code instead of the character.

Examples:

```
(char 65)          -> "A"
(char "Rebel" 1)   -> "e"
(char "Rebel" 1 true) -> 101
```

Notes:

- Out-of-range indices return nil.
- Character codes follow UTF-8 byte values.

See: [get-char](#f-get-char), [string](#f-string)

---

<a name="f-chop"></a>
## chop

```
syntax: (chop lst [int])
syntax: (chop str [int])
```

Description:

Removes elements or characters from the end of a list or
string. When an integer is supplied, removes that many items.
Without an integer, removes one element or character.

The original value is not modified; a new sequence is returned.

Examples:

```
(chop '(1 2 3))       -> (1 2)
(chop "Rebelion")    -> "Rebelio"
(chop "Rebelion" 3)  -> "Rebe"
```

Notes:

- Nondestructive operation.
- Works on lists and strings.

See: [pop](#f-pop), [slice](#f-slice)

---

<a name="f-clean"></a>
## clean

```
syntax: (clean func lst)
```

Description:

Filters `lst` by applying `func` to each element and returning a
new list containing only the elements for which the function
returns true.

Examples:

```
(clean number? '(1 a 2 b))   -> (1 2)
```

Notes:

- Equivalent to `filter`, but with explicit truth predicate.
- Does not modify the input list.

See: [filter](#f-filter), [exists](#f-exists)

---

<a name="f-close"></a>
## close

```
syntax: (close int)
```

Description:

Closes the file descriptor or handle represented by the
integer. Returns true on success or nil on error.

Examples:

```
(set 'fd (open "out.txt" "w"))
(close fd)
```

Notes:

- Always close descriptors after use.
- Use `sys-error` for failure information.

See: [open](#f-open), [read](#f-read)

---

<a name="f-collect"></a>
## collect

```
syntax: (collect val [int])
```

Description:

Repeatedly evaluates `val` and collects non-nil results into a
list. Evaluation continues until nil is returned or until the
optional maximum count is reached.

Examples:

```
(collect (read-line fd))
(collect (random 0 10) 5)
```

Notes:

- Useful for iterative data gathering.
- Stops on nil.

See: [map](#f-map), [filter](#f-filter)

---

<a name="f-command-event"></a>
## command-event

```
syntax: (command-event nil)
syntax: (command-event handler)
```

Description:

Assigns or clears a handler function for command-line input
events. When a handler is set, it is invoked whenever the REPL
receives a command-line event.

Examples:

```
(command-event 'handler)
(command-event nil)
```

Notes:

- Used only in interactive mode.
- Handler receives the raw input line.

See: [reader-event](#f-reader-event), [prompt-event](#f-prompt-event)

---

<a name="f-cond"></a>
## cond

```
syntax: (cond (test body) [(test body) ...])
```

Description:

Evaluates each test in order. When a test evaluates to true,
its corresponding body is executed and the result returned. If
no test succeeds, the result is nil.

Examples:

```
(cond
  ((> 10 20) 'no)
  ((= 1 1)   'yes))
-> yes
```

Notes:

- Tests are evaluated in order.
- Only the first matching branch is executed.

See: [case](#f-case), [if](#f-if)

---

<a name="f-cons"></a>
## cons

```
syntax: (cons val val)
```

Description:

Constructs a new list by attaching the first value as the head
and the second value as the tail. The tail must be a list.

Examples:

```
(cons 1 '(2 3))    -> (1 2 3)
(cons 'a '(b c))   -> (a b c)
```

Notes:

- Fundamental list constructor.
- Tail must be a list.

See: [list](#f-list), [rest](#f-rest)

---

<a name="f-constant"></a>
## constant

```
syntax: (constant sym val [sym val ...])
```

Description:

Defines one or more constant symbols bound to fixed values.
Once defined, the value of a constant cannot be changed.

Examples:

```
(constant PI 3.14159)
```

Notes:

- Attempts to modify a constant produce an error.
- Constants improve code clarity.

See: [set](#f-set), [global](#f-global)

---

<a name="f-context"></a>
## context

```
syntax: (context [sym])
syntax: (context sym val)
```

Description:

Returns the current context, or switches to another context.
When given a symbol and optional value, creates or selects that
context.

Examples:

```
(context)            -> current context
(context 'MyCtx)     -> switch/create
```

Notes:

- Contexts namespace symbols.
- Useful for module-like grouping.

See: [symbols](#f-symbols), [new](#f-new)

---

<a name="f-contextp"></a>
## context?

```
syntax: (context? val)
```

Description:

Returns true if `val` is a context object. Otherwise returns nil.

Examples:

```
(context? (context 'X))   -> true
(context? 10)             -> nil
```

Notes:

- Checks context identity only.

See: [context](#f-context), [symbols](#f-symbols)

---

<a name="f-copy"></a>
## copy

```
syntax: (copy val)
syntax: (copy int [bool])
```

Description:

Copies data from one value to a new value.  
When used with integers and the boolean flag, performs a raw
memory copy starting at an address.

Examples:

```
(copy '(1 2 3))     -> (1 2 3)
(copy 1000 true)    -> raw copy starting at address 1000
```

Notes:

- Regular values are deep-copied.
- Memory variant is FFI-related.

See: [address](#f-address), [cpymem](#f-cpymem)

---

<a name="f-copy-file"></a>
## copy-file

```
syntax: (copy-file str str)
```

Description:

Copies a file from the source path to the destination path.
Returns true on success, or nil on failure.

Examples:

```
(copy-file "from.txt" "to.txt")
```

Notes:

- Destination is overwritten if it exists.
- Use `sys-error` for error details.

See: [delete-file](#f-delete-file), [read-file](#f-read-file)

---

<a name="f-corr"></a>
## corr

```
syntax: (corr lst lst)
```

Description:

Computes the correlation coefficient between two numeric
vectors represented as lists. Returns a floating-point value.

Examples:

```
(corr '(1 2 3) '(2 4 6))   -> 1.0
```

Notes:

- Lists must have equal length.
- Non-numeric entries return nil.

See: [stats](#f-stats), [fft](#f-fft)

---

<a name="f-cos"></a>
## cos

```
syntax: (cos float)
```

Description:

Returns the cosine of the given value expressed in radians.

Examples:

```
(cos 0.0)    -> 1.0
```

Notes:

- Input is interpreted as radians.
- Periodic function.

See: [sin](#f-sin), [tan](#f-tan)

---

<a name="f-cosh"></a>
## cosh

```
syntax: (cosh float)
```

Description:

Returns the hyperbolic cosine of the given value.

Examples:

```
(cosh 0.0)   -> 1.0
```

Notes:

- Hyperbolic function.

See: [sinh](#f-sinh), [tanh](#f-tanh)

---


<a name="f-count"></a>
## count

```
syntax: (count lst lst)
```

Description:

Counts how many elements from the second list appear in the
first list. Returns an integer representing the number of
matches.

Elements are compared using Rebel’s equality rules.

Examples:

```
(count '(a b c a) '(a))        -> 2
(count '(1 2 3 4) '(2 4 6))    -> 2
```

Notes:

- Only exact matches are counted.
- Matching is linear; order is ignored.

See: [find](#f-find), [exists](#f-exists)

---

<a name="f-cpymem"></a>
## cpymem

```
syntax: (cpymem int int int)
```

Description:

Copies raw memory from one address to another.  
The first integer is the source address, the second is the
destination address, and the third is the byte count.

Examples:

```
(cpymem addr1 addr2 64)
```

Notes:

- FFI-level operation; unsafe if used incorrectly.
- Addresses must be valid memory locations.

See: [address](#f-address), [get-int](#f-get-int)

---

<a name="f-crc32"></a>
## crc32

```
syntax: (crc32 str)
```

Description:

Computes the CRC-32 checksum of the given string.  
Returns the checksum as an integer.

Examples:

```
(crc32 "Rebelion")   -> integer
```

Notes:

- Suitable for integrity checks.
- Works on binary strings.

See: [hash](#f-hash)   ; if added later

---

<a name="f-crit-chi2"></a>
## crit-chi2

```
syntax: (crit-chi2 float int)
```

Description:

Computes the critical value for the chi-square distribution with
the given probability and degrees of freedom.

Examples:

```
(crit-chi2 0.95 4)
```

Notes:

- Used in statistical analysis.

See: [crit-t](#f-crit-t), [crit-f](#f-crit-f)

---

<a name="f-crit-f"></a>
## crit-f

```
syntax: (crit-f float int int)
```

Description:

Computes the critical value for the F-distribution for the given
probability and degrees of freedom.

Examples:

```
(crit-f 0.95 2 10)
```

Notes:

- Statistical utility.

See: [crit-chi2](#f-crit-chi2), [crit-t](#f-crit-t)

---

<a name="f-crit-t"></a>
## crit-t

```
syntax: (crit-t float int)
```

Description:

Computes the critical value of Student's t-distribution with the
given probability and degrees of freedom.

Examples:

```
(crit-t 0.975 15)
```

Notes:

- Common in confidence-interval calculations.

See: [crit-f](#f-crit-f), [crit-z](#f-crit-z)

---

<a name="f-crit-z"></a>
## crit-z

```
syntax: (crit-z float)
```

Description:

Returns the critical value of the standard normal (Z) distribution
for the given probability.

Examples:

```
(crit-z 0.975)
```

Notes:

- Probability must be between 0 and 1.

See: [crit-t](#f-crit-t)

---

<a name="f-current-line"></a>
## current-line

```
syntax: (current-line)
```

Description:

Returns the current line number in the file being loaded or
evaluated. When not loading a file, returns nil.

Examples:

```
(current-line)
```

Notes:

- Useful inside scripts being loaded.

See: [load](#f-load), [read-line](#f-read-line)

---

<a name="f-curry"></a>
## curry

```
syntax: (curry func val)
```

Description:

Returns a new function with the first argument of `func` fixed
to the given value. The resulting function accepts the remaining
arguments when invoked.

Examples:

```
(set 'add5 (curry + 5))
(add5 10)    -> 15
```

Notes:

- Produces partially applied functions.

See: [fn](#f-fn), [map](#f-map)

---

<a name="f-date"></a>
## date

```
syntax: (date int [int])
syntax: (date)
```

Description:

Converts a UNIX timestamp into a human-readable date string.
An optional timezone offset may be supplied. Without arguments,
returns the current date and time.

Examples:

```
(date)             -> "2025-01-15 13:22:10"
(date 0)           -> epoch start
```

Notes:

- Format depends on system locale.
- Timezone is in minutes.

See: [date-list](#f-date-list), [now](#f-now)

---

<a name="f-date-list"></a>
## date-list

```
syntax: (date-list int [int])
syntax: (date-list)
```

Description:

Returns a list representing date and time components such as
(year month day hour minute second). Accepts an optional
timezone offset.

With no arguments, returns the current local date/time list.

Examples:

```
(date-list)
(date-list 0)
```

Notes:

- Useful for programmatic date calculations.

See: [date](#f-date), [date-value](#f-date-value)

---

<a name="f-date-parse"></a>
## date-parse

```
syntax: (date-parse str str)
```

Description:

Parses a date string according to a format string and returns a
UNIX timestamp. Format symbols follow standard POSIX rules.

Examples:

```
(date-parse "2025-04-03" "%Y-%m-%d")
```

Notes:

- Returns nil on invalid input.
- Format must match the input exactly.

See: [date-value](#f-date-value)

---

<a name="f-date-value"></a>
## date-value

```
syntax: (date-value int int int [int int int])
syntax: (date-value lst)
syntax: (date-value)
```

Description:

Converts date components (year, month, day, optional time) or a
date list into a UNIX timestamp. With no arguments, returns the
current timestamp.

Examples:

```
(date-value 2025 1 1)     -> timestamp
(date-value '(2025 4 3))  -> timestamp
```

Notes:

- Month is 1–12.
- Time components default to zero.

See: [date](#f-date), [date-list](#f-date-list)

---

<a name="f-debug"></a>
## debug

```
syntax: (debug func)
```

Description:

Enables debug tracing for the specified function. When enabled,
each call prints argument and evaluation information.

Examples:

```
(debug 'myfun)
```

Notes:

- Intended for development.
- Output goes to standard error.

See: [trace](#f-trace)

---

<a name="f-decfn"></a>
## dec

```
syntax: (dec val [num])
```

Description:

Decrements the numeric value stored in `val`. If no number is
given, decrements by 1. Modifies the location in place and
returns the new value.

Examples:

```
(set 'x 10)
(dec x)      -> 9
(dec x 5)    -> 4
```

Notes:

- Destructive update.
- Accepts only numeric deltas.

See: [inc](#f-inc), [--](#f-dec)

---

<a name="f-def-new"></a>
## def-new

```
syntax: (def-new sym [sym])
```

Description:

Copies a symbol definition (value or function) from one context
to another. If a second symbol is provided, it becomes the new
name in the target context.

Examples:

```
(def-new SourceCtx:fun TargetCtx:newfun)
```

Notes:

- Useful for context-based code reuse.

See: [context](#f-context), [new](#f-new)

---

<a name="f-default"></a>
## default

```
syntax: (default context)
```

Description:

Sets the given context as the default context for evaluating
symbols without explicit prefixes. Returns the assigned context.

Examples:

```
(default 'MyCtx)
```

Notes:

- Affects symbol resolution.
- Used for module-like behavior.

See: [context](#f-context), [symbols](#f-symbols)

---

<a name="f-define"></a>
## define

```
syntax: (define (name [param ...]) body ...)
syntax: (define name val)
```

Description:

Defines a new function or assigns a value to a symbol. When the
first form is used, `define` creates a lambda with optional
default parameters. In the second form, it sets a symbol’s value.

Examples:

```
(define (add x y) (+ x y))
(add 3 4) -> 7

(define A 42)
A -> 42
```

Notes:

- Functions created this way evaluate parameters normally.
- Accepts default argument values.

See: [define-macro](#f-define-macro), [fn](#f-fn)

---

<a name="f-define-macro"></a>
## define-macro

```
syntax: (define-macro (name [param ...]) body)
```

Description:

Defines a macro, a function that receives unevaluated arguments
and returns code to be evaluated in place of the call.

Examples:

```
(define-macro (twice x) '(+ ,x ,x))
(twice 10) -> 20
```

Notes:

- Macros manipulate raw syntax.
- Powerful but must be used carefully.

See: [macro?](#f-macrop), [letex](#f-letex)

---


<a name="f-delete"></a>
## delete

```
syntax: (delete sym [bool])
```

Description:

Deletes the given symbol from its context.  
If the optional boolean flag is true, deletion is forced even
when the symbol is protected.

Returns true on success or nil when deletion fails.

Examples:

```
(delete 'x)
(delete 'y true)
```

Notes:

- Removes both value and function bindings.
- Use with caution; deletion is permanent.

See: [set](#f-set), [global](#f-global)

---

<a name="f-delete-file"></a>
## delete-file

```
syntax: (delete-file str)
```

Description:

Deletes the file at the path specified by the string argument.
Returns true on success, or nil on failure.

Examples:

```
(delete-file "old.txt")
```

Notes:

- Use `sys-error` to obtain error details.

See: [copy-file](#f-copy-file), [write-file](#f-write-file)

---

<a name="f-delete-url"></a>
## delete-url

```
syntax: (delete-url str)
```

Description:

Sends an HTTP DELETE request to the given URL.  
Returns the response body as a string, or nil on error.

Examples:

```
(delete-url "http://example.com/item/42")
```

Notes:

- Works for HTTP targets only.
- Use `net-error` for networking diagnostics.

See: [get-url](#f-get-url), [append-file](#f-append-file)

---

<a name="f-destroy"></a>
## destroy

```
syntax: (destroy int [int])
```

Description:

Sends a termination signal to a process identified by the PID
given as the first argument. The optional second argument
specifies the signal number.

Examples:

```
(destroy 1234)
(destroy 1234 9)
```

Notes:

- Signal numbers depend on the operating system.

See: [abort](#f-abort), [exit](#f-exit)

---

<a name="f-det"></a>
## det

```
syntax: (det matrix [float])
```

Description:

Computes the determinant of the given matrix.  
An optional pivot value may be supplied to improve numerical
stability.

Examples:

```
(det '((1 2) (3 4)))   -> -2
```

Notes:

- Matrix must be square.
- Returns a floating-point value.

See: [invert](#f-invert), [transpose](#f-transpose)

---

<a name="f-device"></a>
## device

```
syntax: (device [int])
```

Description:

Sets or retrieves the current I/O device number.  
Without arguments, returns the active device. With a number,
changes the active device.

Examples:

```
(device)       -> current device
(device 1)     -> sets device 1
```

Notes:

- Device numbers depend on implementation.
- Primarily for interactive use.

See: [trace](#f-trace), [print](#f-print)

---

<a name="f-difference"></a>
## difference

```
syntax: (difference lst lst [bool])
```

Description:

Returns a new list containing all elements of the first list
that do not appear in the second list.  
If the optional boolean is true, duplicates may be preserved.

Examples:

```
(difference '(1 2 3) '(2))     -> (1 3)
```

Notes:

- Order is preserved.
- Equality uses Rebel’s comparison rules.

See: [intersect](#f-intersect), [union](#f-union)

---

<a name="f-directory"></a>
## directory

```
syntax: (directory [str])
syntax: (directory str str [val])
```

Description:

Returns a list of file names in a directory.  
With only a path, lists all entries.  
With a path and pattern, filters results. Optional regex or
comparison options may be provided.

Examples:

```
(directory "/tmp")
(directory "/tmp" ".*\\.log$")
```

Notes:

- Result does not include "." or "..".
- Pattern may be a regex.

See: [directory?](#f-directoryp), [real-path](#f-real-path)

---

<a name="f-directoryp"></a>
## directory?

```
syntax: (directory? str)
```

Description:

Returns true if the given path refers to an existing directory.

Examples:

```
(directory? "/tmp")    -> true
(directory? "file.txt")-> nil
```

Notes:

- Follows filesystem permissions and visibility.

See: [file?](#f-filep)

---

<a name="f-display-html"></a>
## display-html

```
syntax: (display-html str [bool])
```

Description:

Renders the given HTML string in a browser or embedded viewer,
depending on the environment. The optional boolean flag controls
whether the HTML is interpreted or shown as raw text.

Examples:

```
(display-html "<b>Rebel</b>")
```

Notes:

- Behavior varies with environment capabilities.

See: [print](#f-print)

---

<a name="f-div"></a>
## div

```
syntax: (div num num [num ...])
syntax: (div num)
```

Description:

Divides the first number by all subsequent numbers sequentially.
With a single argument, returns its reciprocal as a float.

Examples:

```
(div 20 2 5)     -> 2
(div 4)          -> 0.25
```

Notes:

- Similar to `/`, but accepts floats explicitly.
- Left-associative.

See: [/](#f-div-op), [%](#f-mod-op)

---

<a name="f-do-until"></a>
## do-until

```
syntax: (do-until test [body])
```

Description:

Evaluates `body` repeatedly until `test` becomes true.  
The test is evaluated before each iteration.

Examples:

```
(set 'x 0)
(do-until (> x 5) (set 'x (+ x 1)))
```

Notes:

- Pre-test loop.
- Stops as soon as the condition is true.

See: [do-while](#f-do-while), [while](#f-while)

---

<a name="f-do-while"></a>
## do-while

```
syntax: (do-while test body)
```

Description:

Evaluates `body` repeatedly while `test` remains true.  
`body` is guaranteed to run at least once.

Examples:

```
(set 'x 3)
(do-while (> x 0) (set 'x (- x 1)))
```

Notes:

- Post-test loop.

See: [while](#f-while), [do-until](#f-do-until)

---

<a name="f-doargs"></a>
## doargs

```
syntax: (doargs (sym [val]) body)
```

Description:

Iterates over the arguments passed to a function call.  
The symbol receives each argument in sequence; if the optional
break expression evaluates true, the loop terminates early.

Examples:

```
(define (show) (doargs (x) (println x)))
(show 1 2 3)
```

Notes:

- Only valid inside function bodies.
- Accesses caller arguments.

See: [args](#f-args), [map](#f-map)

---

<a name="f-dolist"></a>
## dolist

```
syntax: (dolist (sym seq [val]) body)
```

Description:

Iterates over each element of a list or array, binding the
symbol to each element and evaluating the body.  
An optional break expression may terminate iteration early.

Examples:

```
(dolist (x '(1 2 3)) (print x))
```

Notes:

- Works on lists and arrays.
- Similar to `foreach` in other languages.

See: [map](#f-map), [dotimes](#f-dotimes)

---

<a name="f-dostring"></a>
## dostring

```
syntax: (dostring (sym str [val]) body)
```

Description:

Iterates over characters in a string. On each iteration, the
symbol is bound to the current character. An optional break
condition may be supplied.

Examples:

```
(dostring (c "Rebel") (print c))
```

Notes:

- Processes strings character-by-character.

See: [dolist](#f-dolist), [dotimes](#f-dotimes)

---

<a name="f-dotimes"></a>
## dotimes

```
syntax: (dotimes (sym int [val]) body)
```

Description:

Executes the body a fixed number of times. The symbol receives
each index from 0 up to (count - 1). An optional break
expression can terminate the loop early.

Examples:

```
(dotimes (i 3) (print i))   -> prints 0 1 2
```

Notes:

- Index starts at 0.
- Count must be non-negative.

See: [dolist](#f-dolist), [for](#f-for)

---

<a name="f-dotree"></a>
## dotree

```
syntax: (dotree (sym ctx [bool]) body)
```

Description:

Iterates over all symbols in the given context.  
The optional boolean flag controls whether hidden symbols are
included.

Examples:

```
(dotree (s MyCtx) (println s))
```

Notes:

- Useful for enumerating context contents.

See: [symbols](#f-symbols), [context](#f-context)

---

<a name="f-dump"></a>
## dump

```
syntax: (dump [val])
```

Description:

Returns a serialized representation of the given value.  
Without arguments, returns a dump of all defined symbols.

Examples:

```
(dump "Rebel")
(dump)
```

Notes:

- Output is for inspection, not guaranteed stable across versions.

See: [print](#f-print), [source](#f-source)

---


<a name="f-dup"></a>
## dup

```
syntax: (dup val [int [bool]])
syntax: (dup val)
```

Description:

Returns a sequence containing repeated copies of `val`.  
When an integer is provided, it specifies how many copies to
produce. Without an integer, returns a two-element list
containing `val` twice.

If the optional boolean flag is true, a shallow copy is used.
Otherwise, values may be deep-copied depending on type.

Examples:

```
(dup 5 3)       -> (5 5 5)
(dup '(1 2) 2)  -> ((1 2) (1 2))
(dup "R")       -> ("R" "R")
```

Notes:

- Generates lists unless `val` is a string, in which case the
  result is a string.
- Count must be non-negative.

See: [extend](#f-extend), [push](#f-push)

---

<a name="f-emptyp"></a>
## empty?

```
syntax: (empty? val)
```

Description:

Returns true if `val` is an empty sequence, such as an empty
list, an empty string, or an array with zero length.  
Returns nil for all non-empty values and for non-sequence types.

Examples:

```
(empty? '())          -> true
(empty? "")           -> true
(empty? '(1))         -> nil
```

Notes:

- Checks size, not value content.
- Non-collections return nil.

See: [length](#f-length), [nil?](#f-nilp)

---

<a name="f-encrypt"></a>
## encrypt

```
syntax: (encrypt str str)
```

Description:

Encrypts the first string using the second string as a pad or
key. Returns the encrypted byte sequence as a string.

Decryption is performed by calling `encrypt` again with the same
pad, since this operation is symmetric.

Examples:

```
(encrypt "Rebel" "secretpad")
```

Notes:

- Suitable for simple pad-based encryption.
- Not intended for strong cryptography.

See: [base64-enc](#f-base64-enc)

---

<a name="f-ends-with"></a>
## ends-with

```
syntax: (ends-with lst val)
syntax: (ends-with str str [num])
```

Description:

Checks whether a list or string ends with a given value.  
For lists, compares the last element.  
For strings, compares a suffix; the optional numeric flag
controls case-sensitivity or matching mode depending on
implementation.

Examples:

```
(ends-with '(1 2 3) 3)        -> true
(ends-with "Rebelion" "ion")  -> true
```

Notes:

- Works on lists and strings.
- String mode may support additional options.

See: [starts-with](#f-starts-with), [find](#f-find)

---

<a name="f-env"></a>
## env

```
syntax: (env str value)
syntax: (env str)
syntax: (env)
```

Description:

Accesses environment variables of the current process.  
With no arguments, returns a list of all environment pairs.
With one argument, returns the value of the variable or nil.
With two arguments, sets the environment variable to the given
value.

Examples:

```
(env)    
(env "HOME")
(env "X" "123")
```

Notes:

- Values are strings.
- Setting a variable overwrites previous contents.

See: [args](#f-args), [sys-info](#f-sys-info)

---

<a name="f-erf"></a>
## erf

```
syntax: (erf float)
```

Description:

Computes the error function of the given floating-point value.

Examples:

```
(erf 0.0)    -> 0.0
(erf 1.0)    -> 0.842700
```

Notes:

- Appears in statistics and probability.

See: [normal](#f-normal), [gammai](#f-gammai)

---

<a name="f-error-event"></a>
## error-event

```
syntax: (error-event nil)
syntax: (error-event handler)
```

Description:

Sets or removes a handler for runtime error events.  
When a handler is installed, it receives error information when
an exception occurs.

Examples:

```
(error-event 'handler)
(error-event nil)
```

Notes:

- Handler receives error message and context.
- Useful for custom debugging or logging.

See: [throw-error](#f-throw-error), [command-event](#f-command-event)

---

<a name="f-eval"></a>
## eval

```
syntax: (eval val)
```

Description:

Evaluates the given expression and returns its result.  
If `val` is not an evaluable structure, the value is returned
unchanged.

Examples:

```
(eval '(+ 1 2))   -> 3
(eval 10)         -> 10
```

Notes:

- Use with caution to avoid unintended execution.
- Accepts any value.

See: [apply](#f-apply), [eval-string](#f-eval-string)

---

<a name="f-eval-string"></a>
## eval-string

```
syntax: (eval-string str [sym val int])
```

Description:

Parses and evaluates the Rebel code contained in `str`.  
Optional arguments specify the evaluation context, error return
value, and character offset for error reporting.

Examples:

```
(eval-string "(+ 1 2)")   -> 3
```

Notes:

- Dangerous if used on untrusted input.
- Errors return nil unless otherwise configured.

See: [eval](#f-eval), [read-expr](#f-read-expr)

---

<a name="f-eval-string-js"></a>
## eval-string-js

```
syntax: (eval-string-js str)
```

Description:

Evaluates JavaScript contained in the given string using the
embedded JS engine. Returns the result of the script.

Examples:

```
(eval-string-js "1 + 2")   -> 3
```

Notes:

- Behavior depends on JS engine availability.

See: [eval-string](#f-eval-string)

---

<a name="f-evenp"></a>
## even?

```
syntax: (even? int)
```

Description:

Returns true if the integer is even (i.e., divisible by 2).
Returns nil otherwise.

Examples:

```
(even? 4)     -> true
(even? 5)     -> nil
```

Notes:

- Works only on integers.

See: [odd?](#f-oddp)

---

<a name="f-exec"></a>
## exec

```
syntax: (exec str [str])
```

Description:

Runs an external program specified by the first string.  
Optionally sends the second string to its standard input.

Returns the program output as a string, or nil on failure.

Examples:

```
(exec "echo Hello Rebel")
(exec "cat" "some text")
```

Notes:

- Captures only standard output.
- Use `sys-error` or `net-error` for diagnostics.

See: [process](#f-process), [!](#f-bang)

---

<a name="f-exists"></a>
## exists

```
syntax: (exists func lst)
```

Description:

Applies `func` to each element of `lst` and returns true as soon
as the function returns a non-nil value.  
If no such element exists, returns nil.

Examples:

```
(exists odd? '(2 4 6 7))   -> true
```

Notes:

- Implements "any?" predicate.

See: [forall](#f-for-all), [filter](#f-filter)

---

<a name="f-exit"></a>
## exit

```
syntax: (exit [int])
```

Description:

Terminates the current process with an optional exit status.  
If the status is omitted, a default value of 0 is used.

Examples:

```
(exit)
(exit 2)
```

Notes:

- Performs a normal termination.
- No cleanup hooks are run beyond standard shutdown.

See: [abort](#f-abort)

---

<a name="f-exp"></a>
## exp

```
syntax: (exp float)
```

Description:

Computes the exponential of the given number (e to the power of
the argument). Returns a floating-point result.

Examples:

```
(exp 1.0)    -> 2.718281
```

Notes:

- Equivalent to `e^x`.

See: [log](#f-log), [pow](#f-pow)

---

<a name="f-expand"></a>
## expand

```
syntax: (expand val [val])
```

Description:

Expands macros, symbols, lists, or association structures.  
When given two arguments, replaces keys in the first value
according to the mapping defined in the second value.

Examples:

```
(expand '(+ x 1) '((x 10))) -> (+ 10 1)
```

Notes:

- Often used for template processing or AST rewriting.

See: [letex](#f-letex), [map](#f-map)

---

<a name="f-explode"></a>
## explode

```
syntax: (explode lst [int [bool]])
syntax: (explode str [int [bool]])
```

Description:

Splits a list into sublists or splits a string into substrings.  
The optional integer specifies chunk size.  
The optional boolean controls whether final smaller chunks are
included.

Examples:

```
(explode '(1 2 3 4) 2)      -> ((1 2) (3 4))
(explode "Rebelion" 3)      -> ("Reb" "eli" "on")
```

Notes:

- Works on both lists and strings.
- Chunking rules depend on flags.

See: [parse](#f-parse), [slice](#f-slice)

---

<a name="f-extend"></a>
## extend

```
syntax: (extend seq [seq ...])
```

Description:

Destructively appends all subsequent sequences to the first
sequence. Modifies the original list or string in place and
returns it.

Examples:

```
(set 'x '(1))
(extend x '(2 3))   -> (1 2 3)
```

Notes:

- Destructive counterpart of `append`.

See: [append](#f-append), [push](#f-push)

---

<a name="f-fact"></a>
## factor

```
syntax: (factor int)
```

Description:

Returns a list of the prime factors of the given integer, in
non-decreasing order.

Examples:

```
(factor 60)   -> (2 2 3 5)
```

Notes:

- Input must be positive.

See: [gcd](#f-gcd), [mod](#f-mod)

---

<a name="f-fft"></a>
## fft

```
syntax: (fft lst)
```

Description:

Computes the Fast Fourier Transform of a numeric list and
returns a list of complex frequency components.

Examples:

```
(fft '(1 0 0 0))
```

Notes:

- Input list length should be a power of two.
- Returns complex pairs represented as lists.

See: [ifft](#f-ifft), [corr](#f-corr)

---


<a name="f-file-info"></a>
## file-info

```
syntax: (file-info str [int [bool]])
```

Description:

Returns information about a file specified by the path string.
With no index, returns a list of file attributes such as size,
timestamps, permissions, and type.  
When an index is provided, returns only the selected attribute.
The optional boolean flag may control symbolic-link behavior,
depending on implementation.

Examples:

```
(file-info "data.txt")
(file-info "data.txt" 0)
```

Notes:

- Returned attributes follow system conventions.
- Use `sys-error` for failure details.

See: [file?](#f-filep), [real-path](#f-real-path)

---

<a name="f-filep"></a>
## file?

```
syntax: (file? str [bool])
```

Description:

Returns true if the given path refers to an existing regular
file. The optional boolean flag may cause symbolic links to be
followed or treated differently depending on the system.

Examples:

```
(file? "data.txt")  -> true
(file? "/etc")       -> nil
```

Notes:

- Checks only for files, not directories.

See: [directory?](#f-directoryp), [file-info](#f-file-info)

---

<a name="f-filter"></a>
## filter

```
syntax: (filter func lst)
```

Description:

Returns a new list containing only the elements of `lst` for
which the function returns a non-nil value.  
Elements are processed from left to right.

Examples:

```
(filter number? '(1 a 2 b))   -> (1 2)
```

Notes:

- Does not modify the input list.
- Equivalent to keeping only truthy matches.

See: [clean](#f-clean), [exists](#f-exists)

---

<a name="f-find"></a>
## find

```
syntax: (find val lst [val|val])
syntax: (find str str [val [int]])
```

Description:

Searches for a value or pattern in a list or a string.  
For lists, returns the index of the first matching element.  
For strings, returns the index of the first substring or regex
match.

Optional parameters control comparison functions, regex options,
or starting offsets depending on argument types.

Examples:

```
(find 3 '(1 2 3 4))        -> 2
(find "bel" "Rebel")       -> 2
```

Notes:

- Returns nil when no match is found.
- Supports custom comparison functions.

See: [find-all](#f-find-all), [member](#f-member)

---

<a name="f-find-all"></a>
## find-all

```
syntax: (find-all val lst [val])
syntax: (find-all lst lst [val])
syntax: (find-all str str [val [val]])
```

Description:

Returns a list of all matches found in a list or a string.  
For lists, collects all elements or all matching indices.  
For strings, returns all substrings or regex captures that
match the pattern.

Examples:

```
(find-all "e" "Rebelion")
(find-all '(1 2) '(1 2 3 1 2))
```

Notes:

- Returns an empty list when no matches exist.
- Supports matcher functions and regex modes.

See: [find](#f-find), [member](#f-member)

---

<a name="f-first"></a>
## first

```
syntax: (first seq)
```

Description:

Returns the first element of a list, array, or string.  
For strings, returns the first character.  
Returns nil for empty sequences.

Examples:

```
(first '(1 2 3))   -> 1
(first "Rebel")    -> "R"
```

Notes:

- Opposite of `rest`.

See: [rest](#f-rest), [last](#f-last)

---

<a name="f-flat"></a>
## flat

```
syntax: (flat lst [int])
```

Description:

Flattens nested lists up to the specified depth.  
If no depth is provided, flattens completely.

Examples:

```
(flat '(1 (2 (3 4))))     -> (1 2 3 4)
(flat '(1 (2 (3 4))) 1)   -> (1 2 (3 4))
```

Notes:

- Arrays are not flattened.

See: [explode](#f-explode)

---

<a name="f-float"></a>
## float

```
syntax: (float val [val])
```

Description:

Converts a value to a floating-point number.  
When a default value is provided, it is returned on failure.

Examples:

```
(float "3.14")   -> 3.14
(float "xx" 0)   -> 0
```

Notes:

- Non-convertible strings return nil unless default provided.

See: [int](#f-int), [float?](#f-floatp)

---

<a name="f-floatp"></a>
## float?

```
syntax: (float? val)
```

Description:

Returns true if `val` is a floating-point number.

Examples:

```
(float? 3.14)    -> true
(float? 10)      -> nil
```

Notes:

- Strict type check.

See: [int](#f-int), [number?](#f-numberp)

---

<a name="f-floor"></a>
## floor

```
syntax: (floor num)
```

Description:

Rounds the number downward to the nearest integer.  
Always rounds toward negative infinity.

Examples:

```
(floor 3.9)     -> 3
(floor -1.2)    -> -2
```

Notes:

- Returns a float.

See: [ceil](#f-ceil), [round](#f-round)

---

<a name="f-flt"></a>
## flt

```
syntax: (flt num)
```

Description:

Converts a number to a floating-point representation.  
Similar to `float`, but optimized for speed and simple numeric
conversion.

Examples:

```
(flt 2)       -> 2.0
```

Notes:

- Equivalent to `float` for typical usage.

See: [float](#f-float)

---

<a name="f-fn"></a>
## fn

```
syntax: (fn (params) body)
```

Description:

Creates an anonymous function with the given parameters and
body. Equivalent to a lambda expression.

Examples:

```
(set 'f (fn (x) (+ x 1)))
(f 10)   -> 11
```

Notes:

- Captures surrounding environment.

See: [define](#f-define), [lambda?](#f-lambdap)

---

<a name="f-for"></a>
## for

```
syntax: (for (sym num num [num [val]]) body)
```

Description:

Iterates a variable from a starting value to an ending value,
incrementing by the optional step.  
The optional break expression may terminate the loop early.

Examples:

```
(for (i 1 5) (print i))
```

Notes:

- Step defaults to 1.
- Loop bounds are inclusive.

See: [dotimes](#f-dotimes), [while](#f-while)

---

<a name="f-for-all"></a>
## for-all

```
syntax: (for-all func lst)
```

Description:

Applies the predicate function to each element in the list.
Returns true only if the function returns a non-nil value for
every element.

Examples:

```
(for-all number? '(1 2 3))   -> true
```

Notes:

- Implements "all?" logic.

See: [exists](#f-exists), [map](#f-map)

---

<a name="f-fork"></a>
## fork

```
syntax: (fork val)
```

Description:

Creates a new process that evaluates the given expression.
Returns the process ID of the child process.

Examples:

```
(fork (println "child"))
```

Notes:

- Available only on systems supporting process forking.

See: [process](#f-process), [spawn](#f-spawn)

---

<a name="f-format"></a>
## format

```
syntax: (format str val [val ...])
syntax: (format str lst)
```

Description:

Formats data according to a printf-like format string.  
Arguments may be passed individually or in a list.

Examples:

```
(format "%d %s" 10 "Rebel")     -> "10 Rebel"
(format "%02d" '(7))            -> "07"
```

Notes:

- Follows C-style format specifiers.

See: [print](#f-print), [string](#f-string)

---

<a name="f-fv"></a>
## fv

```
syntax: (fv float float float float [int])
```

Description:

Computes the future value of an investment based on rate,
number of periods, payment, and present value.  
The optional type flag controls payment timing.

Examples:

```
(fv 0.05 10 0 1000)
```

Notes:

- Common financial function.

See: [pv](#f-pv), [nper](#f-nper)

---

<a name="f-gammai"></a>
## gammai

```
syntax: (gammai float float)
```

Description:

Computes the incomplete gamma function for the given arguments.

Examples:

```
(gammai 2.0 3.0)
```

Notes:

- Used in probability distributions.

See: [gammaln](#f-gammaln)

---

<a name="f-gammaln"></a>
## gammaln

```
syntax: (gammaln float)
```

Description:

Returns the natural logarithm of the gamma function for the
given value. Useful for stable computation of large factorials.

Examples:

```
(gammaln 10.0)
```

Notes:

- Works for large arguments where gamma(x) would overflow.

See: [gammai](#f-gammai)

---

<a name="f-gcd"></a>
## gcd

```
syntax: (gcd int [int ...])
```

Description:

Computes the greatest common divisor of all integer
arguments.  
Uses pairwise reduction.

Examples:

```
(gcd 12 18)      -> 6
(gcd 12 18 24)   -> 6
```

Notes:

- Accepts two or more integers.

See: [factor](#f-factor), [mod](#f-mod)

---


<a name="f-get-char"></a>
## get-char

```
syntax: (get-char int)
```

Description:

Reads a single byte from the given memory address and returns it
as an integer.  
Used primarily for FFI and low-level memory inspection.

Examples:

```
(get-char 1000)   -> byte value
```

Notes:

- Address must point to valid readable memory.
- Returns an integer 0–255.

See: [get-int](#f-get-int), [address](#f-address)

---

<a name="f-get-float"></a>
## get-float

```
syntax: (get-float int)
```

Description:

Reads a floating-point value from the memory address given by the
integer. Returns the float stored at that location.

Examples:

```
(get-float addr)
```

Notes:

- Requires correct alignment.
- FFI-level operation.

See: [get-int](#f-get-int), [cpymem](#f-cpymem)

---

<a name="f-get-int"></a>
## get-int

```
syntax: (get-int int)
```

Description:

Reads an integer value from the memory address specified.  
Returns the signed machine integer located at that address.

Examples:

```
(get-int addr)
```

Notes:

- Platform dependent.
- Unsafe if address is invalid.

See: [address](#f-address), [get-long](#f-get-long)

---

<a name="f-get-long"></a>
## get-long

```
syntax: (get-long int)
```

Description:

Reads a long integer value from the given memory address.
Returns the platform’s native long type.

Examples:

```
(get-long addr)
```

Notes:

- For FFI and binary structure handling.

See: [get-int](#f-get-int), [struct](#f-struct)

---

<a name="f-get-string"></a>
## get-string

```
syntax: (get-string int [int [str]])
```

Description:

Reads a string from the memory address given.  
Optional length specifies how many bytes to read.  
Optional limit string truncates the read at the first occurrence
of that delimiter.

Examples:

```
(get-string addr 10)
(get-string addr 50 "\n")
```

Notes:

- May return binary data.
- Without length, reads until null terminator.

See: [read-file](#f-read-file)

---

<a name="f-get-url"></a>
## get-url

```
syntax: (get-url str [str] [int [str]])
```

Description:

Retrieves the content of a URL via HTTP GET.  
Optional strings supply request options or headers, and the
optional timeout sets a maximum wait time.

Returns the response body as a string, or nil on failure.

Examples:

```
(get-url "http://example.com")
```

Notes:

- Use `net-error` for error details.
- Supports additional headers via option string.

See: [append-file](#f-append-file), [delete-url](#f-delete-url)

---

<a name="f-global"></a>
## global

```
syntax: (global sym [sym ...])
```

Description:

Declares symbols as global within function bodies, making them
accessible without context prefixes.

Examples:

```
(global x y)
```

Notes:

- Only affects lookup inside functions.
- Does not assign values.

See: [set](#f-set), [constant](#f-constant)

---

<a name="f-globalp"></a>
## global?

```
syntax: (global? sym)
```

Description:

Returns true if the given symbol is marked as global.  
Returns nil otherwise.

Examples:

```
(global? 'x)
```

Notes:

- Symbol must exist to test its status.

See: [global](#f-global)

---

<a name="f-history"></a>
## history

```
syntax: (history [bool])
```

Description:

Returns the current REPL command history as a list of strings.  
When the optional boolean is true, clears the history.

Examples:

```
(history)
(history true)
```

Notes:

- Only meaningful in interactive mode.

See: [command-event](#f-command-event)

---

<a name="f-if"></a>
## if

```
syntax: (if test val [val])
```

Description:

Evaluates `test`. If true, evaluates and returns the second
argument; otherwise evaluates and returns the third argument if
present, or nil if omitted.

Examples:

```
(if true 1 2)     -> 1
(if false 1 2)    -> 2
(if false 10)     -> nil
```

Notes:

- Strict two- or three-expression conditional.

See: [cond](#f-cond), [when](#f-when)

---

<a name="f-ifft"></a>
## ifft

```
syntax: (ifft lst)
```

Description:

Computes the inverse Fast Fourier Transform of a numeric list and
returns a list representing the reconstructed time-domain signal.

Examples:

```
(ifft '(complex list))
```

Notes:

- Input should represent valid FFT output.

See: [fft](#f-fft)

---

<a name="f-import"></a>
## import

```
syntax: (import str str ["cdecl"])
syntax: (import str)
```

Description:

Loads a shared library (`.so`) and retrieves function pointers by
name for use with the FFI.  
The optional "cdecl" string sets the calling convention when
supported.

Examples:

```
(import "libm.so" "cos")
(import "libcrypto.so")
```

Notes:

- Intended for Unix `.so` libraries only.
- Returns a handle or function object.

See: [callback](#f-callback), [struct](#f-struct)

---

<a name="f-inc"></a>
## inc

```
syntax: (inc val [num])
```

Description:

Increments the numeric value stored in `val`. If no second
argument is given, increments by 1. Operates destructively on the
variable or place.

Examples:

```
(set 'x 10)
(inc x)      -> 11
(inc x 5)    -> 16
```

Notes:

- Opposite of `dec`.

See: [dec](#f-dec), [++](#f-inc-op)

---

<a name="f-index"></a>
## index

```
syntax: (index func lst)
```

Description:

Returns the index of the first element in the list for which the
predicate function returns true. Returns nil if no match is
found.

Examples:

```
(index odd? '(2 4 6 7 8))   -> 3
```

Notes:

- Predicate receives each element.
- Stops on first match.

See: [find](#f-find), [exists](#f-exists)

---

<a name="f-infp"></a>
## inf?

```
syntax: (inf? float)
```

Description:

Returns true if the float is positive or negative infinity.
Returns nil otherwise.

Examples:

```
(inf? (/ 1.0 0.0))     -> true
(inf? 3.14)            -> nil
```

Notes:

- Tests only floating-point infinity.

See: [NaN?](#f-nanp), [float?](#f-floatp)

---

<a name="f-int"></a>
## int

```
syntax: (int val [val [int]])
```

Description:

Converts a value to an integer.  
If conversion fails and a default value is provided, returns the
default instead.  
Optional base specifies conversion base for strings.

Examples:

```
(int "42")      -> 42
(int "2A" nil 16) -> 42
```

Notes:

- Returns nil unless default is given.

See: [float](#f-float), [integer?](#f-integerp)

---

<a name="f-integerp"></a>
## integer?

```
syntax: (integer? val)
```

Description:

Returns true if the value is an integer number.

Examples:

```
(integer? 10)     -> true
(integer? 3.1)    -> nil
```

Notes:

- Strict numeric type check.

See: [float?](#f-floatp), [number?](#f-numberp)

---

<a name="f-intersect"></a>
## intersect

```
syntax: (intersect lst lst [bool])
```

Description:

Returns a list containing the elements common to both lists.
The optional boolean controls whether duplicates are preserved.

Examples:

```
(intersect '(1 2 3) '(2 3 4))   -> (2 3)
```

Notes:

- Order follows the first list.

See: [difference](#f-difference), [union](#f-union)

---

<a name="f-invert"></a>
## invert

```
syntax: (invert matrix [float])
```

Description:

Computes the inverse of a square matrix.  
The optional pivot parameter may improve numerical stability.

Examples:

```
(invert '((1 2) (3 4)))
```

Notes:

- Matrix must be invertible.
- Returns a matrix.

See: [det](#f-det), [transpose](#f-transpose)

---

<a name="f-irr"></a>
## irr

```
syntax: (irr lst [lst [num]])
```

Description:

Computes the internal rate of return for a series of cash flows.
Optionally accepts a list of time periods and an initial guess.

Examples:

```
(irr '(-1000 200 300 400))
```

Notes:

- Returns a floating-point value.
- Financial calculation.

See: [npv](#f-npv), [nper](#f-nper)

---

<a name="f-join"></a>
## join

```
syntax: (join lst [str [bool]])
```

Description:

Concatenates a list of strings using an optional separator
string.  
If the boolean flag is true, appends the separator even after
the last element.

Examples:

```
(join '("Rebel" "Lang") " ")   -> "Rebel Lang"
```

Notes:

- Works only with lists of strings.

See: [string](#f-string), [append](#f-append)

---


<a name="f-json-error"></a>
## json-error

```
syntax: (json-error)
```

Description:

Returns information about the last JSON parsing error.  
If no error has occurred since the previous JSON operation,
returns nil.

Examples:

```
(json-error)
```

Notes:

- Intended for use after `json-parse`.

See: [json-parse](#f-json-parse)

---

<a name="f-json-parse"></a>
## json-parse

```
syntax: (json-parse str)
```

Description:

Parses a JSON-formatted string and returns the corresponding
Rebel structure consisting of lists, strings, numbers, and
contexts. Returns nil on parsing error.

Examples:

```
(json-parse "{\"x\": 10}")
```

Notes:

- Use `json-error` for details on failure.

See: [json-error](#f-json-error)

---

<a name="f-kmeans-query"></a>
## kmeans-query

```
syntax: (kmeans-query lst matrix)
```

Description:

Assigns points in `lst` to the closest centroid in the given
centroid matrix. Returns a list of cluster indices.

Examples:

```
(kmeans-query '((1 2) (5 6)) '((2 2) (6 6)))
```

Notes:

- Distances are Euclidean.
- Input must match centroid dimensionality.

See: [kmeans-train](#f-kmeans-train)

---

<a name="f-kmeans-train"></a>
## kmeans-train

```
syntax: (kmeans-train matrix int context [matrix])
```

Description:

Performs k-means clustering on the given data matrix.  
`int` determines the number of clusters.  
Results are stored in the specified context, optionally using
initial centroids.

Examples:

```
(kmeans-train '((1 2) (5 6)) 2 KCtx)
```

Notes:

- Iterative algorithm.
- Context holds centroid and cluster info.

See: [kmeans-query](#f-kmeans-query)

---

<a name="f-lambdap"></a>
## lambda?

```
syntax: (lambda? val)
```

Description:

Returns true if the value is a lambda or function object created
with `define` or `fn`. Returns nil otherwise.

Examples:

```
(lambda? (fn (x) x))    -> true
(lambda? 123)           -> nil
```

Notes:

- Identifies callable functions.

See: [fn](#f-fn), [primitive?](#f-primitivep)

---

<a name="f-last"></a>
## last

```
syntax: (last seq)
```

Description:

Returns the last element of a list, array, or string.  
For strings, returns the final character.  
Returns nil for empty sequences.

Examples:

```
(last '(1 2 3))    -> 3
(last "Rebel")     -> "l"
```

Notes:

- Opposite of `first`.

See: [first](#f-first), [rest](#f-rest)

---

<a name="f-last-error"></a>
## last-error

```
syntax: (last-error [int])
```

Description:

Returns the last system or network error code.  
With an integer argument, resets or filters the stored error
depending on implementation.

Examples:

```
(last-error)
```

Notes:

- Often used after failed I/O or networking calls.

See: [sys-error](#f-sys-error), [net-error](#f-net-error)

---

<a name="f-legalp"></a>
## legal?

```
syntax: (legal? str)
```

Description:

Checks whether the given string is a valid Rebel symbol name.  
Returns true for acceptable names and nil for illegal ones.

Examples:

```
(legal? "x")         -> true
(legal? "123abc")    -> nil
```

Notes:

- Useful when generating symbols programmatically.

See: [symbol?](#f-symbolp)

---

<a name="f-length"></a>
## length

```
syntax: (length val)
```

Description:

Returns the length of a list, string, or array.  
For non-sequence values, returns 0.

Examples:

```
(length '(1 2 3))      -> 3
(length "Rebel")       -> 5
```

Notes:

- Arrays report their number of rows.

See: [empty?](#f-emptyp), [first](#f-first)

---

<a name="f-let"></a>
## let

```
syntax: (let ((sym [val]) ...) body)
syntax: (let (sym val ...) body)
```

Description:

Creates a lexical binding environment.  
Each symbol is assigned a temporary value for the duration of
the body, after which previous values are restored.

Examples:

```
(let ((x 5)) (+ x 2))   -> 7
```

Notes:

- Bindings are lexically scoped.
- Multiple forms supported.

See: [letn](#f-letn), [local](#f-local)

---

<a name="f-letex"></a>
## letex

```
syntax: (letex ((sym [val]) ...) body)
syntax: (letex (sym val ...) body)
```

Description:

Like `let`, but evaluates the initializers in the current
environment before establishing bindings.  
Useful when macro expansion depends on dynamic evaluation.

Examples:

```
(letex ((x (+ 1 2))) x)   -> 3
```

Notes:

- Expands before binding.

See: [let](#f-let), [define-macro](#f-define-macro)

---

<a name="f-letn"></a>
## letn

```
syntax: (letn ((sym [val]) ...) body)
syntax: (letn (sym val ...) body)
```

Description:

A variant of `let` that binds symbols sequentially, allowing
each initializer to reference earlier bindings.

Examples:

```
(letn ((x 1) (y (+ x 2))) y)   -> 3
```

Notes:

- Supports sequential dependency.
- Similar to `let*` in other Lisps.

See: [let](#f-let), [letex](#f-letex)

---

<a name="f-list"></a>
## list

```
syntax: (list val [val ...])
```

Description:

Creates a new list containing the given values in order.

Examples:

```
(list 1 2 3)         -> (1 2 3)
(list "a" "b")       -> ("a" "b")
```

Notes:

- Basic list constructor.

See: [cons](#f-cons), [append](#f-append)

---

<a name="f-listp"></a>
## list?

```
syntax: (list? val)
```

Description:

Returns true if the value is a list, false otherwise.

Examples:

```
(list? '(1 2))    -> true
(list? 42)        -> nil
```

Notes:

- Does not treat arrays as lists.

See: [array?](#f-arrayp), [atom?](#f-atomp)

---

<a name="f-load"></a>
## load

```
syntax: (load str [str ...] [sym])
```

Description:

Loads and evaluates one or more Rebel source files.  
Optionally evaluates them in a specific context.

Examples:

```
(load "init.rbl")
(load "a.rbl" "b.rbl" MyCtx)
```

Notes:

- File paths must be readable.
- Context controls symbol resolution.

See: [read-file](#f-read-file), [source](#f-source)

---

<a name="f-local"></a>
## local

```
syntax: (local (sym [sym ...]) body)
```

Description:

Declares symbols as local to the body of a function or block.
Each symbol receives an empty initial binding unless overridden.

Examples:

```
(local (x y) (set 'x 1) (+ x 2))
```

Notes:

- Lexical scoping.
- Does not accept initializers; use `let` for that.

See: [let](#f-let), [set](#f-set)

---

<a name="f-log"></a>
## log

```
syntax: (log float [float])
```

Description:

Computes the natural logarithm of the given value.  
With a second argument, computes the logarithm in the specified
base.

Examples:

```
(log 10)        -> 2.302585
(log 8 2)       -> 3
```

Notes:

- Domain must be positive.

See: [exp](#f-exp), [pow](#f-pow)

---

<a name="f-lookup"></a>
## lookup

```
syntax: (lookup val lst [int [val]])
```

Description:

Searches an association list for a key matching `val` and returns
the associated value. If an index is supplied, returns the value
at that position within the matching sublist.  
If a default value is supplied, it is returned when no match is
found.

Examples:

```
(lookup 'x '((x 1) (y 2)))        -> 1
(lookup 'x '((x 1 2)))            -> (1 2)
(lookup 'z '((x 1)) nil "none")   -> "none"
```

Notes:

- Complements `assoc`.

See: [assoc](#f-assoc), [pop-assoc](#f-pop-assoc)

---

<a name="f-lower-case"></a>
## lower-case

```
syntax: (lower-case str)
```

Description:

Returns a new string with all alphabetic characters converted to
lowercase using the system’s locale rules.

Examples:

```
(lower-case "ReBeL")    -> "rebel"
```

Notes:

- Non-alphabetic characters are unchanged.

See: [upper-case](#f-upper-case), [title-case](#f-title-case)

---


<a name="f-macro"></a>
## macro

```
syntax: (macro (name [param ...]) [body ...])
```

Description:

Defines an anonymous macro.  
Macros receive their arguments **unevaluated** and return code
that will be evaluated in the caller’s environment.  
Useful for syntactic extensions and transformations.

Examples:

```
(macro (m x) '(+ ,x ,x))
(m 10)    -> 20
```

Notes:

- Similar to `define-macro`, but anonymous.
- Must return valid Rebel code.

See: [define-macro](#f-define-macro), [macro?](#f-macrop)

---

<a name="f-macrop"></a>
## macro?

```
syntax: (macro? val)
```

Description:

Returns true if `val` is a macro object, nil otherwise.

Examples:

```
(macro? (macro (x) x))   -> true
(macro? 123)             -> nil
```

Notes:

- Distinguishes macros from functions.

See: [lambda?](#f-lambdap)

---

<a name="f-main-args"></a>
## main-args

```
syntax: (main-args int)
syntax: (main-args)
```

Description:

Returns command-line arguments passed to the Rebel interpreter
itself (not to user functions).  
With an index, returns a specific argument; without one, returns
the entire list.

Examples:

```
(main-args)       -> list of args
(main-args 0)     -> executable name
```

Notes:

- Similar to `args`, but for interpreter arguments.

See: [args](#f-args)

---

<a name="f-make-dir"></a>
## make-dir

```
syntax: (make-dir str [int])
```

Description:

Creates a directory with the given path string.  
Optional integer specifies permissions (mode bits) depending on
the operating system.

Examples:

```
(make-dir "data")
```

Notes:

- Use `sys-error` for diagnostics on failure.

See: [directory?](#f-directoryp), [remove-dir](#f-remove-dir)

---

<a name="f-map"></a>
## map

```
syntax: (map func lst [lst ...])
```

Description:

Applies `func` element-wise across one or more lists, returning a
new list containing the results.  
Stops when the shortest list is exhausted.

Examples:

```
(map + '(1 2 3) '(10 20 30))   -> (11 22 33)
(map string '(a b))            -> ("a" "b")
```

Notes:

- Function receives parallel elements from each list.
- Nondestructive.

See: [filter](#f-filter), [exists](#f-exists)

---

<a name="f-mat"></a>
## mat

```
syntax: (mat op matrix matrix)
syntax: (mat op matrix num)
```

Description:

Performs arithmetic operations (`+ - * /`) on matrices or between
a matrix and a scalar.  
When both arguments are matrices, operations occur element-wise.

Examples:

```
(mat + '((1 2) (3 4)) '((1 1) (1 1)))
(mat * '((1 2) (3 4)) 2)
```

Notes:

- Dimensions must match for matrix–matrix operations.

See: [multiply](#f-multiply), [transpose](#f-transpose)

---

<a name="f-match"></a>
## match

```
syntax: (match lst lst [bool])
```

Description:

Attempts to match a pattern list to a target list.  
The optional boolean flag controls whether additional unification
rules or binding behavior is applied.

Examples:

```
(match '(x y) '(1 2))
```

Notes:

- Pattern matching, not regex matching.

See: [unify](#f-unify)

---

<a name="f-max"></a>
## max

```
syntax: (max num [num ...])
```

Description:

Returns the maximum value among all numeric arguments.

Examples:

```
(max 1 9 3)     -> 9
```

Notes:

- Requires at least one argument.

See: [min](#f-min), [sgn](#f-sgn)

---

<a name="f-member"></a>
## member

```
syntax: (member val lst)
syntax: (member str str [num])
```

Description:

Searches for a value in a list or a substring in a string.  
For lists, returns the tail starting from the first match.  
For strings, returns the index of the first occurrence of the
substring.

Examples:

```
(member 3 '(1 2 3 4))       -> (3 4)
(member "bel" "Rebel")      -> 2
```

Notes:

- Returns nil if not found.

See: [find](#f-find), [find-all](#f-find-all)

---

<a name="f-min"></a>
## min

```
syntax: (min num [num ...])
```

Description:

Returns the minimum value among all numeric arguments.

Examples:

```
(min 3 1 6)     -> 1
```

Notes:

- Requires at least one argument.

See: [max](#f-max), [sgn](#f-sgn)

---

<a name="f-mod"></a>
## mod

```
syntax: (mod num num [num ...])
syntax: (mod num)
```

Description:

Computes the modulo (remainder) of sequential divisions.  
With two or more arguments, evaluates left to right.  
With one argument, returns the value itself.

Examples:

```
(mod 10 3)      -> 1
(mod 20 3 2)    -> ((20 mod 3) mod 2)
```

Notes:

- Arguments must be numeric.

See: [%](#f-mod-op), [div](#f-div)

---

<a name="f-mul"></a>
## mul

```
syntax: (mul num num [num ...])
```

Description:

Multiplies all numeric arguments and returns the result.

Examples:

```
(mul 2 3 4)    -> 24
```

Notes:

- Similar to `*`, but always numeric.

See: [*](#f-multiply-op), [pow](#f-pow)

---

<a name="f-multiply"></a>
## multiply

```
syntax: (multiply matrix matrix)
```

Description:

Performs proper matrix multiplication (not element-wise).  
The number of columns in the first matrix must match the number
of rows in the second.

Examples:

```
(multiply '((1 2) (3 4)) '((5 6) (7 8)))
```

Notes:

- Produces a new matrix.

See: [mat](#f-mat), [transpose](#f-transpose)

---

<a name="f-net-accept"></a>
## net-accept

```
syntax: (net-accept int)
```

Description:

Accepts a connection from a listening socket.  
Returns a new socket handle for the accepted connection.

Examples:

```
(net-accept server-sock)
```

Notes:

- Non-blocking or blocking behavior depends on socket flags.

See: [net-listen](#f-net-listen), [net-receive](#f-net-receive)

---

<a name="f-net-close"></a>
## net-close

```
syntax: (net-close int [true])
```

Description:

Closes a network socket.  
The optional true flag forces immediate close without lingering.

Examples:

```
(net-close sock)
```

Notes:

- Always close sockets when finished.

See: [net-connect](#f-net-connect), [net-send](#f-net-send)

---

<a name="f-net-connect"></a>
## net-connect

```
syntax: (net-connect str)
syntax: (net-connect str int [int])
syntax: (net-connect str int [str [int]])
```

Description:

Creates a network connection to a Unix-domain socket or TCP
address.  
Arguments determine host, port, mode, and optional TTL or
timeout depending on format.

Examples:

```
(net-connect "/tmp/sock")
(net-connect "example.com" 80)
```

Notes:

- Returns a socket handle.
- Use `net-error` for connection issues.

See: [net-accept](#f-net-accept), [net-send](#f-net-send)

---

<a name="f-net-error"></a>
## net-error

```
syntax: (net-error [int])
```

Description:

Returns the last network-related error code.  
With an integer argument, may reset or inspect error state
depending on implementation.

Examples:

```
(net-error)
```

Notes:

- Works for all `net-*` functions.

See: [last-error](#f-last-error)

---

<a name="f-net-eval"></a>
## net-eval

```
syntax: (net-eval '((str int val) ...) [int])
syntax: (net-eval str int val [int [func]])
```

Description:

Sends code to remote Rebel nodes for evaluation.  
The first form broadcasts to multiple nodes using a list of
(host port expr) triplets.  
The second form targets a single node. Optional timeout and
callback may be provided.

Examples:

```
(net-eval "example.com" 4242 '(+ 1 2))
```

Notes:

- Requires Rebel nodes configured for remote eval.

See: [append-file](#f-append-file), [process](#f-process)

---

<a name="f-net-interface"></a>
## net-interface

```
syntax: (net-interface [str])
```

Description:

Returns network interface information.  
With no argument, returns info about all interfaces.  
With a specific address or name, returns information for that
interface only.

Examples:

```
(net-interface)
(net-interface "lo0")
```

Notes:

- Output depends on system capabilities.

See: [sys-info](#f-sys-info)

---


<a name="f-net-ipv"></a>
## net-ipv

```
syntax: (net-ipv int)
syntax: (net-ipv)
```

Description:

Gets or sets the IP protocol version.  
With no arguments, returns the current IP version (4 or 6).  
With an integer argument, switches the network stack to IPv4 or
IPv6 if supported.

Examples:

```
(net-ipv)      -> 4
(net-ipv 6)    -> 6
```

Notes:

- Affects all subsequent network operations.

See: [net-connect](#f-net-connect), [net-interface](#f-net-interface)

---

<a name="f-net-listen"></a>
## net-listen

```
syntax: (net-listen int [str [str]])
syntax: (net-listen str)
```

Description:

Creates a listening socket.  
With a port number, listens on that port, optionally with a
specified IP address or mode.  
With a string path, creates a Unix-domain listening socket.

Examples:

```
(net-listen 8080)
(net-listen "/tmp/rebel.sock")
```

Notes:

- Returns a socket handle.
- Use `net-accept` to accept connections.

See: [net-accept](#f-net-accept), [net-close](#f-net-close)

---

<a name="f-net-local"></a>
## net-local

```
syntax: (net-local int)
```

Description:

Returns the local address information (IP and port or Unix path)
associated with the given socket handle.

Examples:

```
(net-local sock)
```

Notes:

- Works on both IPv4/IPv6 and Unix sockets.

See: [net-peer](#f-net-peer)

---

<a name="f-net-lookup"></a>
## net-lookup

```
syntax: (net-lookup str [bool])
syntax: (net-lookup str)
```

Description:

Resolves a hostname to its IP address, or vice versa.  
When given a hostname, returns its numeric address.  
When given an IP address, returns its canonical hostname.  
Optional boolean toggles reverse lookup.

Examples:

```
(net-lookup "example.com")
(net-lookup "127.0.0.1" true)
```

Notes:

- Behavior depends on system resolver.

See: [net-interface](#f-net-interface)

---

<a name="f-net-packet"></a>
## net-packet

```
syntax: (net-packet str)
```

Description:

Creates a network packet object from the given string, typically
used for raw UDP or datagram communication.

Examples:

```
(net-packet "HELLO")
```

Notes:

- Packet format depends on target functions.

See: [net-send-to](#f-net-send-to)

---

<a name="f-net-peek"></a>
## net-peek

```
syntax: (net-peek int)
```

Description:

Returns the number of bytes available to read on the given
socket without blocking, when supported by the system.

Examples:

```
(net-peek sock)
```

Notes:

- Useful before reading large messages.

See: [net-receive](#f-net-receive)

---

<a name="f-net-peer"></a>
## net-peer

```
syntax: (net-peer int)
```

Description:

Returns information about the remote peer connected to the given
socket, including IP address and port or Unix path.

Examples:

```
(net-peer sock)
```

Notes:

- Opposite of `net-local`.

See: [net-local](#f-net-local)

---

<a name="f-net-ping"></a>
## net-ping

```
syntax: (net-ping lst [int [int bool]])
syntax: (net-ping str [int [int bool]])
```

Description:

Sends ICMP echo requests (ping) to a list of addresses or a
single address.  
Optional parameters specify timeout, number of attempts, and
synchronous/asynchronous mode depending on implementation.

Examples:

```
(net-ping "example.com")
(net-ping '("1.1.1.1" "8.8.8.8") 1000)
```

Notes:

- Requires system support for raw sockets or ping helper.

See: [net-lookup](#f-net-lookup)

---

<a name="f-net-receive"></a>
## net-receive

```
syntax: (net-receive int sym int [str])
```

Description:

Receives data from a connected socket.  
The buffer symbol will be set to the received string.  
The integer specifies the maximum bytes to read.  
Optional wait string may indicate blocking behavior.

Examples:

```
(net-receive sock buf 1024)
```

Notes:

- Returns number of bytes received.
- Nil on failure.

See: [net-send](#f-net-send), [net-peek](#f-net-peek)

---

<a name="f-net-receive-from"></a>
## net-receive-from

```
syntax: (net-receive-from int int)
```

Description:

Receives a datagram from a UDP socket.  
Returns a list containing the received data, remote address, and
remote port.

Examples:

```
(net-receive-from sock 2048)
```

Notes:

- Non-connected UDP mode.

See: [net-send-to](#f-net-send-to)

---

<a name="f-net-receive-udp"></a>
## net-receive-udp

```
syntax: (net-receive-udp int int [int [str]])
```

Description:

Receives a UDP packet on the specified port.  
Optional microsecond timeout and interface filter can be given.

Examples:

```
(net-receive-udp 5000 4096)
```

Notes:

- Returns data and metadata depending on system capabilities.

See: [net-send-udp](#f-net-send-udp)

---

<a name="f-net-select"></a>
## net-select

```
syntax: (net-select int str int)
syntax: (net-select lst str int)
```

Description:

Monitors one or more sockets for readiness.  
The mode string specifies `"r"` for read, `"w"` for write, and
timeout is in microseconds.

Examples:

```
(net-select sock "r" 100000)
(net-select '(s1 s2) "w" 0)
```

Notes:

- Behaves like the POSIX `select()` call.

See: [net-peek](#f-net-peek)

---

<a name="f-net-send"></a>
## net-send

```
syntax: (net-send int str [int])
```

Description:

Sends a string over a connected socket.  
Optional integer specifies the number of bytes to send.

Examples:

```
(net-send sock "Hello")
```

Notes:

- Returns number of bytes sent or nil.

See: [net-receive](#f-net-receive)

---

<a name="f-net-send-to"></a>
## net-send-to

```
syntax: (net-send-to str int str int)
```

Description:

Sends a UDP datagram to a specific remote host and port using the
given local socket as the sender.

Examples:

```
(net-send-to "example.com" 9000 "DATA" sock)
```

Notes:

- Does not establish a connection.

See: [net-receive-from](#f-net-receive-from)

---

<a name="f-net-send-udp"></a>
## net-send-udp

```
syntax: (net-send-udp str int str [bool])
```

Description:

Sends a UDP packet to a remote host and port.  
Optional boolean enables broadcast mode or other options.

Examples:

```
(net-send-udp "example.com" 9000 "Hello")
```

Notes:

- Suitable for connectionless communication.

See: [net-receive-udp](#f-net-receive-udp)

---

<a name="f-net-service"></a>
## net-service

```
syntax: (net-service int str)
syntax: (net-service str str)
```

Description:

Resolves port numbers and protocol names.  
Either form returns protocol/service mapping information
depending on arguments.

Examples:

```
(net-service 80 "tcp")
(net-service "http" "tcp")
```

Notes:

- Based on system service database.

See: [net-lookup](#f-net-lookup)

---

<a name="f-net-sessions"></a>
## net-sessions

```
syntax: (net-sessions)
```

Description:

Returns a list of currently active remote-evaluation sessions
managed by the runtime.

Examples:

```
(net-sessions)
```

Notes:

- Used with remote node communication.

See: [net-eval](#f-net-eval)

---

<a name="f-new"></a>
## new

```
syntax: (new context sym [bool])
syntax: (new context)
```

Description:

Creates a new context instance based on an existing context
prototype.  
If a target symbol is given, binds the new context to that name.  
Optional boolean controls whether the context is copied shallowly
or deeply.

Examples:

```
(new BaseCtx Foo)
```

Notes:

- Used to define modular namespaces.

See: [context](#f-context), [symbols](#f-symbols)

---

<a name="f-nilp"></a>
## nil?

```
syntax: (nil? val)
```

Description:

Returns true if the given value is nil (empty list).  
Returns nil otherwise.

Examples:

```
(nil? '())        -> true
(nil? 0)          -> nil
```

Notes:

- Only the empty list is considered nil.

See: [null?](#f-nullp), [empty?](#f-emptyp)

---

<a name="f-normal"></a>
## normal

```
syntax: (normal float float int)
syntax: (normal float float)
```

Description:

Generates random numbers from a normal (Gaussian) distribution
with the specified mean and standard deviation.  
With an integer argument, returns a list of that many samples.
Without it, returns a single sample.

Examples:

```
(normal 0.0 1.0)
(normal 0.0 1.0 5)
```

Notes:

- Uses pseudo-random generator.

See: [random](#f-random), [rand](#f-rand)

---

<a name="f-not"></a>
## not

```
syntax: (not val)
```

Description:

Returns true if `val` is nil or false, and nil otherwise.  
Implements logical negation for conditions.

Examples:

```
(not nil)      -> true
(not 1)        -> nil
```

Notes:

- Only nil is considered false.

See: [and](#f-andfn), [or](#f-or)

---

<a name="f-now"></a>
## now

```
syntax: (now [int [int]])
```

Description:

Returns the current date and time as a list or formatted value
depending on optional parameters.  
Optional minute offset adjusts the returned time.  
Optional index selects a specific component.

Examples:

```
(now)
(now 60)
(now 0 3)
```

Notes:

- Useful for timestamps and logs.

See: [date](#f-date), [time-of-day](#f-time-of-day)

---


<a name="f-nper"></a>
## nper

```
syntax: (nper float float float [float [int]])
```

Description:

Computes the number of periods for an investment based on
interest rate, payment, present value, and optionally future
value and payment type.

Examples:

```
(nper 0.05 -100 1000)
```

Notes:

- Financial time-value function.

See: [pv](#f-pv), [fv](#f-fv)

---

<a name="f-npv"></a>
## npv

```
syntax: (npv float lst)
```

Description:

Computes the net present value of a series of cash flows given a
discount rate.  
The list contains the cash flow values in chronological order.

Examples:

```
(npv 0.1 '(-1000 200 300 400))
```

Notes:

- Returns a float.

See: [irr](#f-irr), [nper](#f-nper)

---

<a name="f-nth"></a>
## nth

```
syntax: (nth int seq)
syntax: (nth lst seq)
```

Description:

Returns the element at the given index of a list, array, or
string.  
Negative indices count backward from the end.  
With a list of indices, returns a sequence of indexed elements.

Examples:

```
(nth 1 '(a b c))         -> b
(nth '(0 2) "Rebel")     -> ("R" "b")
```

Notes:

- Index out of range returns nil.

See: [first](#f-first), [last](#f-last)

---

<a name="f-nullp"></a>
## null?

```
syntax: (null? val)
```

Description:

Returns true if `val` is the empty list, nil otherwise.  
Identical to `nil?`.

Examples:

```
(null? '())      -> true
(null? 1)        -> nil
```

Notes:

- Alias for `nil?`.

See: [nil?](#f-nilp)

---

<a name="f-numberp"></a>
## number?

```
syntax: (number? val)
```

Description:

Returns true if `val` is a number (integer or float). Returns nil
otherwise.

Examples:

```
(number? 42)      -> true
(number? "x")     -> nil
```

Notes:

- Type predicate.

See: [integer?](#f-integerp), [float?](#f-floatp)

---

<a name="f-oddp"></a>
## odd?

```
syntax: (odd? int)
```

Description:

Returns true if the integer is odd. Returns nil otherwise.

Examples:

```
(odd? 3)      -> true
(odd? 4)      -> nil
```

Notes:

- Only integers accepted.

See: [even?](#f-evenp)

---

<a name="f-open"></a>
## open

```
syntax: (open str str [str])
```

Description:

Opens a file or resource for reading or writing.  
The second string specifies access mode, such as `"r"`, `"w"`,
or `"a"`.  
Optional third string supplies additional mode options.

Returns a file handle or nil on failure.

Examples:

```
(set 'fd (open "data.txt" "r"))
```

Notes:

- Must be followed by `close`.

See: [read](#f-read), [write](#f-write)

---

<a name="f-or"></a>
## or

```
syntax: (or val [val ...])
```

Description:

Evaluates arguments from left to right, returning the first
non-nil value.  
If all values are nil, returns nil.

Examples:

```
(or nil 0 5)       -> 0
(or nil nil)       -> nil
```

Notes:

- Logical OR with short-circuit evaluation.

See: [and](#f-andfn), [not](#f-not)

---

<a name="f-pack"></a>
## pack

```
syntax: (pack str [val ...])
syntax: (pack str lst)
syntax: (pack struct [val ...])
syntax: (pack struct lst)
```

Description:

Packs values into a binary string according to a format
descriptor or struct definition.  
Format characters determine how values are encoded.

Examples:

```
(pack "ici" '(1 "A" 3)) 
```

Notes:

- Used for binary protocol construction.

See: [unpack](#f-unpack), [struct](#f-struct)

---

<a name="f-parse"></a>
## parse

```
syntax: (parse str [str [val]])
```

Description:

Splits a string into a list of substrings based on a delimiter
string.  
Optional regex option or additional matching modes may be used
depending on arguments.

Examples:

```
(parse "a,b,c" ",")      -> ("a" "b" "c")
(parse "Rebelion" "")    -> ("R" "e" "b" ...)
```

Notes:

- When regex mode is active, pattern is matched repeatedly.

See: [explode](#f-explode), [find-all](#f-find-all)

---

<a name="f-peek"></a>
## peek

```
syntax: (peek int)
```

Description:

Reads and returns a single character from a file descriptor or
input stream without consuming it.

Examples:

```
(peek fd)
```

Notes:

- Returns integer character code.

See: [read-char](#f-read-char), [read-line](#f-read-line)

---

<a name="f-pipe"></a>
## pipe

```
syntax: (pipe)
```

Description:

Creates a pipe and returns two file descriptors: one for reading
and one for writing.

Examples:

```
(pipe)   -> (read-fd write-fd)
```

Notes:

- Platform-dependent behavior.

See: [process](#f-process)

---

<a name="f-pop"></a>
## pop

```
syntax: (pop lst [int [int ...]])
syntax: (pop lst [lst])
syntax: (pop str [int [int]])
```

Description:

Removes elements from a list or characters from a string,
returning the removed portion and modifying the original
sequence.  
Indices specify positions or ranges to remove.

Examples:

```
(set 'x '(1 2 3 4))
(pop x 1)      -> 2
x              -> (1 3 4)
```

Notes:

- Destructive operation.

See: [push](#f-push), [chop](#f-chop)

---

<a name="f-pop-assoc"></a>
## pop-assoc

```
syntax: (pop-assoc val lst)
syntax: (pop-assoc lst lst)
```

Description:

Removes the first association whose key matches the given
value, returning the removed pair and modifying the association
list.

Examples:

```
(set 'a '((x 1) (y 2)))
(pop-assoc 'x a)    -> (x 1)
```

Notes:

- Destructive counterpart to `assoc`.

See: [assoc](#f-assoc), [lookup](#f-lookup)

---

<a name="f-pow"></a>
## pow

```
syntax: (pow num num [num ...])
syntax: (pow num)
```

Description:

Raises numbers to sequential powers.  
With two or more arguments, computes left-associative exponent
chains.  
With one argument, returns the value itself.

Examples:

```
(pow 2 3)         -> 8
(pow 2 3 2)       -> (2^3)^2
```

Notes:

- Numeric arguments only.

See: [exp](#f-exp), [log](#f-log)

---

<a name="f-prefix"></a>
## prefix

```
syntax: (prefix sym)
```

Description:

Returns a symbol containing the prefix part of a context-bound
symbol (everything before the colon).

Examples:

```
(prefix 'Ctx:x)   -> 'Ctx
```

Notes:

- Works only on context-qualified symbols.

See: [symbol?](#f-symbolp), [term](#f-term)

---

<a name="f-pretty-print"></a>
## pretty-print

```
syntax: (pretty-print [int [str [str]]])
```

Description:

Formats and prints data structures in a human-readable way.  
Optional parameters control maximum line width, indentation
string, and floating-point formatting.

Examples:

```
(pretty-print)
```

Notes:

- Affects printed output only.

See: [print](#f-print), [println](#f-println)

---

<a name="f-primitivep"></a>
## primitive?

```
syntax: (primitive? val)
```

Description:

Returns true if the given value is a primitive Rebel function,
such as built-in operators or core functions.

Examples:

```
(primitive? +)     -> true
```

Notes:

- Distinguishes built-in functions from user-defined ones.

See: [lambda?](#f-lambdap)

---

<a name="f-print"></a>
## print

```
syntax: (print val [val ...])
```

Description:

Prints values to the standard output without inserting a
newline.  
Arguments are printed consecutively as strings.

Examples:

```
(print "Hello" " " "Rebel")
```

Notes:

- Use `println` to append a newline.

See: [println](#f-println), [pretty-print](#f-pretty-print)

---

<a name="f-println"></a>
## println

```
syntax: (println val [val ...])
```

Description:

Prints all arguments followed by a newline.  
Converts values to strings before printing.

Examples:

```
(println "Hello Rebel")
```

Notes:

- Same as `print` but ends with newline.

See: [print](#f-print)

---

<a name="f-prob-chi2"></a>
## prob-chi2

```
syntax: (prob-chi2 float int)
```

Description:

Computes the probability value for the chi-square distribution
given a chi-square statistic and degrees of freedom.

Examples:

```
(prob-chi2 5.0 2)
```

Notes:

- Complement of `crit-chi2`.

See: [crit-chi2](#f-crit-chi2)

---


<a name="f-prob-f"></a>
## prob-f

```
syntax: (prob-f float int int)
```

Description:

Computes the probability (p-value) for the F-distribution using
the given F statistic and numerator/denominator degrees of
freedom.

Examples:

```
(prob-f 3.5 2 10)
```

Notes:

- Complement of `crit-f`.

See: [crit-f](#f-crit-f)

---

<a name="f-prob-t"></a>
## prob-t

```
syntax: (prob-t float int)
```

Description:

Computes the probability (p-value) for Student’s t-distribution
with the given t statistic and degrees of freedom.

Examples:

```
(prob-t 1.8 15)
```

Notes:

- Complement of `crit-t`.

See: [crit-t](#f-crit-t)

---

<a name="f-prob-z"></a>
## prob-z

```
syntax: (prob-z float)
```

Description:

Computes the probability (p-value) of the standard normal
distribution (Z distribution) for the given z-score.

Examples:

```
(prob-z 1.96)
```

Notes:

- Complement of `crit-z`.

See: [crit-z](#f-crit-z)

---

<a name="f-process"></a>
## process

```
syntax: (process str int int [int])
syntax: (process str)
```

Description:

Launches an external program and optionally connects its input,
output, and error streams to Rebel file descriptors.  
Returns a process handle or pid depending on mode.

Examples:

```
(process "ls -l")
```

Notes:

- Mode details depend on environment.

See: [exec](#f-exec), [spawn](#f-spawn)

---

<a name="f-prompt-event"></a>
## prompt-event

```
syntax: (prompt-event nil)
syntax: (prompt-event handler)
```

Description:

Sets or removes a handler invoked whenever the REPL prompt is
displayed.  
The handler can modify prompt behavior or inject actions.

Examples:

```
(prompt-event 'on-prompt)
(prompt-event nil)
```

Notes:

- REPL-only functionality.

See: [command-event](#f-command-event)

---

<a name="f-protectedp"></a>
## protected?

```
syntax: (protected? sym)
```

Description:

Returns true if the given symbol is protected, meaning its value
cannot be modified or deleted without forcing.

Examples:

```
(protected? 'PI)
```

Notes:

- Constants are typically protected.

See: [constant](#f-constant), [delete](#f-delete)

---

<a name="f-push"></a>
## push

```
syntax: (push val lst [int ...])
syntax: (push val lst [lst])
syntax: (push str str [int])
```

Description:

Destructively inserts elements into a list or string.  
For lists, inserts before the given index or indices.  
For strings, inserts a substring at the selected index.

Examples:

```
(set 'x '(1 2 3))
(push 10 x 1)     -> (1 10 2 3)
```

Notes:

- Modifies the target sequence.
- Opposite of `pop`.

See: [pop](#f-pop), [extend](#f-extend)

---

<a name="f-pv"></a>
## pv

```
syntax: (pv float float float [float [int]])
```

Description:

Computes the present value of a series of payments or cash flows
given interest rate, number of periods, payment amount, and
optionally future value and payment timing.

Examples:

```
(pv 0.05 10 100)
```

Notes:

- Financial function related to annuities.

See: [fv](#f-fv), [nper](#f-nper)

---

<a name="f-quote"></a>
## quote

```
syntax: (quote val)
```

Description:

Prevents evaluation of `val`, returning it exactly as written.
Used to refer to literal lists, symbols, and expressions.

Examples:

```
(quote (1 2 3))     -> (1 2 3)
```

Notes:

- Shorthand syntax: `'val`.

See: [quote?](#f-quotep)

---

<a name="f-quotep"></a>
## quote?

```
syntax: (quote? val)
```

Description:

Returns true if the value is a quoted expression, nil otherwise.

Examples:

```
(quote? '(1 2 3))    -> true
```

Notes:

- Useful for macro inspection.

See: [macro?](#f-macrop)

---

<a name="f-rand"></a>
## rand

```
syntax: (rand int [int])
```

Description:

Returns a random integer within the range 0 to the given limit.
If two integers are provided, returns a random integer between
the two bounds.

Examples:

```
(rand 10)         -> 0–9
(rand 5 10)       -> 5–10
```

Notes:

- Uses the internal PRNG.

See: [random](#f-random), [seed](#f-seed)

---

<a name="f-random"></a>
## random

```
syntax: (random float float int)
syntax: (random float float)
```

Description:

Generates random floating-point numbers.  
With two floats, returns a random value within the given range.
With an integer, returns a list of randomly generated values.

Examples:

```
(random 0.0 1.0)
(random -10.0 10.0 5)
```

Notes:

- Continuous uniform distribution.

See: [rand](#f-rand), [seed](#f-seed)

---

<a name="f-randomize"></a>
## randomize

```
syntax: (randomize lst [bool])
```

Description:

Returns a randomized permutation of the list.  
Optional boolean flag selects whether the shuffle is destructive
or nondestructive.

Examples:

```
(randomize '(1 2 3 4))
```

Notes:

- Default is nondestructive.

See: [rand](#f-rand)

---

<a name="f-read"></a>
## read

```
syntax: (read int sym int [str])
```

Description:

Reads raw data from a file descriptor into a symbol, which is
set to the string of bytes read.  
The integer specifies the maximum size.  
Optional waiting string may influence blocking behavior.

Examples:

```
(read fd buf 1024)
```

Notes:

- Returns number of bytes read or nil on error.

See: [write](#f-write), [read-file](#f-read-file)

---

<a name="f-read-char"></a>
## read-char

```
syntax: (read-char [int])
```

Description:

Reads a single character from a file or input stream.  
If no file descriptor is provided, reads from standard input.

Examples:

```
(read-char)
```

Notes:

- Returns integer character code.

See: [peek](#f-peek), [read-line](#f-read-line)

---

<a name="f-read-expr"></a>
## read-expr

```
syntax: (read-expr str [sym [val [int]]])
```

Description:

Parses and evaluates a Rebel expression from a string.  
Optional context, error value, or offset may control evaluation
behavior.

Examples:

```
(read-expr "(+ 1 2)")
```

Notes:

- Returns nil on parse or evaluation errors unless error handler
  is provided.

See: [eval-string](#f-eval-string), [eval](#f-eval)

---

<a name="f-read-file"></a>
## read-file

```
syntax: (read-file str)
```

Description:

Reads the entire contents of a file into a string.  
Returns nil on failure.

Examples:

```
(read-file "data.txt")
```

Notes:

- Returns binary data as well.

See: [write-file](#f-write-file), [append-file](#f-append-file)

---

<a name="f-read-key"></a>
## read-key

```
syntax: (read-key [true])
```

Description:

Reads a single keystroke from the console.  
If the optional argument is true, returns the raw key code.

Examples:

```
(read-key)
```

Notes:

- Blocks until a key is pressed.

See: [read-char](#f-read-char)

---

<a name="f-read-line"></a>
## read-line

```
syntax: (read-line [int])
```

Description:

Reads a single line from a file descriptor or standard input,
returning it as a string without the trailing newline.

Examples:

```
(read-line)
```

Notes:

- Returns nil on end of file.

See: [read-file](#f-read-file), [read-char](#f-read-char)

---

<a name="f-read-utf8"></a>
## read-utf8

```
syntax: (read-utf8 int)
```

Description:

Reads the next UTF-8 encoded character from a file descriptor and
returns it as a string.  
Invalid UTF-8 sequences may return nil or replacement characters.

Examples:

```
(read-utf8 fd)
```

Notes:

- Useful for Unicode-aware processing.

See: [unicode](#f-unicode)

---


<a name="f-reader-event"></a>
## reader-event

```
syntax: (reader-event [handler])
syntax: (reader-event nil)
```

Description:

Sets or removes a handler that intercepts input read by the REPL.
If a handler is installed, every read operation passes the input
string to the handler before evaluation.

Examples:

```
(reader-event 'on-read)
(reader-event nil)
```

Notes:

- Handler can preprocess or log user input.
- REPL-only functionality.

See: [command-event](#f-command-event), [prompt-event](#f-prompt-event)

---

<a name="f-real-path"></a>
## real-path

```
syntax: (real-path [str])
syntax: (real-path str true)
```

Description:

Returns the canonical absolute path of the given file or directory.
Resolves symbolic links when supported.  
With the optional `true` flag, returns the path of the executable
instead.

Examples:

```
(real-path "/tmp/file")
(real-path "rebel" true)
```

Notes:

- Useful for resolving runtime resource paths.

See: [directory](#f-directory), [file-info](#f-file-info)

---

<a name="f-receive"></a>
## receive

```
syntax: (receive int sym)
syntax: (receive)
```

Description:

Waits for a message sent from another process and returns it.  
With two arguments, stores the received value into the symbol.

Examples:

```
(receive)
(receive pid msg)
```

Notes:

- Used for interprocess communication.

See: [send](#f-send), [spawn](#f-spawn)

---

<a name="f-ref"></a>
## ref

```
syntax: (ref val lst [val [true]])
```

Description:

Searches for a value in a list and returns its index.  
Optional comparator or mode flag may modify comparison behavior.

Examples:

```
(ref 3 '(1 2 3 4))     -> 2
```

Notes:

- Returns nil if not found.

See: [ref-all](#f-ref-all), [find](#f-find)

---

<a name="f-ref-all"></a>
## ref-all

```
syntax: (ref-all val lst [val [true]])
```

Description:

Returns a list of all indices where the given value occurs in the
list.  
Optional comparator or flag modifies matching rules.

Examples:

```
(ref-all 1 '(1 2 1 3 1))   -> (0 2 4)
```

Notes:

- Returns empty list when no matches exist.

See: [ref](#f-ref), [find-all](#f-find-all)

---

<a name="f-regex"></a>
## regex

```
syntax: (regex str str [val [int]])
```

Description:

Matches a regular expression pattern against a target string.  
Returns the index of the first match or nil.  
Optional parameters control regex options and starting offset.

Examples:

```
(regex "Reb" "Rebel")   -> 0
```

Notes:

- Captured groups are accessible via `$`.

See: [regex-comp](#f-regex-comp), [$](#f-dollar)

---

<a name="f-regex-comp"></a>
## regex-comp

```
syntax: (regex-comp str [int])
```

Description:

Compiles a regular expression pattern for repeated use.  
Returns a regex object suitable for passing to `regex` and
related functions.

Examples:

```
(set 'r (regex-comp "Reb"))
(regex r "Rebel")
```

Notes:

- Optional integer controls regex flags.

See: [regex](#f-regex)

---

<a name="f-remove-dir"></a>
## remove-dir

```
syntax: (remove-dir str)
```

Description:

Removes (deletes) an empty directory.  
Returns true on success, nil on failure.

Examples:

```
(remove-dir "oldfolder")
```

Notes:

- Directory must be empty.

See: [make-dir](#f-make-dir), [directory?](#f-directoryp)

---

<a name="f-rename-file"></a>
## rename-file

```
syntax: (rename-file str str)
```

Description:

Renames or moves a file from the source path to the destination
path.  
Returns true on success, nil otherwise.

Examples:

```
(rename-file "a.txt" "b.txt")
```

Notes:

- Overwrites destination depending on system.

See: [copy-file](#f-copy-file), [delete-file](#f-delete-file)

---

<a name="f-replace"></a>
## replace

```
syntax: (replace val lst val [val])
syntax: (replace str str val)
syntax: (replace str str val val)
```

Description:

Replaces occurrences of a pattern in a list or string with a new
value.  
Exact behavior depends on argument types:  
- list mode replaces matching elements  
- string mode replaces substrings or regex matches

Examples:

```
(replace 2 '(1 2 3 2) 9)     -> (1 9 3 9)
(replace "e" "Rebel" "E")   -> "REbEl"
```

Notes:

- For complex matching, use regex mode.

See: [find-all](#f-find-all), [parse](#f-parse)

---

<a name="f-reset"></a>
## reset

```
syntax: (reset int)
syntax: (reset true)
syntax: (reset)
```

Description:

Resets the interpreter’s internal state.  
With an integer, sets new memory limits.  
With `true`, performs a full reset.  
Without arguments, clears temporary state.

Examples:

```
(reset)
(reset 2000000)
```

Notes:

- Dangerous in running programs.

See: [sys-info](#f-sys-info)

---

<a name="f-rest"></a>
## rest

```
syntax: (rest seq)
```

Description:

Returns the tail of a list, array, or string (all elements except
the first).  
Returns nil for empty sequences.

Examples:

```
(rest '(1 2 3))     -> (2 3)
(rest "Rebel")      -> "ebel"
```

Notes:

- Opposite of `first`.

See: [first](#f-first), [last](#f-last)

---

<a name="f-reverse"></a>
## reverse

```
syntax: (reverse seq)
```

Description:

Returns a new sequence with the elements in reverse order.  
Supported for lists, arrays, and strings.

Examples:

```
(reverse '(1 2 3))   -> (3 2 1)
(reverse "Rebel")    -> "lebeR"
```

Notes:

- Non-destructive.

See: [rotate](#f-rotate), [sort](#f-sort)

---

<a name="f-rotate"></a>
## rotate

```
syntax: (rotate lst [int])
syntax: (rotate str [int])
```

Description:

Rotates a list or string by the given number of positions.  
Positive values rotate left; negative rotate right.  
If no integer is provided, rotates by one position.

Examples:

```
(rotate '(1 2 3 4) 1)     -> (2 3 4 1)
(rotate "Rebel" -2)       -> "elReb"
```

Notes:

- Works on lists and strings.

See: [reverse](#f-reverse)

---

<a name="f-round"></a>
## round

```
syntax: (round num [int])
```

Description:

Rounds a number to the nearest integer or to the specified number
of digits.  
Nearest-even rules may apply depending on system.

Examples:

```
(round 3.6)      -> 4
(round 3.14159 2)-> 3.14
```

Notes:

- Supports negative digit counts on some systems.

See: [floor](#f-floor), [ceil](#f-ceil)

---

<a name="f-save"></a>
## save

```
syntax: (save str sym [sym ...])
syntax: (save str)
```

Description:

Saves one or more symbols to a file, storing both value and
metadata.  
With only a filename, saves all user-defined symbols.

Examples:

```
(save "state.rbl" x y)
(save "everything.rbl")
```

Notes:

- Format is interpreter-specific.

See: [load](#f-load), [dump](#f-dump)

---

<a name="f-search"></a>
## search

```
syntax: (search int str [bool [val]])
```

Description:

Searches a file for a substring or regex pattern.  
The file is read line by line. Returns the position of the first
match or nil. Optional boolean and flags modify matching mode.

Examples:

```
(search fd "Rebel")
```

Notes:

- Works on open file descriptors.

See: [read-line](#f-read-line), [regex](#f-regex)

---

<a name="f-seed"></a>
## seed

```
syntax: (seed int true [int])
syntax: (seed int)
syntax: (seed)
```

Description:

Initializes or retrieves the state of the random number
generator.  
With no arguments, returns the current seed.  
With an integer, sets the seed.  
The boolean flag may trigger additional reseeding behavior.

Examples:

```
(seed)
(seed 1234)
```

Notes:

- Affects functions `rand`, `random`, and `normal`.

See: [rand](#f-rand), [random](#f-random)

---

<a name="f-seek"></a>
## seek

```
syntax: (seek int [int])
```

Description:

Moves the file pointer of an open file descriptor to the
specified position.  
Returns the new position or nil on error.

Examples:

```
(seek fd 0)
```

Notes:

- Offset is in bytes.

See: [read](#f-read), [write](#f-write)

---

<a name="f-select"></a>
## select

```
syntax: (select lst [int ...])
syntax: (select lst lst)
syntax: (select str [int ...])
syntax: (select str lst)
```

Description:

Extracts elements or characters by index.  
Indices may be provided as separate integers or as a list.
Returns a new list or string containing only the selected items.

Examples:

```
(select '(a b c d) 0 2)     -> (a c)
(select "Rebel" '(1 3))     -> "eb"
```

Notes:

- Out-of-range indices are ignored.

See: [nth](#f-nth), [slice](#f-slice)

---


<a name="f-self"></a>
## self

```
syntax: (self [int ...])
```

Description:

Returns the current function or lambda being executed.  
With integer indices, returns specific parts of the function
definition such as parameters or body elements.

Examples:

```
(self)
(self 0)
```

Notes:

- Useful for reflective or self-modifying code.

See: [source](#f-source), [lambda?](#f-lambdap)

---

<a name="f-semaphore"></a>
## semaphore

```
syntax: (semaphore int 0)
syntax: (semaphore int int)
syntax: (semaphore int int)
syntax: (semaphore int)
syntax: (semaphore)
```

Description:

Creates or manipulates system semaphores.  
A semaphore may be initialized, signaled, or waited on depending
on the integer arguments. With no arguments, returns semaphore
information.

Examples:

```
(semaphore 1 0)
(semaphore 1 1)
```

Notes:

- Behavior depends on platform semaphore implementation.

See: [sync](#f-sync), [wait-pid](#f-wait-pid)

---

<a name="f-send"></a>
## send

```
syntax: (send int val)
syntax: (send)
```

Description:

Sends a message to another Rebel process identified by its pid.  
With a single argument, retrieves the last sent message.

Examples:

```
(send pid 123)
(send)
```

Notes:

- Used with `receive` for IPC.

See: [receive](#f-receive), [spawn](#f-spawn)

---

<a name="f-sequence"></a>
## sequence

```
syntax: (sequence num num [num])
```

Description:

Generates a list of numbers starting at the first value, ending
at the second value, and optionally stepping by the third value.

Examples:

```
(sequence 1 5)       -> (1 2 3 4 5)
(sequence 1 10 2)    -> (1 3 5 7 9)
```

Notes:

- Step may be negative.

See: [series](#f-series)

---

<a name="f-series"></a>
## series

```
syntax: (series val func int)
syntax: (series num num int)
```

Description:

Generates a list using either repeated application of a function
or multiplicative progression depending on argument types.  
In the functional form, applies `func` to update the value for
each step. In numeric form, generates a geometric series.

Examples:

```
(series 1 (fn (x) (+ x 1)) 5)
(series 2 2 4)         -> (2 4 8 16)
```

Notes:

- Versatile sequence generator.

See: [sequence](#f-sequence)

---

<a name="f-set"></a>
## set

```
syntax: (set sym val [sym val ...])
```

Description:

Assigns values to one or more symbols.  
Each symbol is updated with the corresponding value.

Examples:

```
(set 'x 10 'y 20)
```

Notes:

- Basic assignment operator.

See: [setq](#f-setq), [constant](#f-constant)

---

<a name="f-set-locale"></a>
## set-locale

```
syntax: (set-locale [str [int]])
```

Description:

Sets the runtime locale for character classification and string
operations.  
Without arguments, returns the current locale settings.

Examples:

```
(set-locale "en_US.UTF-8")
```

Notes:

- Affects functions like `upper-case`, `lower-case`.

See: [upper-case](#f-upper-case), [lower-case](#f-lower-case)

---

<a name="f-set-ref"></a>
## set-ref

```
syntax: (set-ref val lst val [val])
```

Description:

Updates the first matching element in a list based on a key and
replacement value.  
Optional comparator controls how matches are detected.

Examples:

```
(set-ref 'x '((x 1) (y 2)) '(x 99))
```

Notes:

- Modifies the list.

See: [replace](#f-replace), [pop-assoc](#f-pop-assoc)

---

<a name="f-set-ref-all"></a>
## set-ref-all

```
syntax: (set-ref-all val lst val [val])
```

Description:

Updates all matching elements in a list using the replacement
value.  
Optional comparator controls equality behavior.

Examples:

```
(set-ref-all 1 '(1 2 1 3) 9)    -> (9 2 9 3)
```

Notes:

- Destructive multi-update operation.

See: [set-ref](#f-set-ref)

---

<a name="f-setq"></a>
## setq

```
syntax: (setq place val [place val ...])
```

Description:

Assigns values directly to places (symbols, array references,
list elements).  
Unlike `set`, this operates on generalized locations.

Examples:

```
(setq x 10 (nth 0 lst) 5)
```

Notes:

- More flexible than `set`.

See: [set](#f-set), [swap](#f-swap)

---

<a name="f-sgn"></a>
## sgn

```
syntax: (sgn num val [val [val]])
syntax: (sgn num)
```

Description:

Returns the sign of a number.  
With one argument, returns -1, 0, or 1.  
With additional thresholds, returns custom values depending on
comparisons with the thresholds.

Examples:

```
(sgn -5)           -> -1
(sgn 5 0 "neg" "pos")
```

Notes:

- Multi-branch sign classification.

See: [min](#f-min), [max](#f-max)

---

<a name="f-share"></a>
## share

```
syntax: (share int val)
syntax: (share int)
syntax: (share nil int)
syntax: (share)
```

Description:

Shares memory or values between processes.  
Depending on arguments, reads a shared value, writes to a shared
location, or allocates shared memory.

Examples:

```
(share addr 100)
(share addr)
```

Notes:

- Low-level IPC primitive.

See: [cpymem](#f-cpymem), [get-int](#f-get-int)

---

<a name="f-signal"></a>
## signal

```
syntax: (signal int "ignore" | "default" | "reset")
syntax: (signal int handler)
syntax: (signal int)
```

Description:

Installs or resets signal handlers for OS-level signals such as
INT or TERM.  
Handlers may be symbols or lambda functions.

Examples:

```
(signal 2 "ignore")
```

Notes:

- Behavior depends on OS signal model.

See: [process](#f-process), [sys-error](#f-sys-error)

---

<a name="f-silent"></a>
## silent

```
syntax: (silent [val [val ...]])
```

Description:

Evaluates all arguments in sequence and returns the last result
while suppressing printed output.  
Useful in scripts where intermediate output is undesired.

Examples:

```
(silent (print "x") 5)     -> 5
```

Notes:

- Only suppresses printed output, not side effects.

See: [begin](#f-begin)

---

<a name="f-sin"></a>
## sin

```
syntax: (sin float)
```

Description:

Computes the sine of the given angle in radians.

Examples:

```
(sin 3.14159)
```

Notes:

- Part of standard math functions.

See: [cos](#f-cos), [tan](#f-tan)

---

<a name="f-sinh"></a>
## sinh

```
syntax: (sinh float)
```

Description:

Computes the hyperbolic sine of a number.

Examples:

```
(sinh 1.0)
```

Notes:

- Uses floating-point hyperbolic functions.

See: [cosh](#f-cosh), [tanh](#f-tanh)

---

<a name="f-sleep"></a>
## sleep

```
syntax: (sleep float)
```

Description:

Pauses execution for the given number of milliseconds.  
Accepts fractional values for sub-millisecond precision if
supported.

Examples:

```
(sleep 100)
```

Notes:

- Blocks the current thread.

See: [time](#f-time)

---

<a name="f-slice"></a>
## slice

```
syntax: (slice seq int [int])
```

Description:

Returns a subsequence of a list, array, or string starting at the
index and optionally spanning the given length.  
Does not modify the original sequence.

Examples:

```
(slice '(1 2 3 4) 1 2)      -> (2 3)
(slice "Rebel" 1 3)         -> "ebe"
```

Notes:

- Negative indices count from the end.

See: [select](#f-select), [nth](#f-nth)

---

<a name="f-sort"></a>
## sort

```
syntax: (sort seq [func])
```

Description:

Returns a sorted copy of a list or array.  
Optional comparator function determines sort order.  
Strings are treated as lists of characters.

Examples:

```
(sort '(3 1 4 2))     -> (1 2 3 4)
```

Notes:

- Non-destructive.

See: [reverse](#f-reverse), [unique](#f-unique)

---


<a name="f-source"></a>
## source

```
syntax: (source sym [sym ...])
syntax: (source)
```

Description:

Returns the source code associated with one or more symbols.
With no arguments, returns the most recently defined function or
macro body.

Examples:

```
(source 'my-func)
(source)
```

Notes:

- Useful for introspection and code inspection.

See: [self](#f-self), [lambda?](#f-lambdap)

---

<a name="f-spawn"></a>
## spawn

```
syntax: (spawn sym val [true])
```

Description:

Creates a new lightweight process running the given function or
lambda, optionally passing it an argument.  
If the boolean flag is true, the new process inherits additional
execution settings.

Examples:

```
(spawn 'worker 100)
```

Notes:

- Returns the pid of the new process.

See: [send](#f-send), [receive](#f-receive)

---

<a name="f-sqrt"></a>
## sqrt

```
syntax: (sqrt float)
```

Description:

Computes the square root of a floating-point number.

Examples:

```
(sqrt 9.0)     -> 3.0
```

Notes:

- Argument must be non-negative unless complex support is added.

See: [pow](#f-pow), [exp](#f-exp)

---

<a name="f-ssq"></a>
## ssq

```
syntax: (ssq lst | arr)
```

Description:

Computes the sum of squared elements in a numeric list or array.
Useful for statistical and vector-based calculations.

Examples:

```
(ssq '(1 2 3))     -> 14
```

Notes:

- Input must contain numeric values.

See: [stats](#f-stats)

---

<a name="f-starts-with"></a>
## starts-with

```
syntax: (starts-with lst [val])
syntax: (starts-with str str [num])
```

Description:

Checks whether a list starts with the given element or whether a
string begins with a given substring.  
Optionally accepts matching options for strings.

Examples:

```
(starts-with '(a b c) 'a)        -> true
(starts-with "Rebel" "Re")       -> true
```

Notes:

- Returns nil if the list or string is shorter than the pattern.

See: [ends-with](#f-ends-with), [member](#f-member)

---

<a name="f-stats"></a>
## stats

```
syntax: (stats lst)
```

Description:

Computes basic statistical measures for a numeric list, including
mean, variance, standard deviation, minimum, and maximum.  
Returns them as a list.

Examples:

```
(stats '(1 2 3 4 5))
```

Notes:

- Input must be numeric.

See: [ssq](#f-ssq)

---

<a name="f-string"></a>
## string

```
syntax: (string val [val ...])
```

Description:

Converts all arguments to strings and concatenates them.  
Returns a new string containing the result.

Examples:

```
(string "Rebel " 123)     -> "Rebel 123"
```

Notes:

- Converts nil to an empty string.

See: [append](#f-append), [lower-case](#f-lower-case)

---

<a name="f-stringp"></a>
## string?

```
syntax: (string? val)
```

Description:

Returns true if the value is a string, nil otherwise.

Examples:

```
(string? "Rebel")   -> true
```

Notes:

- For symbol names, use `symbol?`.

See: [symbol?](#f-symbolp)

---

<a name="f-struct"></a>
## struct

```
syntax: (struct sym [str ...])
```

Description:

Defines a binary structure layout for use with `pack`, `unpack`,
and FFI memory access.  
Format strings specify field types such as integers, floats, or
fixed-size strings.

Examples:

```
(struct Point "int" "int")
```

Notes:

- Used for low-level memory/FFI interop.

See: [pack](#f-pack), [unpack](#f-unpack)

---

<a name="f-sub"></a>
## sub

```
syntax: (sub num [num ...])
```

Description:

Subtracts numbers in sequence.  
With one argument, returns the argument unchanged.  
With multiple arguments, subtracts each subsequent number from
the accumulated result.

Examples:

```
(sub 10 1 2)     -> 7
(sub 5)          -> 5
```

Notes:

- Left-associative subtraction.

See: [-](#f-sub-op)

---

<a name="f-swap"></a>
## swap

```
syntax: (swap place place)
```

Description:

Exchanges the contents of two places (symbols, list positions,
array elements, etc.).  
Modifies both places in one operation.

Examples:

```
(set 'a 1 'b 2)
(swap a b)
```

Notes:

- Works on any assignable location.

See: [setq](#f-setq)

---

<a name="f-sym"></a>
## sym

```
syntax: (sym num [sym [val]])
syntax: (sym str [sym [val]])
syntax: (sym sym [sym [val]])
```

Description:

Constructs or resolves symbols from numbers, strings, or other
symbols.  
Optional context argument assigns the symbol to a specific
namespace.  
Optional nil-flag controls behavior when conversion fails.

Examples:

```
(sym "x")
(sym 65)      -> symbol from character code
```

Notes:

- Fundamental symbol creation mechanism.

See: [symbol?](#f-symbolp), [prefix](#f-prefix)

---

<a name="f-symbolp"></a>
## symbol?

```
syntax: (symbol? val)
```

Description:

Returns true if the value is a symbol, nil otherwise.

Examples:

```
(symbol? 'x)      -> true
```

Notes:

- Use `legal?` to validate symbol names.

See: [legal?](#f-legalp)

---

<a name="f-symbols"></a>
## symbols

```
syntax: (symbols [sym])
```

Description:

Returns all symbols in the current context or in the specified
context.  
Symbols include functions, variables, and structures.

Examples:

```
(symbols)
```

Notes:

- Useful for code browsing and reflection.

See: [context](#f-context), [source](#f-source)

---

<a name="f-sync"></a>
## sync

```
syntax: (sync int [func])
syntax: (sync)
```

Description:

Synchronizes access to a shared resource or executes a handler
when a synchronization object becomes available.  
Without arguments, returns synchronization status.

Examples:

```
(sync sem (fn () (println "done")))
```

Notes:

- Works with semaphores via handles.

See: [semaphore](#f-semaphore)

---

<a name="f-sys-error"></a>
## sys-error

```
syntax: (sys-error 0)
syntax: (sys-error int)
syntax: (sys-error)
```

Description:

Retrieves or sets the last system-level error code.  
With zero, clears the error.  
With an integer, returns the matching error message.  
With no arguments, returns the last error code.

Examples:

```
(sys-error)
```

Notes:

- For I/O, file, and network operations.

See: [last-error](#f-last-error), [net-error](#f-net-error)

---

<a name="f-sys-info"></a>
## sys-info

```
syntax: (sys-info [int])
```

Description:

Returns system information such as memory usage, version,
platform strings, or other runtime statistics.  
The optional index selects a specific information field.

Examples:

```
(sys-info)
(sys-info 0)
```

Notes:

- Fields vary by platform.

See: [reset](#f-reset)

---

<a name="f-t-test"></a>
## t-test

```
syntax: (t-test lst num)
syntax: (t-test lst lst [true])
syntax: (t-test lst lst float)
```

Description:

Performs Student’s t-test for statistical comparison.  
Supports single-sample and two-sample modes, with optional
probability or paired comparison flag.

Examples:

```
(t-test '(1 2 3) 2.0)
```

Notes:

- Returns probability or statistic depending on mode.

See: [stats](#f-stats)

---

<a name="f-tan"></a>
## tan

```
syntax: (tan float)
```

Description:

Computes the tangent of a number in radians.

Examples:

```
(tan 1.0)
```

Notes:

- Related to `sin` and `cos`.

See: [sin](#f-sin), [cos](#f-cos)

---

<a name="f-tanh"></a>
## tanh

```
syntax: (tanh float)
```

Description:

Computes the hyperbolic tangent of a number.

Examples:

```
(tanh 1.0)
```

Notes:

- Hyperbolic function family.

See: [sinh](#f-sinh), [cosh](#f-cosh)

---

<a name="f-term"></a>
## term

```
syntax: (term sym)
```

Description:

Returns the term (unqualified name) of a symbol, i.e., the part
after the context prefix.  
Useful for manipulating symbols from different namespaces.

Examples:

```
(term 'Ctx:x)      -> x
```

Notes:

- Opposite of `prefix`.

See: [prefix](#f-prefix)

---


<a name="f-throw"></a>
## throw

```
syntax: (throw val)
```

Description:

Raises an exception with the given value.  
Execution jumps to the nearest enclosing `catch` form.  
If no handler is found, the interpreter aborts evaluation.

Examples:

```
(throw "error")
```

Notes:

- Used for non-local control flow.

See: [catch](#f-catch), [throw-error](#f-throw-error)

---

<a name="f-throw-error"></a>
## throw-error

```
syntax: (throw-error val)
```

Description:

Raises a system-level error and stops execution unless handled by
`error-event`.  
Used to signal internal or unrecoverable errors.

Examples:

```
(throw-error "fatal")
```

Notes:

- Unlike `throw`, this always triggers the error handler.

See: [throw](#f-throw), [error-event](#f-error-event)

---

<a name="f-time"></a>
## time

```
syntax: (time val [int])
```

Description:

Evaluates an expression repeatedly for performance measurement.  
Optional integer specifies the number of iterations.  
Returns the total elapsed time in milliseconds.

Examples:

```
(time (+ 1 2) 10000)
```

Notes:

- Useful for benchmarking functions.

See: [now](#f-now), [time-of-day](#f-time-of-day)

---

<a name="f-time-of-day"></a>
## time-of-day

```
syntax: (time-of-day)
```

Description:

Returns the current time of day as a floating-point number
representing seconds after midnight.

Examples:

```
(time-of-day)
```

Notes:

- High-resolution timer on supporting systems.

See: [now](#f-now)

---

<a name="f-timer"></a>
## timer

```
syntax: (timer handler)
syntax: (timer)
```

Description:

Installs or clears a periodic timer handler.  
When a handler is installed, it is executed at regular time
intervals determined by the system.

Examples:

```
(timer 'tick)
(timer nil)
```

Notes:

- REPL-level periodic events.

See: [prompt-event](#f-prompt-event)

---

<a name="f-title-case"></a>
## title-case

```
syntax: (title-case str [bool])
```

Description:

Converts the string to title case, capitalizing the first letter
of each word.  
Optional boolean controls behavior for non-letter characters.

Examples:

```
(title-case "rebel language")     -> "Rebel Language"
```

Notes:

- Locale-dependent.

See: [lower-case](#f-lower-case), [upper-case](#f-upper-case)

---

<a name="f-trace"></a>
## trace

```
syntax: (trace int)
syntax: (trace nil)
syntax: (trace true)
syntax: (trace)
```

Description:

Controls function call tracing for debugging.  
With an integer, selects a tracing device.  
With `true`, enables tracing to the default device.  
With `nil`, disables tracing.

Examples:

```
(trace true)
(trace nil)
```

Notes:

- Useful for debugging recursive code.

See: [trace-highlight](#f-trace-highlight)

---

<a name="f-trace-highlight"></a>
## trace-highlight

```
syntax: (trace-highlight str str [str str])
```

Description:

Highlights printed trace output by wrapping expressions with
prefix and postfix strings.  
Optional header and footer strings further customize output.

Examples:

```
(trace-highlight ">> " " <<")
```

Notes:

- Affects only traced expressions.

See: [trace](#f-trace)

---

<a name="f-transpose"></a>
## transpose

```
syntax: (transpose matrix)
```

Description:

Computes the transpose of a matrix.  
Rows become columns and columns become rows.

Examples:

```
(transpose '((1 2) (3 4)))
```

Notes:

- Non-destructive.

See: [invert](#f-invert), [multiply](#f-multiply)

---

<a name="f-trim"></a>
## trim

```
syntax: (trim str str)
syntax: (trim str str str)
syntax: (trim str)
```

Description:

Removes leading and trailing characters from a string.  
With one character argument, trims all occurrences of that
character.  
With two characters, trims left and right independently.

Examples:

```
(trim "  Rebel  " " ")
```

Notes:

- Default trimming removes whitespace.

See: [parse](#f-parse)

---

<a name="f-truep"></a>
## true?

```
syntax: (true? val)
```

Description:

Returns true if the value is not nil, and nil otherwise.  
Equivalent to checking whether a value counts as true in
conditional expressions.

Examples:

```
(true? 1)      -> true
(true? nil)    -> nil
```

Notes:

- Only nil is considered false.

See: [not](#f-not)

---

<a name="f-unicode"></a>
## unicode

```
syntax: (unicode str)
```

Description:

Converts a UTF-8 encoded string into a list of Unicode code
points.  
Each character becomes its integer Unicode value.

Examples:

```
(unicode "Rebel")
```

Notes:

- Opposite of `utf8`.

See: [utf8](#f-utf8)

---

<a name="f-unify"></a>
## unify

```
syntax: (unify val val [lst])
```

Description:

Attempts to unify two expressions by finding a consistent set of
bindings that make them structurally equal.  
Optional environment list initializes variable bindings.

Examples:

```
(unify '(x 2) '(1 2))
```

Notes:

- Used for pattern matching and logic programming.

See: [match](#f-match)

---

<a name="f-union"></a>
## union

```
syntax: (union lst lst [lst ...])
```

Description:

Computes the union of two or more lists, returning a list that
contains all unique elements from all inputs.

Examples:

```
(union '(1 2) '(2 3))      -> (1 2 3)
```

Notes:

- Order is based on first occurrences.

See: [intersect](#f-intersect), [unique](#f-unique)

---

<a name="f-unique"></a>
## unique

```
syntax: (unique lst)
```

Description:

Returns a list containing only the unique elements of the input
list, removing duplicates while preserving the original order.

Examples:

```
(unique '(1 2 1 3 2))     -> (1 2 3)
```

Notes:

- Equality is based on standard comparison.

See: [sort](#f-sort), [union](#f-union)

---

<a name="f-unless"></a>
## unless

```
syntax: (unless val body)
```

Description:

Evaluates the body only if the condition is nil.  
Equivalent to `(if (not val) body)`.

Examples:

```
(unless nil (println "yes"))
```

Notes:

- Opposite of `when`.

See: [when](#f-when)

---

<a name="f-unpack"></a>
## unpack

```
syntax: (unpack str num)
syntax: (unpack str str)
syntax: (unpack struct num)
syntax: (unpack struct str)
```

Description:

Unpacks binary data from a number (address) or string according
to a format string or struct definition.  
Returns a list of extracted values.

Examples:

```
(unpack "ici" some-bytes)
```

Notes:

- Inverse of `pack`.

See: [pack](#f-pack), [struct](#f-struct)

---

<a name="f-until"></a>
## until

```
syntax: (until val [body])
```

Description:

Repeats evaluation of the body until the condition becomes true.
If no body is supplied, evaluates the condition repeatedly.

Examples:

```
(until (> x 10) (set 'x (+ x 1)))
```

Notes:

- Condition checked before each iteration.

See: [while](#f-while), [do-until](#f-do-until)

---

<a name="f-upper-case"></a>
## upper-case

```
syntax: (upper-case str)
```

Description:

Returns a new string with all alphabetic characters converted to
uppercase according to the current locale.

Examples:

```
(upper-case "Rebel")     -> "REBEL"
```

Notes:

- Locale-dependent.

See: [lower-case](#f-lower-case), [title-case](#f-title-case)

---


<a name="f-utf8"></a>
## utf8

```
syntax: (utf8 str)
```

Description:

Encodes a list of Unicode code points into a UTF-8 string.  
Each integer in the list becomes its UTF-8 encoded character.

Examples:

```
(utf8 '(82 101 98 101 108))   -> "Rebel"
```

Notes:

- Opposite of `unicode`.

See: [unicode](#f-unicode)

---

<a name="f-utf8len"></a>
## utf8len

```
syntax: (utf8len str)
```

Description:

Returns the number of characters in a UTF-8 encoded string,
counting multi-byte sequences as a single character.

Examples:

```
(utf8len "Rebel")      -> 5
```

Notes:

- Counts characters, not bytes.

See: [utf8](#f-utf8)

---

<a name="f-uuid"></a>
## uuid

```
syntax: (uuid [str])
```

Description:

Generates a UUID string.  
With an optional node identifier string, uses it to influence the
generated UUID.

Examples:

```
(uuid)
(uuid "rebel-node")
```

Notes:

- Returns a 36-character canonical UUID.

See: [random](#f-random)

---

<a name="f-wait-pid"></a>
## wait-pid

```
syntax: (wait-pid int [val | nil])
```

Description:

Waits for a process with the given pid to terminate.  
Optional mode argument modifies wait behavior or returned info.

Examples:

```
(wait-pid pid)
```

Notes:

- Returns exit status or pid depending on system.

See: [spawn](#f-spawn), [process](#f-process)

---

<a name="f-when"></a>
## when

```
syntax: (when val body)
```

Description:

Evaluates the body only if the condition is true.  
Equivalent to `(if val body)`.

Examples:

```
(when (> x 10) (println "ok"))
```

Notes:

- Opposite of `unless`.

See: [unless](#f-unless)

---

<a name="f-while"></a>
## while

```
syntax: (while val body)
```

Description:

Repeatedly evaluates the body while the condition remains true.
The condition is tested before each iteration.

Examples:

```
(while (< x 5) (set 'x (+ x 1)))
```

Notes:

- Pre-test loop.

See: [until](#f-until), [do-while](#f-do-while)

---

<a name="f-write"></a>
## write

```
syntax: (write int str [int])
syntax: (write str str [int])
syntax: (write)
```

Description:

Writes data to a file descriptor or to a file.  
The optional integer limits the number of bytes to write.
Returns the number of bytes written or nil on failure.

Examples:

```
(write fd "Hello")
(write "out.txt" "Data")
```

Notes:

- Use `write-file` for whole-file writes.

See: [read](#f-read), [write-file](#f-write-file)

---

<a name="f-write-char"></a>
## write-char

```
syntax: (write-char int int [int ...])
```

Description:

Writes one or more raw byte values to a file descriptor.  
Each integer represents a byte (0–255).

Examples:

```
(write-char fd 65 66 67)
```

Notes:

- For binary output.

See: [read-char](#f-read-char)

---

<a name="f-write-file"></a>
## write-file

```
syntax: (write-file str str)
```

Description:

Writes a string to a file, replacing its contents.  
Returns the number of bytes written or nil on error.

Examples:

```
(write-file "msg.txt" "Hello Rebel")
```

Notes:

- Use `append-file` to add to existing files.

See: [append-file](#f-append-file), [read-file](#f-read-file)

---

<a name="f-write-line"></a>
## write-line

```
syntax: (write-line [int [str]])
syntax: (write-line str)
```

Description:

Writes a line terminated with a newline character to a file
descriptor or to a file.  
With no arguments, writes an empty line to standard output.

Examples:

```
(write-line "out.txt" "Hello")
(write-line)
```

Notes:

- Returns number of bytes written.

See: [write](#f-write)

---

<a name="f-xfer-event"></a>
## xfer-event

```
syntax: (xfer-event handler)
```

Description:

Installs a handler to process cross-thread or cross-process
transfer events.  
Used internally for asynchronous messaging between execution
contexts.

Examples:

```
(xfer-event 'on-xfer)
```

Notes:

- Advanced feature for cooperative concurrency.

See: [spawn](#f-spawn), [send](#f-send)

---

<a name="f-xml-error"></a>
## xml-error

```
syntax: (xml-error)
```

Description:

Returns details about the last XML parsing error.  
Useful after calls to `xml-parse`. Returns nil if no error is
present.

Examples:

```
(xml-error)
```

Notes:

- Error info is reset after successful parse.

See: [xml-parse](#f-xml-parse)

---

<a name="f-xml-parse"></a>
## xml-parse

```
syntax: (xml-parse str [int [sym [func]]])
```

Description:

Parses an XML document and returns its parsed structure as nested
lists.  
Optional integer modifies parsing options, a context can be
provided for XML tags, and an optional callback is invoked for
each parsed element.

Examples:

```
(xml-parse "<a><b>c</b></a>")
```

Notes:

- Enables lightweight XML handling.

See: [xml-error](#f-xml-error)

---

<a name="f-zerop"></a>
## zero?

```
syntax: (zero? val)
```

Description:

Returns true if the value is zero (integer or float).  
Returns nil for all other values.

Examples:

```
(zero? 0)     -> true
(zero? 1.0)   -> nil
```

Notes:

- Numeric predicate.

See: [sgn](#f-sgn), [number?](#f-numberp)

---

<a name="f-or-op"></a>
## |

```
syntax: (| int int [int ...])
```

Description:

Performs bitwise OR on two or more integers.  
Returns the combined integer result.

Examples:

```
(| 2 4)     -> 6
```

Notes:

- All arguments must be integers.

See: [&](#f-ampersand), [^](#f-xor)

---

<a name="f-tilde"></a>
## ~

```
syntax: (~ int)
```

Description:

Computes the bitwise NOT (complement) of an integer.  
Each bit is inverted.

Examples:

```
(~ 0)       -> -1
```

Notes:

- Uses two’s-complement arithmetic.

See: [|](#f-or-op), [&](#f-ampersand)

---

<a name="f-format2"></a>
## format

```
syntax: (format str val [val ...])
```

Description:

Formats values according to a printf-style format string.  
Returns the formatted string.  
Supports numeric, string, and alignment specifiers depending on
runtime capabilities.

Examples:

```
(format "%d %s" 42 "Rebel")
```

Notes:

- Does not print; only returns formatted output.

See: [string](#f-string)

---

<!-- EOF -->
