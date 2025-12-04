<table>
<tr>
<td><img src="docs/logo_v0.svg" height="50"></td>
<td><h1>Rebel Language</h1></td>
</tr>
</table>

---

[![Ubuntu Rebel](https://github.com/ufko-org/rebel/actions/workflows/linux.yml/badge.svg)](https://github.com/ufko-org/rebel/actions/workflows/linux.yml) 
[![macOS Rebel](https://github.com/ufko-org/rebel/actions/workflows/macos.yml/badge.svg)](https://github.com/ufko-org/rebel/actions/workflows/macos.yml)

---

- Rebel is a pragmatic general-purpose scripting language written in ANSI C.
- Rebel has no build system, just plain make.
- Rebel builds cleanly with Clang, GCC, or TCC; with TCC under 0.5s.
- Rebel comes with a core of 373 [built-in functions](https://github.com/ufko-org/rebel/blob/main/docs/builtin.txt) plus FFI in a single binary under 350KB.
- Rebel has **lisp-like** syntax and some **lisp-like** features.
- Rebel hopes you understand the s-expression **(lisp-like)**.
- Rebel doesn’t compete with other languages.
- Rebel is simply Rebel.

### Rebel Fields of Operation

- **Core Scripting & Flow Control**
  - loops, conditionals, list processing, lambda, functional programming
- **Strings & Data Manipulation**
  - regex, parsing, conversion, unicode, text transformation
- **Math & Statistics**
  - integers, floats, linear algebra, special functions, probability, simulation
- **File & I/O**
  - file handling, directories, reading/writing, streams, HTTP file access
- **Networking**
  - TCP/IP, UDP, HTTP, JSON/XML, net-eval, sockets
- **Processes & Multiprocessing**
  - forks, Cilk-style API, IPC, semaphores, process management
- **Reflection & Customization**
  - macros, events, symbols, environment introspection
- **Finance & Time**
  - basic financial math, date and time functions
- **Integration & Extensibility**
  - import C libraries, extend interpreter, modular namespaces
- **[List of built-in functions](https://github.com/ufko-org/rebel/blob/main/docs/builtin.txt)**

### Rebel runs on: 

- BSD 
- Linux   
- macOS  

## Installation steps

To build Rebel on your system you need to have the following
libraries installed:

- A readline library for line-editing and history support in
  the REPL
- A libffi installation for calling native C functions
  through the FFI interface

If you don't want these features, remove their references
in the Makefile before building.

```
$: git clone https://github.com/ufko-org/rebel.git
$: cd rebel/src
$: make -f Makefile.[bsd|linux|macos]
$: ./rebel
```

### Rebel ~ size OpenBSD 7.8

```
$: ls -l rebel
-rwxr-xr-x  1 ufko  ufko  344560 Nov 19 23:32 rebel*
```

### Rebel REPL in OpenBSD 7.8:

Rebel can change his running shoes mid-sprint.

```
gringo ~ $: rebel
Rebel v.1.0 64-bit on BSD IPv4/6 UTF-8 libffi, options: rebel -h

> (define (sum x) (+ x x))
(lambda (x) (+ x x))
> (sum 2)
4
> (list? sum)
true
> (setf (nth 1 sum) '(- x x))
(- x x)
> (sum 2) ; surprise :D
0
>
```

Rebel remembers things

```
> (define (gen:gen) (inc 0))
(lambda () (inc 0))
> (gen)
1
> (gen)
2
> (gen)
3
>
```

Rebel loves all C libraries 

```
> (import "/usr/lib/libc.so.102.0" "printf")
printf@F3D306ED040
> (printf "Hello rebels :)\n")
Hello rebels :)
16
>
```

### Links:

- Homepage: https://rebel-lang.org
- Codeberg repo (primary): https://codeberg.org/ufko/rebel
- Github repo (mirror): https://github.com/ufko-org/rebel
- DeepWiki analysis: https://deepwiki.com/ufko-org/rebel/1.1-language-heritage-and-philosophy
