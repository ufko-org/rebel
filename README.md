title:Rebel language
css:rebel.css

# Rebel language

[![Ubuntu Rebel](https://github.com/ufko-org/rebel/actions/workflows/linux.yml/badge.svg)](https://github.com/ufko-org/rebel/actions/workflows/linux.yml)
[![macOS Rebel](https://github.com/ufko-org/rebel/actions/workflows/macos.yml/badge.svg)](https://github.com/ufko-org/rebel/actions/workflows/macos.yml)

- Rebel is a pragmatic general-purpose scripting language written in ANSI C.
- Rebel comes with a core of over 370 [built-in functions](https://github.com/ufko-org/rebel/blob/main/built-in-functions.txt) in single binary.
- Rebel has **lisp-like** syntax and some **lisp-like** features.
- Rebel hopes you understand the expression **lisp-like**.
- Rebel is not meant to be compared with or compete against other languages in terms of performance or features
- Rebel is simply Rebel.

### Rebel runs on: 

- BSD 
- Linux   
- macOS  

### Rebel size OpenBSD 7.8

```
$: ls -l rebel
-rwxr-xr-x  1 ufko  ufko  344560 Nov 19 23:32 rebel*
```

### [List of built-in functions](https://github.com/ufko-org/rebel/blob/main/built-in-functions.txt)


### Installation steps:

```
$: git clone https://github.com/ufko-org/rebel.git
$: cd rebel/src
$: make -f Makefile.openbsd
or
$: make -f Makefile.linux
$: ./rebel
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
- Github repo: https://github.com/ufko-org/rebel
- Github organization: https://github.com/rebellang
- DeepWiki analysis: https://deepwiki.com/ufko-org/rebel/1.1-language-heritage-and-philosophy
