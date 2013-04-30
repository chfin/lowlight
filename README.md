# lowlight
A simple, regex based syntax highlighter in Common Lisp.

## Features

* higlighting code in html spans
* themable with css
* string and file highlighting
* simple and flexible highlighting engine
* based on regular expressions

### Supported languages

* Common Lisp

I will probably support more languages in the future,
but you can also define your own language styles.
Also, patches to get your language style included are welcome.

## Installation

ASDF, hopefully Quicklisp in the future.

## Usage

All lowlight functions and macros live in the package `lowlight`

```common-lisp
(in-package #:lowlight)
```

To highlight a string use `light`

```common-lisp
(light :common-lisp ":bla") ;=> "<span class=\"keyword\">:bla</span>"
```

If you want to highlight a whole file, use `light-file`
```common-lisp
(light-file :common-lisp "~/lowlight.lisp" :css "github-colors.css")
```