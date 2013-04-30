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

<pre><code>(<span class="stdmacro">in-package</span> #:<span class="symbol">lowlight</span>)
</code></pre>

To highlight a string use `light`

<pre><code>(<span class="stdfun">light</span> <span class="keyword">:common-lisp</span> <span class="string">":bla"</span>) <span class="comment">;=&gt; "&lt;span class=\"keyword\"&gt;:bla&lt;/span&gt;"
</span></code></pre>

If you want to highlight a whole file, use `light-file`
<pre><code>(<span class="stdfun">light-file</span> <span class="keyword">:common-lisp</span> <span class="string">"~/lowlight.lisp"</span> <span class="keyword">:css</span> <span class="string">"github-colors.css"</span>)
</code></pre>