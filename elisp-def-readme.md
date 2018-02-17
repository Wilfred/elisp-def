# elisp-def

Go to the definition of the symbol at point, intelligently handling
macros, and distinguishing functions from variables.

## Find Function Definitions

``` emacs-lisp
(defvar wh/foo nil)

(defun wh/foo ()
  1)

;; Try elisp-def on the next symbol.
(wh/foo)
```

## Intelligent Namespace Handling

## Macro Aware

``` emacs-lisp
(defvar wh/foo nil)

(defun wh/foo (x)
  1)

(require 'dash)
(->> 1
     ;; elisp-def knows that this is a function, even though there are
     ;; no parens.
     wh/foo)
```

## Find Packages

## Local Bindings

`let`

## Caveats

Macros that rewrite bodies will fail, such as `cl-labels`.

Let macros that repeat bindings.

## Thanks/Inspirations

* [elisp-slime-nav](https://github.com/purcell/elisp-slime-nav)
* `xref` (part of Emacs core)

The fine folks on `#emacs` for answering my questions on elisp
esoterica, particularly Wasamasa.
