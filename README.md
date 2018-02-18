# elisp-def

Go to the definition of the symbol at point, intelligently handling
macros, and distinguishing functions from variables.

It statically analyses your code, and falls back to heuristics. It
should work 99% of the time, so please file bugs if it can't find
definitions for your code.

## Find Definitions

`elisp-def` will find the definition of functions and global variables
at point.

``` emacs-lisp
(defun demo/foo ()
  1)

(defun demo/bar ()
  ;; M-x eval-buffer, then elisp-def on this:
  (demo/foo))
```

It will also use edebug information to find function definitions, so
it finds definitions more often than xref.

## Lisp-2 Awareness

`elisp-def` understands the difference between symbols and functions
and jumps to the correct definition.

``` emacs-lisp
(require 'cc-mode)

;; `c-version' is both a variable and a function.

(defun demo/foo ()
  ;; `elisp-def` will find the function here.
  (c-version))

(defun demo/foo ()
  ;; `elisp-def` will find the variable here.
  (setq c-version t))
```

`elisp-def` understands macros too.

``` emacs-lisp
(require 'dash)

(defvar demo/foo nil)

(defun demo/foo (x)
  x)

(defun demo/bar ()
  (->> 123
       ;; `elisp-def' knows that this is a function, even though there are
       ;; no parens.
       demo/foo))
```

## Find Libraries

`elisp-def` will find libraries, displaying the `provide` declarations
if possible.

``` emacs-lisp
;; `elisp-def' will open python.el here.
(require 'python)

;; Unlike `xref-find-definition', `elisp-def' will not confuse this
;; library name with the macro named `use-package'.
(require 'use-package)

;; `elisp-def' will even find python.el here, because the macro
;; expands to a call to `require'.
(use-package python
  :config
  (setq python-indent-guess-indent-offset-verbose nil))
```

## Local Bindings

`let`

## Caveats

Macros that rewrite bodies will fail, such as `cl-labels`.

Let macros that repeat bindings.

Quoted symbols.

## Thanks/Inspirations

* [elisp-slime-nav](https://github.com/purcell/elisp-slime-nav)
* `xref` (part of Emacs core)

The fine folks on `#emacs` for answering my questions on elisp
esoterica, particularly Wasamasa.
