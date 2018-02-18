# elisp-def [![Build Status](https://travis-ci.org/Wilfred/elisp-def.svg?branch=master)](https://travis-ci.org/Wilfred/elisp-def)

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

`elisp-def` understands local bindings and parameters.

``` emacs-lisp
(defun demo/foo (bar)
  (let ((foo 1))
    ;; `elisp-def' on the FOO below will move point to the let
    ;; binding.
    (setq foo 2)
    ;; `elisp-def' on the BAR below will move point to the function
    ;; parameters line.
    (setq bar 3)))
```

This even works with macros that introduce bindings.

``` emacs-lisp
(require 'dash)
(eval-when-compile
  (require 'cl-lib))

(defun demo/foo (items)
  (cl-destructuring-bind (first second) items
    ;; `elisp-def' knowns that FIRST is bound on line above.
    (message "first is %s" first))
  (-let [(first . rest) items]
    ;; `elisp-def' knowns that FIRST is bound on line above.
    (message "first is %s" first)))
```

`elisp-def` handles nested `let`, `let*` and `condition-case`
intelligently too.

## Caveats

`elisp-def` is limited in its ability to analyse quoted symbols.

``` emacs-lisp
;; `elisp-def' is able to find these quoted symbols because they're
;; only globally bound in one namespace.
(mapcar 'symbol-name '(foo bar baz))
(add-to-list 'auto-mode-alist '("\\.java\\'" . java-mode))

(require 'cc-mode)
(defun demo/calls-fn (sym)
  (funcall sym))

;; Since `c-version' is both a function and a variable, and we're not
;; using a sharp-quote #'c-version, we have to prompt the user.
(demo/calls-fn 'c-version)
```

Macros that rewrite bodies will fail, such as `cl-labels`.

Let macros that repeat bindings.

Quoted symbols.

## Thanks/Inspirations

* [elisp-slime-nav](https://github.com/purcell/elisp-slime-nav)
* `xref` (part of Emacs core)

The fine folks on `#emacs` for answering my questions on elisp
esoterica, particularly Wasamasa.
