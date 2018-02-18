;;; elisp-def.el --- macro-aware go-to-definition for elisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Wilfred Hughes

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Find the definition of the symbol at point, intelligently.
;;
;; TODO: fonts

;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'find-func)
(require 'thingatpt)
(require 'xref)
(require 'ert)

(defun elisp-def--flash-region (start end)
  "Temporarily highlight region from START to END."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face 'highlight)
    (run-with-timer 0.5 nil 'delete-overlay overlay)))

(defun elisp-def--find-library-name (library)
  "A wrapper around `find-library-name' that returns nil if PATH
has no library with that name.

This can happen when users have installed Emacs without its
source code: they have e.g. org.elc but no org.el."
  (condition-case nil
      (find-library-name library)
    (error nil)))

(defun elisp-def--primitive-p (sym callable-p)
  "Return t if SYM is defined in C."
  (if callable-p
      (subrp (indirect-function sym))
    (let ((filename (find-lisp-object-file-name sym 'defvar)))
      (or (eq filename 'C-source)
          (and (stringp filename)
               (equal (file-name-extension filename) "c"))))))

(defun elisp-def--find-feature (sym)
  "Find the buffer and position where feature SYM is defined."
  (let ((path (elisp-def--find-library-name (symbol-name sym)))
        buf pos)
    (when path
      (setq buf (find-file-noselect path))
      (with-current-buffer buf
        (save-excursion
          (save-restriction
            ;; TODO: caller should widen if necessary.
            (widen)

            (goto-char (point-min))
            (re-search-forward
             (rx-to-string
              `(seq "("
                    (0+ whitespace)
                    symbol-start "provide" symbol-end
                    (1+ whitespace)
                    "'" (0+ whitespace)
                    ,(symbol-name sym)))
             nil
             t)
            (setq pos (line-beginning-position))))))
    (list buf pos)))

(defun elisp-def--find-function (sym)
  "Find the buffer and position where function SYM is defined.

This is the function _slot_ of SYM, so SYM may be a function or macro."
  (let ((primitive-p (elisp-def--primitive-p sym t))
        path buf pos)
    (-let [(base-sym . src-path) (find-function-library sym)]
      ;; `base-sym' is the underlying symbol if `sym' is an alias.
      (setq sym base-sym)
      (setq path src-path))
    (when (and primitive-p path find-function-C-source-directory)
      ;; Convert "src/foo.c" to "".
      (setq path (f-expand path
                           (f-parent find-function-C-source-directory))))

    (cond
     (path
      ;; Convert foo.elc to foo.el.
      (-when-let (src-path (elisp-def--find-library-name path))
        ;; Open `path' ourselves, so we can widen before searching.
        (setq buf (find-file-noselect src-path))

        ;; Based on `find-function-noselect'.
        (with-current-buffer buf
          ;; If the symbol is defined outside the current range, widen, otherwise preserve narrowing.
          (save-restriction
            (widen)
            (setq pos
                  (if primitive-p
                      (cdr (find-function-C-source sym path nil))
                    (cdr (find-function-search-for-symbol sym nil path)))))
          (when (or (< pos (point-min))
                    (> pos (point-max)))
            (widen)))))
     (t
      ;; Functions defined interactively may have an edebug property
      ;; that contains the location of the definition.
      (-when-let (edebug-info (get sym 'edebug))
        (-let [marker (if (consp edebug-info)
                          (car edebug-info)
                        edebug-info)]
          (setq buf (marker-buffer marker))
          (setq pos (marker-position marker))))))
    (list buf pos)))

(defun elisp-def--find-variable (sym)
  "Find the buffer and position where variable SYM is defined."
  (let (buf pos)
    (condition-case nil
        (-let [(sym-buf . sym-pos) (find-definition-noselect sym 'defvar)]
          (setq buf sym-buf)
          (setq pos sym-pos))
      (search-failed nil)
      ;; If your current Emacs instance doesn't match the source
      ;; code configured in find-function-C-source-directory, we can
      ;; get an error about not finding source. Try
      ;; `default-tab-width' against Emacs trunk.
      (error nil))
    (list buf pos)))

(defun elisp-def--defined-in (sym)
  "All the namespaces that SYM is globally defined in.
Returns a list '(function variable).

Note that macros are in the same namespace as functions."
  (let (result)
    (when (boundp sym)
      (push 'variable result))
    ;; Function or macro.
    (when (fboundp sym)
      (push 'function result))
    result))

;; TODO: consider the following example:

;; (cl-labels ((foo (x y) (+ x y)))
;;   (foo 1 2))
;;
;; If you expand this, you can see `cl-labels' replaces foo with
;; --cl-foo-- so we can't identify where foo is defined or whether
;; it's a function call or a variable.

(defun elisp-def--sharp-quoted-p ()
  "Is the symbol at point of the form #'foo?"
  (save-excursion
    (re-search-forward (rx symbol-end))
    (backward-sexp)
    (looking-at (rx "#'"))))

(ert-deftest elisp-def--sharp-quoted-p ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "#'abc")
    (goto-char (point-min))
    (dotimes (i 3)
      (message "i %s" i)
      (should (elisp-def--sharp-quoted-p))
      (forward-char))))

(defun elisp-def--namespace-at-point ()
  "Is the symbol at point a function/macro, a global variable, a
quoted variable, or a let-bound variable?"
  ;; If it's a sharp quoted symbol, we know it's a global function
  ;; reference.
  (if (elisp-def--sharp-quoted-p)
      'function
    ;; Otherwise, macro expand the source at point and look at how the
    ;; symbol is used.
    (-let* (((form-start form-end) (elisp-def--defun-start))
            (placeholder (elisp-def--fresh-placeholder))
            (src (elisp-def--source-with-placeholder form-start form-end placeholder))
            (form (read src))
            ;; TODO: what if SYM disappears after expanding? E.g. inside rx.
            (expanded-form (macroexpand-all form))
            (use (elisp-def--use-position expanded-form placeholder)))
      ;; If it's being used as a variable, see if it's let-bound.
      (when (eq use 'variable)
        (let* ((sym (elisp-def--symbol-at-point))
               (bound-syms (elisp-def--bound-syms
                            expanded-form placeholder)))
          (when (memq sym bound-syms)
            (setq use 'bound))))
      use)))

(ert-deftest elisp-def--namespace-at-point ()
  ;; If it's the head of a sexp, this is a function.
  (with-temp-buffer
    (insert "(foo bar)")

    (goto-char (point-min))
    (search-forward "foo")
    (should
     (eq (elisp-def--namespace-at-point)
         'function)))
  ;; If it's an argument to a function, it's a variable.
  (with-temp-buffer
    (insert "(foo bar)")

    (goto-char (point-min))
    (search-forward "bar")
    (should
     (eq (elisp-def--namespace-at-point)
         'variable)))
  ;; Handle let-bound variables.
  (with-temp-buffer
    (insert "(let ((x 1)) (1+ x))")

    (goto-char (point-min))
    (search-forward "1+")
    (search-forward "x")

    (should
     (eq (elisp-def--namespace-at-point)
         'bound)))
  ;; Handle let-bound variables introduced by macros.
  (with-temp-buffer
    (insert "(destructuring-bind (x y) z (1+ x))")

    (goto-char (point-min))
    (search-forward "1+")
    (search-forward "x")

    (should
     (eq (elisp-def--namespace-at-point)
         'bound)))
  ;; Handle function parameters.
  (with-temp-buffer
    (insert "(defun foo (x) (1+ x))")

    (goto-char (point-min))
    (search-forward "1+")
    (search-forward "x")

    (should
     (eq (elisp-def--namespace-at-point)
         'bound)))
  ;; Handle binding introduced by condition-case.
  (with-temp-buffer
    (insert "(condition-case e (foo) (error e))")

    (goto-char (point-min))
    (search-forward "error")
    (search-forward "e")

    (should
     (eq (elisp-def--namespace-at-point)
         'bound)))
  ;; Quoted references.
  (with-temp-buffer
    (insert "(foo 'bar)")

    (goto-char (point-min))
    (search-forward "bar")

    (should
     (eq (elisp-def--namespace-at-point)
         'quoted)))
  ;; Handle references to libraries.
  (with-temp-buffer
    (insert "(require 'foo)")

    (goto-char (point-min))
    (search-forward "foo")

    (should
     (eq (elisp-def--namespace-at-point)
         'library))))

(defun elisp-def--use-position (form sym &optional quoted)
  "Is SYM being used as a function, a global variable, a
library/feature, a bound variable definition, or a quoted symbol
in FORM?

Assumes FORM has been macro-expanded."
  (cond
   ((symbolp form)
    (if (eq form sym)
        ;; Normal reference to the variable we're looking for.
        (if quoted 'quoted 'variable)
      ;; Unrelated variable.
      nil))
   ((consp form)
    (cond
     ;; Lambda parameters are variable definitions.
     ((and (eq (car form) 'lambda)
           (memq sym (cadr form)))
      'definition)
     ;; Let forms can introduce definitions too.
     ((and (memq (car form) (list 'let 'let*))
           (-let [bindings (cadr form)]
             (--any-p
              (or
               ;; (let (foo ...) ...)
               (eq it sym)
               ;; (let ((foo ...)) ...)
               (and (consp it) (eq (car it) sym)))
              bindings)))
      'definition)
     ;; Explicit call to `require'.
     ((and (eq (car form) 'require)
           (equal (car-safe (cdr form)) `(quote ,sym)))
      'library)
     ((eq (car form) sym)
      ;; Function call for the symbol we're looking for.
      (if quoted 'quoted 'function))
     ;; See if this is a quoted form that contains SYM.
     ((eq (car form) 'quote)
      (if (ert--proper-list-p (cdr form))
          (--any (elisp-def--use-position it sym t) (cdr form))
        (elisp-def--use-position (cdr form) sym t)))
     ;; Recurse on the form to see if any arguments contain SYM.
     (t
      (if (ert--proper-list-p form)
          (--any (elisp-def--use-position it sym quoted) form)
        (or
         (elisp-def--use-position (car form) sym quoted)
         (elisp-def--use-position (cdr form) sym quoted))))))
   ((vectorp form)
    ;; All elements in a vector are quoted.
    (--any (elisp-def--use-position it sym t)
           (mapcar #'identity form)))))

(ert-deftest elisp-def--use-position ()
  (should
   (eq (elisp-def--use-position '(foo bar) 'foo)
       'function))
  (should
   (eq (elisp-def--use-position '(foo bar) 'bar)
       'variable))
  (should
   (eq (elisp-def--use-position [foo] 'foo)
       'quoted))
  (should
   (eq (elisp-def--use-position '(foo '(bar)) 'bar)
       'quoted))
  (should
   (eq (elisp-def--use-position '(foo '(baz . bar)) 'bar)
       'quoted))
  ;; Let forms.
  ;; TODO: condition-case
  ;; TODO: error on viewing def of eq.
  ;; Expressions used to assign to variables.
  (should
   (eq (elisp-def--use-position '(let ((foo (bar)))) 'bar)
       'function))
  ;; Let variable declarations.
  (should
   (eq (elisp-def--use-position '(let ((foo (bar)))) 'foo)
       'definition))
  (should
   (eq (elisp-def--use-position '(let (foo) (bar)) 'foo)
       'definition))
  ;; Lambdas parameters are definitions.
  (should
   (eq (elisp-def--use-position '(lambda (foo) 1) 'foo)
       'definition))
  (should
   (eq (elisp-def--use-position '(lambda (foo bar) 1) 'bar)
       'definition)))

(defvar elisp-def--placeholder-num 0)

(defun elisp-def--fresh-placeholder ()
  "Generate a symbol that isn't used anywhere, even in
elisp-def's source code itself.

This differs from `make-symbol', as that doesn't guarantee that
the symbol _name_ is unused."
  (setq elisp-def--placeholder-num
        (1+ elisp-def--placeholder-num))
  (intern
   (format
    "elisp-def--fresh-placeholder-%s"
    elisp-def--placeholder-num)))

(defun elisp-def--defun-start ()
  "Find the start of the top-level form enclosing point."
  (let (start end)
    (save-excursion
      (beginning-of-defun)
      (setq start (point))
      (end-of-defun)
      (setq end (point)))
    (list start end)))

(defun elisp-def--source-with-placeholder (start end placeholder)
  "Return the source between START and END in the current buffer,
but with the symbol at point replaced by symbol PLACEHOLDER."
  (let* ((start-pos (point)))
    ;; Copy that expression into a separate buffer, so we can modify
    ;; the source.
    (let ((src (buffer-substring-no-properties start end)))
      (with-temp-buffer
        (insert src)
        ;; Replace the original symbol at point with a placeholder, so
        ;; we can distinguish it from other occurrences of this symbol within
        ;; the sexp.
        ;;
        ;; The difference of two positions is zero-indexed, but buffer
        ;; positions are one-indexed.
        (goto-char (1+ (- start-pos start)))
        (-let [(sym-start . sym-end) (bounds-of-thing-at-point 'symbol)]
          (delete-region sym-start sym-end))
        (insert (symbol-name placeholder))
        (buffer-string)))))

(ert-deftest elisp-def--source-with-placeholder ()
  ;; Point at the beginning of a symbol.
  (with-temp-buffer
    (insert "(foo bar)")
    (goto-char (point-min))
    ;; Point at start of the 'bar'.
    (search-forward " ")

    (should
     (equal
      (elisp-def--source-with-placeholder
       (point-min) (point-max) 'placeholder)
      "(foo placeholder)")))
  ;; Point the end of a symbol.
  (with-temp-buffer
    (insert "(foo bar)")
    (goto-char (point-min))
    ;; Point at end of the 'bar'.
    (search-forward "r")

    (should
     (equal
      (elisp-def--source-with-placeholder
       (point-min) (point-max) 'placeholder)
      "(foo placeholder)"))))

(defun elisp-def--join-and (items)
  "Join a list of strings with commas and \"and\"."
  (cond
   ((= (length items) 0)
    "")
   ((= (length items) 1)
    (car items))
   (t
    (format "%s and %s"
            (s-join ", " (-drop-last 1 items))
            (-last-item items)))))

(defun elisp-def--bound-syms (form sym &optional accum)
  "Return a list of bound symbols around the symbol SYM in FORM.

We only find bindings from special forms, caller is responsible
for macro-expanding."
  (catch 'done
    ;; If we've hit the symbol we're looking for, we can return the
    ;; bound symbols we found.
    (when (eq form sym)
      (throw 'done accum))

    (when (consp form)
      (let (bindings-found)
        ;; If this is a lambda form, the enclosed forms have the parameters
        ;; too.
        (cond
         ((eq (car form) 'lambda)
          (-let [(_ args . body) form]
            (setq args
                  (--remove (member it '(&optional &rest)) args))
            (setq bindings-found
                  (--map (elisp-def--bound-syms it sym (append accum args))
                         body))))
         ;; (let ((x y)) z)
         ;; We know that x is bound when we evaluate z, but not when we
         ;; evaluate y.
         ((eq (car form) 'let)
          (-let* (((_ var-vals . body) form)
                  (vars nil))
            (--each var-vals
              (if (consp it)
                  (-let [(var val) it]
                    (when (eq var 'XXX)
                      (throw 'done accum))
                    ;; `x' will be bound in the body.
                    (push var vars)
                    ;; `y' will be evaluated without `x' bound.
                    (push (elisp-def--bound-syms val sym accum)
                          bindings-found))
                ;; Otherwise, a variable without a binding, like `z' in
                ;; our example.
                (when (eq it 'XXX)
                  (throw 'done accum))
                (push it vars)))
            (setq vars (nreverse vars))
            (setq bindings-found
                  (append
                   (nreverse bindings-found)
                   (--map (elisp-def--bound-syms it sym (append accum vars))
                          body)))))
         ;; Handle `let*' forms, including bindings introduced by
         ;; previous vars.
         ((eq (car form) 'let*)
          (-let* (((_ var-vals . body) form)
                  (accum-with-vars accum)
                  (vars nil))
            (--each var-vals
              ;; E.g. (let* ((x a) (y b) z) c)
              (if (consp it)
                  (-let [(var val) it]
                    (when (eq var 'XXX)
                      (throw 'done accum))
                    ;; `x' will be bound in the body.
                    (push var vars)
                    ;; `a' will be evaluated without `x' bound.
                    (push (elisp-def--bound-syms val sym accum-with-vars)
                          bindings-found)
                    ;; `x' will be bound when evaluating `b' on the next
                    ;; iteration.
                    (setq accum-with-vars
                          (append accum-with-vars (list var))))
                ;; Otherwise, a variable without a binding, like `z' in
                ;; our example.
                (when (eq it 'XXX)
                  (throw 'done accum))
                (push it vars)))
            (setq vars (nreverse vars))
            (setq bindings-found
                  (append
                   (nreverse bindings-found)
                   (--map (elisp-def--bound-syms it sym (append accum vars))
                          body)))))
         ;; Handle `condition-case', the only other special form that
         ;; can introduce bindings.
         ((eq (car form) 'condition-case)
          (-let [(_ var bodyform . handlers) form]
            (when (eq var 'XXX)
              (throw 'done accum))
            (setq bindings-found
                  (cons
                   (elisp-def--bound-syms bodyform sym accum)
                   (--map (elisp-def--bound-syms it sym (append accum (list var)))
                          handlers)))))

         ;; For other forms (`progn' etc) then just recurse to see if it
         ;; contains XXX. We know that it introduces no new bindings. It is
         ;; actually possible to introduce a global with `setq', but we
         ;; ignore that.
         (t
          (setq bindings-found
                (--map (elisp-def--bound-syms it sym accum)
                       form))))

        ;; For any sublist that didn't contain XXX, we will have
        ;; returned nil. Find the non-empty list, if any.
        (-first #'consp bindings-found)))))

(ert-deftest elisp-def--bound-syms--lambda ()
  (should
   (equal
    (elisp-def--bound-syms '(lambda (x y) XXX) 'XXX)
    (list 'x 'y)))
  (should
   (equal
    (elisp-def--bound-syms '(lambda (x &optional y &rest z) XXX) 'XXX)
    (list 'x 'y 'z))))

(ert-deftest elisp-def--bound-syms--let ()
  ;; Handle bindings introduced by let.
  (should
   (equal
    (elisp-def--bound-syms '(let (x y) XXX) 'XXX)
    (list 'x 'y)))
  (should
   (equal
    (elisp-def--bound-syms '(let ((x 1) (y)) XXX)  'XXX)
    (list 'x 'y)))
  ;; Don't consider previous bindings in the same let.
  (should
   (equal
    (elisp-def--bound-syms
     '(let ((x 1))
        (let ((y 2)
              (z (+ XXX 1)))
          2))
     'XXX)
    (list 'x)))
  ;; If our placeholder is in the variable position, still consider
  ;; previous keybindings.
  (should
   (equal
    (elisp-def--bound-syms
     '(let ((x 1))
        (let (XXX)
          3))
     'XXX)
    (list 'x)))
  (should
   (equal
    (elisp-def--bound-syms
     '(let ((x 1))
        (let ((XXX 2))
          3))
     'XXX)
    (list 'x))))

(ert-deftest elisp-def--bound-syms--let* ()
  (should
   (equal
    (elisp-def--bound-syms '(let* (x y) XXX) 'XXX)
    (list 'x 'y)))
  (should
   (equal
    (elisp-def--bound-syms '(let* ((x 1) (y)) XXX)  'XXX)
    (list 'x 'y)))
  (should
   (equal
    (elisp-def--bound-syms
     '(let ((x 1))
        (let* ((y 2)
               (z (+ XXX 1)))
          2))
     'XXX)
    (list 'x 'y)))
  (should
   (equal
    (elisp-def--bound-syms
     '(let* ((x 1))
        (let* ((XXX 2))
          3))
     'XXX)
    (list 'x))))

(ert-deftest elisp-def--bound-syms--condition-case ()
  (should
   (equal
    (elisp-def--bound-syms
     '(condition-case x (XXX) y)
     'XXX)
    nil))
  (should
   (equal
    (elisp-def--bound-syms
     '(condition-case x y (XXX))
     'XXX)
    (list 'x))))

(ert-deftest elisp-def--bound-syms--progn ()
  (should
   (equal
    (elisp-def--bound-syms '(progn (x y) XXX) 'XXX)
    nil)))

(defun elisp-def--symbol-at-point ()
  "Get the symbol at point, even if we're on a quoted or
sharp-quoted symbol."
  (save-excursion
    (when (looking-at (rx "#"))
      (forward-char))
    (when (looking-at (rx "'"))
      (forward-char))
    (symbol-at-point)))

(defun elisp-def--enclosing-form (depth)
  "Move up DEPTH sexps from point, and return the start and end
positions of the form."
  (save-excursion
    (--dotimes depth
      (let* ((ppss (syntax-ppss))
             (enclosing-start-pos (nth 1 ppss)))
        (goto-char enclosing-start-pos)))
    (list (point)
          (progn
            (forward-sexp)
            (point)))))

(defun elisp-def--binding-form-start ()
  "Return the start position of the form enclosing point
that binds the symbol at point.

For example, where point is shown with |, input:

\(defun foo ()
  (let (bar)
    (setq ba|r 1)))

Output:

\(defun foo ()
  |(let (bar)
     (setq bar 1)))

This an approximation: we incrementally expand macros around
point. If outer macros rewrite inner forms, we may go to the
wrong place. This should be very rare."
  (let* ((sym (elisp-def--symbol-at-point))
         (placeholder (elisp-def--fresh-placeholder))
         (ppss (syntax-ppss)))
    (catch 'found
      ;; Start with the innermost form, and incrementally move outwards.
      (--each (number-sequence 1 (syntax-ppss-depth ppss))
        ;; For each enclosing form, see if it binds the symbol at point.
        (-let* (((start end) (elisp-def--enclosing-form it))
                (src (elisp-def--source-with-placeholder
                      start end placeholder))
                (form (read src))
                (expanded-form (macroexpand-all form))
                (bound-syms (elisp-def--bound-syms
                             expanded-form placeholder)))
          ;; If this enclosing form introduces a binding for the
          ;; symbol we want, we've found the innermost binding!
          (when (memq sym bound-syms)
            (throw 'found start)))))))

(defun elisp-def ()
  "Go to the definition of the symbol at point."
  (interactive)
  (let* ((init-pos (point))
         (sym (elisp-def--symbol-at-point))
         (sym-name (symbol-name sym))
         ;; Try to find the namespace by macro expanding the code.
         (namespace (elisp-def--namespace-at-point)))
    ;; If we couldn't identify a function or variable, see which
    ;; namespaces this symbol is bound in.
    (when (eq namespace 'quoted)
      (-let [namespaces (elisp-def--defined-in sym)]
        (when (null namespaces)
          (user-error "Couldn't identify where %s is defined"
                      sym-name))

        ;; If the symbol is only bound in one namespace, use that.
        (if (= (length namespaces) 1)
            (setq namespace (car namespaces))
          ;; Otherwise, our static analysis has failed, so just ask
          ;; the user.
          (let* ((formatted-namespaces
                  (elisp-def--join-and
                   (--map (format "a %s" it) namespaces)))
                 (prompt (format "%s is %s, choose: "
                                 sym-name
                                 formatted-namespaces)))
            (setq namespace
                  (intern
                   (completing-read prompt namespaces nil t)))))))

    ;; Push the current position, so we can go back.
    (xref-push-marker-stack)

    (-let [(buf pos)
           (cond
            ((eq namespace 'bound)
             (list (current-buffer)
                   (elisp-def--binding-form-start)))
            ((eq namespace 'library)
             (elisp-def--find-feature sym))
            ((eq namespace 'variable)
             (elisp-def--find-variable sym))
            ;; TODO: is treating quoted symbols as functions always
            ;; correct?
            ((memq namespace '(function quoted))
             (elisp-def--find-function sym)))]
      (unless (and buf pos)
        ;; todo: mention if it's due to being a primitive
        (user-error "Couldn't find definition for %s %s"
                    namespace sym))

      (switch-to-buffer buf)
      (goto-char pos))

    ;; POS is actually the start of line where SYM is defined. Work
    ;; out the exact position of SYM, and flash it.
    (let (sexp-end-pos start-pos end-pos)
      (save-excursion
        (forward-sexp)
        (setq sexp-end-pos (point)))
      (save-excursion
        (re-search-forward
         (rx-to-string `(seq symbol-start ,sym-name symbol-end))
         sexp-end-pos
         nil)
        (setq end-pos (point))
        (setq start-pos (- end-pos (length sym-name))))
      (elisp-def--flash-region start-pos end-pos))))

;; Overriding xref-find-definitions.
(define-key lisp-mode-map (kbd "M-.") #'elisp-def)

(provide 'elisp-def)
;;; elisp-def.el ends here
