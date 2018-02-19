;;; elisp-def.el --- macro-aware go-to-definition for elisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Wilfred Hughes
;; Version: 0.1

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

;; Find the definition of the symbol at point,
;; intelligently. Understands namespaces, macros, libraries and local
;; bindings.
;;
;; See full docs at https://github.com/Wilfred/elisp-def
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
    (when (fboundp sym)
      (-let [(base-sym . src-path) (find-function-library sym)]
        ;; `base-sym' is the underlying symbol if `sym' is an alias.
        (setq sym base-sym)
        (setq path src-path)))
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

(defun elisp-def--namespace-at-point ()
  "Is the symbol at point a function/macro, a global variable, a
quoted variable, or a let-bound variable?

Variable references in docstrings and comments are treated as
quoted variables, because they aren't being used at point."
  (catch 'done
    ;; If it's a sharp quoted symbol, we know it's a global function
    ;; reference.
    (if (elisp-def--sharp-quoted-p)
        (throw 'done 'function))

    ;; If we're in a string or comment, we can't infer anything about
    ;; the namespace, so just treat it as quoted.
    (let* ((ppss (syntax-ppss))
           (in-string (nth 3 ppss))
           (in-comment (nth 4 ppss)))
      (when (or in-string in-comment)
        (throw 'done 'quoted)))

    ;; Otherwise, macro expand the source at point and look at how the
    ;; symbol is used.
    (-let* ((ppss (syntax-ppss))
            ((form-start form-end) (elisp-def--enclosing-form
                                    (syntax-ppss-depth ppss)))
            (placeholder (elisp-def--fresh-placeholder))
            (src (elisp-def--source-with-placeholder form-start form-end placeholder))
            (form (condition-case nil
                      (read src)
                    (end-of-file nil)))
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
          (when (ert--proper-list-p form)
            (setq bindings-found
                  (--map (elisp-def--bound-syms it sym accum)
                         form)))))

        ;; For any sublist that didn't contain XXX, we will have
        ;; returned nil. Find the non-empty list, if any.
        (-first #'consp bindings-found)))))

(defun elisp-def--symbol-at-point ()
  "Get the symbol at point, even if we're on a quoted or
sharp-quoted symbol."
  (let* ((sym
          (save-excursion
            (when (looking-at (rx "#"))
              (forward-char))
            (when (looking-at (rx "'"))
              (forward-char))
            (symbol-at-point)))
         (ppss (syntax-ppss))
         (in-string (nth 3 ppss))
         (in-comment (nth 4 ppss)))
    ;; Handle FOO in docstrings.
    (when (and
           (or in-string in-comment)
           (not (or (boundp sym) (fboundp sym)))
           (s-uppercase? (symbol-name sym)))
      (setq sym (intern (downcase (symbol-name sym)))))
    sym))

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

(defun elisp-def--go-to-definition (form-start sym)
  "Move point to the symbol after FORM-START that defines the
variable SYM. Point is put on the first character of that symbol.

For example, if | is point:

\(defun foo ())

=>

\(defun |foo ())
\(foo)

Or for let-bound variables:

\(let ((x 1))
  (foo |x))

=>

\(let ((|x 1))
  (foo x))"
  (let (form form-end)
    (save-excursion
      (goto-char form-start)
      (setq form (read (current-buffer)))

      ;; `read' moves point over the current form.
      (setq form-end (point)))

    (if (memq (car form) (list 'let 'let*))
        (user-error "todo")
      ;; Otherwise, we assume the first occurrence of the symbol is
      ;; the definition. Move to that symbol.
      (goto-char form-start)
      (when
          (re-search-forward
           (rx-to-string `(seq symbol-start ,(symbol-name sym) symbol-end))
           form-end
           nil)
        (backward-sexp)))))

(defun elisp-def ()
  "Go to the definition of the symbol at point."
  (interactive)
  (let* ((sym (elisp-def--symbol-at-point))
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

    ;; Point is now at the start of line where SYM is defined. Work
    ;; out the position of the definition SYM, and flash it.
    (elisp-def--go-to-definition (point) sym)
    (let (sym-end-pos)
      (save-excursion
        (forward-sexp)
        (setq sym-end-pos (point)))
      (elisp-def--flash-region (point) sym-end-pos))))

(define-minor-mode elisp-def-mode
  "Minor mode for finding definitions with `elisp-def'.

\\{elisp-def-mode-map}")

(defvar elisp-def-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-.") #'elisp-def)
    (define-key map (kbd "M-,") #'xref-pop-marker-stack)
    map)
  "Keymap used in `elisp-def-mode'.")

(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook #'elisp-def-mode))

(provide 'elisp-def)
;;; elisp-def.el ends here
