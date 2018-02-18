;;; elisp-def-test.el --- Tests for elisp-def

(require 'elisp-def)

(ert-deftest elisp-def--sharp-quoted-p ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "#'abc")
    (goto-char (point-min))
    (dotimes (i 3)
      (should (elisp-def--sharp-quoted-p))
      (forward-char))))

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
    ;; We require cl to be loaded in order to know how to expand
    ;; `destructuring-bind'.
    (require 'cl)
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

;;; elisp-def-test.el ends here
