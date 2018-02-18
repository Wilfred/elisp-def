;;; elisp-def-test.el --- Tests for elisp-def

(require 'elisp-def)

(defmacro elisp-def--with-temp-buffer (src &rest body)
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (emacs-lisp-mode)
     (insert ,src)
     (goto-char (point-min))
     ,@body))

(ert-deftest elisp-def--symbol-at-point ()
  ;; Symbol at point or quoted symbol.
  (dolist (src '("foo" "#'foo"))
    (elisp-def--with-temp-buffer src
      (should
       (eq
        (elisp-def--symbol-at-point)
        'foo))))
  ;; Backquoted symbols.
  (dolist (src '(";; `foo'" "\"`foo'\""))
    (elisp-def--with-temp-buffer src
      (search-forward "f")
      (should
       (eq
        (elisp-def--symbol-at-point)
        'foo))))
  ;; Docstring conventions, where FOO means a parameter named `foo'..
  (elisp-def--with-temp-buffer "\"FOO\""
    (search-forward "f")
    (should
     (eq
      (elisp-def--symbol-at-point)
      'foo))))

(ert-deftest elisp-def--sharp-quoted-p ()
  (elisp-def--with-temp-buffer "#'abc"
    (dotimes (i 3)
      (should (elisp-def--sharp-quoted-p))
      (forward-char))))

(ert-deftest elisp-def--namespace-at-point ()
  ;; If it's the head of a sexp, this is a function.
  (elisp-def--with-temp-buffer "(foo bar)"
    (search-forward "foo")
    (should
     (eq (elisp-def--namespace-at-point)
         'function)))
  ;; If it's an argument to a function, it's a variable.
  (elisp-def--with-temp-buffer  "(foo bar)"
    (search-forward "bar")
    (should
     (eq (elisp-def--namespace-at-point)
         'variable)))
  ;; Handle let-bound variables.
  (elisp-def--with-temp-buffer "(let ((x 1)) (1+ x))"
    (search-forward "1+")
    (search-forward "x")

    (should
     (eq (elisp-def--namespace-at-point)
         'bound)))
  ;; We require cl to be loaded in order to know how to expand
  ;; `destructuring-bind'.
  (require 'cl)
  ;; Handle let-bound variables introduced by macros.
  (elisp-def--with-temp-buffer "(destructuring-bind (x y) z (1+ x))"
    (search-forward "1+")
    (search-forward "x")

    (should
     (eq (elisp-def--namespace-at-point)
         'bound)))
  ;; Handle function parameters.
  (elisp-def--with-temp-buffer "(defun foo (x) (1+ x))"
    (search-forward "1+")
    (search-forward "x")

    (should
     (eq (elisp-def--namespace-at-point)
         'bound)))
  ;; Handle binding introduced by condition-case.
  (elisp-def--with-temp-buffer "(condition-case e (foo) (error e))"
    (search-forward "error")
    (search-forward "e")

    (should
     (eq (elisp-def--namespace-at-point)
         'bound)))
  ;; Quoted references.
  (elisp-def--with-temp-buffer "(foo 'bar)"
    (search-forward "bar")

    (should
     (eq (elisp-def--namespace-at-point)
         'quoted)))
  ;; Handle references to libraries.
  (elisp-def--with-temp-buffer "(require 'foo)"
    (search-forward "foo")

    (should
     (eq (elisp-def--namespace-at-point)
         'library)))

  ;; Treat docstrings and comments as quoted.
  (elisp-def--with-temp-buffer ";; `foo'"
    (search-forward "f")
    (should
     (eq (elisp-def--namespace-at-point)
         'quoted)))
  (elisp-def--with-temp-buffer ";; `foo'"
    (search-forward "f")
    (eq (elisp-def--namespace-at-point)
        'quoted))
  (elisp-def--with-temp-buffer "\"FOO\""
    (search-forward "F")
    (should
     (eq (elisp-def--namespace-at-point)
         'quoted))))

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
  (elisp-def--with-temp-buffer "(foo bar)"
    ;; Point at start of the 'bar'.
    (search-forward " ")

    (should
     (equal
      (elisp-def--source-with-placeholder
       (point-min) (point-max) 'placeholder)
      "(foo placeholder)")))
  ;; Point the end of a symbol.
  (elisp-def--with-temp-buffer "(foo bar)"
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

(ert-deftest elisp-def--bound-syms--improper ()
  "Don't crash on improper lists."
  (should
   (equal
    (elisp-def--bound-syms
     '(lambda (x)
        (when x
          '(foo . bar))
        XXX)
     'XXX)
    (list 'x))))

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

(ert-deftest elisp-def--find-function--unbound ()
  (should
   (equal
    (elisp-def--find-function 'no-such-func)
    (list nil nil))))

;;; elisp-def-test.el ends here
