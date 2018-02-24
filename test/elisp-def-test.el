;;; elisp-def-test.el --- Tests for elisp-def

(require 'elisp-def)

(ert-deftest elisp-def--let-bind-index ()
  (should
   (= 2
      (elisp-def--let-bind-index
       '(let ((x 1)
              (y 2)
              (x 3))
          placeholder)
       'x
       'placeholder)))
  (should
   (= 0
      (elisp-def--let-bind-index
       '(let ((x 1)
              (x (placeholder))
              (x 3))
          y)
       'x
       'placeholder))))

(defmacro elisp-def--with-temp-buffer (src &rest body)
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (emacs-lisp-mode)
     (insert ,src)
     (goto-char (point-min))
     ,@body))

(ert-deftest elisp-def--let* ()
  "Ensure we go to the right position in let* forms."
  (elisp-def--with-temp-buffer "(defun foo ()
  (let* ((x 1)
         (y 2)
         ;; comment containing x
         (x 3))
    (+ x 1)))"
    (search-forward "+ x")
    (backward-char)
    (elisp-def)
    (should
     (looking-at "x 3"))))

(ert-deftest elisp-def--let ()
  "Ensure we go to the right position in let forms."
  (elisp-def--with-temp-buffer "(defun foo ()
  (let ((x 1))
    (let ((x 2)
          (y (+ x 1))))))"
    (search-forward "+ x")
    (backward-char)
    (elisp-def)
    (should
     (looking-at "x 1"))))

;; (ert-deftest elisp-def--defun ()
;;   "Ensure we go to the right position in defun forms."
;;   (elisp-def--with-temp-buffer "(defun demo/foo ()
;;   nil)
;; \(defun demo/bar ()
;;   (demo/foo))"
;;     (eval-buffer)
;;     (search-forward "(demo/foo)")
;;     (backward-char)
;;     (elisp-def)
;;     (should
;;      (looking-at "demo/foo"))))

(ert-deftest elisp-def--docstring-parameter ()
  "Ensure we go to the right position from a docstring reference.."
  (elisp-def--with-temp-buffer "(defun demo/foo (bar)
  \"See BAR.\"
  nil)"
    (eval-buffer)
    (search-forward "See B")
    (elisp-def)
    (should
     (looking-at "bar)"))))

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
  (elisp-def--with-temp-buffer "\"Return FOO.\""
    (search-forward "F")
    (should
     (eq
      (elisp-def--symbol-at-point)
      'foo)))
  ;; {} is actually legal in an elisp symbol, but in a docstring it's
  ;; almost certainly not part of the symbol name.
  (elisp-def--with-temp-buffer "\"\\{foo-mode-map}\""
    (search-forward "f")
    (should
     (eq
      (elisp-def--symbol-at-point)
      'foo-mode-map))))

(ert-deftest elisp-def--sharp-quoted-p ()
  (elisp-def--with-temp-buffer "#'abc"
    (dotimes (i 3)
      (should (elisp-def--sharp-quoted-p))
      (forward-char))))

(ert-deftest elisp-def--enclosing-form ()
  "Ensure we find the enclosing form position for both sexps and
strings."
  (elisp-def--with-temp-buffer "(foo (bar))"
    (search-forward "b")
    (should
     (equal
      (elisp-def--enclosing-form 1)
      (list 6 11))))
  (elisp-def--with-temp-buffer "(foo \"bar\")"
    (search-forward "b")
    (should
     (equal
      (elisp-def--enclosing-form 1)
      (list 6 11)))))

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
  ;; let* variable declarations.
  (should
   (eq (elisp-def--use-position '(let ((foo (bar))) (baz)) 'foo)
       'definition))
  ;; Lambdas parameters are definitions.
  (should
   (eq (elisp-def--use-position '(lambda (foo) 1) 'foo)
       'definition))
  (should
   (eq (elisp-def--use-position '(lambda (foo bar) 1) 'bar)
       'definition))
  ;; Known higher-order functions with function arguments.
  (should
   (eq (elisp-def--use-position '(funcall 'foo) 'foo)
       'function))
  (should
   (eq (elisp-def--use-position '(mapcar 'foo bar) 'foo)
       'function))
  ;; The first sexp in a cond clause is an expression, not a
  ;; function call.
  (should
   (eq (elisp-def--use-position
        '(cond (foo 1))
        'foo)
       'variable))
  ;; The first sexp in a cond clause may still contain a function
  ;; call.
  (should
   (eq (elisp-def--use-position
        '(cond (bar 1) ((foo) 1))
        'foo)
       'function)))

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
      "(foo placeholder)")))
  ;; Point inside a string.
  (elisp-def--with-temp-buffer "(foo \"bar\")"
    (search-forward "b")

    (should
     (equal
      (elisp-def--source-with-placeholder
       (point-min) (point-max) 'placeholder)
      "(foo (elisp-def--string placeholder))"))))

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
  ;; Basic case: all the variables bound.
  (should
   (equal
    (elisp-def--bound-syms '(let* (x y) XXX) 'XXX)
    (list 'x 'y)))
  ;; The variables before, but not the variables after.
  (should
   (equal
    (elisp-def--bound-syms '(let* (x y XXX z) abc) 'XXX)
    (list 'x 'y)))
  (should
   (equal
    (elisp-def--bound-syms '(let* ((x 1) (y)) XXX)  'XXX)
    (list 'x 'y)))
  ;; Nested lets.
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

;; TODO: test primitive functions.

;;; elisp-def-test.el ends here
