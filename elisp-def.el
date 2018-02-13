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
;; TODO: features (require/provide)
;; TODO: macro-expand and work out what bindings we are in.
;; TODO: fonts

;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'find-func)

(defun elisp-def--flash-region (start end)
  "Temporarily highlight region from START to END."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face 'highlight)
    (run-with-timer 0.5 nil 'delete-overlay overlay)))

(defun elisp-def--find-library-name (path)
  "A wrapper around `find-library-name' that returns nil if PATH
has no library with that name.

This can happen when users have installed Emacs without its
source code: they have e.g. org.elc but no org.el."
  (condition-case _err
      (find-library-name path)
    (error nil)))

(defun elisp-def--primitive-p (sym callable-p)
  "Return t if SYM is defined in C."
  (if callable-p
      (subrp (indirect-function sym))
    (let ((filename (find-lisp-object-file-name sym 'defvar)))
      (or (eq filename 'C-source)
          (and (stringp filename)
               (equal (file-name-extension filename) "c"))))))

(defun elisp-def--find-global (sym callable-p)
  "Find the buffer and position where SYM is globally defined."
  (let ((primitive-p (elisp-def--primitive-p sym callable-p))
        (path nil)
        (buf nil)
        (pos nil))
    (when callable-p
      (-let [(base-sym . src-path) (find-function-library sym)]
        ;; `base-sym' is the underlying symbol if `sym' is an alias.
        (setq sym base-sym)
        (setq path src-path)))
    (when (and primitive-p path find-function-C-source-directory)
      ;; Convert "src/foo.c" to "".
      (setq path (f-expand path
                           (f-parent find-function-C-source-directory))))

    (cond
     ((and callable-p path)
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
     (callable-p
      ;; Functions defined interactively may have an edebug property
      ;; that contains the location of the definition.
      (-when-let (edebug-info (get sym 'edebug))
        (-let [marker (if (consp edebug-info)
                          (car edebug-info)
                        edebug-info)]
          (setq buf (marker-buffer marker))
          (setq pos (marker-position marker)))))
     ((not callable-p)
      (condition-case _err
          (-let [(sym-buf . sym-pos) (find-definition-noselect sym 'defvar)]
            (setq buf sym-buf)
            (setq pos sym-pos))
        (search-failed nil)
        ;; If your current Emacs instance doesn't match the source
        ;; code configured in find-function-C-source-directory, we can
        ;; get an error about not finding source. Try
        ;; `default-tab-width' against Emacs trunk.
        (error nil))))
    (list buf pos)))

(defun elisp-def--namespaces (sym)
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

(defun elisp-def ()
  "Go to the definition of the symbol at point."
  (interactive)
  (-let* ((sym (symbol-at-point))
          (sym-name (symbol-name sym))
          (namespaces (elisp-def--namespaces sym))
          (namespace nil))
    (if (> (length namespaces) 1)
        ;; User chooses.
        (let* ((formatted-namespaces
                (elisp-def--join-and
                 (--map (format "a %s" it) namespaces)))
               (prompt (format "%s is %s, choose: "
                               sym-name
                               formatted-namespaces)))
          (setq namespace
                (intern
                 (completing-read prompt namespaces nil t))))

      ;; This symbol is only bound in one namespace.
      (setq namespace (car namespaces)))

    (-let [(buf pos) (elisp-def--find-global sym (eq namespace 'function))]
      (unless (and buf pos)
        ;; todo: mention if it's due to being a primitive
        (user-error "Could not find definition for %s %s"
                    namespace sym))

      (switch-to-buffer buf)
      (goto-char pos))
    ;; TODO: push position so we can pop.

    ;; POS is actually the start of line where SYM is defined. Work
    ;; out the exact position of SYM, and flash it.
    (let (start-pos end-pos)
      (save-excursion
        (search-forward sym-name)
        (setq end-pos (point))
        (setq start-pos (- end-pos (length sym-name))))
      (elisp-def--flash-region start-pos end-pos))))

;; Overriding xref-find-definitions.
(global-set-key (kbd "M-.") #'elisp-def)

(provide 'elisp-def)
;;; elisp-def.el ends here
