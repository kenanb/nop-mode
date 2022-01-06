;;; nop-lang.el --- Core language definitions for nop-mode.

;; Copyright (C) 2021 Kenan Bölükbaşı

;; Author: Kenan Bölükbaşı <kenanbolukbasi@gmail.com>
;; Keywords: convenience, languages, outlines, tools
;; Homepage: https://github.com/kenanb/nop-mode

;;; Commentary:

;; TODO : Skip over string literals.

;;; Code:

;;; Dependencies [#.]

(require 'nop-base)

;;; Language Definitions [#.]
;;
;;

;; Common comment syntax.

(defclass nop--semicolon-comment-parser (nop--directive-parser)
  ((lookup-regexp    :initform ";")
   (comment-prefix   :initform ";")
   (decor-regexp-set :initform '(";"))))

(defclass nop--sharp-sign-comment-parser (nop--directive-parser)
  ((lookup-regexp    :initform "#")
   (comment-prefix   :initform "#")
   (decor-regexp-set :initform '("#"))))

(defclass nop--double-slash-comment-parser (nop--directive-parser)
  ((lookup-regexp    :initform "//")
   (comment-prefix   :initform "//")
   (decor-regexp-set :initform '("/"))))

;; Language specific

(defclass nop--c/c++-parser (nop--directive-parser)
  ((lookup-regexp    :initform "/[/*]")
   (comment-prefix   :initform "//")
   (decor-regexp-set :initform '("/"))))

(cl-defmethod nop--skip-over-block-comment ((parser nop--c/c++-parser))
  (when (eq (preceding-char) ?*) (search-forward "*/") t))

(defclass nop--common-lisp-parser (nop--semicolon-comment-parser)
  ((lookup-regexp    :initform ";\\|#|")))

(cl-defmethod nop--skip-over-block-comment ((parser nop--common-lisp-parser))
  (when (eq (preceding-char) ?\|) (search-forward "|#") t))

(defclass nop--python-parser (nop--sharp-sign-comment-parser)
  ((lookup-regexp    :initform "#\|\"\"\"")))

(cl-defmethod nop--skip-over-block-comment ((parser nop--python-parser))
  ;; Triple quote string-literal syntax is used for docstrings in python.
  (when (eq (preceding-char) ?\") (search-forward "\"\"\"") t))

(defclass nop--nasm-parser (nop--semicolon-comment-parser)
  ())

(defclass nop--gas-parser (nop--directive-parser)
  ((lookup-regexp    :initform "#\|/*")))

(cl-defmethod nop--skip-over-block-comment ((parser nop--gas-parser))
  (when (eq (preceding-char) ?*) (search-forward "*/") t))

(setf nop-major-mode-alist
      '((c++-mode . nop--c/c++-parser)
        (c-mode . nop--c/c++-parser)
        (csharp-mode . nop--c/c++-parser)
        (go-mode . nop--c/c++-parser)

        (rust-mode . nop--double-slash-comment-parser)
        (zig-mode . nop--double-slash-comment-parser)

        (lisp-mode . nop--common-lisp-parser)
        (emacs-lisp-mode . nop--semicolon-comment-parser)

        (python-mode . nop--python-parser)

        (sh-mode . nop--sharp-sign-comment-parser)

        (makefile-mode . nop--sharp-sign-comment-parser)
        (makefile-gmake-mode . nop--sharp-sign-comment-parser)

        (asm-mode . nop--gas-parser)
        (nasm-mode . nop--nasm-parser)))

;;; Nop Provide [#.]
;;
;;

(provide 'nop-lang)

;;; nop-lang.el ends here
