;;; nop-lang.el --- Core language definitions for nop-mode.

;; Copyright (C) 2021 Kenan Bölükbaşı

;; Author: Kenan Bölükbaşı <kenanbolukbasi@gmail.com>
;; Keywords: convenience, languages, outlines, tools
;; Homepage: https://github.com/kenanb/nop-mode

;;; Code:

;;
;;
;;;
;;; Dependencies
;;;
;;
;;

(require 'nop-base)

;;
;;
;;;
;;; Language Definitions
;;;
;;
;;

(defclass nop--c/c++-parser (nop--directive-parser)
  ((lookup-regexp    :initform "/[/*]")
   (comment-prefix   :initform "//")
   (decor-regexp-set :initform '("/"))))

(cl-defmethod nop--skip-over-multiline ((parser nop--c/c++-parser))
  (when (eq (preceding-char) ?*) (search-forward "*/") t))

(defclass nop--semicolon-comment-parser (nop--directive-parser)
  ((lookup-regexp    :initform ";")
   (comment-prefix   :initform ";")
   (decor-regexp-set :initform '(";"))))

(defclass nop--sharpsign-comment-parser (nop--directive-parser)
  ((lookup-regexp    :initform "#")
   (comment-prefix   :initform "#")
   (decor-regexp-set :initform '("#"))))

(defclass nop--common-lisp-parser (nop--semicolon-comment-parser)
  ((lookup-regexp    :initform ";\\|#|")))

(cl-defmethod nop--skip-over-multiline ((parser nop--common-lisp-parser))
  (when (eq (preceding-char) ?\|) (search-forward "|#") t))

(defclass nop--emacs-lisp-parser (nop--semicolon-comment-parser)
  ())

(defclass nop--python-parser (nop--sharpsign-comment-parser)
  ())

(defclass nop--makefile-parser (nop--sharpsign-comment-parser)
  ())

(defclass nop--asm-parser (nop--sharpsign-comment-parser)
  ())

(setf nop-major-mode-alist
      '((c++-mode . nop--c/c++-parser)
        (c-mode . nop--c/c++-parser)
        (lisp-mode . nop--common-lisp-parser)
        (emacs-lisp-mode . nop--emacs-lisp-parser)
        (python-mode . nop--python-parser)
        (makefile-mode . nop--makefile-parser)
        (makefile-gmake-mode . nop--makefile-parser)
        (asm-mode . nop--asm-parser)))

;;
;;
;;;
;;; Nop Provide
;;;
;;
;;

(provide 'nop-lang)

;;; nop-lang.el ends here
