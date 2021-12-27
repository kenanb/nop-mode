;;; nop-code.el --- Programming-centric minor mode complement for nop-mode.

;; Copyright (C) 2021 Kenan Bölükbaşı

;; Author: Kenan Bölükbaşı <kenanbolukbasi@gmail.com>
;; Keywords: convenience, languages, outlines, tools
;; Homepage: https://github.com/kenanb/nop-mode

;;; Code:

;;;;;;;;;;;;;;;;
;;; Dependencies
;;
;;

(require 'nop-base)


;;;;;;;;;;;;;;;;;;;;;;;;
;;; Nop Code Enable / Disable
;;
;;

(defun nop-code-enable ())

(defun nop-code-disable ())

;;
;;
;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Nop Code Minor Mode
;;
;;

;;;###autoload
(define-minor-mode nop-code-mode
  "Toggle Nop-Code mode."
  ;; The initial value.
  :init-value nil
  ;; The indicator for the mode line.
  :lighter " Nop-Code"
  ;; The minor mode bindings.
  :keymap (let ((map (make-sparse-keymap)))
            map)
  :group 'nop
  (if nop-code-mode (nop-code-enable) (nop-code-disable)))

;;;;;;;;;;;;;;;;;;;;;
;;; Nop Provide
;;
;;

(provide 'nop-code)

;;; nop-code.el ends here
