;;; nop-mark.el --- Authoring-centric minor mode complement for nop-mode.

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
;;; Nop Mark Enable / Disable
;;
;;

(defun nop-mark-enable ())

(defun nop-mark-disable ())

;;
;;
;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Nop Mark Minor Mode
;;
;;

;;;###autoload
(define-minor-mode nop-mark-mode
  "Toggle Nop mode."
  ;; The initial value.
  :init-value nil
  ;; The indicator for the mode line.
  :lighter " Nop-Mark"
  ;; The minor mode bindings.
  :keymap (let ((map (make-sparse-keymap)))
            map)
  :group 'nop
  (if nop-mark-mode (nop-mark-enable) (nop-mark-disable)))

;;;;;;;;;;;;;;;;;;;;;
;;; Nop Provide
;;
;;

(provide 'nop-mark)

;;; nop-mark.el ends here
