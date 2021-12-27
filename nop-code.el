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
;;; Face Definitions
;;
;;

(defface nop-code-base '((t :foreground "gainsboro"))
  "Default face for highlighting an overlay in nop-mode."
  :version "0.1"
  :group 'nop-overlay)

(setplist 'nop--code-overlay '(face nop-code-base display "[!]"))
(defconst nop--code-overlay '((category nop--code-overlay)))


;;;;;;;;;;;;;;;;;;;;;;;;
;;; Nop Code Enable / Disable
;;
;;

(defun nop--code-enable ()
  "Generates [!] markers replacing the actual nop directives."
  (dolist (d (nop--parse-buffer))
    (nop--call-for-each-node
     (lambda (d depth-list)
       (nop--generate-overlay (nop--outer-r (oref d positions))
                              nop--code-overlay))
     d)))

(defun nop--code-disable ()
  (remove-overlays))

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
  (if nop-code-mode (nop--code-enable) (nop--code-disable)))

;;;;;;;;;;;;;;;;;;;;;
;;; Nop Provide
;;
;;

(provide 'nop-code)

;;; nop-code.el ends here
