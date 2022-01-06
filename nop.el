;;; nop.el --- Nop: Narrative Oriented Programming extensions for Emacs.

;; Copyright (C) 2021 Kenan Bölükbaşı

;; Author: Kenan Bölükbaşı <kenanbolukbasi@gmail.com>
;; Created: 31 October 2021
;; Version: 0.0.1
;; Keywords: convenience, languages, outlines, tools
;; Homepage: https://github.com/kenanb/nop-mode

;;; Code:

;;; Dependencies [#.]

(require 'nop-base)
(require 'nop-lang)

;; Modes
(require 'nop-code)
(require 'nop-read)
(require 'nop-mark)

;;; Nop Minor Mode [#.]
;;
;;

(defun nop-cycle-modes ()
  (interactive)
  (cond
   (nop-read-mode (nop-read-mode -1) (nop-code-mode 1))
   (nop-code-mode (nop-code-mode -1) (nop-mark-mode 1))
   (nop-mark-mode (nop-mark-mode -1) (nop-read-mode 1))
   (t (nop-read-mode 1))))

;;;###autoload
(define-minor-mode nop-mode
  "Toggle Nop mode."
  ;; The initial value.
  :init-value nil
  ;; The indicator for the mode line.
  :lighter " Nop"
  ;; The minor mode bindings.
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c !") 'nop-cycle-modes)
            map)
  :group 'nop
  (if nop-mode
      (progn
        (let ((most-specific-available-mode
               (alist-get
                ;; NOTE : We rely on derived-mode-p to return the most specific
                ;; matching mode, but the behaviour is not documented.
                (apply #'derived-mode-p (mapcar #'car nop-major-mode-alist))
                nop-major-mode-alist)))
          (when most-specific-available-mode
            (setf nop--parser (make-instance most-specific-available-mode))))
        (nop-cycle-modes))
    (cond
     (nop-read-mode (nop-read-mode -1))
     (nop-code-mode (nop-code-mode -1))
     (nop-mark-mode (nop-mark-mode -1)))
    (setf nop--parser nil)))

;;;###autoload
(define-globalized-minor-mode global-nop-mode
  nop-mode
  (lambda () (nop-mode t))
  :group 'nop)

;;; Nop Provide [#.]
;;
;;

(provide 'nop)

;;; nop.el ends here
