;;; nop-mark.el --- Authoring-centric minor mode complement for nop-mode.

;; Copyright (C) 2021 Kenan Bölükbaşı

;; Author: Kenan Bölükbaşı <kenanbolukbasi@gmail.com>
;; Keywords: convenience, languages, outlines, tools
;; Homepage: https://github.com/kenanb/nop-mode

;;; Code:

;;; Dependencies [#.]

(require 'nop-base)

;;; Face Definitions [#.]
;;
;;

(defface nop-mark-base '((t :weight semi-bold :foreground "black"))
  "Default face for highlighting an overlay in nop-mark-mode."
  :version "0.1"
  :group 'nop-overlay)

(defface nop-mark-dspec '((t :inherit nop-mark-base
                             :background "dim grey"
                             :foreground "white smoke"
                             ))
  "Face for highlighting the dspec of an overlay in nop-mark-mode."
  :version "0.1"
  :group 'nop-overlay)

(defface nop-mark-annotations '((t :inherit nop-mark-base
                                   :foreground "light grey"))
  "Face for annotation nop marker with extra information in nop-mark-mode."
  :version "0.1"
  :group 'nop-overlay)

(defconst nop--mark-directive-padding (propertize " "
                                                  'face 'nop-mark-dspec
                                                  'display '(space :width .5)))

(defun nop--mark-overlay-properties (d)
  ;; higher is higher priority
  (cons '(priority 100)
        (cond ((nop--label-directive-p d)
               `((face header-line)
                 (after-string ,(propertize " < LABEL "
                                            'face 'nop-mark-annotations))))
              ((nop--anchor-directive-p d)
               `((face highlight)
                 (after-string ,(propertize " < JUMP "
                                            'face 'nop-mark-annotations))))
              ((nop--tree-directive-p d)
               `((help-echo "TREE")
                 (face nop-mark-base)
                 (after-string ,(propertize (format " < TREE: %s " (oref d depth))
                                            'face 'nop-mark-annotations))))
              (t
               '((face mode-line-highlight)
                 (after-string ,(propertize " < OTHER "
                                            'face 'nop-mark-annotations)))))))

;;; Nop Mark Enable / Disable [#.]
;;
;;

(defun nop--mark-enable ()
  "Generates [!] markers replacing the actual nop directives."
  (dolist (d (nop--parse-buffer))
    (nop--call-for-each-node
     (lambda (d depth-list)
       (unless (and (nop--tree-directive-p d) (eq :default (oref d kind)))
         (nop--generate-overlay (nop--outer-r (oref d positions))
                                (nop--mark-overlay-properties d))
         (nop--generate-overlay (nop--dspec-r (oref d positions))
                                `((priority 101)
                                  (face nop-mark-dspec)
                                  (before-string ,nop--mark-directive-padding)
                                  (after-string ,nop--mark-directive-padding)))))
     d)))

(defun nop--mark-disable ()
  (remove-overlays))

;;; Nop Mark Minor Mode [#.]
;;
;;

;;;###autoload
(define-minor-mode nop-mark-mode
  "Toggle Nop-Mark mode."
  ;; The initial value.
  :init-value nil
  ;; The indicator for the mode line.
  :lighter " Nop-Mark"
  ;; The minor mode bindings.
  :keymap (let ((map (make-sparse-keymap)))
            map)
  :group 'nop
  (if nop-mark-mode (nop--mark-enable) (nop--mark-disable)))

;;; Nop Mark Provide [#.]
;;
;;

(provide 'nop-mark)

;;; nop-mark.el ends here
