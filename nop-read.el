;;; nop-read.el --- Navigation-centric minor mode complement for nop-mode.

;; Copyright (C) 2021 Kenan Bölükbaşı

;; Author: Kenan Bölükbaşı <kenanbolukbasi@gmail.com>
;; Keywords: convenience, languages, outlines, tools
;; Homepage: https://github.com/kenanb/nop-mode

;;; Commentary:

;; Once the  whole code  is annotated  with NOP directives,  we can  write short
;; navigation  macros  to "guide  reader  through  the  code, to  communicate  a
;; specific aspect".  So it turns the code into a presentation. This can also be
;; used to make remote communication over code easier.

;;; Code:

;;;;;;;;;;;;;;;;
;;; Dependencies
;;
;;

(require 'nop-base)


;;;;;;;;;;;;;;;;;;;;;;;;
;;; Overlay Generation
;;
;;

(defconst +nop-ov-size+ 10)

(defface nop-ov-base '((t :extend t))
  "Default face for highlighting an overlay in nop-mode."
  :version "0.1"
  :group 'nop-overlay)

(defun nop-color-component (idx a v init)
  (format "%02X" (- init (truncate (* (+ v (* .5 a idx)) idx)))))

(defun nop-color (index acceleration component-velocities component-offsets)
  (format "#%s" (cl-loop for v in component-velocities and o in component-offsets
                         concat (nop-color-component index acceleration v o))))

(defun nop-gen-faces (name-prefix doc-fmt fn)
  (cl-loop for depth below +nop-ov-size+
           collect
           (custom-declare-face
            (intern (concat name-prefix (number-to-string depth)))
            `((t :inherit nop-ov-base
                 ,@(funcall fn depth)))
            (format doc-fmt depth))))

(defconst +nop-ov-drawer-faces+
  (nop-gen-faces "nop-ov-drawer-face-"
                 "Drawer content box face for depth %s."
                 (lambda (depth)
                   (list :background (nop-color depth -.5 '(#xA #xB #xC) '(#xFD #xF8 #xF0))))))

(defconst +nop-ov-handle-faces+
  (nop-gen-faces "nop-ov-handle-face-"
                 "Drawer handle line face for depth %s."
                 (lambda (depth)
                   (list :overline "black"
                         :background (nop-color depth .25 '(#x9 #x9 #x5) '(#xED #xED #xE2))))))

(defconst +nop-ov-shadow-faces+
  (nop-gen-faces "nop-ov-shadow-face-"
                 "Summary line face for depth %s, active when drawer is closed."
                 (lambda (depth)
                   (list :foreground (nop-color depth .25 '(7 7 7) '(#x9E #x9E #x9E))))))

(defconst +nop-ov-title-face+
  (custom-declare-face 'nop-ov-t-face `((t
                                         ;; :family "Source Sans Pro"
                                         :weight semi-bold
                                         ;; :height 0.9
                                         :distant-foreground "rosy brown"
                                         ;; :foreground "maroon"
                                         :foreground "saddle brown")) ""))

(setplist '+nop-overlay-invisible+ '(invisible t priority 100))
(defconst +nop-overlay-invisible+ '((category +nop-overlay-invisible+)))

(defconst +nop-ov-margin-block-width+ 4)

(defun nop-generate-margin-strings (max-depth depths)
  (cl-loop with size-m = (* (1+ max-depth) +nop-ov-margin-block-width+)
           with margin-template = (concat (make-string size-m ?\s) (propertize " " 'face 'default))
           ;; with margin-l = (copy-sequence margin-template)
           with margin-r = (copy-sequence margin-template)
           ;; with margin-lh = (copy-sequence margin-template)
           with margin-rh = (copy-sequence margin-template)
           for last = (1+ max-depth) then curr
           for curr in depths

           for drawer-face = (elt +nop-ov-drawer-faces+ curr)
           for handle-face = (elt +nop-ov-handle-faces+ curr) then drawer-face

           for last-m = (* last +nop-ov-margin-block-width+)
           for curr-m = (* curr +nop-ov-margin-block-width+)

           do
           ;; (put-text-property curr-m last-m
           ;;                    'face drawer-face
           ;;                    margin-l)
           ;; (put-text-property curr-m last-m
           ;;                    'face handle-face
           ;;                    margin-lh)
           (put-text-property (- size-m last-m) (- size-m curr-m)
                              'face drawer-face
                              margin-r)
           (put-text-property (- size-m last-m) (- size-m curr-m)
                              'face handle-face
                              margin-rh)

           finally return (vector
                           (concat
                            ;; (propertize " " 'display `((margin left-margin) ,margin-l))
                            (propertize " " 'display `((margin right-margin) ,margin-r)))
                           (concat
                            ;; (propertize " " 'display `((margin left-margin) ,margin-lh))
                            (propertize " " 'display `((margin right-margin) ,margin-rh))))))

(defun nop-tree-overlay-p (ov)
  (eq '+nop-overlay-tree+
      (overlay-get ov 'category)))


;;;;;;;;;;;;;;;;;;;;;;;;
;;; Navigation Commands
;;
;;

(defun nop-nav-jump-to-directive (d)
  (goto-char (oref (nop-info-r (oref d positions)) begin)))

(defun nop-get-last-node-of-subtree (d)
  (let* ((last-top-level (or (car (last (oref d continuations))) d))
         (last-child-node
          (cl-find-if #'nop-tree-directive-p
                      (oref last-top-level children)
                      :from-end t)))
    (if last-child-node
        (nop-get-last-node-of-subtree last-child-node)
      last-top-level)))

(defun nop-get-nearest-handle ()
  ;; The last overlay ends at buffer-end. Look behind if point is at point-max.
  (let* ((p (if (eq (point) (point-max)) (1- (point)) (point)))
         (ov (cl-find-if 'nop-tree-overlay-p (overlays-at p t))))
    ;; (message "Overlay : %s" ov)
    (overlay-get ov 'handle)))

(defun nop-nav-jump-forward ()
  (interactive)
  ;; No overlay at the end of buffer.
  (let* ((handle (nop-get-nearest-handle))
         (directive (overlay-get handle 'directive))
         (fwd-node (oref (nop-get-last-node-of-subtree directive) next-node))

         (drawer (overlay-get handle 'drawer))
         (collapsed (overlay-get handle 'collapsed)))
    (when fwd-node
      (nop-nav-jump-to-directive fwd-node)))
  (recenter))

(defun nop-find-hovered-node (&optional from-head)
  (let* ((handle (nop-get-nearest-handle))
         (directive (overlay-get handle 'directive)))
    (if from-head directive
      (cl-loop with p = (point)
               for curr = directive then next
               for next = (oref curr next-node)
               while next
               ;; Looking for the first succeeding entry that's not behind the point.
               until (< p (oref (oref next positions) begin))
               finally return curr))))

(defun nop-nav-home (&optional from-head)
  (nop-nav-jump-to-directive (nop-find-hovered-node from-head)))

(defun nop-nav-home-from-body ()
  (interactive)
  (nop-nav-home))

(defun nop-nav-home-from-head ()
  (interactive)
  (nop-nav-home t))

(defun nop-nav-step (backward &optional from-head)
  (cl-loop for curr = (nop-find-hovered-node from-head) then candidate
           for candidate = (slot-value curr (if backward 'prev-node 'next-node))
           ;; Looking for the first candidate that's not behind the point.
           while (and from-head candidate (eq (oref candidate kind) :merged))
           finally (when candidate (nop-nav-jump-to-directive candidate)))
  (recenter))

(defun nop-nav-step-forward-from-body ()
  (interactive)
  (nop-nav-step nil))

(defun nop-nav-step-forward-from-head ()
  (interactive)
  (nop-nav-step nil t))

(defun nop-nav-step-backward-from-body ()
  (interactive)
  (nop-nav-step t))

(defun nop-nav-step-backward-from-head ()
  (interactive)
  (nop-nav-step t t))


;;;;;;;;;;;;;;;;;;;;;;;;
;;; Visibility Adjustment
;;
;;

(defun nop-recurse-for-subtree (recurse-fn d &rest args)
  (cl-loop for c in-ref (oref d children) do (apply recurse-fn c args))
  (cl-loop for c in-ref (oref d continuations) do (apply recurse-fn c args)))

(defun nop-adjust-subtree-overlays (d collapse)
  (when (nop-tree-directive-p d)
    (if (eq :merged (oref d kind))
        (nop-recurse-for-subtree #'nop-adjust-subtree-overlays d collapse)
      (cl-loop with handle = (oref d handle)
               for c in '(handle drawer ellipsis title)
               for co = (overlay-get handle c)
               ;; Deletion "detaches" the overlay.
               if collapse do (delete-overlay co)
               ;; Moving will "reattach" it.
               else do (move-overlay co
                                     (overlay-get co 'cached-start)
                                     (overlay-get co 'cached-end)))
      ;; Terminate if directive is a collapsed head.
      (unless (overlay-get (oref d handle) 'collapsed)
        (nop-recurse-for-subtree #'nop-adjust-subtree-overlays d collapse)))))

(defun nop-nav-expand-node (d)
  (with-slots (kind depth handle) d
    (unless (eq :default kind)
      (let ((title (overlay-get handle 'title))
            (ellipsis (overlay-get handle 'ellipsis))
            (drawer (overlay-get handle 'drawer)))

        ;; Property to keep explicit drawer state.
        (overlay-put handle 'collapsed nil)

        (overlay-put ellipsis 'before-string nil)

        (overlay-put ellipsis 'after-string nil)

        ;; Show drawer
        (overlay-put drawer 'display nil)

        (store-substring (overlay-get title 'before-string) depth ?\N{U+25BC}))

      (nop-recurse-for-subtree #'nop-adjust-subtree-overlays d nil))))

(defun nop-nav-collapse-node (d)
  (with-slots (kind depth handle) d
    (unless (eq :default kind)
      (let ((title (overlay-get handle 'title))
            (ellipsis (overlay-get handle 'ellipsis))
            (drawer (overlay-get handle 'drawer))
            (face (list (elt +nop-ov-shadow-faces+ depth)
                        (elt +nop-ov-drawer-faces+ depth))))

        ;; Jump to the beginning of the directive that will be collapsed.
        (nop-nav-jump-to-directive d)

        ;; Property to keep explicit drawer state.
        (overlay-put handle 'collapsed t)

        (overlay-put ellipsis 'before-string
                     (propertize (format "%s\N{U+22EF}" (make-string depth ?\s)) 'face face))

        (overlay-put ellipsis 'after-string
                     (propertize " " 'face face
                                 'display '(space :align-to (- right-margin 25))))

        ;; Hide drawer
        ;; A blank line is required to maintain correct margins on collapse.
        (overlay-put drawer 'display (propertize
                                      (format "[ %s ] (%s) \n"
                                              (upcase (substring (symbol-name kind))) depth)
                                      'face face))

        (store-substring (overlay-get title 'before-string) depth ?\N{U+25B6}))

      ;; Necessary to prevent properties of nested collapsed overlays from
      ;; leaking into visual representation of the collapsed ancestor.
      (nop-recurse-for-subtree #'nop-adjust-subtree-overlays d t))))

(defun nop-nav-toggle-node-visibility ()
  (interactive)
  (let* ((handle (nop-get-nearest-handle))
         (drawer (overlay-get handle 'drawer))
         (directive (overlay-get handle 'directive)))
    ;; (message "Directive : %s" (oref directive description))
    (if (overlay-get handle 'collapsed)
        (nop-nav-expand-node directive)
      (nop-nav-collapse-node directive))))


;;;;;;;;;;;;;;;;;;;;;;;;
;;; Visibility Commands
;;
;;

(defun nop-apply-immediate-children (fn d)
  (cl-loop for c in (oref d children) do (funcall fn c)))

(defun nop-apply-all-children (fn d)
  (nop-apply-immediate-children fn d)
  (cl-loop for c in (oref d continuations) do (nop-apply-immediate-children fn c)))

(defun nop-nav-expand-subtree-from-body ()
  (interactive)
  (nop-apply-immediate-children #'nop-nav-expand-node (nop-find-hovered-node)))

(defun nop-nav-expand-subtree-from-head ()
  (interactive)
  (nop-apply-all-children #'nop-nav-expand-node (nop-find-hovered-node t)))

(defun nop-nav-collapse-subtree-from-body ()
  (interactive)
  (let ((d (nop-find-hovered-node)))
    (nop-apply-immediate-children #'nop-nav-collapse-node d)
    (nop-nav-jump-to-directive d)))

(defun nop-nav-collapse-subtree-from-head ()
  (interactive)
  (let ((d (nop-find-hovered-node t)))
    (nop-apply-all-children #'nop-nav-collapse-node d)
    (nop-nav-jump-to-directive d)))

(defun nop-remove-overlays ()
  (interactive)
  (read-only-mode -1)
  (remove-overlays))


;;;;;;;;;;;;;;;;;;;;;;;;
;;; Overlay Generation
;;
;;

(defun nop-prepare-for-overlay (max-width)
  (let ((margin-width (* (1+ max-depth) +nop-ov-margin-block-width+)))
    ;; (setf left-margin-width margin-width)
    (setf right-margin-width margin-width))
  (set-window-buffer (selected-window) (current-buffer))
  (remove-overlays)
  (read-only-mode 1)
  (fringe-mode 0)
  (setf truncate-lines t))

(defun nop-generate-title-overlay (d)
  "Generate title overlay for the directive."
  (with-slots (depth positions kind) d
    (let* ((c (eq kind :merged))
           (h-face (list +nop-ov-title-face+
                         (elt (if c +nop-ov-drawer-faces+ +nop-ov-handle-faces+) depth)))
           (sym (if c ?\N{U+25BD} ?\N{U+25BC})))
      (nop-generate-overlay (nop-info-r positions)
                            `((category +nop-overlay-directive+)
                              (help-echo "TREE")
                              (before-string ,(propertize (format "%s%c"
                                                                  (make-string depth ?\s)
                                                                  sym) 'face h-face))
                              (priority 100)
                              (face ,h-face))))))

(defun nop-generate-tree-overlay (bpos epos depth &optional line-prefix handle)
  "Generate title overlay for the directive."
  (nop-generate-overlay (nop-range :begin bpos :end epos)
                        `((priority ,depth)
                          (face ,(elt (if handle +nop-ov-handle-faces+
                                        +nop-ov-drawer-faces+)
                                      depth))
                          (category +nop-overlay-tree+)
                          (line-prefix ,line-prefix))))

(defun nop-generate-tree-overlays (d max-depth depth-list)
  (with-slots (depth kind positions) d
    (let* ((m-face (elt +nop-ov-drawer-faces+ depth))
           (title (nop-generate-title-overlay d)))

      ;; Create the overlay representing a head node, and its continuations.
      ;; Overlay for the tree directive encompass all continuations.
      ;; So we skip merged directives.
      (unless (eq kind :merged)
        (seq-let [prefix prefix-h]
            (nop-generate-margin-strings max-depth (cons depth depth-list))
          (let* ((enode (nop-get-last-node-of-subtree d))
                 (bgn-pos (oref positions begin))
                 (mid-pos (1+ (oref positions end))) ; newline should be part of handle.
                 (end-pos (if (oref enode next-node)
                              (oref (oref (oref enode next-node) positions) begin)
                            (buffer-end 1)))
                 (handle (nop-generate-tree-overlay bgn-pos mid-pos depth prefix-h t))
                 (ellipsis (nop-generate-tree-overlay mid-pos mid-pos depth))
                 (drawer (nop-generate-tree-overlay mid-pos end-pos depth prefix)))
            (overlay-put drawer 'handle handle)
            (overlay-put handle 'handle handle)
            (overlay-put handle 'title title)
            (overlay-put handle 'ellipsis ellipsis)
            (overlay-put handle 'drawer drawer)
            (overlay-put handle 'directive d)
            (oset d handle handle)))))))

(defun nop-select-overlay-properties (d current-depth)
  (let ((indent (make-string current-depth ?\s)))
    ;; higher is higher priority
    (cons '(priority 100)
          (cond ((nop-label-directive-p d)
                 `((help-echo "LABEL")
                   (before-string ,(propertize (format "%s\N{U+1433}" indent) 'face 'header-line))
                   (face header-line)))
                ((nop-jump-directive-p d)
                 `((help-echo "JUMP")
                   (before-string ,(propertize (format "%s\N{U+140A}" indent) 'face 'highlight))
                   (face highlight)))
                (t
                 '((help-echo "OTHER")
                   (face mode-line-highlight)))))))


;;;;;;;;;;;;;;;;;;;;;;;;
;;; Nop Mark Enable / Disable
;;
;;

(cl-defun nop-call-for-each-node (d max-depth depth-list fn &optional (vdepth 0))
  (funcall fn d vdepth max-depth depth-list)
  (when (nop-tree-directive-p d)
    ;; (message "Traversing children of %s" (oref d description))
    (cl-loop for c in-ref (oref d children)
             do (nop-call-for-each-node
                 c max-depth (cons (oref d depth) depth-list) fn (1+ vdepth)))
    ;; (message "Traversing continuations of %s" (oref d description))
    (cl-loop for c in-ref (oref d continuations)
             do (nop-call-for-each-node c max-depth depth-list fn vdepth))))

(defun nop-read-enable ()
  (interactive)
  "Processes the whole buffer, and creates the initial list of blocks."
  (save-excursion
    (goto-char (point-min))
    (let* ((default (make-instance 'nop-tree-directive))
           (directives (list default))
           (max-depth 0))

      (oset default description (format "Default: %s" (buffer-name)))

      ;; Buffer analysis pass: Generates directives.
      (while (re-search-forward "/[/*]" nil t)
        (when-let ((d (nop-search-directive-in-comment)))
          ;; Populated list is reverse of buffer order.
          (push d directives)))

      (nop-propagate-tree-directive-depths directives)

      (dolist (d (nop-merge directives))
        (setf max-depth (max max-depth (plist-get (oref d arbitrary) :max-depth))))

      (nop-prepare-for-overlay max-depth)

      (dolist (d (nop-merge directives))
        (nop-call-for-each-node
         d max-depth nil
         (lambda (d vdepth max-depth depth-list)
           ;; (message "Applying fn to %s - %s" vdepth (oref d description))
           (if (nop-tree-directive-p d)
               (nop-generate-tree-overlays d max-depth depth-list)
             (nop-generate-overlay (nop-info-r (oref d positions))
                                   (nop-select-overlay-properties d (elt depth-list 0))))

           (nop-generate-overlay (nop-indent-r (oref d positions))
                                 +nop-overlay-invisible+)
           (nop-generate-overlay (nop-cprefix-r (oref d positions))
                                 +nop-overlay-invisible+)
           (nop-generate-overlay (nop-dspec-r (oref d positions))
                                 +nop-overlay-invisible+)
           (nop-generate-overlay (nop-dprefix-r (oref d positions))
                                 +nop-overlay-invisible+)
           (nop-generate-overlay (nop-dsuffix-r (oref d positions))
                                 +nop-overlay-invisible+)))

        (nop-call-for-each-node
         d max-depth nil
         (lambda (d vdepth max-depth depth-list)
           (when (nop-tree-directive-p d)
             (unless (eq (oref d kind) :merged)
               (nop-nav-collapse-node d)))))

        ;; In order to avoid special-casing the default everywhere, simply detach
        ;; the default title and handle overlays in the end.
        (delete-overlay (overlay-get (oref default handle) 'title))
        (delete-overlay (oref default handle)))))
  (recenter))

(defun nop-read-disable ()
  (save-excursion
    (remove-overlays))
  (recenter))

;;
;;
;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Nop Read Minor Mode
;;
;;

;;;###autoload
(define-minor-mode nop-read-mode
  "Toggle Nop-Read mode."
  ;; The initial value.
  :init-value nil
  ;; The indicator for the mode line.
  :lighter " Nop-Read"
  ;; The minor mode bindings.
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "<tab>") #'nop-nav-toggle-node-visibility)
            (define-key map (kbd "<mouse-1>") #'nop-nav-toggle-node-visibility)
            (define-key map (kbd "+") #'nop-nav-expand-subtree-from-body)
            (define-key map (kbd "C-+") #'nop-nav-expand-subtree-from-head)
            (define-key map (kbd "-") #'nop-nav-collapse-subtree-from-body)
            (define-key map (kbd "C--") #'nop-nav-collapse-subtree-from-head)
            (define-key map (kbd "q") #'nop-remove-overlays)
            (define-key map (kbd "b") #'beginning-of-buffer)
            (define-key map (kbd "e") #'end-of-buffer)
            (define-key map (kbd "n") #'nop-nav-step-forward-from-body)
            (define-key map (kbd "N") #'nop-nav-step-forward-from-head)
            (define-key map (kbd "p") #'nop-nav-step-backward-from-body)
            (define-key map (kbd "P") #'nop-nav-step-backward-from-head)
            (define-key map (kbd "h") #'nop-nav-home-from-body)
            (define-key map (kbd "H") #'nop-nav-home-from-head)
            (define-key map (kbd "f") #'nop-nav-jump-forward)
	        map)
  :group 'nop-read
  (if nop-read-mode (nop-read-enable) (nop-read-disable)))

;;;;;;;;;;;;;;;;;;;;;
;;; Nop Provide
;;
;;

(provide 'nop-read)

;;; nop-read.el ends here
