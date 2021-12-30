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
;;; Face Definitions
;;;
;;
;;

(defface nop-read-base '((t :extend t))
  "Default face for highlighting an overlay in nop-mode."
  :version "0.1"
  :group 'nop-overlay)

(defface nop-read-title '((t
                           :weight semi-bold
                           :distant-foreground "rosy brown"
                           :foreground "saddle brown"))
  "Title face for nop-read-mode overlays."
  :version "0.1"
  :group 'nop-overlay)

(defface nop-read-title-active '((t
                                  :weight semi-bold
                                  :distant-foreground "#FCFCFC"
                                  :foreground "#121212"))
  "Title face for nop-read-mode overlays."
  :version "0.1"
  :group 'nop-overlay)

(defconst nop--drawer-faces
  (nop--gen-faces "nop-read-drawer-" 'nop-read-base
                  "Drawer content box face for depth %s."
                  (lambda (depth)
                    (list :background (nop--color depth -.5 '(#xA #xB #xC) '(#xFD #xF8 #xF0))))))

(defconst nop--handle-faces
  (nop--gen-faces "nop-read-handle-" 'nop-read-base
                  "Drawer handle line face for depth %s."
                  (lambda (depth)
                    (list :overline "black"
                          :background (nop--color depth .25 '(#x9 #x9 #x5) '(#xED #xED #xE2))))))

(defconst nop--handle-active-faces
  (nop--gen-faces "nop-read-handle-active-" 'nop-read-base
                  "Drawer content box face for depth %s."
                  (lambda (depth)
                    (list :overline "black"
                          :background (nop--color depth -.1 '(#x9 #x9 #x5) '(#xC0 #xD8 #xFD))))))

(defconst nop--shadow-faces
  (nop--gen-faces "nop-read-shadow-" 'nop-read-base
                  "Summary line face for depth %s, active when drawer is closed."
                  (lambda (depth)
                    (list :foreground (nop--color depth .25 '(7 7 7) '(#x9E #x9E #x9E))))))

;;
;;
;;;
;;; Overlay Generation
;;;
;;
;;

(setplist 'nop--overlay-invisible '(invisible t priority 100))
(defconst nop--overlay-invisible '((category nop--overlay-invisible)))

(defconst nop--ov-margin-block-width 4)

(defun nop--add-margin-indent-for-depth (margin depth size-m faces &optional reverse limit-m)
  (let* ((face (elt faces depth))
         (curr-m (* depth nop--ov-margin-block-width))
         (last-m (or limit-m size-m))
         (b (if reverse curr-m (- size-m last-m)))
         (e (if reverse last-m (- size-m curr-m))))
    (put-text-property b e 'face face margin)
    curr-m))

(defun nop--generate-indentation (max-depth size-m depths template faces &optional reverse)
  (cl-loop with margin = (copy-sequence template)
           for last-m = size-m then curr-m
           for curr in depths
           for curr-m = (nop--add-margin-indent-for-depth
                         margin curr size-m faces reverse last-m)
           finally return margin))

(defun nop--generate-prefixes (; l
                               r)
  (concat
   ;; (propertize " " 'display `((margin left-margin) ,l))
   (propertize " " 'display `((margin right-margin) ,r))))

(defun nop--generate-margin-strings (max-depth depths)
  (let* ((size-m (* (1+ max-depth) nop--ov-margin-block-width))
         (template (concat (make-string size-m ?\s) (propertize " " 'face 'default)))
         ;; (margin-l (nop--generate-indentation max-depth (cdr depths) template nop--drawer-faces t))
         ;; (margin-lh (copy-sequence margin-l))
         ;; (margin-lha (copy-sequence margin-l))
         (margin-r (nop--generate-indentation max-depth size-m (cdr depths) template nop--drawer-faces))
         (margin-rh (copy-sequence margin-r))
         (margin-rha (copy-sequence margin-r)))
    ;; (nop--add-margin-indent-for-depth margin-l   (car depths) size-m nop--drawer-faces)
    ;; (nop--add-margin-indent-for-depth margin-lh  (car depths) size-m nop--handle-faces)
    ;; (nop--add-margin-indent-for-depth margin-lh  (car depths) size-m nop--handle-active-faces)
    (nop--add-margin-indent-for-depth margin-r   (car depths) size-m nop--drawer-faces)
    (nop--add-margin-indent-for-depth margin-rh  (car depths) size-m nop--handle-faces)
    (nop--add-margin-indent-for-depth margin-rha (car depths) size-m nop--handle-active-faces)

    (vector (nop--generate-prefixes margin-r)
            (nop--generate-prefixes margin-rh)
            (nop--generate-prefixes margin-rha))))

(defun nop--tree-overlay-p (ov)
  (eq 'nop--overlay-tree
      (overlay-get ov 'category)))

;;
;;
;;;
;;; Navigation Routines
;;;
;;
;;

(defun nop--nav-jump-to-directive (d)
  (goto-char (oref (nop--info-r (oref d positions)) begin)))

(defun nop--get-last-node-of-subtree (d)
  (let* ((last-top-level (or (car (last (oref d continuations))) d))
         (last-child-node
          (cl-find-if #'nop--tree-directive-p
                      (oref last-top-level children)
                      :from-end t)))
    (if last-child-node
        (nop--get-last-node-of-subtree last-child-node)
      last-top-level)))

(defun nop--successor-in-direction (d backwardp)
  (slot-value d (if backwardp 'prev-node 'next-node)))

(defun nop--get-primary (d)
  (cl-loop with depth = (oref d depth)
           for prev = d then (oref prev prev-node)
           while (or (eq :merged (oref prev kind))
                     (not (= depth (oref prev depth))))
           finally return prev))

(defun nop--possibly-visible-successor-in-direction (d backwardp)
  (if backwardp
      (if (eq :merged (oref d kind))
          (nop--get-primary d)
        (oref d prev-node))
    (oref (nop--get-last-node-of-subtree d) next-node)))

(defun nop--node-visible-p (d)
  (let* ((primary (nop--get-primary d))
         (handle (nop--get-arbitrary primary :handle))
         (primary-expanded (not (overlay-get handle 'collapsed)))
         (primary-visible (overlay-start handle)))
    ;; If handle is not visible, node is invisible regardless.
    (cond
     ;; If primary is :DEFAULT, both primary and its continuation nodes are
     ;; visible.
     ((eq :default (oref primary kind)) t)
     ((eq :merged (oref d kind)) (and primary-visible primary-expanded))
     (t primary-visible))))

(defun nop--next-visible-node (backwardp primaryp)
  (cl-loop for curr = (nop--cached-active primaryp) then candidate
           while curr

           for curr-visible = t then candidate-visible

           for candidate = (funcall (if curr-visible #'nop--successor-in-direction
                                      #'nop--possibly-visible-successor-in-direction)
                                    curr backwardp)
           while candidate

           for candidate-visible = (nop--node-visible-p candidate)

           thereis (when (and candidate

                              ;; NOTE : Not skipping hidden candidates will cause the cached
                              ;; hovered node and calculated hovered node to diverge.
                              candidate-visible

                              ;; If PRIMARYP is requested, skip merged directives.
                              (not (and primaryp (eq (oref candidate kind) :merged))))

                     candidate)))

(defun nop--get-nearest-handle ()
  ;; The last overlay ends at buffer-end. Look behind if point is at point-max.
  (let* ((p (if (eq (point) (point-max)) (1- (point)) (point)))
         (ov (cl-find-if 'nop--tree-overlay-p (overlays-at p t))))
    (overlay-get ov 'handle)))

(defun nop--find-hovered-node (&optional primaryp)
  (let* ((handle (nop--get-nearest-handle))
         (directive (overlay-get handle 'directive)))
    (if primaryp directive
      (cl-loop with p = (point)
               for curr = directive then next
               for next = (oref curr next-node)
               while next
               ;; Looking for the first succeeding entry that's not behind the point.
               until (< p (oref (oref next positions) begin))
               finally return curr))))

(defun nop--get-nearest-visible-node (&optional primaryp)
  (let* ((backwardp (cl-minusp (- (point) nop--last-point)))
         (found (nop--find-hovered-node primaryp))
         (invisiblep (not (nop--node-visible-p found)))
         (collapsed-primary-p (and (not (eq :merged (oref found kind)))
                                   (overlay-get (nop--get-arbitrary found :handle) 'collapsed)))
         (collapsed-on-point-p (and collapsed-primary-p
                                    (> (point) (oref (nop--info-r (oref found positions)) end)))))
    (cond
     (invisiblep
      (setf found (nop--next-visible-node backwardp primaryp))
      (nop--nav-jump-to-directive found))
     (collapsed-on-point-p
      ;; If possible, make sure to jump over the collapsed section.
      (unless backwardp ; If EOF, stay at found.
        (setf found (or (nop--next-visible-node nil primaryp) found)))
      (nop--nav-jump-to-directive found)))

    found))

;;
;;
;;;
;;; Active Node Tracking
;;;
;;
;;

;; (cl-declaim (optimize (speed 3) (safety 2)))

(defvar-local nop--active-primary nil)
(defvar-local nop--active-focused nil)

(define-inline nop-assert-cached-active (&optional primaryp)
  (inline-quote
   (cl-assert (eq ,(if primaryp 'nop--active-primary 'nop--active-focused)
                  (nop--find-hovered-node ,primaryp))
              nil
              "Cached active [ %s (%s) ] used in %S is outdated: [ %s (%s) ]"
              nop--last-point
              (oref ,(if primaryp 'nop--active-primary 'nop--active-focused) description)
              (cadr (backtrace-frame 7))
              (point)
              (oref (nop--find-hovered-node ,primaryp) description))))

;; (define-inline nop-assert-cached-active (&optional primaryp) nil)

(defun nop--adjust-primary-activation (node active-handle active-title)
  (when node
    (let* ((depth (oref node depth))
           (handle (nop--get-arbitrary node :handle))
           (title (nop--get-arbitrary node :title))
           (before-string (overlay-get title 'before-string))
           (h-face (elt (if active-handle nop--handle-active-faces nop--handle-faces) depth))
           (t-face (list (if active-title 'nop-read-title-active 'nop-read-title) h-face))
           (prefix (funcall (if active-handle #'cdr #'car) (overlay-get handle 'prefixes))))
      (overlay-put title 'face t-face)
      (overlay-put title 'before-string (propertize before-string 'face t-face))
      (overlay-put handle 'face h-face)
      (overlay-put handle 'line-prefix prefix))))

(defun nop--adjust-focused-activation (node active)
  (when node
    (let* ((depth (oref node depth))
           (title (nop--get-arbitrary node :title))
           (before-string (overlay-get title 'before-string))
           (t-face (list (if active 'nop-read-title-active 'nop-read-title)
                         (elt nop--drawer-faces depth))))
      (overlay-put title 'face t-face)
      (overlay-put title 'before-string (propertize before-string 'face t-face)))))

(defun nop--read-post-command ()
  (unless (eq (point) nop--last-point)

    (let* ((new-primary (nop--get-nearest-visible-node t))
           (new-focused (nop--get-nearest-visible-node))
           (old-primary nop--active-primary)
           (old-focused nop--active-focused)
           (primary-changed-p (not (eq new-primary old-primary)))
           (active-changed-p (not (eq new-focused old-focused)))
           (active-old-primary-p (eq old-primary old-focused))
           (active-new-primary-p (eq new-primary new-focused)))

      (when active-changed-p

        (cond
         (primary-changed-p
          (nop--adjust-primary-activation old-primary nil nil)
          (nop--adjust-primary-activation new-primary t active-new-primary-p)
          (unless active-old-primary-p
            (nop--adjust-focused-activation old-focused nil))
          (unless active-new-primary-p
            (nop--adjust-focused-activation new-focused t)))

         (active-old-primary-p
          (nop--adjust-primary-activation new-primary t nil)
          (nop--adjust-focused-activation new-focused t))

         (active-new-primary-p
          (nop--adjust-primary-activation new-primary t t)
          (nop--adjust-focused-activation old-focused nil))

         (t
          (nop--adjust-focused-activation old-focused nil)
          (nop--adjust-focused-activation new-focused t)))

        (setf nop--active-primary new-primary
              nop--active-focused new-focused)))

    (setf nop--last-point (point))))

;;
;;
;;;
;;; Navigation Commands
;;;
;;
;;

(defun nop-nav-jump-forward ()
  (interactive)
  (nop-assert-cached-active t)
  ;; No overlay at the end of buffer.
  (let* ((handle (nop--get-nearest-handle))
         (directive (overlay-get handle 'directive))
         (fwd-node (oref (nop--get-last-node-of-subtree directive) next-node))

         (drawer (overlay-get handle 'drawer))
         (collapsed (overlay-get handle 'collapsed)))
    (when fwd-node
      (nop--nav-jump-to-directive fwd-node)))
  (recenter))

(defun nop--cached-active (primaryp)
  (if primaryp nop--active-primary nop--active-focused))

(defun nop--nav-home (&optional primaryp)
  (nop--nav-jump-to-directive (nop--cached-active primaryp)))

(defun nop-nav-home-focused ()
  (interactive)
  (nop-assert-cached-active)
  (nop--nav-home))

(defun nop-nav-home-primary ()
  (interactive)
  (nop--nav-home t))

(defun nop--nav-step (backwardp &optional primaryp)
  (when-let ((found (nop--next-visible-node backwardp primaryp)))
    (nop--nav-jump-to-directive found))
  (recenter))

(defun nop-nav-step-forward-focused ()
  (interactive)
  (nop-assert-cached-active)
  (nop--nav-step nil))

(defun nop-nav-step-forward-primary ()
  (interactive)
  (nop-assert-cached-active t)
  (nop--nav-step nil t))

(defun nop-nav-step-backward-focused ()
  (interactive)
  (nop-assert-cached-active)
  (nop--nav-step t))

(defun nop-nav-step-backward-primary ()
  (interactive)
  (nop-assert-cached-active t)
  (nop--nav-step t t))

;;
;;
;;;
;;; Visibility Adjustment
;;;
;;
;;

(defun nop--recurse-for-subtree (recurse-fn d &rest args)
  (cl-loop for c in-ref (oref d children) do (apply recurse-fn c args))
  (cl-loop for c in-ref (oref d continuations) do (apply recurse-fn c args)))

(defun nop--adjust-subtree-overlays (d collapse)
  (when (nop--tree-directive-p d)
    (if (eq :merged (oref d kind))
        (nop--recurse-for-subtree #'nop--adjust-subtree-overlays d collapse)
      (cl-loop with handle = (nop--get-arbitrary d :handle)
               for c in '(handle drawer ellipsis title)
               for co = (overlay-get handle c)
               ;; Deletion "detaches" the overlay.
               if collapse do (delete-overlay co)
               ;; Moving will "reattach" it.
               else do (move-overlay co
                                     (overlay-get co 'cached-start)
                                     (overlay-get co 'cached-end)))
      ;; Terminate if directive is a collapsed primary.
      (unless (overlay-get (nop--get-arbitrary d :handle) 'collapsed)
        (nop--recurse-for-subtree #'nop--adjust-subtree-overlays d collapse)))))

(defun nop--nav-expand-node (d)
  (when (nop--tree-directive-p d)
    (with-slots (kind depth) d
      (unless (eq :default kind)
        (let ((handle (nop--get-arbitrary d :handle))
              (title (nop--get-arbitrary d :title))
              (ellipsis (overlay-get handle 'ellipsis))
              (drawer (overlay-get handle 'drawer)))

          ;; Property to keep explicit drawer state.
          (overlay-put handle 'collapsed nil)

          (overlay-put ellipsis 'before-string nil)

          (overlay-put ellipsis 'after-string nil)

          ;; Show drawer
          (overlay-put drawer 'display nil)

          (store-substring (overlay-get title 'before-string) depth ?\N{U+25BC}))

        (nop--recurse-for-subtree #'nop--adjust-subtree-overlays d nil)))))

(defun nop--nav-collapse-node (d)
  (when (nop--tree-directive-p d)
    (with-slots (kind depth) d
      (unless (eq :default kind)
        (let ((handle (nop--get-arbitrary d :handle))
              (title (nop--get-arbitrary d :title))
              (ellipsis (overlay-get handle 'ellipsis))
              (drawer (overlay-get handle 'drawer))
              (face (list (elt nop--shadow-faces depth)
                          (elt nop--drawer-faces depth))))

          ;; Jump to the beginning of the directive that will be collapsed.
          (nop--nav-jump-to-directive d)

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
        (nop--recurse-for-subtree #'nop--adjust-subtree-overlays d t)))))

(defun nop-nav-toggle-node-visibility ()
  (interactive)
  (let* ((handle (nop--get-nearest-handle))
         (drawer (overlay-get handle 'drawer))
         (directive (overlay-get handle 'directive)))
    (if (overlay-get handle 'collapsed)
        (nop--nav-expand-node directive)
      (nop--nav-collapse-node directive))))

;;
;;
;;;
;;; Visibility Commands
;;;
;;
;;

(defun nop--apply-immediate-children (fn d)
  (cl-loop for c in (oref d children) do (funcall fn c)))

(defun nop--apply-all-children (fn d)
  (nop--apply-immediate-children fn d)
  (cl-loop for c in (oref d continuations) do (nop--apply-immediate-children fn c)))

(defun nop-nav-expand-subtree-focused ()
  (interactive)
  (nop-assert-cached-active)
  (nop--apply-immediate-children #'nop--nav-expand-node nop--active-focused))

(defun nop-nav-expand-subtree-primary ()
  (interactive)
  (nop-assert-cached-active t)
  (nop--apply-all-children #'nop--nav-expand-node nop--active-primary))

(defun nop-nav-collapse-subtree-focused ()
  (interactive)
  (nop-assert-cached-active)
  (let ((d nop--active-focused))
    (nop--apply-immediate-children #'nop--nav-collapse-node d)
    (nop--nav-jump-to-directive d)))

(defun nop-nav-collapse-subtree-primary ()
  (interactive)
  (nop-assert-cached-active t)
  (let ((d nop--active-primary))
    (nop--apply-all-children #'nop--nav-collapse-node d)
    (nop--nav-jump-to-directive d)))

(defun nop-nav-buffer-begin ()
  (interactive)
  (goto-char (buffer-end -1)))

(defun nop-nav-buffer-end ()
  (interactive)
  (goto-char (buffer-end 1)))

(defun nop-remove-overlays ()
  (interactive)
  (read-only-mode -1)
  (remove-overlays))

;;
;;
;;;
;;; Overlay Generation
;;;
;;
;;

(defun nop--generate-title-overlay (d)
  "Generate title overlay for the directive."
  (with-slots (depth positions kind) d
    (let* ((c (eq kind :merged))
           (h-face (list 'nop-read-title
                         (elt (if c nop--drawer-faces nop--handle-faces) depth)))
           (sym (if c ?\N{U+25BD} ?\N{U+25BC})))
      (nop--generate-overlay (nop--info-r positions)
                             `((category nop--overlay-directive)
                               (help-echo "TREE")
                               (before-string ,(propertize (format "%s%c"
                                                                   (make-string depth ?\s)
                                                                   sym) 'face h-face))
                               (priority 100)
                               (face ,h-face))))))

(defun nop--generate-tree-overlay (bpos epos depth &optional prefixes handle)
  "Generate title overlay for the directive."
  (nop--generate-overlay (nop--range :begin bpos :end epos)
                         `((priority ,depth)
                           (face ,(elt (if handle nop--handle-faces
                                         nop--drawer-faces)
                                       depth))
                           (category nop--overlay-tree)
                           (prefixes ,prefixes)
                           ;; CAR is default prefix, CDR is active prefix.
                           (line-prefix ,(car prefixes)))))

(defun nop--generate-tree-overlays (d max-depth depth-list)
  (with-slots (depth kind positions) d
    (let* ((m-face (elt nop--drawer-faces depth))
           (title (nop--generate-title-overlay d)))

      (nop--set-arbitrary d :title title)
      ;; Create the overlay representing a primary node, and its continuations.
      ;; Overlay for the tree directive encompass all continuations.
      ;; So we skip merged directives.
      (unless (eq kind :merged)
        (seq-let [prefix prefix-h prefix-a]
            (nop--generate-margin-strings max-depth (cons depth depth-list))
          (let* ((enode (nop--get-last-node-of-subtree d))
                 (bgn-pos (oref positions begin))
                 (mid-pos (1+ (oref positions end))) ; newline should be part of handle.
                 (end-pos (if (oref enode next-node)
                              (oref (oref (oref enode next-node) positions) begin)
                            (buffer-end 1)))
                 (handle (nop--generate-tree-overlay bgn-pos mid-pos depth (cons prefix-h prefix-a) t))
                 (ellipsis (nop--generate-tree-overlay mid-pos mid-pos depth))
                 (drawer (nop--generate-tree-overlay mid-pos end-pos depth (list prefix))))
            (overlay-put drawer 'handle handle)
            (overlay-put handle 'handle handle)
            (overlay-put handle 'title title)
            (overlay-put handle 'ellipsis ellipsis)
            (overlay-put handle 'drawer drawer)
            (overlay-put handle 'directive d)
            (nop--set-arbitrary d :handle handle)))))))

(defun nop--read-overlay-properties (d current-depth)
  (let ((indent (make-string current-depth ?\s)))
    ;; higher is higher priority
    (cons '(priority 100)
          (cond ((nop--label-directive-p d)
                 `((help-echo "LABEL")
                   (before-string ,(propertize (format "%s\N{U+1433}" indent) 'face 'header-line))
                   (face header-line)))
                ((nop--jump-directive-p d)
                 `((help-echo "JUMP")
                   (before-string ,(propertize (format "%s\N{U+140A}" indent) 'face 'highlight))
                   (face highlight)))
                (t
                 '((help-echo "OTHER")
                   (face mode-line-highlight)))))))

;;
;;
;;;
;;; Nop Read Enable / Disable
;;;
;;
;;

;; (defvar-local nop--left-margin-width-copy nil)
(defvar-local nop--right-margin-width-copy nil)
(defvar-local nop--fringe-mode-copy nil)
(defvar-local nop--truncate-lines-copy nil)
(defvar-local nop--buffer-read-only-copy nil)
(defvar-local nop--last-point nil)

(defun nop--before-read-mode (max-depth)
  (let ((margin-width (* (1+ max-depth) nop--ov-margin-block-width)))
    ;; (cl-shiftf nop--left-margin-width-copy left-margin-width margin-width)
    (cl-shiftf nop--right-margin-width-copy right-margin-width margin-width))
  ;; Necessary to activate margin width change.
  (set-window-buffer (selected-window) (current-buffer))
  (remove-overlays)
  (setf nop--buffer-read-only-copy buffer-read-only)
  (read-only-mode 1)
  (setf nop--fringe-mode-copy fringe-mode)
  (fringe-mode 0)
  (cl-shiftf nop--truncate-lines-copy truncate-lines t))

(defun nop--after-read-mode ()
  (setf nop--last-point nil)
  ;; (setf left-margin-width nop--left-margin-width-copy)
  (setf right-margin-width nop--right-margin-width-copy)
  ;; Necessary to activate margin width change.
  (set-window-buffer (selected-window) (current-buffer))
  (remove-overlays)
  (unless nop--buffer-read-only-copy (read-only-mode -1))
  (fringe-mode nop--fringe-mode-copy)
  (setf truncate-lines nop--truncate-lines-copy))

(defun nop--read-enable (&optional collapsed)
  "Generates nested drawer overlays for nop directives. Buffer is read-only."
  (let* ((merged (nop--parse-buffer))
         (default (car merged))
         (max-depth 0))

    (dolist (d merged)
      (setf max-depth (max max-depth (nop--get-arbitrary d :max-depth))))

    (nop--before-read-mode max-depth)

    (dolist (d merged)
      (nop--call-for-each-node
       (lambda (d depth-list max-depth)
         (if (nop--tree-directive-p d)
             (nop--generate-tree-overlays d max-depth depth-list)
           (nop--generate-overlay (nop--info-r (oref d positions))
                                  (nop--read-overlay-properties d (elt depth-list 0))))

         (nop--generate-overlay (nop--indent-r (oref d positions))
                                nop--overlay-invisible)
         (nop--generate-overlay (nop--cprefix-r (oref d positions))
                                nop--overlay-invisible)
         (nop--generate-overlay (nop--dspec-r (oref d positions))
                                nop--overlay-invisible)
         (nop--generate-overlay (nop--dprefix-r (oref d positions))
                                nop--overlay-invisible)
         (nop--generate-overlay (nop--dsuffix-r (oref d positions))
                                nop--overlay-invisible))
       d nil (list max-depth))

      (when collapsed
        (nop--call-for-each-node
         (lambda (d depth-list)
           (when (nop--tree-directive-p d)
             (unless (eq (oref d kind) :merged)
               (nop--nav-collapse-node d))))
         d))

      ;; In order to avoid special-casing the default everywhere, simply detach
      ;; the default title and handle overlays in the end.
      (delete-overlay (nop--get-arbitrary default :title))
      (delete-overlay (nop--get-arbitrary default :handle))))

  (setf nop--last-point (point))
  (setf nop--active-primary (nop--get-nearest-visible-node t))
  (setf nop--active-focused (nop--get-nearest-visible-node))
  (add-hook 'post-command-hook #'nop--read-post-command 0 t)

  (when collapsed
    (recenter)))

(defun nop--read-disable ()
  (remove-hook 'post-command-hook #'nop--read-post-command t)
  (setf nop--active-primary nil)
  (setf nop--active-focused nil)

  (nop--after-read-mode))

;;
;;
;;;
;;; Nop Read Minor Mode
;;;
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
            (define-key map (kbd "C-+") #'nop-nav-expand-subtree-focused)
            (define-key map (kbd "+") #'nop-nav-expand-subtree-primary)
            (define-key map (kbd "C--") #'nop-nav-collapse-subtree-focused)
            (define-key map (kbd "-") #'nop-nav-collapse-subtree-primary)
            (define-key map (kbd "q") #'nop-remove-overlays)
            (define-key map (kbd "b") #'nop-nav-buffer-begin)
            (define-key map (kbd "e") #'nop-nav-buffer-end)
            (define-key map (kbd ">") #'nop-nav-step-forward-focused)
            (define-key map (kbd ".") #'nop-nav-step-forward-primary)
            (define-key map (kbd "<") #'nop-nav-step-backward-focused)
            (define-key map (kbd ",") #'nop-nav-step-backward-primary)
            (define-key map (kbd "H") #'nop-nav-home-focused)
            (define-key map (kbd "h") #'nop-nav-home-primary)
            (define-key map (kbd "f") #'nop-nav-jump-forward)
	        map)
  :group 'nop-read
  (if nop-read-mode (nop--read-enable) (nop--read-disable)))

;;
;;
;;;
;;; Nop Read Provide
;;;
;;
;;

(provide 'nop-read)

;;; nop-read.el ends here
