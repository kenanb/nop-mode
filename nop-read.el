;;;; -*- lexical-binding: t -*-

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

(defconst nop--scale-coefficient 1.75)

(defface nop-read-title `((t :weight semi-bold))
  "Title face for nop-read-mode overlays."
  :version "0.1"
  :group 'nop-overlay)

(defface nop-read-title-active `((t :weight semi-bold))
  "Active title face for nop-read-mode overlays."
  :version "0.1"
  :group 'nop-overlay)

(defvar nop--drawer-faces
  (nop--gen-faces "nop-read-drawer-"
                  'nop-read-base
                  "Drawer content box face for depth %s."))

(defvar nop--shadow-faces
  (nop--gen-faces "nop-read-shadow-"
                  'nop-read-base
                  "Summary line face for depth %s, active when drawer is closed."))

(defvar nop--handle-faces
  (nop--gen-faces "nop-read-handle-"
                  'nop-read-base
                  "Drawer handle line face for depth %s."))

(defvar nop--handle-active-faces
  (nop--gen-faces "nop-read-handle-active-"
                  'nop-read-base
                  "Drawer content box face for depth %s."))

(defvar nop--base-colors nil)
(defvar nop--faces-dirty t)

(defun nop--ensure-face-attributes ()
  "Generates faces that have their background and foreground color
information calculated based on the current DEFAULT face."

  (when nop--faces-dirty

    (lwarn 'nop :debug "Recalculating nop-read-mode faces.")

    (seq-let [bg fg ch] (nop--calculate-color-info)

      ;; NOTE : Base (0-offset) colors must be used for this calculation.
      (setf nop--faces-dirty nil nop--base-colors (list bg fg))

      (set-face-attribute 'nop-read-title nil
                          :foreground
                          (nop--color (* nop--scale-coefficient nop--overlay-max-depth) .25 ch bg))

      (set-face-attribute 'nop-read-title-active nil
                          :foreground (nop--color 0 .25 ch fg))

      (nop--set-faces "nop-read-drawer-"
                      (lambda (depth)
                        (list :background (nop--color depth .25 ch bg))))

      (seq-let [bg-handle fg-handle ch-handle] (nop--calculate-color-info -3000)
        (nop--set-faces "nop-read-handle-"
                        (lambda (depth)
                          (list :overline (nop--gen-color fg)
                                :background (nop--color depth .25 ch-handle bg-handle)))))

      (seq-let [bg-active fg-active ch-active] (nop--calculate-color-info '(-20000 -10000 0))
        (nop--set-faces "nop-read-handle-active-"
                        (lambda (depth)
                          (list :overline (nop--gen-color fg)
                                :background (nop--color depth -.1 ch-active bg-active))))))

    (seq-let [bg fg ch] (nop--calculate-color-info -20000)
      (nop--set-faces "nop-read-shadow-"
                      (lambda (depth)
                        (list :foreground (nop--color depth .25 ch bg)))))))

(defun nop--check-faces-dirty (&rest r)
  (seq-let [bg fg ch] (nop--calculate-color-info)
    (unless (equal nop--base-colors (list bg fg))

      (lwarn 'nop :debug "Default face changed. Setting nop-read-mode faces dirty.")

      (setf nop--faces-dirty t)
      (when nop-read-mode (nop--ensure-face-attributes)))))

(advice-add #'enable-theme :after #'nop--check-faces-dirty)
(advice-add #'disable-theme :after #'nop--check-faces-dirty)

;;
;;
;;;
;;; Overlay Generation
;;;
;;
;;

;; TODO : If directive line has code, handle should begin after code, and omit indentation.

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

(defun nop--next-visible-node (current backwardp primaryp)
  (cl-loop for curr = current then candidate
           while curr

           for curr-visible = t then candidate-visible

           for candidate = (funcall (if curr-visible #'nop--successor-in-direction
                                      #'nop--possibly-visible-successor-in-direction)
                                    curr backwardp)
           while candidate

           for candidate-visible = (nop--node-visible-p candidate)

           thereis (when (and candidate

                              ;; NOTE : Not skipping hidden candidates will cause the cached
                              ;;        hovered node and calculated hovered node to diverge.
                              candidate-visible

                              ;; If PRIMARYP is requested, skip merged directives.
                              (not (and primaryp (eq (oref candidate kind) :merged))))

                     candidate)))

(defun nop--get-nearest-handle ()
  ;; The last overlay ends at buffer-end. Look behind if point is at point-max.
  (let* ((p (if (eq (point) (point-max)) (1- (point)) (point)))
         (ov (cl-find-if 'nop--tree-overlay-p (overlays-at p t))))
    (overlay-get ov 'handle)))

(defun nop--locate-focused (begin-node)
  (cl-loop with p = (point)
           for curr = begin-node then next
           for next = (oref curr next-node)
           while next
           ;; Looking for the first succeeding entry that's not behind the point.
           until (< p (oref (oref next positions) begin))
           finally return curr))

(defun nop--get-nearest-visible-node-pair (last-point)
  (let* ((backwardp (cl-minusp (- (point) last-point)))
         (handle (nop--get-nearest-handle))
         (primary (overlay-get handle 'directive))
         (primary-collapsed-p (overlay-get handle 'collapsed))
         (focused (nop--locate-focused primary))
         (past-handle-p (> (point) (oref (nop--info-r (oref primary positions)) end)))
         (collapsed-on-point-p (and primary-collapsed-p past-handle-p)))
    (if collapsed-on-point-p
        ;; If possible, make sure to jump over the collapsed section.
        ;; If EOF, backtrack to primary.
        (if backwardp
            (progn
              (nop--nav-jump-to-directive primary)
              (cons primary primary))
          (setf focused (or (nop--next-visible-node primary nil nil) primary))
          (nop--nav-jump-to-directive focused)
          (cons (overlay-get (nop--get-nearest-handle) 'directive) focused))
      (cons primary focused))))

;;
;;
;;;
;;; Active Node Tracking
;;;
;;
;;

;; (cl-declaim (optimize (speed 3) (safety 2)))

(defvar-local nop--last-point nil)
(defvar-local nop--active-primary nil)
(defvar-local nop--active-focused nil)

(define-inline nop-assert-cached-primary (fn-name)
  (inline-quote
   (cl-assert (eq (overlay-get (nop--get-nearest-handle) 'directive)
                  nop--active-primary)
              nil
              "Cached primary [ %s (%s) ] used in %S is outdated: [ %s (%s) ]"
              nop--last-point
              (nop--get-description nop--active-primary)
              ,fn-name
              (point)
              (nop--get-description (overlay-get (nop--get-nearest-handle) 'directive)))))

(define-inline nop-assert-cached-focused (fn-name)
  (inline-quote
   (cl-assert (eq (nop--locate-focused (overlay-get (nop--get-nearest-handle) 'directive))
                  nop--active-focused)
              nil
              "Cached focused [ %s (%s) ] used in %S is outdated: [ %s (%s) ]"
              nop--last-point
              (nop--get-description nop--active-focused)
              ,fn-name
              (point)
              (nop--get-description (nop--locate-focused
                                     (overlay-get (nop--get-nearest-handle) 'directive))))))

;; (define-inline nop-assert-cached-primary (fn-name) nil)
;; (define-inline nop-assert-cached-focused (fn-name) nil)

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

    (let* ((new-nodes (nop--get-nearest-visible-node-pair nop--last-point))
           (new-primary (car new-nodes))
           (new-focused (cdr new-nodes))
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

(defvar nop-read-recenter-after-jump t)

(defun maybe-recenter ()
  (when nop-read-recenter-after-jump (recenter)))

(defun nop-nav-step-forward-shallow ()
  (interactive)
  (nop-assert-cached-primary "step-forward-shallow")
  ;; No overlay at the end of buffer.
  (let* ((handle (nop--get-nearest-handle))
         (directive (overlay-get handle 'directive))
         (fwd-node (oref (nop--get-last-node-of-subtree directive) next-node))

         (drawer (overlay-get handle 'drawer))
         (collapsed (overlay-get handle 'collapsed)))
    (when fwd-node
      (nop--nav-jump-to-directive fwd-node)))
  (maybe-recenter))

;; TODO
(defun nop-nav-step-backward-shallow ()
  (interactive))

(defun nop--cached-active (primaryp)
  (if primaryp nop--active-primary nop--active-focused))

(defun nop--nav-home (primaryp)
  (nop--nav-jump-to-directive (nop--cached-active primaryp)))

(defun nop-nav-home-focused ()
  (interactive)
  (nop-assert-cached-focused "home-focused")
  (nop--nav-home nil))

(defun nop-nav-home-primary ()
  (interactive)
  (nop--nav-home t))

;; BUG : Step triggers outdated cache assertion if point is at the end of the handle.
(defun nop--nav-step (backwardp primaryp)
  (let* ((primary (nop--cached-active t))
         (handle (nop--get-arbitrary primary :handle))
         (primary-collapsed-p (overlay-get handle 'collapsed))
         (focused (nop--cached-active primaryp))
         (past-title-p (> (point) (oref (nop--info-r (oref focused positions)) end))))

    ;; NOTE : Under normal circumstances, point shouldn't be inside the collapsed drawer.
    ;;        Because post-command-hook attempts to prevent that.
    ;;        Howver, it is currently not guaranteed.

    (if (and backwardp (not primary-collapsed-p) past-title-p)
        (progn
          ;; If point in drawer, backwards step should jump to title.
          (nop--nav-jump-to-directive focused)
          (maybe-recenter))
      (when-let ((found (nop--next-visible-node focused backwardp primaryp)))
        (nop--nav-jump-to-directive found)
        (maybe-recenter)))))

(defun nop-nav-step-forward-focused ()
  (interactive)
  (nop-assert-cached-focused "step-forward-focused")
  (nop--nav-step nil nil))

(defun nop-nav-step-forward-primary ()
  (interactive)
  (nop-assert-cached-primary "step-forward-primary")
  (nop--nav-step nil t))

(defun nop-nav-step-backward-focused ()
  (interactive)
  (nop-assert-cached-focused "step-backward-focused")
  (nop--nav-step t nil))

(defun nop-nav-step-backward-primary ()
  (interactive)
  (nop-assert-cached-primary "step-backward-primary")
  (nop--nav-step t t))

(defconst nop-smart-step-min-lines 3)
(defconst nop-smart-step-max-lines 9)

;; TODO
(defun nop--nav-smart-step (backwardp))

;; TODO
(defun nop-nav-step-forward-context ()
  (interactive)
  (nop-assert-cached-primary "step-forward-context")
  (nop--nav-smart-step nil))

;; TODO
(defun nop-nav-step-backward-context ()
  (interactive)
  (nop-assert-cached-primary "step-backward-context")
  (nop--nav-smart-step t))

;;
;;
;;;
;;; Visibility Adjustment
;;;
;;
;;

(defconst nop--kind-descriptions
  (list :none "Uncategorized"
        :continuation "-"
        :link "-"
        :note "Developer Note"
        :todo "Todo Item"
        :kludge "Workaround"
        :unit-test "Unit Test"
        :header "Source Header"
        :preprocessor "Preprocessor Directive"
        :declaration "Variables - Aliases"
        :function "Function Definition"
        :macro "Macro Definition"
        :class "Class Definition"
        :block "Code Block"
        :iteration "Iteration"
        :recursion "Recursion"
        :selection "Conditional Selection"
        :condition "Conditional Clause"
        :scope-init "Block Scope Init"
        :scope-exit "Block Scope Exit"
        :assertion "Assertion"
        :logging "Logging"
        :exception "Exception - Error Handling"
        :validation "Validation - Postcondition"
        :guard-clause "Guard Clause - Precondition"))

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
        (let* ((handle (nop--get-arbitrary d :handle))
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
        (let* ((handle (nop--get-arbitrary d :handle))
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
                                   'display '(space :align-to (- right-margin 30))))

          ;; Hide drawer
          ;; A blank line is required to maintain correct margins on collapse.
          (overlay-put drawer 'display (propertize
                                        (format "[ %s ] (%s) \n"
                                                (plist-get nop--kind-descriptions kind) depth)
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
  (nop-assert-cached-focused "expand-subtree-focused")
  (nop--apply-immediate-children #'nop--nav-expand-node nop--active-focused))

(defun nop-nav-expand-subtree-primary ()
  (interactive)
  (nop-assert-cached-primary "expand-subtree-primary")
  (nop--apply-all-children #'nop--nav-expand-node nop--active-primary))

(defun nop-nav-collapse-subtree-focused ()
  (interactive)
  (nop-assert-cached-focused "collapse-subtree-focused")
  (let ((d nop--active-focused))
    (nop--apply-immediate-children #'nop--nav-collapse-node d)
    (nop--nav-jump-to-directive d)))

(defun nop-nav-collapse-subtree-primary ()
  (interactive)
  (nop-assert-cached-primary "collapse-subtree-primary")
  (let ((d nop--active-primary))
    (nop--apply-all-children #'nop--nav-collapse-node d)
    (nop--nav-jump-to-directive d)))

(defun nop-nav-buffer-begin ()
  (interactive)
  (goto-char (buffer-end -1)))

(defun nop-nav-buffer-end ()
  (interactive)
  (goto-char (buffer-end 1)))

;; TODO
(defun nop-nav-incremental-expand ()
  (interactive))

;; TODO
(defun nop-nav-incremental-collapse ()
  (interactive))

;; TODO
(defun nop-nav-global-expand ()
  (interactive))

;; TODO
(defun nop-nav-global-collapse ()
  (interactive))

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
                               (before-string ,(propertize (format "%s%c "
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
          (cond ((nop--bookmark-directive-p d)
                 `((help-echo "BOOKMARK")
                   (before-string ,(propertize (format "%s\N{U+1433} " indent) 'face 'header-line))
                   (face header-line)))
                ((nop--anchor-directive-p d)
                 `((help-echo "ANCHOR")
                   (before-string ,(propertize (format "%s\N{U+140A} " indent) 'face 'highlight))
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
(defvar-local nop--header-line-format-copy nil)

(defun nop--before-read-mode (max-depth)

  (nop--ensure-face-attributes)

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
  (let ((new-nodes (nop--get-nearest-visible-node-pair nop--last-point)))
    (setf nop--active-primary (car new-nodes))
    (setf nop--active-focused (cdr new-nodes)))
  (add-hook 'post-command-hook #'nop--read-post-command 0 t)

  (when collapsed (recenter))

  (setf nop--header-line-format-copy header-line-format)

  (setq header-line-format
        '(:eval (list (nop--get-description nop--active-primary)))))


(defun nop--read-disable ()
  (setf header-line-format nop--header-line-format-copy)
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

            ;; TODO : Tab should cycle between visibility levels.
            ;; Ensure tab toggles visibility regardless of key translation.
            (define-key map (kbd "TAB") #'nop-nav-toggle-node-visibility)
            (define-key map (kbd "<tab>") #'nop-nav-toggle-node-visibility)

            ;; (define-key map (kbd "<up>") #'nop-nav-step-backward-context)
            ;; (define-key map (kbd "<down>") #'nop-nav-step-forward-context)
            ;; (define-key map (kbd "<right>") #'nop-nav-incremental-expand)
            ;; (define-key map (kbd "<left>") #'nop-nav-incremental-collapse)

            (define-key map (kbd "<C-up>") #'nop-nav-step-backward-focused)
            (define-key map (kbd "<C-down>") #'nop-nav-step-forward-focused)
            (define-key map (kbd "<C-right>") #'nop-nav-expand-subtree-focused)
            (define-key map (kbd "<C-left>") #'nop-nav-collapse-subtree-focused)

            (define-key map (kbd "<M-up>") #'nop-nav-step-backward-primary)
            (define-key map (kbd "<M-down>") #'nop-nav-step-forward-primary)
            (define-key map (kbd "<M-right>") #'nop-nav-expand-subtree-primary)
            (define-key map (kbd "<M-left>") #'nop-nav-collapse-subtree-primary)

            (define-key map (kbd "<M-C-up>") #'nop-nav-step-backward-shallow)
            (define-key map (kbd "<M-C-down>") #'nop-nav-step-forward-shallow)
            (define-key map (kbd "<M-C-right>") #'nop-nav-global-expand)
            (define-key map (kbd "<M-C-left>") #'nop-nav-global-collapse)

            (define-key map (kbd "<") #'nop-nav-buffer-begin)
            (define-key map (kbd ">") #'nop-nav-buffer-end)

            (define-key map (kbd "<mouse-1>") #'nop-nav-toggle-node-visibility)

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
