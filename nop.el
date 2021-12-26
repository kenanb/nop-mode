;;; nop.el --- Nop: Narrative Oriented Programming extensions for Emacs.

;; Copyright (C) 2021 Kenan Bölükbaşı

;; Author: Kenan Bölükbaşı <kenanbolukbasi@gmail.com>
;; Created: 31 October 2021
;; Version: 0.0.1
;; Keywords: narrative, code, literate programming, development, tools
;; Homepage: http://kenanb.com


;; Once the  whole code  is annotated  with NOP directives,  we can  write short
;; navigation  macros  to "guide  reader  through  the  code, to  communicate  a
;; specific aspect".  So it turns the code into a presentation. This can also be
;; used to make remote communication over code easier.

;;;;;;;;;;;;;;;;
;;; Dependencies
;;
;;

(require 'cl-lib)
(require 'cl-extra)
(require 'eieio)
(require 'wid-edit)
(require 'widget)
(require 'cus-edit)
(require 'seq)
(require 'info)
(require 'pp)

(eval-when-compile (require 'subr-x))


;;;;;;;;;
;;; Group
;;
;;

;;;###autoload
(defgroup nop nil
  "Narrative Oriented Programming extensions for Emacs."
  :tag "Nop"
  :prefix "nop-"
  :group 'extensions
  :group 'convenience
  :link '(url-link :tag "Website" ...)
  :link '(url-link :tag "Libraries by Kenan Bölükbaşı" ...)
  :link '(url-link :tag "Download" ...)
  :link '(url-link :tag "Description" ...)
  :link '(url-link :tag "Send Bug Report" ...))

;;;###autoload
(defun nop-customize ()
  "Customize nop group."
  (interactive)
  (customize-group 'nop))


;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utilities
;;
;;


(cl-defgeneric nop-debug (instance)
  (:documentation "Format a readable representation of object in message area."))


;;;;;;;;;;;;;;;;;;;;;;;;
;;; Range
;;
;;
;; Position (between two characters) range.
;; Represents text range: [x,y)

(defclass nop-range ()
  ((begin :initarg :begin :initform 0 :type fixnum)
   (end :initarg :end :initform 0 :type fixnum)))

(defun nop-range-length (range)
  (with-slots (begin end) range
    (- end begin)))

(defun nop-apply-range (range fn)
  (with-slots (begin end) range
    (funcall fn begin end)))

(defun nop-debug-range (range)
  (with-slots (begin end) range
    (if (or (zerop begin) (zerop end))
        (format "[ %d, %d )" begin end)
      (format "[ %d(%c), %d(%c) )"
              begin (char-after begin)
              end (char-after end)))))

(defun nop-generate-overlay (r p)
  (let ((ov (nop-apply-range r #'make-overlay)))

    ;; We cache  the end  of overlays, so  we can temporarily  reduce them  to 0
    ;; size, when needed.   Alternatively, we can try priority of  0 with cached
    ;; priority.  The difference is, if there are any properties not provided by
    ;; parent overlay, this will still leak into view.
    (overlay-put ov 'cached-start (oref r begin))
    (overlay-put ov 'cached-end (oref r end))

    (dolist (kv p)
      (apply #'overlay-put ov kv))
    ov))


;;;;;;;;;;;;;;;;;;;;;;;;
;;; Positions and Ranges
;;
;;
;;                  comment
;;       content   /     directive
;;            /   /     /     spec
;;   begin   /   /     /     /                                       end
;;  /       /   /     /     /  info                         suffix  /
;; .       .   .     .     .   .                                 . .
;; |       |   |     |     |   |                                 | |
;; | | | | |/|/| ... | |[|!|4|.| |N|a|r|r|a|t|i|v|e| |t|e|x|t|.| |]|EOL
;; |       |   |     |     |   |                                 | |
;; '       '   '     '     '   '                                 ' '
;;  \_____/ \_/ \___/ \___/ \_/ \_______________________________/ V
;;      |    |    |     |    |                 |                  |
;;      | cprefix |     |  dspec            intinfo            dsuffix
;;      |      extinfo  |
;; indent            dprefix
;;
;;                          \_______________ inner _____________/
;;
;;                      \___________________ outer _______________/
;;
;;              \_________________________ comment _______________/
;;
;;          \_____________________________ content _______________/
;;
;;  \________________________________________ line _______________/

(defconst +nop-dprefix-string+  " [!")
(defconst +nop-dsuffix-char+ ?\])

(defconst +nop-cprefix-length+  2) ; "//"
(defconst +nop-dspec-length+ 2) ; Example: "3C"
(defconst +nop-dprefix-length+ (length +nop-dprefix-string+))
(defconst +nop-dsuffix-length+ 1) ; single char

(defconst +nop-jump-expansion-default+ 1)

(defclass nop-positions ()
  ((begin     :initarg :begin
              :initform 0
              :type fixnum)
   (content   :initarg :content
              :initform 0
              :type fixnum)
   (comment   :initarg :comment
              :initform 0
              :type fixnum)
   (directive :initarg :directive
              :initform 0
              :type fixnum)
   (spec      :initarg :spec
              :initform 0
              :type fixnum)
   (info      :initarg :info
              :initform 0
              :type fixnum)
   (suffix    :initarg :suffix
              :initform 0
              :type fixnum)
   (end       :initarg :end
              :initform 0
              :type fixnum))
  "Represents the change positions.")

(defun nop-make-positions (begin comment directive end)
  "Assumes cursor is looking at directive.
This call only assumes that a valid directive identifier is found.
It can still decide that the contents are invalid, and return nil."
  (nop-positions :begin begin
                 ;; Comment relative
                 :content (- comment +nop-cprefix-length+)
                 :comment comment
                 ;; Directive relative
                 :directive directive
                 :spec (+ directive +nop-dprefix-length+)
                 :info (+ directive +nop-dprefix-length+ +nop-dspec-length+)
                 ;; End-relative
                 :suffix (- end +nop-dsuffix-length+)
                 :end end))

(cl-defmacro nop-def-range-generator (range-name
                                      bgn-key
                                      end-key
                                      &key
                                      bgn-offset
                                      end-offset)
  (let* ((fn-sym (intern (concat "nop-" (symbol-name range-name) "-r")))
         (pos (gensym)))
    `(defun ,fn-sym (,pos)
       (with-slots (,bgn-key ,end-key) ,pos
         (nop-range :begin ,(if bgn-offset `(+ ,bgn-key ,bgn-offset) bgn-key)
                    :end   ,(if end-offset `(+ ,end-key ,end-offset) end-key))))))


(nop-def-range-generator indent begin content)
(nop-def-range-generator cprefix content comment)
(nop-def-range-generator extinfo comment directive)
(nop-def-range-generator dprefix directive spec)
(nop-def-range-generator dspec spec info)
(nop-def-range-generator intinfo info suffix)
(nop-def-range-generator dsuffix suffix end)

(nop-def-range-generator line begin end)
(nop-def-range-generator content content end)
(nop-def-range-generator comment comment end)
(nop-def-range-generator outer spec end :bgn-offset -2)
(nop-def-range-generator inner spec suffix)

(defun nop-info-r (positions)
  (let ((intinfo (nop-intinfo-r positions)))
    (if (zerop (nop-range-length intinfo))
        (nop-extinfo-r positions)
      intinfo)))

(defun nop-type-char (positions)
  (char-after (oref positions spec)))

(defun nop-kind-char (positions)
  (char-after (+ (oref positions spec) 1)))

(defun nop-inner-string (positions)
  (nop-apply-range (nop-inner-r positions)
                   #'buffer-substring-no-properties))

(defun nop-comment-string (positions)
  (nop-apply-range (nop-comment-r positions)
                   #'buffer-substring-no-properties))

(defun nop-info-string (positions)
  (string-trim (nop-apply-range (nop-info-r positions)
                                #'buffer-substring-no-properties)
               "[ \t\n\r/]+"
               "[ \t\n\r]+"))

(cl-defmethod nop-debug ((positions nop-positions))
  (format "< Positions: D: %s. I: %s >"
          (nop-debug-range (nop-dspec-r positions))
          (nop-debug-range (nop-info-r positions))))



;;;;;;;;;;;;;;;;;;;;;;;;
;;; Directive
;;
;;


(defclass nop-directive ()
  ((description :initform "No description"
                :type string)
   (positions :initarg :positions
              :initform (make-instance 'nop-positions)
              :type nop-positions))
  :documentation
  "Represents a parsed nop directive entry.")


(defclass nop-jump-directive (nop-directive)
  ((target :initform "invalid"
           :type string
           :documentation "
Label of the target location to jump.")

   (expansion :initform 0
              :type fixnum
              :documentation "
Number of levels that should be expanded on jump at target location.")))


(defclass nop-kind-directive (nop-directive)

  ((kind :initform :root
         :type keyword
         :documentation "
The role and semantics of directive."))

  :abstract t)


(defclass nop-tree-directive (nop-kind-directive)

  ((depth :initarg :depth
          :initform 0
          :type (or fixnum keyword)
          :documentation "
The nesting level of the tree directive.")

   (children  :initform nil
              :type list
              :documentation "
The nesting level of the tree directive.")

   (continuations  :initform nil
                   :type list
                   :documentation "
The nesting level of the tree directive.")

   (next-node :initform nil
              :type (or nop-tree-directive null)
              :documentation "
The tree directive that marks the end of the immediate contents of this.")

   (prev-node :initform nil
              :type (or nop-tree-directive null)
              :documentation "
The tree directive of which the immediate contents are terminated by this.")

   (handle :initform nil
           :type (or overlay null)
           :documentation "
The canonical overlay representing this directive.")

   (arbitrary :initform nil
              :type list
              :documentation "
Arbitrary attributes associated with the directive.")))


(defclass nop-label-directive (nop-kind-directive)

  ((name :initform "invalid"
         :type string
         :documentation "
The identifier of the label.")))


(cl-defmethod nop-debug ((d nop-directive))
  (format "< %-15s: %s\n%s\n>" "Directive" (type-of d)
          (mapconcat (lambda (x) (format "  %s" x))
                     (reverse (nop-debug-directive d))
                     "\n")))


(defun nop-fmt-slot (name value)
  (format "%-15s: %s" name value))


(cl-defgeneric nop-debug-directive (directive)

  (:documentation "List a description string for each slot.")

  (:method ((d nop-directive))
           (list (nop-fmt-slot "identity"
                               (sxhash-eq d))
                 (nop-fmt-slot "description"
                               (oref d description))
                 (nop-fmt-slot "positions"
                               (nop-debug (oref d positions)))))

  (:method ((d nop-kind-directive))
           (cons (nop-fmt-slot "kind"
                               (oref d kind))
                 (cl-call-next-method)))

  (:method ((d nop-tree-directive))
           (cons (nop-fmt-slot "cont"
                               (oref d continuations))
                 (cons (nop-fmt-slot "children"
                                     (length (oref d children)))
                       (cons (nop-fmt-slot "depth"
                                           (oref d depth))
                             (cl-call-next-method)))))

  (:method ((d nop-label-directive))
           (cons (nop-fmt-slot "name"
                               (oref d name))
                 (cl-call-next-method)))

  (:method ((d nop-jump-directive))
           (cons (nop-fmt-slot "target"
                               (oref d target))
                 (cons (nop-fmt-slot "expansion"
                                     (oref d expansion))
                       (cl-call-next-method)))))


(cl-defgeneric nop-process-extra-info (directive info-string)

  (:documentation "Parse the info section of given string.")

  (:method ((d nop-directive) info-string)
           (oset d description (if (> (length info-string) 10)
                                   (substring info-string 0 10)
                                 info-string)))

  (:method ((d nop-label-directive) info-string)
           (cl-call-next-method)
           (oset d description (format "Label: %s" info-string))
           (oset d name info-string))

  (:method ((d nop-jump-directive) info-string)
           (cl-call-next-method)
           (oset d description (format "Jump: %s" info-string))
           (oset d target info-string)))


(cl-defgeneric nop-parse-directive (directive)

  (:documentation "Parse the info section of given string.")

  (:method ((d nop-directive))
           (with-slots (positions) d
             ;; (message "T: %s" (nop-inner-string positions))
             (nop-process-extra-info d (nop-info-string positions))))

  (:method :after
           ((d nop-kind-directive))
           (oset d kind
                 (cl-case (nop-kind-char (oref d positions))
                   (?. :continue)
                   (?> :link)
                   (?? :conditional)
                   (?I :iteration)
                   (?C :class)
                   (?F :function)
                   (?R :region)
                   (?T :todo)
                   (?B :branch))))

  (:method :after
           ((d nop-jump-directive))
           (oset d expansion
                 (or (get-char-code-property
                      (nop-kind-char (oref d positions))
                      'decimal-digit-value)
                     +nop-jump-expansion-default+)))

  (error "Called nop-parse-directive with non-directive instance of type %s. Object is: %s"
         (type-of directive)
         directive))


;;;;;;;;;;;;;;;;;;;;;;;;
;;; Buffer processing
;;
;;

(defun nop-generate-directive (positions)
  (let* ((c (nop-type-char positions))
         (directive (cl-case c
                      (?> (make-instance 'nop-jump-directive :positions positions))

                      ;; Kind directives
                      (?< (make-instance 'nop-label-directive :positions positions))
                      (?. (make-instance 'nop-tree-directive :positions positions :depth :cpy))
                      (?+ (make-instance 'nop-tree-directive :positions positions :depth :inc))
                      (?- (make-instance 'nop-tree-directive :positions positions :depth :dec))
                      (t (let ((depth (get-char-code-property c 'decimal-digit-value)))
                           (if (numberp depth)
                               (make-instance 'nop-tree-directive :positions positions :depth depth)
                             (make-instance 'nop-directive :positions positions)))))))
    ;; (message (nop-debug positions))
    (nop-parse-directive directive)
    directive))

(cl-defun nop-generate-directive-positions (&aux (comment-pos (point))
                                                 begin-pos
                                                 directive-pos
                                                 end-pos)
  "Locates a possible nop directive in comment, leaving cursor at the end of comment.
Assumes cursor is looking at comment-position."
  (if (eq (preceding-char) ?*)

      ;; Multi-line comment. Unsupported.
      (progn (search-forward "*/") :unsupported-comment-style)

    ;; Single-line comment.
    (setf begin-pos (line-beginning-position))

    ;; Start from end and possibly prepend comment outside the field as description.
    (end-of-line)
    (setf end-pos (point))
    (save-excursion
      (cond
       ((not (eq (preceding-char) +nop-dsuffix-char+)) :no-directive-candidate)
       ((not (search-backward +nop-dprefix-string+ comment-pos t)) :no-directive)
       (t (setf directive-pos (point))
          (nop-make-positions begin-pos comment-pos directive-pos end-pos))))))

(defconst +directive-search-messages+
  (list :unsupported-comment-style "Currently, only one-line comment syntax is supported."
        :no-directive-candidate "No directive in comment: No dsuffix delimiter."
        :no-directive "No directive in comment: No dprefix delimiter."))

(defun nop-search-directive-in-comment ()
  "Possibly generates a directive using the comment at current position.
Leaves cursor at the end of comment. Assumes cursor is looking at comment-position."
  (pcase (nop-generate-directive-positions)
    ((and (pred keywordp) kw) (message (plist-get +directive-search-messages+ kw)) nil)
    (positions (nop-generate-directive positions))))



;;;;;;;;;;;;;;;;;;;;;;;;
;;; Directive merge
;;
;;

(defun nop-merge-continue (source directives)
  "Returns either a keyword describing a condition, or the result of nop-merge-new-source.
If the list has exhausted, continuation is invalid."
  (if (not directives) :exhausted
    (let* ((target (car directives))
           (skip (or (not (nop-tree-directive-p target))
                     (member (oref target kind) '(:ignore :merged))
                     (> (oref target depth) (oref source depth)))))
      (if skip (nop-merge-continue source (cdr directives))
        (if (< (oref target depth) (oref source depth)) :scope-exit
          (nop-merge-new-source directives))))))

(cl-defun nop-merge-new-source (directives &aux (source (car directives)))
  "Assumes SOURCE is a tree directive. Returns list of continuations, or nil if there was an error."
  (with-slots (description kind) source
    (cl-case kind
      (:continue
       (setf kind :ignore)
       (pcase (nop-merge-continue source (cdr directives))
         (:scope-exit (message "ERROR: [INVALID CONTINUATION] No head node at given depth: %s\n"
                               description) nil)
         (:exhausted  (message "ERROR: [INVALID CONTINUATION] Exhausted directives: %s\n"
                               description) nil)
         ((pred null) nil)
         ;; Mark current entry as merged, only if the nested lookup succeeded.
         (result (setf kind :merged)
                 (cons source result))))
      (t (list source)))))

(cl-defun nop-process-continuations (directives &aux (source (car directives)))
  "Possibly registers a series of continuations to a head node."

  (if-let ((clist (and
                   ;; Only process tree directives.
                   (nop-tree-directive-p source)
                   ;; Skip (possibly already populated) head node.
                   (eq (oref source kind) :continue)
                   ;; Returned continuation list is ordered bottom-up.
                   (reverse (nop-merge-new-source directives)))))
      (oset (car clist) continuations (cdr clist))))

(defun nop-queue-collect (directive queued)
  ;; If there are multiple directives in the same slot in queue, they are guaranteed to be
  ;; consecutive in tree, because everything there was possibly in between that was inner is already
  ;; collected, and anything outer must have collected the inner entries in queue.
  ;;
  ;; Both the ordering within the same depth, and across depths is aligned with lexical ordering of
  ;; blocks in buffer.
  (cl-loop for qrest on queued
           for dl = (car qrest)
           for diff = (and dl (- (oref (car dl) depth) (oref directive depth)))

           while (and diff (cl-plusp diff)) nconc dl into x

           finally do
           (oset directive children x)

           ;; :MERGED collects its children, but is not queued to be added as a child.
           (unless (eq (oref directive kind) :merged)
             (if (eq diff 0)
                 (push directive (car qrest))
               (push (list directive) qrest)))
           finally return qrest))

(defmacro nop-queue-push (directive place)
  `(setf ,place (nop-queue-collect ,directive ,place)))

(defun nop-merge (directives)
  ;; DIRECTIVES is in reverse of buffer order.
  (cl-loop with max-depth = 0 and leaves and queue and next
           for dl on directives
           for d = (car dl)
           if (nop-tree-directive-p d) do

           ;; Register continuations under head nodes.
           (nop-process-continuations dl)

           ;; :IGNORE isn't considered for children collection at all.
           (unless (eq (oref d kind) :ignore)
             (when next (oset next prev-node d))
             (cl-shiftf (oref d next-node) next d)
             (nop-queue-push d queue)
             (oset d children (nconc leaves (oref d children)))
             (setf leaves nil)
             (if (or (cl-plusp (oref d depth)) (eq (oref d kind) :merged))
                 (setf max-depth (max (oref d depth) max-depth))
               (oset d arbitrary (plist-put (oref d arbitrary) :max-depth max-depth))
               (setf max-depth 0)))

           else do (push d leaves)

           ;; Assumes at this point we have a single depth left, that collected everything: ROOT
           ;; finally (message "%s" directives)
           finally return (car queue)))



;;;;;;;;;;;;;;;;;;;;;;;;
;;; Depth propagation
;;
;;

(defun nop-propagate-tree-directive-depths (directives)
  ;; DIRECTIVES is reverse of buffer order.
  (let ((base 0))
    (dolist (d (reverse directives))
      (when (nop-tree-directive-p d)
        (with-slots (depth) d
          (setf base (pcase depth
                       ((pred numberp) depth)
                       (:cpy base)
                       (:inc (1+ base))
                       (:dec (1- base)))
                depth base))))))



;;;;;;;;;;;;;;;;;;;;;;;;
;;; Overlay selection
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
                   (list :background (nop-color depth -.5 '(#xA #xB #xC) '(#xFA #xF4 #xEA))))))

(defconst +nop-ov-handle-faces+
  (nop-gen-faces "nop-ov-handle-face-"
                 "Drawer handle line face for depth %s."
                 (lambda (depth)
                   (list :overline "black"
                         :background (nop-color depth .25 '(#xA #xA #x6) '(#xED #xED #xE2))))))

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

(defun nop-nav-jump-forward ()
  (interactive)
  (let* ((overlays (overlays-at (point) t))
         (ov (cl-find-if 'nop-tree-overlay-p overlays))
         (handle (overlay-get ov 'handle))
         (directive (overlay-get handle 'directive))
         (fwd-node (oref (nop-get-last-node-of-subtree directive) next-node))

         (drawer (overlay-get handle 'drawer))
         (collapsed (overlay-get handle 'collapsed)))
    (when fwd-node
      (nop-nav-jump-to-directive fwd-node)))
  (recenter))

(defun nop-find-hovered-node (&optional from-head)
  (let* ((overlays (overlays-at (point) t))
         (ov (cl-find-if 'nop-tree-overlay-p overlays))
         (handle (overlay-get ov 'handle))
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
    (unless (eq :root kind)
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
    (unless (eq :root kind)
      (let ((title (overlay-get handle 'title))
            (ellipsis (overlay-get handle 'ellipsis))
            (drawer (overlay-get handle 'drawer))
            (face (list (elt +nop-ov-shadow-faces+ depth)
                        (elt +nop-ov-drawer-faces+ depth))))

        ;; Jump to the beginning of the directive that will be collapsed.
        (nop-nav-jump-to-directive d)

        ;; Property to keep explicit drawer state.
        (overlay-put handle 'collapsed t)

        ;; A blank line is required to maintain correct margins on collapse.
        (overlay-put ellipsis 'before-string
                     (propertize (format "%s\N{U+22EF}" (make-string depth ?\s)) 'face face))

        (overlay-put ellipsis 'after-string
                     (propertize " " 'face face
                                 'display '(space :align-to (- right-margin 25))))

        ;; Hide drawer
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
  (let* ((overlays (overlays-at (point) t))
         (ov (cl-find-if 'nop-tree-overlay-p overlays))
         (handle (overlay-get ov 'handle))
         (drawer (overlay-get handle 'drawer))
         (directive (overlay-get handle 'directive)))
    (unless (eq :root (oref directive kind))
      (if (overlay-get handle 'collapsed)
          (nop-nav-expand-node directive)
        (nop-nav-collapse-node directive)))
    (message "Overlay   : %s" ov)
    (message "Directive : %s" (oref directive description))))

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

(defconst +nop-box-map+ (define-keymap
                          "<tab>" #'nop-nav-toggle-node-visibility
                          "<mouse-1>" #'nop-nav-toggle-node-visibility
                          "+" #'nop-nav-expand-subtree-from-body
                          "C-+" #'nop-nav-expand-subtree-from-head
                          "-" #'nop-nav-collapse-subtree-from-body
                          "C--" #'nop-nav-collapse-subtree-from-head
                          "q" #'nop-remove-overlays
                          "b" #'beginning-of-buffer
                          "e" #'end-of-buffer
                          "n" #'nop-nav-step-forward-from-body
                          "N" #'nop-nav-step-forward-from-head
                          "p" #'nop-nav-step-backward-from-body
                          "P" #'nop-nav-step-backward-from-head
                          "h" #'nop-nav-home-from-body
                          "H" #'nop-nav-home-from-head
                          "f" #'nop-nav-jump-forward))

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
                          (keymap ,+nop-box-map+)
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
                 (epos (if (oref enode next-node)
                           (oref (oref (oref enode next-node) positions) begin)
                         (buffer-end 1)))
                 (mpos (1+ (oref positions end))) ; newline should be part of handle.
                 (bpos (oref positions begin))
                 (handle (nop-generate-tree-overlay bpos mpos depth prefix-h t))
                 (ellipsis (nop-generate-tree-overlay mpos mpos depth))
                 (drawer (nop-generate-tree-overlay mpos epos depth prefix)))
            (overlay-put drawer 'handle handle)
            (overlay-put handle 'handle handle)
            (overlay-put handle 'title title)
            (overlay-put handle 'ellipsis ellipsis)
            (overlay-put handle 'drawer drawer)
            (overlay-put handle 'directive d)
            (oset d handle handle)))))))

(defun nop-prepare-for-overlay (max-width)
  (let ((margin-width (* (1+ max-depth) +nop-ov-margin-block-width+)))
    ;; (setf left-margin-width margin-width)
    (setf right-margin-width margin-width))
  (set-window-buffer (selected-window) (current-buffer))
  (remove-overlays)
  (read-only-mode 1)
  (fringe-mode 0)
  (setf truncate-lines t))

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
;;; User API
;;
;;

(cl-defun nop-call-for-each-node (d max-depth depth-list fn &optional (vdepth 0))
  (funcall fn d vdepth max-depth depth-list)
  (when (nop-tree-directive-p d)
    (message "Traversing children of %s" (oref d description))
    (cl-loop for c in-ref (oref d children)
             do (nop-call-for-each-node
                 c max-depth (cons (oref d depth) depth-list) fn (1+ vdepth)))
    (message "Traversing continuations of %s" (oref d description))
    (cl-loop for c in-ref (oref d continuations)
             do (nop-call-for-each-node c max-depth depth-list fn vdepth))))

(defun nop-narrate-buffer ()
  (interactive)
  "Processes the whole buffer, and creates the initial list of blocks."
  (save-excursion
    (goto-char (point-min))
    (let* ((root (make-instance 'nop-tree-directive))
           (directives (list root))
           (max-depth 0))

      (oset root description (format "Root: %s" (buffer-name)))

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
                                 +nop-overlay-invisible+)
           (message "Applied fn to %s - %s" vdepth (oref d description))))

        (nop-call-for-each-node
         d max-depth nil
         (lambda (d vdepth max-depth depth-list)
           (when (nop-tree-directive-p d)
             (unless (eq (oref d kind) :merged)
               (nop-nav-collapse-node d))))))))
  (recenter))

;;
;;
;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Nop Minor Mode
;;
;;

;; ;;;###autoload
;; (define-minor-mode nop-mode
;;   "Toggle Nop mode."
;;   ;; The initial value.
;;   :init-value nil
;;   ;; The indicator for the mode line.
;;   :lighter " Nop"
;;   ;; The minor mode bindings.
;;   :keymap (let ((map (make-sparse-keymap)))
;;             (define-key map (kbd "C-c !") 'nop-dummy)
;;             (define-key map [menu-bar nop-menu]
;;               (cons "Nop" (make-sparse-keymap)))
;;             (define-key map [menu-bar nop-menu nop-settings]
;;               '("Settings" . nop-customize))
;;             (define-key-after map [menu-bar nop-menu nop-p-menu]
;;               (cons "Project" (make-sparse-keymap)) 'nop-dummy)
;;             (define-key map [menu-bar nop-menu nop-p-menu autoselect]
;;               '("Autoselect" . nop-dummy))
;;             (define-key-after map [menu-bar nop-menu generate]
;;               '("Generate" . nop-dummy) 'nop-p-menu)
;;             (define-key map [tool-bar separator] '(menu-item "--"))
;;             (define-key-after map [tool-bar nop-button]
;;               (list 'menu-item "Generate Asset" 'nop-dummy
;;                     :image (find-image '((:type xpm :file "etc/nop-icon.xpm")
;;                                          (:type pbm :file "etc/nop-icon.pbm")))
;;                     :help "Generate asset from string at current position of cursor")
;;               'separator)
;;             map)
;;   :group 'nop
;;   ;; (nop-load-libs)
;;   ;; (nop-load-projects)
;;   )

;; ;;;###autoload
;; (define-globalized-minor-mode global-nop-mode
;;   nop-mode 
;;   (lambda () (nop-mode t))
;;   :group 'nop)


;;;;;;;;;;;;;;;;;;;;;
;;; Nop Provide
;;
;;

(provide 'nop)

;;; nop.el ends here
