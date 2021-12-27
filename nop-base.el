;;; nop-base.el --- Base functionality for nop-mode.

;; Copyright (C) 2021 Kenan Bölükbaşı

;; Author: Kenan Bölükbaşı <kenanbolukbasi@gmail.com>
;; Keywords: convenience, languages, outlines, tools
;; Homepage: https://github.com/kenanb/nop-mode

;;; Code:

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

  ((kind :initform :default
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


;;;;;;;;;;;;;;;;;;;;;;;;
;;; Debug Printing
;;
;;

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


;;;;;;;;;;;;;;;;;;;;;;;;
;;; Directive Parsing
;;
;;

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
;;; Directive Merge
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

           ;; Assumes at this point we have a single depth left, that collected everything: DEFAULT
           ;; finally (message "%s" directives)
           finally return (car queue)))


;;;;;;;;;;;;;;;;;;;;;;;;
;;; Depth Propagation
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

;;
;;
;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;
;;; Nop Provide
;;
;;

(provide 'nop-base)

;;; nop-base.el ends here