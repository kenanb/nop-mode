;;; nop-base.el --- Base functionality for nop-mode.

;; Copyright (C) 2021 Kenan Bölükbaşı

;; Author: Kenan Bölükbaşı <kenanbolukbasi@gmail.com>
;; Keywords: convenience, languages, outlines, tools
;; Homepage: https://github.com/kenanb/nop-mode

;;; Code:

;;; Dependencies [#.]

(require 'cl-lib)
(require 'cl-extra)
(require 'eieio)
(require 'wid-edit)
(require 'widget)
(require 'cus-edit)
(require 'seq)
(require 'info)
(require 'pp)
(require 'rx)

(eval-when-compile (require 'subr-x))

;;; Group [#3H]
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

;;; Utilities [#0.]
;;
;;


(cl-defgeneric nop--debug (instance)
  (:documentation "Format a readable representation of object in message area."))

;;; Face Utilities [#1F]
;;
;;

(defconst nop--overlay-max-depth 9)

(defun nop--calculate-color-info (&optional offset face)
  (let* ((scale (truncate (* (+ .75 nop--scale-coefficient)
                             (1+ nop--overlay-max-depth))))
         (bg (color-values (face-attribute (or face 'default) :background)))
         (fg (color-values (face-attribute (or face 'default) :foreground)))
         (change (cl-mapcar (lambda (x y) (/ (- x y) scale)) fg bg))
         (magnitude (cl-loop with m = 0 with m-abs = 0
                             for c in change for c-abs = (abs c)
                             if (> c-abs m-abs) do (setf m c m-abs c-abs)
                             finally return m))
         (uniform-base-offset (cond
                               ((null offset) '(0 0 0))
                               ((listp offset) offset)
                               (t (list offset offset offset))))
         (absolute-base-offset (if (cl-minusp magnitude)
                                   uniform-base-offset
                                 (mapcar #'- uniform-base-offset))))
    (vector (cl-mapcar #'+ bg absolute-base-offset)
            (cl-mapcar #'+ fg absolute-base-offset)
            change)))

(defun nop--gen-color (components)
  (apply #'format "#%04X%04X%04X" components))

(defun nop--color-component (idx a v init)
  (format "%04X" (+ init (truncate (* (+ v (* .5 a idx)) idx)))))

(defun nop--color (index acceleration component-velocities component-offsets)
  (format "#%s" (cl-loop for v in component-velocities and o in component-offsets
                         concat (nop--color-component index acceleration v o))))

(defun nop--gen-faces (name-prefix inherit doc-fmt &rest specs)
  (cl-loop for depth to nop--overlay-max-depth
           collect
           (custom-declare-face
            (intern (concat name-prefix (number-to-string depth)))
            `((t :inherit ,inherit ,@specs))
            (format doc-fmt depth))))

(defun nop--set-faces (name-prefix fn)
  (cl-loop for depth to nop--overlay-max-depth
           do
           (apply #'set-face-attribute
                  (intern (concat name-prefix (number-to-string depth))) nil
                  (funcall fn depth))))


;;; Positions and Ranges [#1]
;;
;;
;;                 decor  comment
;;       content   /     /     directive
;;            /   /     /     /     spec        delimiter     path
;;   begin   /   /     /     /     /                     \     \    suffix   end
;;  /       /   /     /     /     /    info               \     \        |  /
;; .       .   .     .     .     .     .                   .     .       . .
;; |       |   |     |     |     |     |                   |     |       | |
;; |       |/|/| ... | ... | |[|#|4|F| |I|n|f|o| |s|t|r|.| |#|3| |p|a|t|h|]|EOL
;; |       |   |     |     |     |   | |                 |   | | |       | |
;; '       '   '     '     '     '   ' '                 '   ' ' '       ' '
;;  \_____/ \_/       \___/ \___/ \_/   \_______________/     V   \_____/ V
;;      |    |          |     |    |             |            |      |    |
;;      | cprefix       |     |   dspec       intinfo       ispec    | dsuffix
;;      |            extinfo  |                                  pathinfo
;; indent                  dprefix
;;
;;                                \_______________ inner _______________/
;;
;;                            \___________________ outer _________________/
;;
;;              \_______________________________ comment _________________/
;;
;;          \___________________________________ content _________________/
;;
;;  \______________________________________________ line _________________/

(defconst nop--dprefix-string " [#")
(defconst nop--dsuffix-char ?\])
(defconst nop--delimiter-char ?\#)
(defconst nop--dsuffix-length 1)

(defun nop--regexp-for-lang (cprefix decorations)
  (rx-to-string
   `(seq line-start

         ;; code ( Not allowed for tree nodes )
         (group-n 1 (*? any))

         ;; indent
         (* blank)

         ;; cprefix
         (group-n 2 ,cprefix)

         ;; decor
         (* (any blank ,@decorations))

         ;; extinfo
         (group-n 3 (* any))

         ;; dprefix
         ,nop--dprefix-string

         ;; dspec
         (group-n 4 (* (any alnum "{}<>?@." ?+ ?-)))

         (? ?\s
            ;; inner info
            (? (group-n 5 (* any))
               ?\s))

         (? ,nop--delimiter-char
            ;; ispec
            (group-n 6 (* digit))

            (? ?\s
               ;; identifier
               (group-n 7 (* (any alnum ?- ?_)))))

         ;; line-end
         ,nop--dsuffix-char line-end) t))

(defconst nop--expansion-default 1)

;; Position [#+C]

(defclass nop--positions ()
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

;; Range [#C]

;; Position (between two characters) range.
;; Represents text range: [x,y)

(defclass nop--range ()
  ((begin :initarg :begin :initform 0 :type fixnum)
   (end :initarg :end :initform 0 :type fixnum)))

(defun nop--range-length (range)
  (with-slots (begin end) range
    (- end begin)))

(defun nop--apply-range (range fn)
  (with-slots (begin end) range
    (funcall fn begin end)))

(defun nop--debug-range (range)
  (with-slots (begin end) range
    (if (or (zerop begin) (zerop end))
        (format "[ %d, %d )" begin end)
      (format "[ %d(%c), %d(%c) )"
              begin (char-after begin)
              end (char-after end)))))

(defun nop--generate-overlay (r p)
  (let ((ov (nop--apply-range r #'make-overlay)))

    ;; We cache  the end  of overlays, so  we can temporarily  reduce them  to 0
    ;; size, when needed.   Alternatively, we can try priority of  0 with cached
    ;; priority.  The difference is, if there are any properties not provided by
    ;; parent overlay, this will still leak into view.
    (overlay-put ov 'cached-start (oref r begin))
    (overlay-put ov 'cached-end (oref r end))

    (dolist (kv p)
      (apply #'overlay-put ov kv))
    ov))

;; Range Generators [#M]

(cl-defmacro define-nop--range-generator (fn-sym
                                          bgn-key
                                          end-key
                                          &key
                                          bgn-offset
                                          end-offset)
  (let* ((pos (gensym)))
    `(defun ,fn-sym (,pos)
       (with-slots (,bgn-key ,end-key) ,pos
         (nop--range :begin ,(if bgn-offset `(+ ,bgn-key ,bgn-offset) bgn-key)
                     :end   ,(if end-offset `(+ ,end-key ,end-offset) end-key))))))

(define-nop--range-generator nop--indent-r begin content)
(define-nop--range-generator nop--cprefix-r content comment)
(define-nop--range-generator nop--extinfo-r comment directive)
(define-nop--range-generator nop--dprefix-r directive spec)
(define-nop--range-generator nop--dspec-r spec info)
(define-nop--range-generator nop--intinfo-r info suffix)
(define-nop--range-generator nop--dsuffix-r suffix end)

(define-nop--range-generator nop--line-r begin end)
(define-nop--range-generator nop--content-r content end)
(define-nop--range-generator nop--comment-r comment end)
(define-nop--range-generator nop--outer-r spec end :bgn-offset -2)
(define-nop--range-generator nop--inner-r spec suffix)

(defun nop--info-r (positions)
  (let ((intinfo (nop--intinfo-r positions)))
    (if (zerop (nop--range-length intinfo))
        (nop--extinfo-r positions)
      intinfo)))

(defun nop--inner-string (positions)
  (nop--apply-range (nop--inner-r positions)
                    #'buffer-substring-no-properties))

(defun nop--comment-string (positions)
  (nop--apply-range (nop--comment-r positions)
                    #'buffer-substring-no-properties))

(defun nop--info-string (positions)
  (string-trim (nop--apply-range (nop--info-r positions)
                                 #'buffer-substring-no-properties)
               "[ \t\n\r/]+"
               "[ \t\n\r]+"))

(cl-defmethod nop--debug ((positions nop--positions))
  (format "< Positions: D: %s. I: %s >"
          (nop--debug-range (nop--dspec-r positions))
          (nop--debug-range (nop--info-r positions))))

;;; Directive [#1C]
;;
;;

(defclass nop--directive ()

  ((path
    :initform nil
    :type (or string null)
    :documentation "
The path associated with the directive.")

   (positions
    :initarg :positions
    :initform (make-instance 'nop--positions)
    :type nop--positions)

   (expansion :initform 0
              :type fixnum
              :documentation "
Number of levels that should be expanded on jump at target location."))

  :documentation
  "Represents a parsed nop directive entry."

  :abstract t)


(defclass nop--label-directive (nop--directive)

  ()

  :documentation
  "Represents a widget that can be jump target."

  :abstract t)

(defclass nop--bookmark-directive (nop--label-directive)

  ()

  :documentation
  "A label that's minimally visible in read mode. The calculated expansion applies to the
tree node the bookmark is a child of.")


(defclass nop--tree-directive (nop--label-directive)

  ((depth
    :initarg :depth
    :initform '(:abs . 0)
    :type (or fixnum cons)
    :documentation "
The nesting level of the tree directive.")

   (kind
    :initarg :kind
    :initform :default
    :type keyword
    :documentation "
The role and semantics of directive.")

   (children
    :initform nil
    :type list
    :documentation "
The nesting level of the tree directive.")

   (continuations
    :initform nil
    :type list
    :documentation "
The nesting level of the tree directive.")

   (next-node
    :initform nil
    :type (or nop--tree-directive null)
    :documentation "
The tree directive that marks the end of the immediate contents of this.")

   (prev-node
    :initform nil
    :type (or nop--tree-directive null)
    :documentation "
The tree directive of which the immediate contents are terminated by this.")

   (arbitrary
    :initform nil
    :type list
    :documentation "
Arbitrary attributes associated with the directive."))

  :documentation
  "A label that represents a foldable entity. The calculated expansion applies directly to the node.")


(defclass nop--anchor-directive (nop--directive)

  ()

  :documentation
  "Represents a widget that allows jumping to a label widget with the specified path.
Expansion specified in anchor overrides expansion specified in label.")

;;; Debug Printing [#+5F]
;;
;;

(cl-defmethod nop--debug ((d nop--directive))
  (format "< %-15s: %s\n%s\n>" "Directive" (type-of d)
          (mapconcat (lambda (x) (format "  %s" x))
                     (reverse (nop--debug-directive d))
                     "\n")))


(defun nop--fmt-slot (name value)
  (format "%-15s: %s" name value))


(cl-defgeneric nop--debug-directive (directive)

  (:documentation "List a description string for each slot.")

  (:method ((d nop--directive))
           (list (nop--fmt-slot "identity"
                                (sxhash-eq d))
                 (nop--fmt-slot "description"
                                (nop--get-description d))
                 (nop--fmt-slot "path"
                                (oref d path))
                 (nop--fmt-slot "expansion"
                                (oref d expansion))
                 (nop--fmt-slot "positions"
                                (nop--debug (oref d positions)))))

  (:method ((d nop--tree-directive))
           (apply #'list
                  (nop--fmt-slot "cont"
                                 (length (oref d continuations)))
                  (nop--fmt-slot "children"
                                 (length (oref d children)))
                  (nop--fmt-slot "depth"
                                 (oref d depth))
                  (nop--fmt-slot "kind"
                                 (oref d kind))
                  (cl-call-next-method))))

;;; Directive Parsing [#1F]
;;
;;

(cl-defgeneric nop--get-description (directive)

  (:documentation "Get a description for directive, for debug printing.")

  (:method ((d nop--tree-directive))
           (if (eq :default (oref d kind))
               (format "%s" (buffer-name))
             (nop--info-string (oref d positions))))

  (:method ((d nop--bookmark-directive))
           (format "Bookmark[ %s ]" (oref d path)))

  (:method ((d nop--anchor-directive))
           (cl-call-next-method)
           (format "Anchor[ %s ]" (oref d path))))


(cl-defgeneric nop--parse-directive (directive)

  (:documentation "Parse the info section of given string.")

  (:method ((d nop--directive))
           (with-slots (positions) d
             ;; (lwarn 'nop :debug "Parsed directive: %s" (nop--inner-string positions))
             ;; TODO : Decide path and expansion!
             (oset d path "")
             (oset d expansion 0)))

  (:method :after
           ((d nop--bookmark-directive))
           (unless (oref d path)
             (lwarn 'nop :debug "Bookmark ( at %s ) is unreachable due to missing path."
                    (oref (oref d positions) directive))))

  (error "Called nop--parse-directive with non-directive instance of type %s. Object is: %s"
         (type-of directive)
         directive))

(defun nop--lex-option (option)
  (cl-case option

    ;; Type specifiers.
    (?@ '(type . nop--anchor-directive))
    (?! '(type . nop--bookmark-directive))

    ;; Kind specifiers for tree directives.
    (?. '(kind . :continuation))
    (?> '(kind . :link))

    (?N '(kind . :note))
    (?T '(kind . :todo))
    (?K '(kind . :kludge))

    (?U '(kind . :unit-test))

    (?H '(kind . :header))
    (?P '(kind . :preprocessor))

    (?D '(kind . :declaration))
    (?F '(kind . :function))
    (?M '(kind . :macro))
    (?C '(kind . :class))

    (?B '(kind . :block))
    (?I '(kind . :iteration))
    (?R '(kind . :recursion))
    (?S '(kind . :selection))
    (?? '(kind . :condition))
    (?{ '(kind . :scope-init))
    (?} '(kind . :scope-exit))

    (?A '(kind . :assertion))
    (?L '(kind . :logging))
    (?E '(kind . :exception))
    (?V '(kind . :validation))
    (?G '(kind . :guard-clause))

    ;; Depth modifiers for tree directives.
    (?+ '(mod . :inc))
    (?- '(mod . :dec))

    ;; Depth specifiers for tree directives.
    (t (let ((depth (get-char-code-property option 'decimal-digit-value)))
         (if (numberp depth) (cons 'depth depth) nil)))))

(defun nop--generate-directive (positions)
  (let* ((dspec-r (nop--dspec-r positions))
         (tokens (cl-loop for i from (oref dspec-r begin) below (oref dspec-r end)
                          collect (nop--lex-option (char-after i))))
         (type (alist-get 'type tokens 'nop--tree-directive))
         (specified-depth (alist-get 'depth tokens))
         ;; If no depth or mod specified, copy.
         (mod (alist-get 'mod tokens (if specified-depth :abs :cpy)))
         (directive
          (apply #'make-instance type
                 :positions positions
                 (cl-case type
                   (nop--tree-directive (list :kind (alist-get 'kind tokens :none)
                                              ;; If no depth specified, default to "modify-by-1".
                                              :depth (cons mod (or specified-depth 1))))
                   (t nil)))))
    ;; (lwarn 'nop :debug "Positions for generated directive: %s" (nop--debug positions))
    (nop--parse-directive directive)
    directive))

;;; Language Specific Parser [#1C]
;;
;;

(defvar-local nop--parser nil
  "Active parser for the buffer.")

(defvar nop-major-mode-alist nil
  "Parser mappings for supported major modes.")

(defclass nop--directive-parser ()
  ((matches
    :type list
    :initform (make-list 16 nil)
    :documentation
    "Match data to reuse during parse.")
   (lookup-regexp
    :allocation :class
    :type string
    :documentation
    "Expression used for initial beginning-of-comment lookup.
This must match both single line and block comment types for the language.")
   (comment-prefix
    :allocation :class
    :type string
    :documentation
    "Single line comment prefix for language.
The actual NOP directives are only searched in single line comments.")
   (decor-regexp-set
    :initarg :decoration
    :type list
    :documentation
    "List of characters used to match possible decorative prefix from the beginning of directive info.")
   (directive-regexp
    :allocation :class
    :type (or string null)
    :initform nil
    :documentation
    "Automatically calculated during parser instantiation.")))

(cl-defmethod initialize-instance :after
  ((this nop--directive-parser) &optional slots)
  (unless (oref this directive-regexp)
    (oset this directive-regexp
          (nop--regexp-for-lang (oref this comment-prefix)
                                (oref this decor-regexp-set)))))

(cl-defgeneric nop--skip-over-block-comment (parser)
  (:documentation "Checks if immediately preceeding comment match indicates a block comment.
In case of block comment, leaves point immediately past the end of block comment, and returns T.
Otherwise, leaves point at the current location, and returns NIL.")

  nil)

(cl-defun nop--generate-directive-positions (parser &aux (comment-pos (point)))
  "Locates a possible nop directive in comment, leaving cursor at the end of comment.
Assumes cursor is looking at comment-position."
  (if (nop--skip-over-block-comment parser) :unsupported-comment-style

    (with-slots (matches) parser
      ;; Start from end and possibly prepend comment outside the field as description.
      (end-of-line)
      (save-excursion
        (cond
         ((not (eq (preceding-char) nop--dsuffix-char)) :no-directive-candidate)
         ((not (search-backward nop--dprefix-string comment-pos t)) :no-directive)
         (t (beginning-of-line)
            (if (not (looking-at (oref parser directive-regexp))) :invalid-directive
              (match-data nil matches)
              (nop--positions
               :begin (marker-position (elt matches 0))
               :content (marker-position (elt matches 4))
               :comment (marker-position (elt matches 6))
               :directive (marker-position (elt matches 7))
               :spec (marker-position (elt matches 8))
               :info (marker-position (or (elt matches 10) (elt matches 9)))
               :suffix (- (marker-position (elt matches 1)) nop--dsuffix-length)
               :end (marker-position (elt matches 1))))))))))

(defconst directive-search-messages
  (list :unsupported-comment-style "Currently, only one-line comment syntax is supported."
        :no-directive-candidate "No directive in comment: No dsuffix delimiter."
        :no-directive "No directive in comment: No dprefix delimiter."
        :invalid-directive "No directive in comment: Invalid syntax."))

(defun nop--search-directive-in-comment (parser)
  "Possibly generates a directive using the comment at current position.
Leaves cursor at the end of comment. Assumes cursor is looking at comment-position."
  (pcase (nop--generate-directive-positions parser)
    ((and (pred keywordp) kw)
     ;; (lwarn 'nop :debug (plist-get directive-search-messages kw))
     nil)
    (positions (nop--generate-directive positions))))

;;; Directive Merge [#1F]
;;
;;

(defun nop--merge-continuation (source directives)
  "Returns either a keyword describing a condition, or the result of nop--merge-new-source.
If the list has exhausted, continuation is invalid."
  (if (not directives) :exhausted
    (let* ((target (car directives))
           (skip (or (not (nop--tree-directive-p target))
                     (member (oref target kind) '(:ignore :merged))
                     (> (oref target depth) (oref source depth)))))
      (if skip (nop--merge-continuation source (cdr directives))
        (if (< (oref target depth) (oref source depth)) :scope-exit
          (nop--merge-new-source directives))))))

(cl-defun nop--merge-new-source (directives &aux (source (car directives)))
  "Assumes SOURCE is a tree directive. Returns list of continuations, or nil if there was an error."
  (with-slots (kind depth) source
    (cl-case kind
      (:continuation
       (setf kind :ignore)
       (pcase (nop--merge-continuation source (cdr directives))
         (:scope-exit
          (lwarn 'nop :debug
                 "%s No primary node at depth %s to merge continuation prefixed '%s'."
                 "Invalid continuation directive found."
                 depth (nop--get-description source))
          nil)
         (:exhausted
          (lwarn 'nop :debug
                 "%s Exhausted all nodes while trying to merge continuation prefixed '%s'."
                 "Invalid continuation directive found."
                 (nop--get-description source))
          nil)
         ((pred null) nil)
         ;; Mark current entry as merged, only if the nested lookup succeeded.
         (result (setf kind :merged)
                 (cons source result))))
      (t (list source)))))

(cl-defun nop--process-continuations (directives &aux (source (car directives)))
  "Possibly registers a series of continuations to a primary node."

  (if-let ((clist (and
                   ;; Only process tree directives.
                   (nop--tree-directive-p source)
                   ;; Skip (possibly already populated) primary node.
                   (eq (oref source kind) :continuation)
                   ;; Returned continuation list is ordered bottom-up.
                   (reverse (nop--merge-new-source directives)))))
      (oset (car clist) continuations (cdr clist))))

(defun nop--queue-collect (directive queued)
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

(defmacro nop--queue-push (directive place)
  `(setf ,place (nop--queue-collect ,directive ,place)))

(defun nop--set-arbitrary (d key val)
  (oset d arbitrary (plist-put (oref d arbitrary) key val)))

(defun nop--get-arbitrary (d key)
  (plist-get (oref d arbitrary) key))

(defun nop--merge (directives)
  ;; DIRECTIVES is in reverse of buffer order.
  (cl-loop with max-depth = 0 and leaves and queue and next
           for dl on directives
           for d = (car dl)
           if (nop--tree-directive-p d) do

           ;; Register continuations under primary nodes.
           (nop--process-continuations dl)

           ;; :IGNORE isn't considered for children collection at all.
           (unless (eq (oref d kind) :ignore)
             (when next (oset next prev-node d))
             (cl-shiftf (oref d next-node) next d)
             (nop--queue-push d queue)
             (oset d children (nconc leaves (oref d children)))
             (setf leaves nil)
             (if (or (cl-plusp (oref d depth)) (eq (oref d kind) :merged))
                 (setf max-depth (max (oref d depth) max-depth))
               (nop--set-arbitrary d :max-depth max-depth)
               (setf max-depth 0)))

           else do (push d leaves)

           ;; Assumes at this point we have a single depth left, that collected everything: DEFAULT
           finally return (car queue)))

;;; Depth Propagation [#1F]
;;
;;

(defun nop--propagate-tree-directive-depths (directives)
  ;; DIRECTIVES is reverse of buffer order.
  (let ((base 0))
    (dolist (d (reverse directives))
      (when (nop--tree-directive-p d)
        (with-slots (depth) d
          (setf base (cl-case (car depth)
                       (:abs (cdr depth))
                       (:cpy base)
                       (:inc (+ base (cdr depth)))
                       (:dec (- base (cdr depth))))
                depth base))))))

;;; Buffer Processing [#1F]
;;
;;

(cl-defun nop--call-for-each-node (fn d &optional depth-list args)
  (apply fn d depth-list args)
  (when (nop--tree-directive-p d)
    (cl-loop for c in-ref (oref d children)
             do (nop--call-for-each-node
                 fn c (cons (oref d depth) depth-list) args))
    (cl-loop for c in-ref (oref d continuations)
             do (nop--call-for-each-node fn c depth-list args))))

(defun nop--parse-buffer ()
  "Processes the whole buffer, and creates the initial list of blocks."
  (save-excursion
    (goto-char (point-min))
    (let* ((default (make-instance 'nop--tree-directive))
           (directives (list default))
           ;; Match data to restore after parse.
           (existing-match-data (match-data)))

      (if nop--parser

          (unwind-protect
              ;; Buffer analysis pass: Generates directives.
              (while (re-search-forward (oref nop--parser lookup-regexp) nil t)
                (when-let ((d (nop--search-directive-in-comment nop--parser)))
                  ;; Populated list is reverse of buffer order.
                  (push d directives)))

            ;; Restore match-data.
            (set-match-data existing-match-data)

            ;; Reseat parser markers.
            (match-data nil (oref nop--parser matches) t))

        (lwarn 'nop :warning
               "Parse failed: No registered NOP parser for major mode '%s'."
               major-mode))

      (nop--propagate-tree-directive-depths directives)

      (nop--merge directives))))

;;; Nop Provide [#0.]
;;
;;

(provide 'nop-base)

;;; nop-base.el ends here
