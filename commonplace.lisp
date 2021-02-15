;;;; commonplace.lisp

(in-package #:commonplace)

(defstruct (span :conc-name) origin start len)
(defstruct name)
(defstruct (doc-name (:include name)) main version)
(defstruct (hash-name (:include name)) hash)
(defstruct (scroll-name (:include name)) scroll)
(defstruct (scratch-name (:include name)))
(defstruct leaf name owner)
(defstruct (content-leaf (:include leaf)) contents)
(defstruct (doc (:include leaf)) type spans links)
(defstruct (link (:include leaf)) type endsets)
(defstruct endset name)
(defstruct (span-endset (:include endset)) spans)
(defstruct (doc-endset (:include endset)) doc-name)
(defstruct (concatalink (:conc-name ccl-)) type endsets)
(defstruct (concatatext-endset (:include endset) (:conc-name cce-)) spans)

(defparameter local-scroll-name+ (make-scroll-name :scroll "local"))
(defparameter scratch-name+ (make-scratch-name))
(defparameter upstream+ "http://localhost:4242/")
(defparameter test-repo+ "~/lisp/commonplace/test-repo")
(defparameter user+ "Me")
(defparameter private-key-file+ "~/.ssh/id_rsa")
(defparameter public-key-file+ "~/.ssh/id_rsa.pub")
(defparameter private-key-passphrase+ nil)
(defparameter default-hash-length+ 6)

(defvar acceptor* nil)
(defvar http-stream*) ; used by the HTTP client to represent an open connection
(defvar repo-path* (sb-posix:getcwd)) ; defaults to current directory

;;; Common functions

(defmacro probe (form)
  (with-unique-names (val)
    `(let ((,val ,form)) (format T ,(format nil "~A: ~~S~%" form) ,val) ,val)))

(defmacro awhen (test &body body)
  `(let ((,(intern "IT") ,test))
     (when ,(intern "IT") ,@body)))

(defmacro aif (a b c)
  `(let ((,(intern "IT") ,a))
     (if ,(intern "IT") ,b ,c)))

(defun lift (list &optional others)
  (cond ((endp list) nil)
	((endp (cdr list)) (cons (car list) (reverse others)))
	(T (lift (cdr list) (cons (car list) others)))))

(defun drop (n list)
  (cond ((zerop n) list)
	((null list) nil)
	(T (drop (1- n) (cdr list)))))

(defmacro recur (args vals &body body)
  `(labels ((recur ,args ,@body))
     (recur ,@vals)))

(defun flatten (list)
  (cond ((endp list) nil)
	((listp (car list)) (append (car list) (flatten (cdr list))))
	(T (cons (car list) (flatten (cdr list))))))

(defun string-starts-with (prefix string)
  (and (>= (length string) (length prefix))
       (string= prefix string :end2 (length prefix))))

(defmacro pushend (x place)
  `(if ,place (setf (cdr (last ,place)) (list ,x)) (setf ,place (list ,x))))

(defun insert-at (x list n)
  (if (zerop n) (cons x list) (cons (car list) (insert-at x (cdr list) (1- n)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-keyword (x)
    (intern (string-upcase (with-output-to-string (s) (princ x s))) (symbol-package :x))))

(defmacro build ((name constructor) &body body)
  `(let ((,name ,constructor)) ,@body ,name))

;;; Span operations

(defun span (origin start length) (make-span :origin origin :start start :len length))
(defun edit-span (span &key (origin (origin span)) (start (start span)) (len (len span)))
  (span origin start len))
(defun next-pos (s) (+ (start s) (len s)))
(defun span-end (s) (1- (next-pos s)))
(defun same-origin (s1 s2) (equalp (origin s1) (origin s2)))
(defun span-diff (fn s1 s2 &optional (offset 0))
  (+ offset (- (funcall fn s1) (funcall fn s2))))

(defun span-contains (span point &optional (adjustment 0))
  (let ((offset (- point (start span) adjustment)))
    (and (>= offset 0) (< offset (len span)))))

(defun overlapping-p (s1 s2 &key (adjust1 0) (adjust2 0))
  "True if the spans contain any shared content. The spans may be position shifted by :adjust1
and :adjust2 if specified."
  (not (or (< (+ (span-end s1) adjust1) (+ (start s2) adjust2))
	   (< (+ (span-end s2) adjust2) (+ (start s1) adjust1)))))

(defun abutting-p (s1 s2)
  "True if span s2 follows span s1 with no gap between them."
  (eq (next-pos s1) (start s2)))

(defun mergeable-p (s1 s2)
  "True if spans s1 and s2 could be merged into a single contiguous span containing the
contents of both."
  (and (same-origin s1 s2) (or (overlapping-p s1 s2) (abutting-p s1 s2))))

(defun duplicating-p (s1 s2)
  "True if spans s1 and s2 share any content (i.e. there is some data contained in both of
them)."
  (and (same-origin s1 s2) (overlapping-p s1 s2)))

(defun merge-spans (s1 s2 &optional only-overlaps)
  "Attempts to merge two spans into one, returning a list of results (either one span or
the two originals). If :only-overlaps is true, they will only be merged if they duplicate some
content, not if they merely abut each other."
  (if (if only-overlaps (duplicating-p s1 s2) (mergeable-p s1 s2))
      (let ((start (min (start s1) (start s2))))
	(list (span (origin s1) start (- (max (next-pos s1) (next-pos s2)) start))))
      (list s1 s2)))

(defun length-sum (spans) (reduce #'+ spans :key #'len))

(define-condition point-out-of-bounds-error (error)
  ((point :initarg :point)
   (spans :initarg :spans))
  (:documentation "Raised whenever a span operation is passed an out of bounds point argument
(i.e. a point value that does not lie within the list of spans that the operation is being
performed on)."))

(defmethod print-object ((object point-out-of-bounds-error) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "P~S SPANS:~S" (slot-value object 'point) (slot-value object 'spans))))

(defun out-of-bounds (point spans)
  "Helper method that raises an point-out-of-bounds error."
  (error 'point-out-of-bounds-error :point point :spans spans))

(defun divide-span-at-point (s1 point)
  "Split a span into two at the given point, returning a list of the resulting spans. If the
given point lies outside the span then the returned list contains only the original span."
  (with-slots (start length) s1
    (if (and (span-contains s1 point) (> point (start s1)))
	(list (edit-span s1 :len (- point start))
	      (edit-span s1 :start (+ start point -1) :len (1+ (- length point))))
	(list s1))))

(defun divide-span (s length)
  "Divide a span into two, with the first being the given length and the second being the
remainder. Results are returned as a list. If there is no remainder then the list contains
only the original span."
  (if (or (zerop length) (>= length (len s)))
      (list s)
      (list (edit-span s :len length)
	    (edit-span s :start (+ (start s) length) :len (- (len s) length)))))

(defun merge-span-lists (list1 list2)
  "Take two lists of spans and concatenate them, merging the spans at the join point into one
if possible. The inverse operation of SPLIT-SPANS-AT."
  (cond
    ((endp list1) list2)
    ((endp list2) list1)
    (T (let ((lifted (lift list1)))
	 (append (cdr lifted) (merge-spans (car lifted) (car list2)) (cdr list2))))))

(defun merge-all (list)
  "Merge all adjacent spans that abut or overlap."
  (if (endp (cdr list)) list
      (let ((merged (merge-spans (car list) (cadr list))))
	(if (cadr merged) (cons (car list) (merge-all (cdr list)))
	    (merge-all (cons (car merged) (cddr list)))))))

(defun deduplicate (list)
  "Merge all adjacent spans that overlap (i.e. duplicate a portion of each other's content).
This differs from MERGE-ALL in that it won't merge spans if they merely abut."
  (if (endp (cdr list)) list
      (let ((merged (merge-spans (car list) (cadr list) T)))
	(if (cadr merged) (cons (car list) (deduplicate (cdr list)))
	    (deduplicate (cons (car merged) (cddr list)))))))

(defun split-spans-at (list point)
  "Divide a list of spans into two list at the given point (with the second list inclusive of
the point itself)."
  (labels
      ((split (s l p collected)
	 (cond ((zerop p) (list (nreverse collected) (if s (cons s l) l)))
	       ((null s) (out-of-bounds point list))
	       ((> (len s) p)
		(let ((split (divide-span s p)))
		  (list (nreverse (cons (car split) collected)) (cons (cadr split) l))))
	       (T (split (car l) (cdr l) (- p (len s)) (cons s collected))))))
    (if (endp list) (out-of-bounds point list))
    (split (car list) (cdr list) point nil)))

(defun split-twice (list start length)
  "Divide a list of span into three, with the central section having given start and length."
  (let ((div (split-spans-at list start)))
    (cons (car div) (split-spans-at (cadr div) length))))

(defun crop (spans start length)
  "Create spans representing the subset of some other spans delimited by start and length."
  (second (split-twice spans start length)))

(defun insert-spans (spans new-spans point)
  "Return a list of spans with some new spans inserted into an old list."
  (let ((div (split-spans-at spans point)))
    (merge-span-lists (car div) (merge-span-lists new-spans (cadr div)))))

(defun delete-spans (spans start length)
  "Construct a new list of spans with a section removed."
  (let ((div (split-twice spans start length)))
    (merge-span-lists (first div) (third div))))

(defun move-spans (spans start length new-pos)
  "Move a section from one place to another."
  (let ((div (split-twice spans start length)))
    (insert-spans (merge-span-lists (first div) (third div)) (second div) new-pos)))

(defun transclude-spans (source-spans start length target-spans insert-point)
  "Transclude content from one set of spans into another."
  (insert-spans target-spans (crop source-spans start length) insert-point))

(defun find-span (spans point)
  "Find the span that encompasses the given point. Returns NIL if not found."
  (recur (spans pos) (spans 0)
    (cond ((endp spans) nil)
	  ((span-contains (car spans) point pos) (values (car spans) pos))
	  (T (recur (cdr spans) (+ pos (len (car spans))))))))

(defun get-concatatext-position (spans point-origin point &optional (pos 0))
  "Finds the first position of some content represented by POINT-ORIGIN and POINT within a
list of spans."
  (with-slots (origin start len) (car spans)
    (cond ((endp spans) nil)
	  ((and (equal origin point-origin) (span-contains (car spans) point))
	   (values (+ pos (- point start)) (car spans)))
	  (T (get-concatatext-position (cdr spans) point-origin point (+ pos len))))))

(defun transform-intersection (s i fn)
  "Breaks s into a list of spans according to which parts overlap with i, calling fn on the
parts that do"
  (if (not (and (same-origin s i) (overlapping-p s i))) (list s)
      (collecting
       (let ((length (span-diff #'start i s)))
	 (if (> length 0) (collect (edit-span s :len length)))
	 (let ((start (max (start s) (start i))))
	   (collect
	       (funcall fn (edit-span s :start start
				      :len (- (min (next-pos s) (next-pos i)) start))
			length))))
       (let ((length (span-diff #'span-end s i)))
	 (if (> length 0) (collect (edit-span s :start (next-pos i) :len length)))))))

(defun append-new-text (doc text)
  "Append new text to the end of a document. The text will be stored on the local scroll."
  (pushend (append-to-local-scroll text) (doc-spans doc)))

;;; Document names

(defun generate-doc-name-string ()
  "Generate a name string for a new document. Currently this is just a random string."
  (let ((name (ironclad:byte-array-to-hex-string
	       (map-into (make-array '(3)) (lambda () (random 256))))))
    (if (probe-file (get-doc-name-path name))
	(generate-doc-name-string)
	name)))

(defun get-doc-name-path (name) (repo-path "names/" name))

(defun new-doc-name (version-name &optional pre-chosen-name)
  "Create a new doc name in the repository and associate it with the given version. The name
is random. Specifying a name with PRE-CHOSEN-NAME is an internal feature intended to support
predictable testing."
  (build (name (or pre-chosen-name (generate-doc-name-string)))
    (with-open-file (s (get-doc-name-path name)
		       :direction :output :if-does-not-exist :create :if-exists :error)
      (princ (hash-name-hash version-name) s))))

(defun update-doc-name (name new-hash)
  "Repoint a doc name to a new doc hash."
  (with-open-file (s (get-doc-name-path name)
		     :direction :output :if-does-not-exist :error :if-exists :overwrite)
      (princ (hash-name-hash new-hash) s)))

(defun resolve-doc-name (name)
  "Resolve a doc name to the hash it names."
  (with-open-file (s (get-doc-name-path name) :if-does-not-exist :error)
    (make-hash-name :hash (read-line s))))

(defun delete-doc-name (name) (delete-file (get-doc-name-path name)))

;;; Leaf names

(defmethod get-path-extensions ((name doc-name))
  (error "Cannot convert doc name to path name: ~S" name))

(defmethod get-path-extensions ((name scroll-name))
  (list "scrolls/" (scroll-name-scroll name)))

(defmethod get-path-extensions ((name scratch-name)) (list "0"))

(defmethod get-path-extensions ((name hash-name))
  (list "public/" (hash-name-hash name)))

(defun parse-doc-name (str)
  (build (name (make-doc-name))
    (let* ((parts (split-sequence #\( str))
	   (version (cadr parts)))
      (setf (doc-name-main name) (car parts))
      (when version
	(if (or (<= (length version) 1) (not (eql #\) (elt version (1- (length version))))))
	    (error "Doc name with version must be of the format 'name(version)', actually '~A'"
		   str))
	(setf (doc-name-version name) (subseq version 0 (1- (length version))))))))

(defmethod serialize-name ((name scroll-name))
  (format nil "scroll/~A" (scroll-name-scroll name)))

(defmethod serialize-name ((name doc-name))
  (if (doc-name-version name)
      (format nil "~A(~A)" (doc-name-main name) (doc-name-version name))
      (doc-name-main name)))

(defmethod serialize-name ((name hash-name)) (hash-name-hash name))

(defmethod serialize-name ((name scratch-name)) "0")

(defun local-scroll-name-p (name) (and (scroll-name-p name)
				       (string= "local" (scroll-name-scroll name))))

(defun set-hash-name (leaf serialized-leaf)
  (setf (leaf-name leaf) (make-hash-name :hash (create-hash serialized-leaf))))

(defun get-next-version-name (old-name)
  (make-doc-name :main (doc-name-main old-name)
		 :version (format nil "~A" (1+ (parse-integer (doc-name-version old-name))))))

;;; Leaf operations

(defmethod leaf-type ((leaf content-leaf)) :content)
(defmethod leaf-type ((leaf link)) :link)
(defmethod leaf-type ((leaf doc)) (doc-type leaf))

(defun iterate-doc (doc on-clip on-link)
  "Iterate over the clips and links in a doc, calling the specified functions on each item."
  (mapc on-clip (doc-spans doc))
  (mapc on-link (doc-links doc)))

(defun iterate-spans (doc on-span)
  "Call ON-SPAN on all the spans in a doc, whether they are in clips or links."
  (iterate-doc doc on-span (lambda (l)
			     (dolist (e (link-endsets l))
			       (if (span-endset-p e)
				   (dolist (s (span-endset-spans e))
				     (funcall on-span s)))))))

(defun replace-spans (doc new-span-fn)
  "Calls NEW-SPAN-FN on each span in a document (in both clips and links) and replaces the
span with the list of spans returned."
  (multiple-value-bind (clips links)
      (with-collectors (clips links)
	(iterate-doc
	 doc
	 (compose #'clips new-span-fn)
	 (lambda (l)
	   (let ((new (copy-link l))
		 (endsets (mapcar (lambda (e)
				    (if (span-endset-p e)
					(let ((newe (copy-span-endset e)))
					  (setf (span-endset-spans newe)
						(flatten
						 (mapcar new-span-fn (span-endset-spans e))))
					  newe)
					e))
				  (link-endsets l))))
	     (setf (link-endsets new) endsets)
	     (links new)))))
    (new-doc-leaf (flatten clips) links)))

(defun editable-p (leaf) (and (leaf-name leaf) (not (hash-name-p (leaf-name leaf)))))

(defun new-content-leaf (text)
  "Create a new content leaf holding the given text."
  (make-content-leaf :owner user+ :contents text))

(defun new-doc-leaf (&optional spans links)
  "Create a new document leaf with the given spans and links."
  (make-doc :owner user+ :type :doc :spans spans :links links))

(defun load-all-contents (spans &optional (cache (make-cache)))
  "Load all the leaves refered to by the given spans into a cache."
  (dolist (a (mapcar #'origin spans))
    (when (not (in-cache a cache))
      (let ((doc (get-from-cache a cache)))
	(if (scroll-name-p a) (load-all-contents (doc-spans doc) cache)))))
  cache)

(defun generate-concatatext (spans &optional (cache (load-all-contents spans)))
  "Generate the concatatext of the given spans. The concatatext is the assembled content
defined by the spans. It is created by concatenating the content referred to by each span."
  (apply #'concatenate 'string (mapcar (lambda (s) (apply-span s cache)) spans)))

(defun apply-span (span &optional (cache (make-cache)))
  "Extract the text referred to by a span."
  (let ((contents (get-from-cache (origin span) cache)))
    (if (scroll-name-p (origin span))
	(generate-concatatext (crop (doc-spans contents) (start span) (len span)) cache)
	(subseq (content-leaf-contents contents) (start span) (next-pos span)))))

;;; Leaf cache

(defun make-cache () (make-hash-table :test 'equalp))

(defun in-cache (name cache) (nth-value 1 (gethash name cache)))

(defun get-from-cache (name cache)
  (build (value (gethash name cache))
    (when (not (in-cache name cache))
      (setf value (load-and-parse name))
      (setf (gethash name cache) value))))

;;; Links

(defun link (type endsets) (make-link :owner user+ :type type :endsets endsets))
(defun doc-endset (name doc-or-doc-name)
  (make-doc-endset :name name :doc-name (if (doc-p doc-or-doc-name) (doc-name doc-or-doc-name)
					    doc-or-doc-name)))
(defun span-endset (name spans) (make-span-endset :name name :spans spans))
(defun cc-endset (name spans) (make-concatatext-endset :name name :spans spans))

(defun create-cclink-from-spec (spec)
  "Create a concatalink object from a tree representation of the link."
  (labels ((do-endsets (spec &optional name)
	     (let ((x (car spec)))
	       (typecase x
		 (keyword (if name (error "Endset ~S has no contents" name))
			  (do-endsets (cdr spec) (string-downcase (symbol-name x))))
		 (string (cons (doc-endset name (parse-doc-name x))
			       (do-endsets (cdr spec))))
		 (cons (cons (cc-endset name (if (consp (car x))
						 (mapcar #'do-span x)
						 (list (do-span x))))
			     (do-endsets (cdr spec)))))))
	   (do-span (list)
	     (if (not (= 3 (length list)))
		 (error "Span requires origin, start and length only: ~S" list))
	     (if (not (stringp (first list)))
		 (error "Origin must be a document name"))
	     (if (not (integerp (second list)))
		 (error "Start must be an integer: ~S" (second list)))
	     (if (not (integerp (third list)))
		 (error "Length must be an integer: ~S" (third list)))
	     (apply #'span (resolve-doc-name (car list)) (cdr list))))
    (make-concatalink :type (car spec) :endsets (do-endsets (cdr spec)))))

(defun remap-link-spans (link span-predicate replace-fn)
  "Helper function for rewriting spans in a link to a different form (such as moving then
between namespaces."
  (mapcar (lambda (e) (if (funcall span-predicate e)
			  (make-span-endset-from-cc-endset replace-fn e)
			  e))
	  (ccl-endsets link)))

(defun make-span-endset-from-cc-endset (replace-fn cc-endset)
  "Convert a concatalink endset to a span endset."
  (span-endset (cce-name cc-endset)
	       (merge-all (apply #'append (mapcar (lambda (s) (funcall replace-fn s))
						  (cce-spans cc-endset))))))

(defun coerce-to-link (link &optional (cache (make-cache)))
  "Convert LINK to a link. Link may be a link (in which case it is returned unchanged) or a
concatalink (in which case it is remapped to a span link)."
  (typecase link
    (link link)
    (concatalink (link (ccl-type link)
		       (remap-link-spans link #'concatatext-endset-p
					 (lambda (s)
					   (crop (doc-spans (get-from-cache (origin s) cache))
						 (start s)
						 (len s))))))
    (T (error "Could not coerce to link: ~S" link))))

;;; Scrolls and publishing

(defun scroll-span-p (span)
  (equal (origin span) local-scroll-name+))

(defun scratch-span-p (span)
  (equal (origin span) scratch-name+))

(defun append-to-local-scroll (content)
  "Append some content to the local private scroll and return the span representing it."
  (let ((scratch-path (uiop:native-namestring (name-to-path scratch-name+)))
	(length (length content))
	(scratch-contents (content-leaf-contents (load-and-parse scratch-name+)))
	(scroll (load-and-parse local-scroll-name+)))
    (let* ((span-for-scroll (span scratch-name+ (length scratch-contents) length)))
      (with-open-file (s scratch-path :direction :output :if-exists :append)
	(princ content s))
      (let ((scroll-position (length-sum (doc-spans scroll))))
	(setf (doc-spans scroll) (merge-span-lists (doc-spans scroll) (list span-for-scroll)))
	(save-leaf scroll)
	(span local-scroll-name+ scroll-position length)))))

(defun migrate-scroll-spans-to-scroll-targets (doc scroll-spans)
  "Takes a doc and the list of spans that make up a scroll, and converts all spans in the doc
that reference the scroll to spans that reference the scroll's contents."
  (replace-spans doc (lambda (s)
		       (if (scroll-span-p s) (crop scroll-spans (start s) (len s)) s))))

(defun get-scratch-spans (doc)
  "Get all spans in a doc referencing the scratch file."
  (collecting (iterate-spans doc (lambda (s) (if (scratch-span-p s) (collect s))))))

(defun build-map-from-scratch-spans (spans)
  "Used when migrating content from the scratch file to a leaf. The list of spans referring
to the scratch file are rewritten such that overlapping spans are combined into one. The
resulting spans are in a similar order to the original spans, but will no duplicated content.
This list of spans can then be used to generate a new leaf containing all the scratch
content."
  (let ((deduped (deduplicate (sort (copy-list spans) #'< :key #'start))))
    (collecting
      (dolist (s spans)
	(awhen (find (start s) deduped :test (lambda (p s) (span-contains s p)))
	  (collect it)
	  (setf deduped (remove it deduped)))))))

(defun create-leaf-from-map (map)
  "Take a map created by BUILD-MAP-FROM-SCRATCH-SPANS and generate its concatatext as a new
leaf. Find the hash name of the new leaf immediately so it can be used in the rest of the
publishing process."
  (build (leaf (new-content-leaf (generate-concatatext map)))
    (set-hash-name leaf (serialize-leaf leaf))))

(defun migrate-scratch-spans-to-leaf (doc map leaf-name)
  "Rewrite scratch spans so that they refer to the contents of LEAF-NAME. The MAP argument
describes the original scratch positions of the contents in the new leaf and should be
created by BUILD-MAP-FROM-SCRATCH-SPANS."
  (replace-spans
   doc
   (lambda (s)
     (if (scratch-span-p s)
	 (span leaf-name
	       (get-concatatext-position map scratch-name+ (start s))
	       (len s))
	 s))))

(defun rewrite-scratch-span (span map leaf-name &optional (pos 0))
  "Attempt to convert a span referring to scratch to a new span referring to LEAF-NAME, using
the given MAP (created by BUILD-MAP-FROM-SCRATCH-SPANS) as a guide to where the contents can
be found in the leaf."
  (cond ((endp map) (list span))
	(T (mapcan (lambda (s)
		     (rewrite-scratch-span
		      s (cdr map) (+ pos (len (car map))) leaf-name))
		   (transform-intersection
		    span
		    (car map)
		    (lambda (x p) (span leaf-name (+ pos p) (len x))))))))

(defun migrate-scroll-spans-to-leaf (scroll-spans map leaf-name)
  (mapcan (lambda (s) (rewrite-scratch-span s map leaf-name)) scroll-spans))

(defun publish (doc)
  "Take a doc and convert it to a publishable form. The is achieved by the following
operations:

    1. All contents referred to by the doc that are in the scratch file are migrated to a
       permanent home in a new leaf.

    2. The local scroll is updated to refer to this new leaf instead of the scratch file.

    3. All references to the local scroll in the document are replaced by references to the
       leaves that hold the content.

The concatatext and links of the document should be unchanged, but the doc is internally
rewritten to reference only the permanent homes of the contents, rather than the scrols they
originated from."
  (let* ((scroll-spans (doc-spans (load-and-parse local-scroll-name+)))
	 (migrated-to-scratch (migrate-scroll-spans-to-scroll-targets doc scroll-spans))
	 (map (build-map-from-scratch-spans (get-scratch-spans migrated-to-scratch)))
	 (new-leaf (create-leaf-from-map map))
	 (fully-migrated
	  (migrate-scratch-spans-to-leaf migrated-to-scratch map (leaf-name new-leaf)))
	 (migrated-scroll-spans
	  (migrate-scroll-spans-to-leaf scroll-spans map (leaf-name new-leaf))))
    (save-leaf new-leaf)
    (save-leaf fully-migrated)
    (let ((new-scroll (new-doc-leaf migrated-scroll-spans nil)))
      (setf (leaf-name new-scroll) local-scroll-name+)
      (save-leaf new-scroll))))

;;; File processing

(defun read-stream-content-into-string (stream &key (buffer-size 4096))
  "Use DRAIN rather than using this directly, as hopefully it will eventually be replaced by
the implementation in ALEXANDRIA."
  (let ((*print-pretty* nil))
    (with-output-to-string (datum)
      (let ((buffer (make-array buffer-size :element-type 'character)))
        (loop
          :for bytes-read = (read-sequence buffer stream)
          :do (write-sequence buffer datum :start 0 :end bytes-read)
          :while (= bytes-read buffer-size))))))

(defun drain (stream)
  "Read the contents of a text file into a string."
  (read-stream-content-into-string stream))

;;; Repo management

(defun working-directory () (cl-fad:pathname-as-directory (sb-posix:getcwd)))
(defun set-test-repo () (setf repo-path* test-repo+))

(defun merge-path (base &rest extensions)
  (apply #'cl-fad:merge-pathnames-as-file
	 (cl-fad:pathname-as-directory base)
	 extensions))

(defun repo-path (&rest extensions) (apply #'merge-path repo-path* extensions))
(defun name-to-path (name) (apply #'repo-path (get-path-extensions name)))

(defun init (&optional new-dir-name)
  "Initialize a new repo called NEW-DIR-NAME in the current directory."
  (let ((repo-path*
	 (if new-dir-name
	     (cl-fad:merge-pathnames-as-directory
	      (working-directory)
	      (concatenate 'string new-dir-name "/"))
	     repo-path*)))
    (ensure-directories-exist (repo-path "public/"))
    (ensure-directories-exist (repo-path "scrolls/"))
    (ensure-directories-exist (repo-path "names/"))
    (save-leaf (make-doc :name local-scroll-name+ :owner user+ :type :local-scroll))
    (save-leaf (make-content-leaf :name scratch-name+ :owner user+))
    repo-path*))

(defun load-and-parse (name) (parse-leaf (load-by-name name) name))

(defun save-leaf (leaf)
  (let ((serialized (serialize-leaf leaf))
	(name (leaf-name leaf)))
    (if (or (not name) (hash-name-p name)) (set-hash-name leaf serialized))
    (save-by-name (leaf-name leaf) serialized)
    leaf))

(defun load-by-name (name)
  (with-open-file (s (name-to-path name) :if-does-not-exist :error)
    (if (null s) (return-from load-by-name nil))
    (drain s)))

(defun save-by-name (name contents)
  (with-open-file (s (name-to-path name) :direction :output :if-exists :supersede)
    (princ contents s)))

(defun leaf-missing (name) (not (probe-file (name-to-path name))))

;;; Parsing leaves

(defun parse-leaf (v name)
  (with-input-from-string (s v)
    (let* ((type (parse-type (read-line s)))
	   (owner (read-line s))
	   (content-separator (read-line s))
	   (contents (drain s)))
      (assert (string= content-separator "-"))
      (cond
	((represents-doc-p type)
	 (let* ((spans-links (parse-doc-contents contents)))
	   (make-doc :name name
		     :owner owner
		     :type type
		     :spans (first spans-links)
		     :links (second spans-links))))
	((eq type :link) (parse-link name owner contents))
	(T (make-content-leaf :name name
			      :owner owner
			      :contents contents))))))

(defun parse-type (type-string) (make-keyword type-string))

(defun represents-doc-p (type) (find type '(:doc :scroll :local-scroll)))

(defun parse-span-name (str)
  (cond ((string= "0" str) scratch-name+)
	((string-starts-with "scroll/" str) (make-scroll-name :scroll (subseq str 7)))
	(T (make-hash-name :hash str))))

(defun parse-doc-contents (contents)
  "Parses the contents part of a leaf as if it were a document, returning (list spans links)."
  (with-input-from-string (s contents)
    (multiple-value-bind (spans links)
	(with-collectors (spans links)
	  (block nil
	    (loop :do
		 (multiple-value-bind (line end) (read-line s nil :eof)
		   (cond
		     ((eq :eof line))
		     ((string-starts-with "span:" line) (spans (parse-span-line line)))
		     ((string-starts-with "link:" line) (links (parse-link-include line)))
		     (T (error "Could not understand line '~S'" line)))
		   (if end (return))))))
      (list spans links))))

(defun split-span-into-parts (span)
  (split-sequence #\space span))

(defun parse-span-line (span)
  (parse-span-section (subseq span 5)))

(defun parse-span-section (section)
    (let ((parts (split-sequence #\, section)))
    (assert (= (length parts) 3))
    (assert (string-starts-with "start=" (second parts)))
    (assert (string-starts-with "length=" (third parts)))
    (span (parse-span-name (first parts))
	  (read-from-string (subseq (second parts) 6))
	  (read-from-string (subseq (third parts) 7)))))

(defun parse-link-include (link-include-line)
  (let ((name (subseq link-include-line 5)))
    (handler-case (load-and-parse (make-hash-name :hash name))
      (file-error () (error 'leaf-not-found :type :leaf :name name)))))

(defun parse-link (name owner contents)
  (let ((type-endsets (parse-link-contents contents nil)))
	   (make-link :name name
		      :owner owner
		      :type (car type-endsets)
		      :endsets (cdr type-endsets))))

(defun parse-cclink (contents)
  (let ((type-endsets (parse-link-contents contents T)))
    (make-concatalink :type (car type-endsets) :endsets (cdr type-endsets))))

(defun parse-link-contents (link as-cclink)
  (let ((parts (split-sequence #\; (subseq link 0 (1- (length link))))))
    (cons (car parts) (mapcar (lambda (e) (parse-endset e as-cclink)) (cdr parts)))))

(defun parse-endset (endset as-cclink)
  (let* ((name-and-contents (split-sequence #\# endset))
	 (name (if (cdr name-and-contents) (car name-and-contents)))
	 (contents (if (cdr name-and-contents) (cadr name-and-contents)
		       (car name-and-contents))))
    (cond ((string-starts-with "span:" contents)
	   (if as-cclink
               (make-concatatext-endset :name name :spans (parse-span-endset-spans contents))
	       (make-span-endset :name name :spans (parse-span-endset-spans contents))))
	  ((string-starts-with "doc:" contents)
           (make-doc-endset :name name :doc-name (parse-doc-ref contents)))
	  (T (error "Could not understand endset '~S'" contents)))))

(defun parse-span-endset-spans (span)
  (let ((nested-spans (split-sequence #\+ span :start 5)))
    (mapcar #'parse-span-section nested-spans)))

(defun parse-doc-ref (doc) (parse-doc-name (subseq doc 4)))

;;; Serializing leaves

(defun serialize-leaf (leaf)
  (concatenate 'string (serialize-leaf-header leaf)
	       (cond ((doc-p leaf) (serialize-doc-contents leaf))
		     ((link-p leaf) (serialize-link-contents leaf))
		     (T (content-leaf-contents leaf)))))

(defun serialize-leaf-header (leaf)
  (with-output-to-string (s)
    (labels ((p (str) (format s "~A~%" str)))
      (p (string-downcase (symbol-name (leaf-type leaf))))
      (p (leaf-owner leaf))
      (p "-"))))

(defun serialize-doc-contents (doc)
  (with-output-to-string (s)
    (dolist (span (doc-spans doc))
      (princ (serialize-span-line span) s))
    (dolist (link (doc-links doc))
      (princ (serialize-link-include link) s))))

(defun serialize-endset (endset)
  (with-output-to-string (s)
    (if (endset-name endset) (format s "~A#" (endset-name endset)))
    (if (doc-endset-p endset) (format s "doc:~A" (serialize-name (doc-endset-doc-name endset)))
	(format s "span:~{~A~^+~}"
		(mapcar #'serialize-span-section (span-endset-spans endset))))))

(defun serialize-span-line (span)
  (format nil "span:~A~%" (serialize-span-section span)))

(defun serialize-span-section (section)
  (format nil "~A,start=~A,length=~A"
	  (serialize-name (origin section)) (start section) (len section)))

(defun serialize-link-include (link)
  (if (null (link-name link))
      (error "Attempt to serialize a document that has an unsaved link")
      (format nil "link:~A~%" (serialize-name (link-name link)))))

(defun serialize-link-contents (link)
  (format nil "~A;~{~A~^;~}~%"
	  (link-type link)
	  (mapcar #'serialize-endset (link-endsets link))))

;;; Server

(defun serve (&optional (port 4242))
  (if (not acceptor*) (set-acceptor port) (stop))
  (init-acceptor)
  (hunchentoot:start acceptor*))

(defun set-acceptor (port)
  (setf acceptor* (make-instance 'hunchentoot:easy-acceptor :port port)))

(defun stop ()
  (hunchentoot:stop acceptor*))

(defun init-acceptor ()  (hunchentoot:define-easy-handler (serve-leaf :uri "/leaf") (name)
    (setf (hunchentoot:content-type*) "text/plain") ; how should this be handled?
					; TODO Cannot assume :CONTENT below
    (build (leaf (load-by-name (parse-span-name name)))
      (if (null leaf) (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)))))

;;; Client

(defun merge-url (&rest parts)
  (if (endp parts) (return-from merge-url ""))
  (let ((part (car parts)))
    (if (string-starts-with "/" part) (setf part (subseq part 1)))
    (let ((last (1- (length part))))
      (if (string= #\/ (elt part last)) (setf part (subseq part 0 last))))
    (concatenate 'string part (if (cdr parts) "/" "") (apply #'merge-url (cdr parts)))))

(defmacro with-http-client (&body body)
  `(let ((http-stream* (if (boundp 'http-stream*) http-stream* nil)))
     ,@body))

(defun ensure-leaf (name keep-alive &optional force-download)
  "Make sure a leaf is in the cache, and return its contents"
  (if (or force-download (leaf-missing name))
      (let ((leaf (get-leaf-from-server name keep-alive)))
	(if leaf (save-by-name name leaf))
	leaf)
      (load-by-name name)))

(defun ensure-leaves (names &optional force-download)
  "Makes sure a list of leaves are in the cache, returning those that could not be retrieved"
  (with-collectors (not-found)
    (with-http-client
      (dolist (name names)
	(if (not (ensure-leaf name T force-download)) (not-found name))))))

(defun download-folio (doc-name &optional force-download)
  "Download a doc and all leaves required to make up its spans, returns those that weren't
found"
  (with-http-client
    (let ((doc (ensure-leaf doc-name T force-download)))
      (when doc
	(ensure-leaves (mapcar #'origin (doc-spans (parse-leaf doc doc-name)))
		       force-download)))))

(defun get-leaf-from-server (name keep-alive)
  (multiple-value-bind (body status-code headers uri new-stream must-close reason-phrase)
      (drakma:http-request
       (merge-url upstream+ "leaf")
	:parameters (list (cons "name" (serialize-name name)))
	:close (not keep-alive)
	:stream http-stream*)
    (declare (ignore headers) (ignore uri))
    (cond (must-close
	   (if http-stream* (close http-stream*))
	   (setf http-stream* nil))
	  (T (setf http-stream* new-stream)))
    (case status-code
      (404 nil)
      (200 body)
      (otherwise (error "Server returned ~A, reason:'~A'" status-code reason-phrase)))))

;;; Base64

(defun encode-sextet (s)
  (cond ((< s 26) (code-char (+ 65 s)))
	((< s 52) (code-char (+ 71 s)))
	((< s 62) (code-char (- s 4)))
	((= s 62) #\+)
	((= s 63) #\/)
	(T #\=)))

(defun decode-char (c)
  (or (position c "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/") 0))

(defun compute-sextets (a b c)
  (list (ash (logand a #xFC) -2)
	(logior (ash (logand a 3) 4) (ash (logand (or b 0) #xF0) -4))
	(if b (logior (ash (logand b #xF) 2) (ash (logand (or c 0) #xC0) -6)) 64)
	(if c (logand c #x3F) 64)))

(defun compute-octets (a b c d)
  (list (logior (ash a 2) (ash (logand b 48) -4))
	(logior (ash (logand b 15) 4) (ash (logand c 60) -2))
	(logior (ash (logand c 3) 6) d)))

(defun encode-chars (&rest octets)
  (mapcar #'encode-sextet (apply #'compute-sextets octets)))

(defun decode-chars (&rest chars)
  (build (codes (apply #'compute-octets (mapcar #'decode-char chars)))
    (do ((c (car (last codes)) (car (last codes))))
	((not (zerop c)))
      (setf codes (butlast codes)))))

(defun safe-char-code (c) (if c (char-code c)))

(defun base64-encode-octets (octets)
  (build (arr (make-array '(0) :element-type 'character :adjustable T :fill-pointer 0))
    (let ((pos 0))
      (labels ((pull () (if (>= pos (length octets)) nil
			    (prog1 (aref octets pos) (incf pos)))))
	(loop
	   for a = (pull)
	   for b = (pull)
	   for c = (pull)
	   while a
	   do (dolist (x (encode-chars a b c)) (vector-push-extend x arr)))))))

(defun base64-decode-string (str)
  (build (arr (make-array '(0) :element-type '(unsigned-byte 8) :adjustable T :fill-pointer 0))
    (let ((pos 0))
      (labels ((pull () (if (>= pos (length str)) nil (prog1 (aref str pos) (incf pos)))))
	(loop
	   for a = (pull)
	   while a
	   for b = (pull)
	   for c = (pull)
	   for d = (pull)
	   do (dolist (x (decode-chars a b c d)) (vector-push-extend x arr)))))))

;;; Signatures

(defun get-digest-for-string (str)
					; TODO handle non-ASCII text
  (ironclad:digest-sequence :sha256 (ironclad:ascii-string-to-byte-array str)))

(defun encrypt-digest (digest &optional
				(private-key-file private-key-file+)
				(passphrase private-key-passphrase+))
  (let ((key (ssh-keys:parse-private-key-file private-key-file :passphrase passphrase)))
    (ironclad:sign-message key digest)))

(defun calculate-signature (str)
  (encrypt-digest (get-digest-for-string str)))

(defun create-signature-line (rest-of-leaf)
  (base64-encode-octets (calculate-signature rest-of-leaf)))

(defun verify-leaf-signature (leaf-string)
  (let* ((first-line-end (position #\newline leaf-string))
	 (from-file (subseq leaf-string 0 first-line-end))
	 (calculated (create-signature-line (subseq leaf-string (1+ first-line-end)))))
    (equal from-file calculated)))

(defun create-hash (leaf-string &optional (length default-hash-length+))
  (subseq (ironclad:byte-array-to-hex-string (get-digest-for-string leaf-string)) 0 length))

;;; Test repo

(defun recreate-test-repo ()
  (set-test-repo)
  (cl-fad:delete-directory-and-files repo-path* :if-does-not-exist :ignore)
  (init)
  (build (doc (new-doc "test-doc"))
    (format T "Created doc ~A~%" doc)
    (append-text doc "0123456789")
    (format T "Appended text '0123456789' to ~A~%" doc)
    (append-text doc (format nil "ABCDEFGHIJKLMNOPQRSTUVWXYZ~%~%"))
    (format T "Appended text 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' to ~A~%" doc)
    (let ((imported (import-file (repo-path "../" "elegy.txt") "elegy")))
      (format T "Imported poetry as ~A~%" imported)
      (transclude doc (doc-length doc) imported 54 188)
      (format T "Transcluded 188 characters poetry from ~A into ~A~%" imported doc)
      (format T "Adding quote link~%")
      (let ((q (new-link
		(link "quote"
		      (list
		       (doc-endset "origin" (parse-doc-name imported))
		       (span-endset nil (list (span (resolve-doc-name doc) 38 188))))))))
	(add-link doc q))
      (format T "Adding verse link using link spec with span~%")
      (add-link doc `("verse" :lines (,doc 38 188)))
      (format T "Adding doctext link using link spec with doc name~%")
      (add-link doc `("doctest" ,imported))
      (format T "Inserting 'insertedAt2' link~%")
      (insert-link doc `("insertedAt2" ,imported) 2)
      (format T "Inserting and deleting link~%")
      (insert-link doc `("toBeDeleted" , imported) 1)
      (remove-link doc 1)
      (format T "~A~%" (export-text doc (repo-path "test-doc-output.txt"))))))
