;;;; commonplace.lisp

(in-package #:commonplace)

(defstruct (span :conc-name) origin start len)
(defstruct name)
(defstruct (tumbler-name (:include name)) parts)
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

;;;; Common functions

(defmacro not-implemented (name args)
  `(defun ,name ,args (declare ,@ (mapcar (lambda (a) `(ignore ,a)) args))
	  (error "Function not implemented ~A" ',name)))

(defmacro probe (form)
  (with-unique-names (val)
    `(let ((,val ,form)) (format T ,(format nil "~A: ~~A~%" form) ,val) ,val)))

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

(defun make-keyword (x)
  (intern (string-upcase (with-output-to-string (s) (princ x s))) (symbol-package :x)))

;;; Span operations

(defun span (origin start length) (make-span :origin origin :start start :len length))

(defun edit-span (span &key (origin (origin span)) (start (start span)) (len (len span)))
  (span origin start len))

(defun next-pos (s) (+ (start s) (len s)))

(defun span-end (s) (1- (next-pos s)))

(defun same-origin (s1 s2) (equalp (origin s1) (origin s2)))

(defun span-contains (span point &optional (adjustment 0))
  (let ((offset (- point (start span) adjustment)))
    (and (>= offset 0) (< offset (len span)))))

(defun overlapping-p (s1 s2 &key (adjust1 0) (adjust2 0))
  (not (or (< (+ (span-end s1) adjust1) (+ (start s2) adjust2))
	   (< (+ (span-end s2) adjust2) (+ (start s1) adjust1)))))

(defun abutting-p (s1 s2)
  (eq (next-pos s1) (start s2)))

(defun mergeable-p (s1 s2)
  (and (same-origin s1 s2) (or (overlapping-p s1 s2) (abutting-p s1 s2))))

(defun duplicating-p (s1 s2)
  (and (same-origin s1 s2) (overlapping-p s1 s2)))

(defun merge-spans (s1 s2 &optional only-overlaps)
  (if (if only-overlaps (duplicating-p s1 s2) (mergeable-p s1 s2))
      (let ((start (min (start s1) (start s2))))
	(list (span (origin s1) start (- (max (next-pos s1) (next-pos s2)) start))))
      (list s1 s2)))

(defun divide-span-at-point (s1 point)
  (with-slots (start length) s1
    (if (and (span-contains s1 point) (> point (start s1)))
	(list (edit-span s1 :len (- point start))
	      (edit-span s1 :start (+ start point -1) :len (1+ (- length point))))
	(list s1))))

(defun divide-span (s length)
  (if (or (zerop length) (>= length (len s)))
      (list s)
      (list (edit-span s :len length)
	    (edit-span s :start (+ (start s) length) :len (- (len s) length)))))

(defun merge-span-lists (list1 list2)
  (cond
    ((endp list1) list2)
    ((endp list2) list1)
    (T (let ((lifted (lift list1)))
	 (append (cdr lifted) (merge-spans (car lifted) (car list2)) (cdr list2))))))

(defun merge-all (list)
  "Merge all spans that abut or overlap"
  (if (endp (cdr list)) list
      (let ((merged (merge-spans (car list) (cadr list))))
	(if (cadr merged) (cons (car list) (merge-all (cdr list)))
	    (merge-all (cons (car merged) (cddr list)))))))

(defun deduplicate (list)
  "Merge all spans that overlap (i.e. duplicate a portion of each other's content"
  (if (endp (cdr list)) list
      (let ((merged (merge-spans (car list) (cadr list) T)))
	(if (cadr merged) (cons (car list) (deduplicate (cdr list)))
	    (deduplicate (cons (car merged) (cddr list)))))))

(defun divide-list (list point &optional collected)
  "Divide a list of spans into two list at the given point (included in the second list)"
  (let ((s (car list)))
    (cond ((endp list) (list (nreverse collected) nil))
	  ((zerop point) (list (nreverse collected) list))
	  ((> (len s) point)
	   (let ((split (divide-span s point)))
	     (list (nreverse (cons (car split) collected)) (cons (cadr split) (cdr list)))))
	  (T (divide-list (cdr list) (- point (len s)) (cons s collected))))))

(defun divide-twice (list start length)
  "Divide a list of span into three, with the central section having given start and length"
  (let ((div (divide-list list start)))
    (cons (car div) (divide-list (cadr div) length))))

(defun extract-range (spans start length)
  "Create spans representing the subset of some other spans delimited by start and length."
  (second (divide-twice spans start length)))

(defun insert-spans (spans new-spans point)
  "Insert a list of spans into the middle of some existing spans."
  (let ((div (divide-list spans point)))
    (merge-span-lists (car div) (merge-span-lists new-spans (cadr div)))))

(defun delete-spans (spans start length)
  "Remove a section from some spans."
  (let ((div (divide-twice spans start length)))
    (merge-span-lists (first div) (third div))))

(defun move-spans (spans start length new-pos)
  (let ((div (divide-twice spans start length)))
    (insert-spans (merge-span-lists (first div) (third div)) (second div) new-pos)))

(defun transclude-spans (source-spans start length target-spans insert-point)
  "Transclude content from one set of spans into another."
  (insert-spans target-spans (extract-range source-spans start length) insert-point))

(defun find-span (spans point)
  (recur (spans pos) (spans 0)
    (cond ((endp spans) nil)
	  ((span-contains (car spans) point pos) (values (car spans) pos))
	  (T (recur (cdr spans) (+ pos (len (car spans))))))))

(defun get-concatatext-position (spans point-origin point &optional (pos 0))
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
       (let ((length (- (start i) (start s))))
	 (if (> length 0) (collect (edit-span s :len length)))
	 (let ((start (max (start s) (start i))))
	   (collect
	       (funcall fn (edit-span s :start start
				      :len (- (min (next-pos s) (next-pos i)) start))
			length))))
       (let ((length (- (span-end s) (span-end i))))
	 (if (> length 0) (collect (edit-span s :start (next-pos i) :len length)))))))

(defun append-new-text (doc text)
  (pushend (append-to-local-scroll text) (doc-spans doc)))

;;;; Document names

(defun generate-doc-name-string ()
  (ironclad:byte-array-to-hex-string (map-into (make-array '(3)) (lambda () (random 256)))))

(defun get-doc-name-path (name) (repo-path "names/" name))

(defun new-doc-name (version-name)
  (let ((name (generate-doc-name-string)))
    (with-open-file (s (get-doc-name-path name)
		       :direction :output :if-does-not-exist :create :if-exists :error)
      (princ (hash-name-hash version-name) s))
    name))

(defun update-doc-name (name new-hash)
  (with-open-file (s (get-doc-name-path name)
		     :direction :output :if-does-not-exist :error :if-exists :overwrite)
      (princ (hash-name-hash new-hash) s)))

(defun resolve-doc-name (name)
  (with-open-file (s (get-doc-name-path name) :if-does-not-exist :error)
    (make-hash-name :hash (read-line s))))

(defun delete-doc-name (name) (delete-file (get-doc-name-path name)))

;;;; Leaf names

(defmethod get-path-extensions ((name tumbler-name))
  (list "public/" (format nil "~{~A~^_~}" (tumbler-name-parts name))))

(defmethod get-path-extensions ((name scroll-name))
  (list "scrolls/" (scroll-name-scroll name)))

(defmethod get-path-extensions ((name scratch-name)) (list "0"))

(defmethod get-path-extensions ((name hash-name))
  (list "public/" (hash-name-hash name)))

(defmethod init-name-from-string ((name tumbler-name) str)
  (setf (tumbler-name-parts name) (mapcar #'parse-integer (split-sequence #\. str))))

(defmethod init-name-from-string ((name scroll-name) str)
  (setf (scroll-name-scroll name) (subseq str 7)))

(defmethod init-name-from-string ((name scratch-name) str))

(defmethod init-name-from-string ((name hash-name) str)
  (setf (hash-name-hash name) str))

(defmethod serialize-name ((name scroll-name))
  (format nil "scroll/~A" (scroll-name-scroll name)))

(defmethod serialize-name ((name tumbler-name))
  (format nil "~{~A~^.~}" (tumbler-name-parts name)))

(defmethod serialize-name ((name hash-name)) (hash-name-hash name))

(defmethod serialize-name ((name scratch-name)) "0")

(defun local-scroll-name-p (name) (and (scroll-name-p name)
				       (string= "local" (scroll-name-scroll name))))

(defun set-hash-name (leaf serialized-leaf)
  (setf (leaf-name leaf) (make-hash-name :hash (create-hash serialized-leaf))))

(defun get-next-version-name (old-name)
  (let ((parts (tumbler-name-parts old-name)))
    (make-tumbler-name :parts (nconc (butlast parts) (list (1+ (car (last parts))))))))

;;;; Leaf operations

(defmethod leaf-type ((leaf content-leaf)) :content)
(defmethod leaf-type ((leaf link)) :link)
(defmethod leaf-type ((leaf doc)) (doc-type leaf))

(defun iterate-doc (doc on-clip on-link)
  (mapc on-clip (doc-spans doc))
  (mapc on-link (doc-links doc)))

(defun iterate-spans (doc on-span)
  (iterate-doc doc on-span (lambda (l)
			     (dolist (e (link-endsets l))
			       (if (span-endset-p e)
				   (dolist (s (span-endset-spans e))
				     (funcall on-span s)))))))

(defun replace-spans (doc new-span-fn)
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
  (make-content-leaf :owner user+ :contents text))

(defun new-doc-leaf (&optional spans links)
  (make-doc :owner user+ :type :doc :spans spans :links links))

(defun make-new-version (doc)
  (new-doc-leaf (doc-spans doc) (doc-links doc)))

(defun create-content-from-file (path)
  "Import text from a file"
  (let ((contents (make-fillable-string)))
    (with-open-file (s path)
      (loop for c = (read-char s nil) while c do (vector-push-extend c contents)))
    (new-content-leaf contents)))

(defun load-all-contents (spans &optional (cache (make-cache)))
  (dolist (a (mapcar #'origin spans))
    (when (not (in-cache a cache))
      (let ((doc (get-from-cache a cache)))
	(if (scroll-name-p a) (load-all-contents (doc-spans doc) cache)))))
  cache)

(defun generate-concatatext (spans &optional (contents-hash (load-all-contents spans)))
  (apply #'concatenate 'string
	 (mapcar (lambda (s) (apply-span s contents-hash)) spans)))

(defun generate-concatatext-clip (spans start length
				  &optional (contents-hash (load-all-contents spans)))
    (apply #'concatenate 'string
	 (mapcar (lambda (s) (apply-span s contents-hash))
		 (extract-range spans start length))))

(defun apply-span (span contents-hash)
  "Extract the text of a span from a collection of contents leaves."
  (let ((contents (gethash (origin span) contents-hash)))
    (if (scroll-name-p (origin span))
	(generate-concatatext-clip (doc-spans contents) (start span) (len span) contents-hash)
	(subseq (content-leaf-contents contents) (start span) (next-pos span)))))

;;;; Leaf cache

(defun make-cache () (make-hash-table :test 'equalp))

(defun in-cache (name cache) (nth-value 1 (gethash name cache)))

(defun get-from-cache (name cache)
  (let ((value (gethash name cache)))
    (when (not (in-cache name cache))
      (setf value (load-and-parse name))
      (setf (gethash name cache) value))
    value))

;;;; Links

(defun link (type &rest endsets) (make-link :owner user+ :type type :endsets endsets))
(defun doc-endset (name doc)
  (make-doc-endset :name name :doc-name (if (doc-p doc) (doc-name doc) doc)))
(defun span-endset (name &rest spans) (make-span-endset :name name :spans spans))

(defun add-link-to-end (doc link) (pushend link (doc-links doc)))
(defun insert-link (doc link n) (setf (doc-links doc) (insert-at link (doc-links doc) n)))
(defun remove-link (doc link)
  (setf (doc-links doc) (remove link (doc-links doc) :test #'equalp)))

(defun create-link-from-spec (spec)
  (labels ((do-endsets (spec &optional name)
	     (let ((x (car spec)))
	       (cond
		 ((keywordp x)
		  (if name (error "Endset ~S has not contents" name))
		  (do-endsets (cdr spec) x))
		 ((stringp x) (cons (doc-endset name x) (do-endsets (cdr spec))))
		 ((consp x) (cons (apply #'span-endset name
					 (if (consp (car x))
					     (mapcar #'do-span x)
					     (list (do-span x))))
				  (do-endsets (cdr spec)))))))
	   (do-span (list)
	     (if (not (= 3 (length list)))
		 (error "Span requires origin, start and length only: ~S" list))
	     (if (not (integerp (second list)))
		 (error "Start must be an integer: ~S" (second list)))
	     (if (not (integerp (third list)))
		 (error "Length must be an integer: ~S" (third list)))
	     (apply #'span list)))
    (if (link-p spec) spec
	(link-to-span-space
	 (apply #'link (make-keyword (car spec)) (do-endsets (cdr spec)))))))

(defun link-to-span-space (link &optional (cache (make-cache)))
  (apply #'link (link-type link) (mapcar (lambda (e) (endset-to-span-space e cache))
					 (link-endsets link))))

(defun endset-to-span-space (endset &optional (cache (make-cache)))
  (if (doc-endset-p endset) endset
      (apply #'span-endset (span-endset-name endset)
	     (merge-all
	      (apply #'append
		     (mapcar (lambda (s)
			       (extract-range (doc-spans (get-from-cache (origin s) cache))
					      (start s)
					      (len s)))
			     (span-endset-spans endset)))))))

;;;; Scrolls and publishing

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
      (let ((scroll-position (get-next-local-scroll-pos)))
	(setf (doc-spans scroll) (merge-span-lists (doc-spans scroll) (list span-for-scroll)))
	(save-leaf scroll)
	(span local-scroll-name+ scroll-position length)))))

(defun get-next-local-scroll-pos ()
  (apply #'+ (mapcar #'len (doc-spans (load-and-parse local-scroll-name+)))))

(defun migrate-scroll-spans-to-scroll-targets (doc scroll-spans)
  (replace-spans
   doc
   (lambda (s) (if (scroll-span-p s)
		   (extract-range scroll-spans (start s) (len s))
		   s))))

(defun get-scratch-spans (doc)
  (collecting (iterate-spans doc (lambda (s) (if (scratch-span-p s) (collect s))))))

(defun build-map-from-scratch-spans (spans)
  (let ((deduped (deduplicate (sort (copy-list spans) #'< :key #'start))))
    (collecting
      (dolist (s spans)
	(awhen (find (start s) deduped :test (lambda (p s) (span-contains s p)))
	  (collect it)
	  (setf deduped (remove it deduped)))))))

(defun create-leaf-from-map (map)
  (let ((leaf (new-content-leaf (generate-concatatext map))))
    (set-hash-name leaf (serialize-leaf leaf))
    leaf))

(defun migrate-scratch-spans-to-leaf (doc map leaf-name)
  (replace-spans
   doc
   (lambda (s)
     (if (scratch-span-p s)
	 (span leaf-name
	       (get-concatatext-position map scratch-name+ (start s))
	       (len s))
	 s))))

(defun rewrite-scratch-span (span map pos leaf-name)
  (cond ((endp map) (list span))
	(T (mapcan (lambda (s)
		     (rewrite-scratch-span
		      s (cdr map) (+ pos (len (car map))) leaf-name))
		   (transform-intersection
		    span
		    (car map)
		    (lambda (x p) (span leaf-name (+ pos p) (len x))))))))

(defun migrate-scroll-spans-to-leaf (scroll-spans map leaf-name)
  (mapcan (lambda (s) (rewrite-scratch-span s map 0 leaf-name)) scroll-spans))

(defun publish (doc)
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

;;;; Ugly file processing stuff
;;;; (get rid of this and do it properly!)

(defun make-fillable-string ()
  (make-array '(0) :element-type 'character :adjustable T :fill-pointer 0))

(defun drain (stream constructor)
  (do ((c (read-char stream nil :eof) (read-char stream nil :eof)))
	  ((eq c :eof))
	(funcall constructor c)))

;;; Repo management

(defun set-test-repo ()
  (setf repo-path* test-repo+))

(defun repo-path (&rest extensions)
  (apply #'cl-fad:merge-pathnames-as-file
	 (cl-fad:pathname-as-directory repo-path*)
	 extensions))

(defun name-to-path (name) (apply #'repo-path (get-path-extensions name)))

(defun init ()
  (ensure-directories-exist (repo-path "public/"))
  (ensure-directories-exist (repo-path "scrolls/"))
  (ensure-directories-exist (repo-path "names/"))
  (save-leaf (make-doc :name local-scroll-name+ :owner user+ :type :local-scroll))
  (save-leaf (make-content-leaf :name scratch-name+ :owner user+)))

(defun load-and-parse (name)
  (parse-vector (load-by-name name) name))

(defun save-leaf (leaf)
  (let ((serialized (serialize-leaf leaf))
	(name (leaf-name leaf)))
    (if (or (not name) (hash-name-p name)) (set-hash-name leaf serialized))
    (save-by-name (leaf-name leaf) serialized)
    leaf))

(defun load-by-name (name)
  (let ((file (make-fillable-string)))
    (with-open-file (s (name-to-path name) :if-does-not-exist nil)
      (if (null s) (return-from load-by-name nil))
      (drain s (lambda (c) (vector-push-extend c file)))
      file)))

(defun save-by-name (name contents)
  (with-open-file (s (name-to-path name) :direction :output :if-exists :supersede)
    (princ contents s)))

(defun leaf-missing (name)
  (not (probe-file (name-to-path name))))

;;;; Parsing leaves

(defun parse-vector (v name)
  (with-input-from-string (s v)
    (let* ((type (parse-type (read-line s)))
	   (owner (read-line s))
	   (content-separator (read-line s))
	   (contents (make-fillable-string)))
      (assert (string= content-separator "-"))
      (drain s (lambda (c) (vector-push-extend c contents)))
      (cond
	((represents-doc-p type)
	 (let* ((spans-links (parse-doc-contents contents)))
	   (make-doc :name name
		     :owner owner
		     :type type
		     :spans (first spans-links)
		     :links (second spans-links))))
	((eq type :link)
	 (let ((type-endsets (parse-link-contents contents)))
	   (make-link :name name
		      :owner owner
		      :type (car type-endsets)
		      :endsets (cdr type-endsets))))
	(T (make-content-leaf :name name
			      :owner owner
			      :contents contents))))))

(defun parse-type (type-string) (make-keyword type-string))

(defun represents-doc-p (type) (find type '(:doc :scroll :local-scroll)))

(defun parse-name (str)
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
    (span (parse-name (first parts))
	  (read-from-string (subseq (second parts) 6))
	  (read-from-string (subseq (third parts) 7)))))

(defun parse-link-include (link-include-line) (load-and-parse (subseq link-include-line 5)))

(defun parse-link-contents (link)
  (let ((parts (split-sequence #\; (subseq link 0 (1- (length link))))))
    (cons (car parts) (mapcar #'parse-endset (cdr parts)))))

(defun parse-endset (endset)
  (let* ((name-and-contents (split-sequence #\# endset))
	 (name (if (cdr name-and-contents) (car name-and-contents)))
	 (contents (if (cdr name-and-contents) (cadr name-and-contents)
		       (car name-and-contents))))
    (cond ((string-starts-with "span:" contents)
           (make-span-endset :name name :spans (parse-span-endset-spans contents)))
	  ((string-starts-with "doc:" contents)
           (make-doc-endset :name name :doc-name (parse-doc-ref contents)))
	  (T (error "Could not understand endset '~S'" contents)))))

(defun parse-span-endset-spans (span)
  (let ((nested-spans (split-sequence #\+ span :start 5)))
    (mapcar #'parse-span-section nested-spans)))

(defun parse-doc-ref (doc)
  (parse-name (subseq doc 4)))

;;;; Serializing leaves

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
  (format nil "link:~A~%" (serialize-name (link-name link))))

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
    (let ((leaf (load-by-name (parse-name name))))
      (if (null leaf) (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+))
      leaf)))

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
	(ensure-leaves (mapcar #'origin (doc-spans (parse-vector doc doc-name)))
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

;;;; Base64

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
  (let ((codes (apply #'compute-octets (mapcar #'decode-char chars))))
    (do ((c (car (last codes)) (car (last codes))))
	((not (zerop c)))
      (setf codes (butlast codes)))
    codes))

(defun safe-char-code (c) (if c (char-code c)))

(defun base64-encode-octets (octets)
  (let ((arr (make-fillable-string))
	(pos 0))
    (labels ((pull () (if (>= pos (length octets)) nil (prog1 (aref octets pos) (incf pos)))))
      (loop
	for a = (pull)
	for b = (pull)
	for c = (pull)
	while a
	do (dolist (x (encode-chars a b c)) (vector-push-extend x arr)))
      arr)))

(defun base64-decode-string (str)
  (let ((arr (make-array '(0) :element-type '(unsigned-byte 8) :adjustable T :fill-pointer 0))
	(pos 0))
    (labels ((pull () (if (>= pos (length str)) nil (prog1 (aref str pos) (incf pos)))))
      (loop
	for a = (pull)
	while a
	for b = (pull)
	for c = (pull)
	for d = (pull)
	do (dolist (x (decode-chars a b c d)) (vector-push-extend x arr)))
      arr)))

;;;; Signatures

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

;;;; Test repo
(defun recreate-test-repo ()
  (set-test-repo)
  (cl-fad:delete-directory-and-files repo-path* :if-does-not-exist :ignore)
  (init)
  (let ((doc (new-doc)))
    (format T "Created doc ~A~%" doc)
    (append-text doc "0123456789")
    (format T "Appended text '0123456789' to ~A~%" doc)
    (append-text doc (format nil "ABCDEFGHIJKLMNOPQRSTUVWXYZ~%~%"))
    (format T "Appended text 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' to ~A~%" doc)
    (let ((imported (import-file (repo-path "../" "elegy.txt"))))
      (format T "Imported poetry as ~A~%" imported)
      (transclude doc (doc-length doc) imported 54 188)
      (format T "Transcluded 188 characters poetry from ~A into ~A~%" imported doc)
      (format T "~A~%" (export-text doc)))))
