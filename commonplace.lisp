;;;; commonplace.lisp

(in-package #:commonplace)

(defstruct (span :conc-name) origin start len)
(defstruct name type parts)
(defstruct leaf name owner type sig)
(defstruct (content-leaf (:include leaf)) contents)
(defstruct (doc (:include leaf)) spans links)
(defstruct link type endsets)
(defstruct endset name)
(defstruct (span-endset (:include endset)) spans)
(defstruct (doc-endset (:include endset)) doc-name)

(defparameter local-scroll-name+ (make-name :type :local-scroll :parts "local"))
(defparameter scratch-name+ (make-name :type :scratch :parts '(0)))
(defparameter editable-signature+ "EDITABLE")
(defparameter upstream+ "http://localhost:4242/")
(defparameter test-repo+ "~/lisp/commonplace/test-repo")
(defparameter user+ "Me")

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

;;; Span operations

(defun span (origin start length) (make-span :origin origin :start start :len length))

(defun edit-span (span &key (origin (origin span)) (start (start span)) (len (len span)))
  (span origin start len))

(defun next-pos (s) (+ (start s) (len s)))

(defun span-end (s) (1- (next-pos s)))

(defun same-origin (s1 s2) (equal (origin s1) (origin s2)))

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
  (let ((lifted (lift list1)))
    (append (cdr lifted) (merge-spans (car lifted) (car list2)) (cdr list2))))

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

(defun transclude (source-spans start length target-spans insert-point)
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

;;;; Leaf names

(defun scroll-name-p (name) (or (eq (name-type name) :scroll) (local-scroll-name-p name)))
(defun doc-name-p (name) (eq (name-type name) :doc))
(defun content-name-p (name) (eq (name-type name) :content))
(defun link-name-p (name) (eq (name-type name) :link))
(defun local-scroll-name-p (name) (eq (name-type name) :local-scroll))
(defun scratch-name-p (name) (eq (name-type name) :scratch))

(defun get-next-version-name (old-name)
  (if (doc-name-p old-name)
      (let ((parts (name-parts old-name)))
	(make-name :type :doc :parts (nconc (butlast parts) (list (1+ (car (last parts)))))))
      (error "Can only get next version for a doc name")))

;;;; Leaf operations

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
    (new-doc-leaf (doc-name doc) (flatten clips) links)))

(defun editable-p (leaf) (equal editable-signature+ (leaf-sig leaf)))

(defun new-content-leaf (name text)
  (make-content-leaf :name name :owner user+ :sig "SIG" :type "content"
		     :contents text))

(defun new-doc-leaf (name &optional spans links)
  (make-doc :name name :owner user+ :sig editable-signature+ :type "doc"
	    :spans spans :links links))

(defun new-version (doc &optional (new-name (get-next-version-name (leaf-name doc))))
  (new-doc-leaf new-name (doc-spans doc) (doc-links doc)))

(defun create-content-from-file (name path)
  "Import text from a file"
  (let ((contents (make-fillable-string)))
    (with-open-file (s path)
      (loop for c = (read-char s nil) while c do (vector-push-extend c contents)))
    (new-content-leaf name contents)))

(defun load-all-contents (spans &optional (index (make-hash-table :test 'equal)))
  (dolist (a (mapcar #'origin spans))
    (when (not (nth-value 1 (gethash a index)))
      (setf (gethash a index) (load-and-parse a))
      (if (scroll-name-p a) (load-all-contents (doc-spans (gethash a index)) index))))
  index)

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

;;;; Scrolls and publishing

(defun scroll-span-p (span)
  (equal (origin span) local-scroll-name+))

(defun scratch-span-p (span)
  (equal (origin span) scratch-name+))

(defun append-to-local-scroll (content)
  "Append some content to the local private scroll and return the span representing it."
  (let ((scratch (uiop:native-namestring (name-to-path scratch-name+)))
	(scroll (uiop:native-namestring (name-to-path local-scroll-name+)))
	(length (length content))
	(scratch-contents (content-leaf-contents (load-and-parse scratch-name+))))
    (let* ((span-for-scroll (span scratch-name+ (length scratch-contents) length)))
      (with-open-file (s scratch :direction :output :if-exists :append)
	(princ content s))
      (let ((scroll-position (get-next-local-scroll-pos)))
	(with-open-file (s scroll :direction :output :if-exists :append)
	  (princ (serialize-span-line span-for-scroll) s))
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

(defun create-leaf-from-map (map name)
  (new-content-leaf name (generate-concatatext map)))

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

;; TODO passing a name is a workaround until it is calculated
(defun publish (doc leaf-name)
  (let* ((scroll-spans (doc-spans (load-and-parse local-scroll-name+)))
	 (migrated-to-scratch (migrate-scroll-spans-to-scroll-targets doc scroll-spans))
	 (map (build-map-from-scratch-spans (get-scratch-spans migrated-to-scratch)))
	 (new-leaf (create-leaf-from-map map leaf-name))
	 (fully-migrated
	  (migrate-scratch-spans-to-leaf migrated-to-scratch map (leaf-name new-leaf)))
	 (migrated-scroll-spans
	  (migrate-scroll-spans-to-leaf scroll-spans map (leaf-name new-leaf))))
    (save-leaf new-leaf)
    (save-leaf fully-migrated)
    (save-leaf (new-doc-leaf local-scroll-name+ migrated-scroll-spans nil))))

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

(defun repo-path () (cl-fad:pathname-as-directory repo-path*))

(defun name-to-path (name)
  (let* ((parts (name-parts name))
	 (sub (ecase (name-type name)
		((:scroll :local-scroll) (list "scrolls/" parts))
		(:doc (list "public/" (format nil "~{~A~^_~}" parts)))
		(:content (list "public/" (format nil "~{~A~^_~}" parts)))
		(:scratch (list "public/" (format nil "~{~A~^_~}" parts)))
		(:link (list "public/" (format nil "~{~A~^_~}" parts))))))
    (apply #'cl-fad:merge-pathnames-as-file (repo-path) sub)))

(defun init ()
  (labels ((make-file (name type)
	     (with-open-file (s (name-to-path name)
				:direction :output
				:if-exists :overwrite
				:if-does-not-exist :create)
	       (write-line (serialize-name name) s)
	       (write-line user+ s)
	       (write-line type s)
	       (write-line "-" s))))
    (ensure-directories-exist (public-path))
    (ensure-directories-exist (scrolls-path))
    (make-file local-scroll-name+ "scroll")
    (make-file scratch-name+ "scratch")))

(defun load-and-parse (name)
  (parse-vector (load-by-name name)))

(defun save-leaf (leaf)
  (cond ((content-leaf-p leaf) (save-by-name (leaf-name leaf) (serialize-content-leaf leaf)))
	((doc-p leaf) (save-by-name (leaf-name leaf) (serialize-doc leaf)))
	(T (error "Should be a leaf: ~A" leaf))))

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

(defun parse-vector (v)
  (with-input-from-string (s v)
    (let* ((sig (read-line s))
	   (type (read-line s))
	   (name (parse-name (read-line s) type))
	   (owner (read-line s))
	   (content-separator (read-line s))
	   (contents (make-fillable-string)))
      (assert (string= content-separator "-"))
      (drain s (lambda (c) (vector-push-extend c contents)))
      (if (or (string= type "doc") (string= type "scroll"))
	  (let ((spans-links (parse-doc-contents contents)))
	    (make-doc :sig sig
		      :name name
		      :owner owner
		      :type type
		      :spans (first spans-links)
		      :links (second spans-links)))
	  (make-content-leaf :sig sig
			     :name name
		             :owner owner
			     :type type
			     :contents contents)))))

(defun parse-name (name-string type)
  (let ((real-type (if (stringp type) (intern (string-upcase type) (symbol-package :foo))
		       type)))
    (make-name :type real-type
	       :parts (if (string-starts-with "scroll/" name-string)
			  (list :scroll (subseq name-string 7))
			  (mapcar #'parse-integer (split-sequence #\. name-string))))))

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
    (span (parse-name (first parts) :content)
	   (read-from-string (subseq (second parts) 6))
	  (read-from-string (subseq (third parts) 7)))))

(defun parse-link-include (link)
  (let ((parts (split-sequence #\; link :start 5)))
    (make-link :type (car parts) :endsets (mapcar #'parse-endset (cdr parts)))))

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
  (parse-name (subseq doc 4) :doc))

;;;; Serializing leaves

(defun serialize-doc (doc)
  (concatenate 'string (serialize-leaf-header doc) (serialize-doc-contents doc)))

(defun serialize-content-leaf (leaf)
  (concatenate 'string (serialize-leaf-header leaf) (content-leaf-contents leaf)))

(defun serialize-name (name)
  (if (scroll-name-p name)
      (format nil "scroll/~A" (name-parts name))
      (format nil "~{~A~^.~}" (name-parts name))))

(defun serialize-leaf-header (leaf)
  (with-output-to-string (s)
    (labels ((p (str) (format s "~A~%" str)))
      (p (leaf-sig leaf))
      (p (leaf-type leaf))
      (p (serialize-name (leaf-name leaf)))
      (p (leaf-owner leaf))
      (p "-"))))

(defun serialize-doc-contents (doc)
  (with-output-to-string (s)
    (dolist (span (doc-spans doc))
      (princ (serialize-span-line span) s))
    (dolist (link (doc-links doc))
      (format s "link:~A;~{~A~^;~}~%"
	      (link-type link)
	      (mapcar #'serialize-endset (link-endsets link))))))

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
    (let ((leaf (load-by-name (parse-name name :content))))
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
	(ensure-leaves (mapcar #'origin (doc-spans (parse-vector doc)))
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
