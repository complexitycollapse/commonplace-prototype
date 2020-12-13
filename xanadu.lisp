;;;; xanadu.lisp

(in-package #:xanadu)

#|
name
creator
type
-
span:address1,start=1,length=200
span:address2,start=3,length=100
span:scroll/local,start=10235,length=40

link:italics;span:address1,start=20,length=10
link:title;title#span:address2,start=80,length=90;doc#doc:address
link:bold;span:address1,start=14,length=5+address2,start=10,length=5
|#

#|
(setf foo (parse-vector (format nil "2.1~%me~%doc~%-~%span:1.1,start=0,length=10~%span:1.2,start=2,length=3~%span:1.3,start=3,length=5~%span:1.4,start=4,length=10~%span:scroll/local,start=4,length=4~%link:italics;blah#span:1.2.3.4,start=1,length=100+1.88,start=200,length=11~%link:title;span:1.99,start=0,length=10;document#doc:1.2.3.4~%")))
|#

(defparameter local-scroll-name+ '(:scroll "local"))
(defparameter scratch-name+ '(0))
(defparameter editable-signature+ "EDITABLE")
(defparameter upstream+ "http://localhost:4242/")
(defparameter test-repo+ "~/lisp/xanadu/test-repo")
(defparameter user+ "Me")

(defvar acceptor* nil)
(defvar http-stream*) ; used by the HTTP client to represent an open connection
(defvar repo-path* (sb-posix:getcwd)) ; defaults to current directory

(defstruct leaf name owner type sig)
(defstruct (content-leaf (:include leaf)) contents)
(defstruct (doc (:include leaf)) spans links)
(defstruct span origin start length)
(defstruct link type endsets)
(defstruct endset name)
(defstruct (span-endset (:include endset)) spans)
(defstruct (doc-endset (:include endset)) doc-name)

(defmacro not-implemented (name args)
  `(defun ,name ,args (declare ,@ (mapcar (lambda (a) `(ignore ,a)) args))
	  (error "Function not implemented ~A" ',name)))

(defmacro probe (form)
  (with-unique-names (val)
    `(let ((,val ,form)) (format T ,(format nil "~A: ~~A~%" form) ,val) ,val)))

(defun string-starts-with (prefix string)
  (and (>= (length string) (length prefix))
       (string= prefix string :end2 (length prefix))))

(defun merge-url (&rest parts)
  (if (endp parts) (return-from merge-url ""))
  (let ((part (car parts)))
    (if (string-starts-with "/" part) (setf part (subseq part 1)))
    (let ((last (1- (length part))))
      (if (string= #\/ (elt part last)) (setf part (subseq part 0 last))))
    (concatenate 'string part (if (cdr parts) "/" "") (apply #'merge-url (cdr parts)))))

;;; Repo management

(defun set-test-repo ()
  (setf repo-path* test-repo+))

(defun to-file-name (parts)
  (if (scroll-name-p parts)
      (cadr parts)
      (format nil "~{~A~^_~}" parts)))

(defun parse-name (name-string)
  (if (string-starts-with "scroll/" name-string)
      (list :scroll (subseq name-string 7))
      (mapcar #'parse-integer (split-sequence #\. name-string))))

(defun scrolls-path ()
  (cl-fad:merge-pathnames-as-directory (cl-fad:pathname-as-directory repo-path*) "scrolls/"))

(defun public-path ()
  (cl-fad:merge-pathnames-as-directory (cl-fad:pathname-as-directory repo-path*) "public/"))

(defun name-to-path (name)
  (let ((subdirectory (if (scroll-name-p name) (scrolls-path) (public-path))))
    (cl-fad:merge-pathnames-as-file subdirectory (to-file-name name))))

(defun init ()
  (labels ((make-file (name type)
	     (with-open-file (s (name-to-path name)
				:direction :output
				:if-exists :overwrite
				:if-does-not-exist :create)
	       (write-line (print-name name) s)
	       (write-line user+ s)
	       (write-line type s)
	       (write-line "-" s))))
    (ensure-directories-exist (public-path))
    (ensure-directories-exist (scrolls-path))
    (make-file local-scroll-name+ "scroll")
    (make-file scratch-name+ "scratch")))

(defun load-by-name (name)
  (let ((file (make-fillable-string)))
    (with-open-file (s (name-to-path name) :if-does-not-exist nil)
      (if (null s) (return-from load-by-name nil))
      (drain s (lambda (c) (vector-push-extend c file)))
      file)))

(defun save-by-name (name contents)
  (with-open-file (s (name-to-path name) :direction :output :if-exists :supersede)
    (princ contents s)))

;;; Leaf processing

(defun scroll-name-p (parts) (and (listp parts) (eq (car parts) :scroll)))
(defun editable-p (leaf) (equal editable-signature+ (leaf-sig leaf)))

(defun print-name (parts)
  (if (scroll-name-p parts)
      (format nil "scroll/~A" (cadr parts))
      (format nil "~{~A~^.~}" parts)))

(defun make-fillable-string ()
  (make-array '(0) :element-type 'character :adjustable T :fill-pointer 0))

(defun drain (stream constructor)
  (do ((c (read-char stream nil :eof) (read-char stream nil :eof)))
	  ((eq c :eof))
	(funcall constructor c)))

(defun new-content-leaf (name type text)
  (make-content-leaf :name name :owner user+ :sig "SIG" :type type
		     :contents text))

(defun new-doc-leaf (name &optional spans links)
  (make-doc :name name :owner user+ :sig editable-signature+ :type "doc"
	    :spans spans :links links))

(defun span (origin start length) (make-span :origin origin :start start :length length))

(defun create-content-from-file (name type path)
  "Import text from a file"
  (let ((contents (make-fillable-string)))
    (with-open-file (s path)
      (loop for c = (read-char s nil)
	 while c
	 do (vector-push-extend c contents)))
    (new-content-leaf name type contents)))

(defun get-next-version-name (old-name)
  (nconc (butlast old-name) (list (1+ (car (last old-name))))))

(defun new-version (doc &optional (new-name (get-next-version-name (leaf-name doc))))
  (new-doc-leaf new-name (doc-spans doc) (doc-links doc)))

(defun load-and-parse (name)
  (parse-vector (load-by-name name)))

(defun parse-vector (v)
  (with-input-from-string (s v)
    (let ((sig (read-line s))
	  (name (parse-name (read-line s)))
	  (owner (read-line s))
	  (type (read-line s))
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
  (parse-name (subseq doc 4)))

(defun load-all-contents (doc &optional (index (make-hash-table :test 'equal)))
  (dolist (a (mapcar #'span-origin (doc-spans doc)))
    (when (not (nth-value 1 (gethash a index)))
      (setf (gethash a index) (load-and-parse a))
      (if (scroll-name-p a) (load-all-contents (gethash a index) index))))
  index)

(defun apply-span (span contents-hash)
  "Extract the text of a span from a collection of contents leaves."
  (let ((contents (gethash (span-origin span) contents-hash)))
    (if (scroll-name-p (span-origin span))
	(generate-concatatext-clip contents (span-start span) (span-length span) contents-hash)
	(subseq (content-leaf-contents contents) (span-start span) (span-end span)))))

(defun generate-concatatext (doc &optional (contents-hash (if (doc-p doc) (load-all-contents doc))))
  (apply #'concatenate 'string
	 (mapcar (lambda (s) (apply-span s contents-hash))
		 (if (listp doc) doc (doc-spans doc)))))

(defun generate-concatatext-clip (doc start length
				  &optional (contents-hash (load-all-contents doc)))
    (apply #'concatenate 'string
	 (mapcar (lambda (s) (apply-span s contents-hash))
		 (extract-range-from-spans (doc-spans doc) start length))))

(defun serialize-leaf-header (leaf)
  (with-output-to-string (s)
    (labels ((p (str) (format s "~A~%" str)))
      (p (leaf-sig leaf))
      (p (print-name (leaf-name leaf)))
      (p (leaf-owner leaf))
      (p (leaf-type leaf))
      (p "-"))))

(defun serialize-content-leaf (leaf)
  (concatenate 'string (serialize-leaf-header leaf) (content-leaf-contents leaf)))

(defun serialize-doc-contents (doc)
  (with-output-to-string (s)
    (dolist (span (doc-spans doc))
      (princ (serialize-span-line span) s))
    (dolist (link (doc-links doc))
      (format s "link:~A;~{~A~^;~}~%"
	      (link-type link)
	      (mapcar #'serialize-endset (link-endsets link))))))

(defun serialize-doc (doc)
  (concatenate 'string (serialize-leaf-header doc) (serialize-doc-contents doc)))

(defun serialize-endset (endset)
  (with-output-to-string (s)
    (if (endset-name endset) (format s "~A#" (endset-name endset)))
    (if (doc-endset-p endset) (format s "doc:~A" (print-name (doc-endset-doc-name endset)))
	(format s "span:~{~A~^+~}"
		(mapcar #'serialize-span-section (span-endset-spans endset))))))

(defun serialize-span-line (span)
  (format nil "span:~A~%" (serialize-span-section span)))

(defun serialize-span-section (section)
  (format nil "~A,start=~A,length=~A"
	  (print-name (span-origin section)) (span-start section) (span-length section)))

(defun save-contents (leaf)
  (save-by-name (leaf-name leaf) (serialize-content-leaf leaf)))

(defun save-doc (doc)
  (save-by-name (doc-name doc) (serialize-doc doc)))

;;;; Scrolls

(defun append-to-local-scroll (content)
  "Append some content to the local private scroll and return the span representing it."
  (let ((scratch (uiop:native-namestring (name-to-path scratch-name+)))
	(scroll (uiop:native-namestring (name-to-path local-scroll-name+)))
	(length (length content))
	(scratch-contents (content-leaf-contents (parse-vector (load-by-name scratch-name+)))))
    (let* ((span-for-scroll (span scratch-name+ (length scratch-contents) length)))
      (with-open-file (s scratch :direction :output :if-exists :append)
	(princ content s))
      (let ((scroll-position (get-next-local-scroll-pos)))
	(with-open-file (s scroll :direction :output :if-exists :append)
	  (princ (serialize-span-line span-for-scroll) s))
	(span local-scroll-name+ scroll-position length)))))

(defun get-next-local-scroll-pos ()
  (apply #'+ (mapcar #'span-length
		     (doc-spans (parse-vector (load-by-name local-scroll-name+))))))

#|
DONE create the new leaf
DONE Figure out how docs are stored until they are published
DONE Convert doc to published one
DONE Rewrite the scroll so that it points to the new leaves
What about scroll spans in the document that have already been mograted?
|#

;; TODO passing a name is a workaround until it is calculated
(defun publish (doc contents-name)
  (let* ((map (create-leaf-map doc))
	 (content (get-scroll-content-for-map map))
	 (final-doc (migrate-doc-spans-to-permanent-contents doc contents-name map))
	 (scroll (load-and-parse local-scroll-name+))
	 (final-scroll (migrate-scroll-spans-to-permanent-contents
			(doc-spans scroll) map contents-name)))

    ;; Create the new contents leaf
    (save-by-name contents-name
		  (serialize-content-leaf (new-content-leaf contents-name "text" content)))

    ;; Create the finalized document
    (setf (leaf-sig final-doc) "SIG")
    (save-by-name (doc-name doc) (serialize-doc final-doc))

    ; Rewrite the scroll to point at the leaf
    (setf (doc-spans scroll) final-scroll)
    (save-by-name local-scroll-name+ (serialize-doc scroll))))

(defun create-leaf-map (doc)
  (merge-all-map-duplicates (get-all-scroll-spans doc)))

(defun get-all-scroll-spans (doc)
  (collecting (iterate-spans doc
			     (lambda (s) (if (equal (span-origin s) local-scroll-name+)
					     (collect s))))))

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
						(mapcar new-span-fn (span-endset-spans e)))
					  newe)
					e))
				  (link-endsets l))))
	     (setf (link-endsets new) endsets)
	     (links new)))))
    (new-doc-leaf (doc-name doc) clips links)))

(defun merge-all-map-duplicates (spans)
  "Take a list spans from a map and merge all mergeable spans"
  (labels ((match (span others)
	     (if (endp others) (values span nil nil)
		 (multiple-value-bind (spans merged) (merge-adjacent-spans span (car others))
		   (if merged
		       (values (car spans) (cdr others) T)
		       (multiple-value-bind (new-span other-spans changed)
			   (match span (cdr others))
			 (values new-span (cons (car others) other-spans) changed)))))))
    (if (endp spans) nil
	(multiple-value-bind (new-span new-spans changed)
	    (match (car spans) (cdr spans))
	  (if changed (merge-all-map-duplicates (cons new-span new-spans))
	      (cons (car spans) (merge-all-map-duplicates (cdr spans))))))))

(defun get-scroll-content-for-map (map)
  "Returns the content referenced by a map"
  (let* ((scroll (load-and-parse local-scroll-name+))
	(index (load-all-contents scroll)))
    (apply
     #'concatenate
     'string
     (collecting
       (dolist (span (remove-if-not #'scroll-name-p map :key #'span-origin))
	 (collect (generate-concatatext-clip
		   scroll (span-start span) (span-length span) index)))))))

(defun migrate-doc-spans-to-permanent-contents (doc new-origin-name map)
  (replace-spans
   doc
   (lambda (s) (if (scroll-name-p (span-origin s))
		   (let ((pos 0))
		     (block nil
		       (dolist (m map)
			 (if (span-contains m (span-start s))
			     (return (span new-origin-name
					   (+ pos (- (span-start s) (span-start m)))
					   (span-length s))))
			 (incf pos (span-length m)))))
		   s))))

(defun get-leaf-offset-from-map (point map &optional (offset 0))
  "Find the position in the new leaf that corresponds to an old scroll position"
  (let ((span (car map)))
    (if (and (>= point (span-start span)) (< point (span-end span)))
	(+ offset (- point (span-start span)))
	(get-leaf-offset-from-map point (cdr map) (+ offset (span-length span))))))

(defun merge-adjacent-spans (1st-span 2nd-span)
  "Merge two spans if they have the same origin and overlap or are adjacent"
  (let* ((1st (if (<= (span-start 1st-span) (span-start 2nd-span)) 1st-span 2nd-span))
	 (2nd (if (eq 1st 2nd-span) 1st-span 2nd-span)))
    (if (<= (span-start 2nd) (span-end 1st))
	(values (list (adjust-span-length 1st (- (max (span-end 1st) (span-end 2nd))
						 (span-start 1st))))
		T)
	(values (list 1st 2nd) nil))))

(defun migrate-scroll-spans-to-permanent-contents (spans map leaf-name)
  "Repoint a scroll's spans from scratch to a new leaf described by map"
  (labels ((over-scroll (spans pos)
	     (cond ((endp spans) nil)
		   ((null (car spans)) (over-scroll (cdr spans) pos))
		   ((not (equal (span-origin (car spans)) '(0)))
		    (cons (car spans) (over-scroll (cdr spans)
						   (+ pos (span-length (car spans))))))
		   (T (let ((new (remap-scroll-span (car spans) pos map 0 leaf-name)))
			(cons (car new) (over-scroll (append (cdr new) (cdr spans))
						     (+ pos (span-length (car new))))))))))
    (over-scroll spans 0)))

(defun remap-scroll-span (span pos map mpos leaf-name)
  "Repoint an individual scoll span from scratch to a new leaf. Returns a list of spans, the
first of which is rewritten and the second (if any) is the unrewritten remainder."
  (if (endp map) (return-from remap-scroll-span (list span)))
  (let* ((m (car map))
	 (epos (+ pos (span-length span))) ; scroll space position of the end of span
	 (ms (span-start m))
	 (mes (span-end m)))
    (cond
      ((and (< pos ms) (>= epos ms))
       (split-span span (- ms pos)))
      ((and (>= pos ms) (<= epos mes))
       (list (span leaf-name (+ mpos (- pos ms)) (span-length span))))
      ((and (>= pos ms) (< pos mes))
       (let ((split (split-span span (- mes pos))))
	 (list (span leaf-name (+ mpos (- pos ms)) (span-length (car split)))
	       (cadr split))))
      (T (remap-scroll-span span pos (cdr map) (+ mpos (span-length m)) leaf-name)))))

;; Span operations

(defun span-end (span)
  (+ (span-start span) (span-length span)))

(defun span-contains (span point)
  (let ((offset (- point (span-start span))))
    (and (>= offset 0) (< offset (span-length span)))))

(defun adjust-span-length (span length)
  (span (span-origin span) (span-start span) length))

(defun adjust-span-start (span start-adjust)
  (span (span-origin span)
	(+ (span-start span) start-adjust)
	(- (span-length span) start-adjust)))

(defun split-span (span length-of-first)
  (list (adjust-span-length span length-of-first)
	(adjust-span-start span length-of-first)))

(defun divide-spans (spans division-point)
  "Divide a list of spans into two lists at the given division point."
  (let* ((spans spans)
	 (before
	  (with-collectors (before)
	    (labels ((recur (n)
		       (let* ((span (car spans))
			      (len (if span (span-length span) 0)))
			 (cond ((null spans) nil)
			       ((>= n len) (before span) (pop spans) (recur (- n len)))
			       ((zerop n) nil)
			       (T (pop spans)
				  (let ((split (split-span span n)))
				    (before (car split))
				    (push (cadr split) spans)))))))
	      (recur division-point)))))
    (list before spans)))

(defun divide-out-section (spans start length)
  "Divide a list of spans into three lists"
  (let ((split (divide-spans spans start)))
    (cons (car split) (divide-spans (cadr split) length))))

(defun merge-spans (1st-list 2nd-list)
  "Join two lists of spans into one, merging spans if necessary"
  (let ((1st (car 1st-list)) (2nd (car 2nd-list)))
    (cond ((null 1st) 2nd-list)
	  ((endp (cdr 1st-list))
	   (if (and 2nd (equal (span-origin 1st) (span-origin 2nd))
		    (= (span-start 2nd) (span-end 1st)))
	       (cons (span (span-origin 1st) (span-start 1st) (+ (span-length 1st)
								 (span-length 2nd)))
		     (cdr 2nd-list))
	       (cons 1st-list 2nd-list)))
	  (T (cons 1st (merge-spans (cdr 1st-list) 2nd-list))))))

(defun merge-spans-twice (1st-list 2nd-list 3rd-list)
  (merge-spans 1st-list (merge-spans 2nd-list 3rd-list)))

(defun extract-range-from-spans (spans start length)
  "Create spans representing the subset of some other spans delimited by start and length."
  (second (divide-out-section spans start length)))

(defun insert-spans (spans new-spans n)
  "Insert a list of spans into the middle of some existing spans."
  (let ((split (divide-spans spans n)))
    (merge-spans-twice (car split) new-spans (cadr split))))

(defun delete-spans (spans start length)
  "Remove a section from some spans."
  (let ((divided (divide-out-section spans start length)))
    (append (first divided) (third divided))))

(defun transclude (source-spans start length target-spans insert-point)
  "Transclude content from one set of spans into another."
  (insert-spans target-spans
		(extract-range-from-spans source-spans start length)
		insert-point))

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
    (let ((leaf (load-by-name (parse-name name))))
      (if (null leaf) (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+))
      leaf)))

;;; Client

(defmacro with-http-client (&body body)
  `(let ((http-stream* (if (boundp 'http-stream*) http-stream* nil)))
     ,@body))

(defun leaf-missing (name)
  (not (probe-file (name-to-path name))))

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
  "Download a doc and all leaves required to make up its spans, returns those that weren't found"
  (with-http-client
    (let ((doc (ensure-leaf doc-name T force-download)))
      (when doc
	(ensure-leaves (mapcar #'span-origin (doc-spans (parse-vector doc)))
		       force-download)))))

(defun get-leaf-from-server (name keep-alive)
  (multiple-value-bind (body status-code headers uri new-stream must-close reason-phrase)
      (drakma:http-request
       (merge-url upstream+ "leaf")
	:parameters (list (cons "name" (print-name name)))
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
