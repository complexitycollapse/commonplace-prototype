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
(defparameter upstream+ "http://localhost:4242/")
(defparameter test-repo+ "~/lisp/xanadu/test-repo")
(defparameter user+ "Me")

(defvar acceptor* nil)
(defvar http-stream*) ; used by the HTTP client to represent an open connection
(defvar repo-path* (sb-posix:getcwd)) ; defaults to current directory

(defstruct leaf name owner type)
(defstruct (content-leaf (:include leaf)) contents)
(defstruct (doc (:include leaf)) spans links)
(defstruct span origin start length)

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
  (make-content-leaf :name name :owner user+ :type type :contents text))

(defun new-doc-leaf (name type)
  (make-doc :name name :owner user+ :type type :spans nil :links nil))

(defun span (origin start length) (make-span :origin origin :start start :length length))

(defun create-content-from-file (name type path)
  "Import text from a file"
  (let ((contents (make-fillable-string)))
    (with-open-file (s path)
      (loop for c = (read-char s nil)
	 while c
	 do (vector-push-extend c contents)))
    (new-content-leaf name type contents)))

(defun parse-vector (v)
  (with-input-from-string (s v)
    (let ((name (parse-name (read-line s)))
	  (owner (read-line s))
	  (type (read-line s))
	  (content-separator (read-line s))
	  (contents (make-fillable-string)))
      (assert (string= content-separator "-"))
      (drain s (lambda (c) (vector-push-extend c contents)))
      (if (or (string= type "doc") (string= type "scroll"))
	  (let ((spans-links (parse-doc-contents contents)))
	    (make-doc :name name
		      :owner owner
		      :type type
		      :spans (first spans-links)
		      :links (second spans-links)))
	  (make-content-leaf :name name
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
    (cons (car parts) (mapcar #'parse-endset (cdr parts)))))

(defun parse-endset (endset)
  (let* ((name-and-contents (split-sequence #\# endset))
	 (name (if (cdr name-and-contents) (car name-and-contents)))
	 (contents (if (cdr name-and-contents) (cadr name-and-contents)
		       (car name-and-contents)))
	 (parsed-contents
	  (cond ((string-starts-with "span:" contents)
                 (list :span (parse-span-endset contents)))
		((string-starts-with "doc:" contents)
                 (list :doc (parse-doc-ref contents)))
		(T (error "Could not understand endset '~S'" contents)))))
    (cons name parsed-contents)))

(defun parse-span-endset (span)
  (let ((nested-spans (split-sequence #\+ span :start 5)))
    (mapcar #'parse-span-section nested-spans)))

(defun parse-doc-ref (doc)
  (parse-name (subseq doc 4)))

(defun get-doc-span-addresses (doc)
  "Returns all of the addresses contained in all the spans of a document."
  (mapcar #'span-origin (doc-spans doc)))

(defun make-index (key-fn data)
  "Create an indexing hash table for some data, with a key derived from the data."
  (let ((hash (make-hash-table :test 'equal)))
    (dolist (d data) (setf (gethash (funcall key-fn d) hash) d))
    hash))

(defun load-all-contents (doc)
  "Create a hash table of all the contents leaves required by a doc's spans, indexed by name."
  (let* ((load-local-scroll nil)
	 (leaves (mapcar (lambda (a)
			   (if (equal a local-scroll-name+) (setf load-local-scroll T))
			   (parse-vector (load-by-name a)))
			 (get-doc-span-addresses doc))))
    (if load-local-scroll (push (parse-vector (load-by-name scratch-name+)) leaves))
    (make-index #'leaf-name leaves)))

(defun apply-span (span contents-hash)
  "Extract the text of a span from a collection of contents leaves."
  (let ((contents (gethash (span-origin span) contents-hash)))
    (if (scroll-name-p (span-origin span))
	(generate-concatatext-clip contents contents-hash (span-start span) (span-length span))
	(subseq (content-leaf-contents contents)
		(span-start span)
		(+ (span-start span) (span-length span))))))

(defun generate-concatatext (doc contents-hash)
  (apply #'concatenate 'string
	 (mapcar (lambda (s) (apply-span s contents-hash)) (doc-spans doc))))

(defun generate-concatatext-clip (doc contents-hash start length)
    (apply #'concatenate 'string
	 (mapcar (lambda (s) (apply-span s contents-hash))
		 (extract-range-from-spans (doc-spans doc) start length))))

(defun serialize-leaf-header (leaf)
  (with-output-to-string (s)
    (labels ((p (str) (format s "~A~%" str)))
      (p (print-name (leaf-name leaf)))
      (p (leaf-owner leaf))
      (p (leaf-type leaf))
      (p "-"))))

(defun serialize-content-leaf (leaf)
  (concatenate 'string (serialize-leaf-header leaf) (content-leaf-contents leaf)))

(defun serialize-doc-contents (doc)
  (with-output-to-string (s)
    (dolist (span (doc-spans doc))
      (format s "span:~A~%"(serialize-span-section span)))
    (dolist (link (doc-links doc))
      (format s "link:~A;~{~A~^;~}~%" (car link) (mapcar #'serialize-endset (cdr link))))))

(defun serialize-doc (doc)
  (concatenate 'string (serialize-leaf-header doc) (serialize-doc-contents doc)))

(defun serialize-endset (endset)
  (with-output-to-string (s)
    (if (car endset) (format s "~A#" (car endset)))
    (if (string= (cadr endset) "DOC") (format s "doc:~A" (print-name (third endset)))
	(format s "span:~{~A~^+~}" (mapcar #'serialize-span-section (third endset))))))

(defun serialize-span-section (section)
  (format nil "~A,start=~A,length=~A"
	  (print-name (span-origin section)) (span-start section) (span-length section)))

(defun save-contents (leaf)
  (save-by-name (leaf-name leaf) (serialize-content-leaf leaf)))

(defun save-doc (doc)
  (save-by-name (doc-name doc) (serialize-doc doc)))

(defun append-to-local-scroll (content)
  "Append some content to the local private scroll and return the span representing it."
  (let ((pathname (uiop:native-namestring (name-to-path local-scroll-name+))))
    (let ((start (osicat-posix:stat-size (osicat-posix:stat pathname))))
      (with-open-file (s pathname :direction :output :if-exists :append)
	(princ content s))
      (list (list :scroll "local") start (length content)))))

(defun insert-spans (spans new-spans n)
  "Insert a list of spans into the middle of some existing spans."
  (let ((split (divide-spans spans 0 n)))
    (append (first split) new-spans (second split))))

(defun adjust-span-length (span length)
  (span (span-origin span) (span-start span) length))

(defun adjust-span-start (span start-adjust)
  (span (span-origin span)
	(+ (span-start span) start-adjust)
	(- (span-length span) start-adjust)))

(defun extract-range-from-spans (spans start length)
  "Create spans representing the subset of some other spans delimited by start and length."
  (second (divide-spans spans 0 start (+ start length))))

(defun attempt-fuse (spans)
  "Join adjacent spans into a single span if possible."
  (let ((1st (car spans))
	(2nd (cadr spans)))
    (cond ((null 1st) nil)
	  ((null 2nd) spans)
	  ((and (equal (span-origin 1st) (span-origin 2nd))
		(= (span-start 2nd) (+ (span-start 1st) (span-length 1st))))
	   (cons
	    (list (span-origin 1st) (span-start 1st) (+ (span-length 1st) (span-length 2nd)))
	    (attempt-fuse (cddr spans))))
	  (T (cons 1st (attempt-fuse (cdr spans)))))))

(defun transclude (source-spans start length target-spans insert-point)
  "Transclude content from one set of spans into another."
  (attempt-fuse
   (insert-spans target-spans
		 (extract-range-from-spans source-spans start length) insert-point)))

(defun delete-content (spans start length)
  "Remove a section from some spans."
  (let ((divided (divide-spans spans 0 start (+ start length))))
    (append (first divided) (third divided))))

(defun divide-spans-by-one-point (spans division-point)
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
				 (before (adjust-span-length span n))
				 (push (adjust-span-start span n) spans))))))
	      (recur division-point)))))
    (list before spans)))

(defun divide-spans (spans 1st-division-point &rest other-division-points)
  "Divide a list of spans into n lists at the division points."
  (if (or (endp spans) (endp other-division-points))
      (list spans)
      (let* ((p (car other-division-points))
	     (division (divide-spans-by-one-point spans (- p 1st-division-point))))
	(cons (first division)
	      (apply #'divide-spans (second division) p (cdr other-division-points))))))

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
	(ensure-leaves (get-doc-span-addresses (parse-vector doc)) force-download)))))

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
