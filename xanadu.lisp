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
(setf foo (parse-vector (format nil "1.1~%me~%doc~%-~%span:1.1,start=0,length=10~%span:1.2,start=2,length=3~%span:1.3,start=3,length=5~%span:1.4,start=4,length=10~%span:scroll/local,start=4,length=4~%link:italics;blah#span:1.2.3.4,start=1,length=100+1.88,start=200,length=11~%link:title;span:1.99,start=0,length=10;document#doc:1.2.3.4")))
|#

(defparameter local-scroll-name+ '(:scroll "local"))

(defstruct leaf name owner type)
(defstruct (content-leaf (:include leaf)) contents)
(defstruct (doc (:include leaf)) spans links)

(defmacro not-implemented (name args)
  `(defun ,name ,args (declare ,@ (mapcar (lambda (a) `(ignore ,a)) args))
	  (error "Function not implemented ~A" ',name)))

(defmacro probe (form)
  (with-unique-names (val)
    `(let ((,val ,form)) (format T ,(format nil "~A: ~~A~%" form) ,val) ,val)))

(defun string-starts-with (prefix string)
  (and (>= (length string) (length prefix))
       (string= prefix string :end2 (length prefix))))

(defun scroll-name-p (parts) (and (listp parts) (eq (car parts) :scroll)))

(defun print-name (parts)
  (if (scroll-name-p parts)
      (format nil "scroll/~A" (cadr parts))
      (format nil "~{~A~^.~}" parts)))

(defun parse-name (name-string)
  (if (string-starts-with "scroll/" name-string)
      (list :scroll (subseq name-string 7))
      (mapcar #'parse-integer (split-sequence #\. name-string))))

(defparameter repo+ "~/lisp/xanadu/test-repo")

(defun name-to-path (ns name)
  (if (scroll-name-p name)
      (format nil "~A/~A/~A" repo+ "scrolls" (cadr name))
      (format nil "~A/~A/~A" repo+ ns (print-name name))))

(defun make-fillable-string ()
  (make-array '(0) :element-type 'character :adjustable T :fill-pointer 0))

(defun load-by-name (ns name)
  (let ((file (make-fillable-string)))
    (with-open-file (s (name-to-path ns name))
      (drain s (lambda (c) (vector-push-extend c file)))
      file)))

(defun save-by-name (ns name contents)
  (with-open-file (s (name-to-path ns name) :direction :output :if-exists :supersede)
    (princ contents s)))

(defun drain (stream constructor)
  (do ((c (read-char stream nil :eof) (read-char stream nil :eof)))
	  ((eq c :eof))
	(funcall constructor c)))

(defun parse-vector (v)
  (with-input-from-string (s v)
    (let ((name (parse-name (read-line s)))
	  (owner (read-line s))
	  (type (read-line s))
	  (content-separator (read-line s))
	  (contents (make-fillable-string)))
      (assert (string= content-separator "-"))
      (drain s (lambda (c) (vector-push-extend c contents)))
      (if (string= type "doc")
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
		 (multiple-value-bind (line end) (read-line s)
		   (cond ((string-starts-with "span:" line) (spans (parse-single-span line)))
			 ((string-starts-with "link:" line) (links (parse-link line)))
			 (T (error "Could not understand line '~S'" line)))
		   (if end (return))))))
      (list spans links))))

(defun split-span-into-parts (span)
  (split-sequence #\space span))

(defun parse-single-span (span)
  (parse-span-section (subseq span 5)))

(defun parse-span-section (section)
    (let ((parts (split-sequence #\, section)))
    (assert (= (length parts) 3))
    (assert (string-starts-with "start=" (second parts)))
    (assert (string-starts-with "length=" (third parts)))
    (list (parse-name (first parts)) ; address
	  (read-from-string (subseq (second parts) 6)) ; start
	  (read-from-string (subseq (third parts) 7))))) ; length

(defun parse-link (link)
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
  (mapcar #'car (doc-spans doc)))

(defun make-index (key-fn data)
  "Create an indexing hash table for some data, with a key derived from the data."
  (let ((hash (make-hash-table :test 'equal)))
    (dolist (d data) (setf (gethash (funcall key-fn d) hash) d))
    hash))

(defun load-all-contents (doc)
  "Create a hash table of all the contents leaves required by a doc's spans, indexed by name."
  (make-index (lambda (x) (leaf-name x))
	      (mapcar (lambda (a) (parse-vector (load-by-name "contents" a)))
		      (get-doc-span-addresses doc))))

(defun apply-span (span contents-hash)
  "Extract the text of a span from a collection of contents leaves."
  (subseq (content-leaf-contents (gethash (car span) contents-hash))
	  (second span)
	  (+ (second span) (third span))))

(defun generate-concatatext (doc contents-hash)
  (apply #'concatenate 'string
	 (mapcar (lambda (s) (apply-span s contents-hash)) (doc-spans doc))))

(defun serialize-leaf-header (leaf)
  (with-output-to-string (s)
    (princ (print-name (leaf-name leaf)))
    (princ (leaf-owner leaf))
    (princ (leaf-type leaf))
    (princ "-")))

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
	  (print-name (car section)) (second section) (third section)))

(defun save-contents (leaf)
  (save-by-name "contents" (leaf-name leaf) (serialize-content-leaf leaf)))

(defun save-doc (doc)
  (save-by-name "docs" (doc-name doc) (serialize-doc doc)))

(defun append-to-local-scroll (content)
  "Append some content to the local private scroll and return the span representing it."
  (let ((pathname (uiop:native-namestring (name-to-path "scrolls" local-scroll-name+))))
    (let ((start (osicat-posix:stat-size (osicat-posix:stat pathname))))
      (with-open-file (s pathname :direction :output :if-exists :append)
	(princ content s))
      (list (list :scroll "local") start (length content)))))

(defun transclude-spans (transclusion-spans insert-point target-spans)
  "Transclude some material, represented by some spans, into a list of spans."
  (attempt-fuse (rewrite-spans target-spans transclusion-spans insert-point)))

(defun rewrite-spans (spans new-spans n)
  (let ((span (car spans)))
    (cond ((zerop n) (append new-spans spans))
	  ((endp span) (error "Attempt to insert past end."))
	  ((>= n (third span))
	   (cons span (rewrite-spans (cdr spans) new-spans (- n (third span)))))
	  (T (cons (adjust-span-length span n)
		   (append new-spans (cons (adjust-span-start span n) (cdr spans))))))))

(defun adjust-span-length (span length)
  (list (first span) (second span) length))

(defun adjust-span-start (span start-adjust)
  (list (first span) (+ (second span) start-adjust) (- (third span) start-adjust)))

(defun extract-spans (spans start length)
  "Create spans representing the subset of some other spans delimited by start and length."
  (labels ((starting (spans n)
	     (let* ((span (car spans))
		    (len (third span)))
	       (cond ((null span) (error "Start is past end of document."))
		     ((>= n len) (starting (cdr spans) (- n len)))
		     (T (ending (cons (adjust-span-start span n) (cdr spans)) length)))))
	   (ending (spans n)
	     (let* ((span (car spans))
		    (len (third span)))
	       (cond ((zerop n) nil)
		     ((null span) (error "Attempt to extract content past end."))
		     ((>= n len) (cons span (ending (cdr spans) (- n len))))
		     (T (list (adjust-span-length span n)))))))
    (starting spans start)))

(defun attempt-fuse (spans)
  "Join adjacent spans into a single span if possible."
  (let ((1st (car spans))
	(2nd (cadr spans)))
    (cond ((null 1st) nil)
	  ((null 2nd) spans)
	  ((and (equal (car 1st) (car 2nd))
		(= (second 2nd) (+ (second 1st) (third 1st))))
	   (cons (list (car 1st) (second 1st) (+ (third 1st) (third 2nd)))
		 (attempt-fuse (cddr spans))))
	  (T (cons 1st (attempt-fuse (cdr spans)))))))

(defun transclude (source start length target insert-point)
  "Transclude content from one document into another, returning the new spans."
  (transclude-spans (extract-spans (doc-spans source) start length)
		    insert-point
		    (doc-spans target)))

(defun delete-content (doc start length)
  "Create spans representing the content of part of a document excluding start and length."
  (labels ((starting (spans n)
	     (let* ((span (car spans))
		    (len (third span)))
	       (cond ((null span) nil)
		     ((>= n len) (cons span (starting (cdr spans) (- n len))))
		     ((zerop n) (start-ending spans n))
		     (T (cons (adjust-span-length span n)
			      (start-ending spans n))))))
	   (start-ending (spans n)
	     (ending (cons (adjust-span-start (car spans) n) (cdr spans)) length))
	   (ending (spans n)
	     (let* ((span (car spans))
		    (len (third span)))
	       (cond ((zerop n) spans)
		     ((null span) nil)
		     ((>= n len) (ending (cdr spans) (- n len)))
		     (T (cons (adjust-span-start span n) (cdr spans)))))))
    (starting (doc-spans doc) start)))
