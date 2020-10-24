;;;; xanadu.lisp

(in-package #:xanadu)

#|
name
creator
type
-
span:address1,start=1,length=200
span:address2,start=3,length=100

link:italics;span:address1,start=20,length=10
link:title;title#span:address2,start=80,length=90;doc#doc:address
link:bold;span:address1,start=14,length=5+address2,start=10,length=5
|#

#|
(parse-vector (format nil "docname~%me~%doc~%-~%span:1.1,start=0,length=10~%span:1.2,start=2,length=3~%span:1.3,start=3,length=5~%span:1.4,start=4,length=10~%link:italics;blah#span:1.2.3.4,start=1,length=100+1.88,start=200,length=11~%link:title;span:1.99,start=0,length=10;document#doc:1.2.3.4"))

(parse-doc-contents (cdr (assoc :contents *)))
|#

(defmacro not-implemented (name args)
  `(defun ,name ,args (declare ,@ (mapcar (lambda (a) `(ignore ,a)) args))
	  (error "Function not implemented ~A" ',name)))

(defun string-starts-with (prefix string)
  (string= prefix string :end2 (length prefix)))

(defun print-name (parts)
  (format nil "~{~A~^.~}" parts))

(defun parse-name (name-string)
  (mapcar #'read-from-string (split-sequence #\. name-string)))

(defparameter repo+ "~/lisp/xanadu/test-repo")

(defun name-to-path (ns name) (format nil "~A/~A/~A" repo+ ns (print-name name)))

(defun make-fillable-string ()
  (make-array '(0) :element-type 'character :adjustable T :fill-pointer 0))

(defun load-by-name (ns name)
  (let ((file (make-fillable-string)))
    (with-open-file (s (name-to-path ns name))
      (drain s (lambda (c) (vector-push-extend c file)))
      file)))

(defun save-by-name (ns name contents)
  (with-open-file (s (name-to-path ns name) :direction :output :if-exists :supersede)
    (loop :for c :across contents :do (write-char c s))))

(defun drain (stream constructor)
  (do ((c (read-char stream nil :eof) (read-char stream nil :eof)))
	  ((eq c :eof))
	(funcall constructor c)))

(defun parse-vector (v)
  (with-input-from-string (s v)
    (let ((name (parse-name (read-line s)))
	  (creator (read-line s))
	  (type (read-line s))
	  (content-separator (read-line s))
	  (contents (make-fillable-string)))
      (assert (string= content-separator "-"))
      (drain s (lambda (c) (vector-push-extend c contents)))
      (list (cons :name name)
	    (cons :creator creator)
	    (cons :type type)
	    (cons :contents contents)))))

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
		 (list :doc (parse-doc contents)))
		(T (error "Could not understand endset '~S'" contents)))))
    (cons name parsed-contents)))

(defun parse-span-endset (span)
  (let ((nested-spans (split-sequence #\+ span :start 5)))
    (mapcar #'parse-span-section nested-spans)))

(defun parse-doc (doc)
  (parse-name (subseq doc 4)))

(defun get-doc-span-addresses (doc)
  "Returns all of the addresses contained in all the spans of a document."
  (mapcar #'car (car doc)))

(defun make-index (key-fn data)
  "Create an indexing hash table for some data, with a key derived from the data."
  (let ((hash (make-hash-table :test 'equal)))
    (dolist (d data) (setf (gethash (funcall key-fn d) hash) d))
    hash))

(defun load-all-contents (doc)
  "Create a hash table of all the contents leaves required by a doc's spans, indexed by name."
  (make-index (lambda (x) (cdr (assoc :name x)))
	      (mapcar (lambda (a) (parse-vector (load-by-name "contents" a)))
		      (get-doc-span-addresses doc))))

(defun apply-span (span contents-hash)
  "Extract the text of a span from a collection of contents leaves."
  (subseq (cdr (assoc :contents (gethash (car span) contents-hash)))
	  (second span)
	  (+ (second span) (third span))))

(defun generate-concatatext (doc contents-hash)
  (apply #'concatenate 'string
	 (mapcar (lambda (s) (apply-span s contents-hash)) (car doc))))
