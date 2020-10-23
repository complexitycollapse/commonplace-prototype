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
(parse-vector (format nil "docname~%me~%doc~%-~%span:add1,start=11,length=66~%span:add2,start=2,length=22~%span:add3,start=3,length=33~%span:add4,start=4,length=44~%link:italics;blah#span:1234,start=1,length=100+foo,start=200,length=11~%link:title;span:add99,start=0,length=10;document#doc:doc-address"))

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

(defun name-to-path (name) (format nil "~A/~{~A~^/~}" repo+ name))

(defun ensure-directories-for-name (name)
  (ensure-directories-exist (name-to-path name)))

(defun make-fillable-string ()
  (make-array '(0) :element-type 'character :adjustable T :fill-pointer 0))

(defun load-by-name (name)
  (let ((file (make-fillable-string)))
    (with-open-file (s (name-to-path name))
      (drain s (lambda (c) (vector-push-extend c file)))
      file)))

(defun save-by-name (name contents)
  (ensure-directories-for-name name)
  (with-open-file (s (name-to-path name) :direction :output :if-exists :supersede)
    (loop :for c :across contents :do (write-char c s))))

(defun drain (stream constructor)
  (do ((c (read-char stream nil :eof) (read-char stream nil :eof)))
	  ((eq c :eof))
	(funcall constructor c)))

(defun parse-vector (v)
  (with-input-from-string (s v)
    (let ((name (read-line s))
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
  "Parses the contents part of a leaf as if it were a document returning (list spans links)."
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
    (list (first parts) ; address
	  (subseq (second parts) 6) ; start
	  (subseq (third parts) 7)))) ; length

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
  (subseq doc 4))
