;;;; xanadu.lisp

(in-package #:xanadu)

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
	  (contents (make-fillable-string)))
      (read-line s) ; this is the separator of the head from the contents
      (drain s (lambda (c) (vector-push-extend c contents)))
      (list (cons :name name)
	    (cons :creator creator)
	    (cons :type type)
	    (cons :contents contents)))))
