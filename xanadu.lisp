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

(defun load-by-name (name)
  (let ((file (make-array '(0) :element-type 'character :adjustable T :fill-pointer 0)))
    (with-open-file (s (name-to-path name))
      (do ((c (read-char s nil :eof) (read-char s nil :eof)))
	  ((eq c :eof) file)
	(vector-push-extend c file)))))

(defun save-by-name (name contents)
  (ensure-directories-for-name name)
  (with-open-file (s (name-to-path name) :direction :output :if-exists :supersede)
    (loop :for c :across contents :do (write-char c s))))
