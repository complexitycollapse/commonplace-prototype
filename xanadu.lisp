;;;; xanadu.lisp

(in-package #:xanadu)

(defun print-name (parts)
  (format nil "~{~A~^.~}" parts))

(defun parse-name (name-string)
  (mapcar #'read-from-string (split-sequence #\. name-string)))
