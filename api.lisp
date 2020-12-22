;;;; api.lisp

(in-package #:cla)

(defun new-doc (name)
  (if (not (leaf-missing name)) (error "There already exists a leaf called ~A" name))
  (save-leaf (new-doc-leaf name)))

(defun append-text (name text)
  (modify-spans name (lambda (spans) (pushend (append-to-local-scroll text) spans) spans)))

(defun insert-text (name point text)
  (modify-spans name (lambda (spans)
		       (insert-spans spans (list (append-to-local-scroll text)) point))))

(defun delete-text (name start length)
  (modify-spans name (lambda (spans) (delete-spans spans start length))))

(defun move-text (name start length new-pos)
  (modify-spans name (lambda (spans) (move-spans spans start length new-pos))))

(defun transclude (destination-name insert-point source-name start length)
  (modify-spans destination-name
		(lambda (target-spans)
		  (let ((source (safe-load-doc source-name)))
		    (transclude (doc-spans source) start length target-spans insert-point)))))

(defun safe-load-doc (name)
  (if (stringp name) (setf name (parse-name name)))
  (if (leaf-missing name) (error "No leaf has the name ~A" (serialize-name name)))
  (load-and-parse name))

(defun modify-spans (name fn)
  (if (stringp name) (setf name (parse-name name)))
  (let ((doc (safe-load-doc name)))
    (if (not (editable-p doc)) (error "Document ~A is immutable." (serialize-name name)))
    (setf (doc-spans doc) (funcall fn (doc-spans doc)))
    (save-leaf doc)))
