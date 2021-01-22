;;;; api.lisp

(in-package :commonplace)

(defmacro with-safely-loaded-doc (name &body body)
  `(f-with-safely-loaded-doc ,name (lambda (,(intern "DOC")) ,@body)))

(defun new-doc () (new-doc-name (leaf-name (save-leaf (new-doc-leaf)))))

(defun append-text (name text)
  (modify-spans name (lambda (spans)
		       (merge-span-lists spans (list (append-to-local-scroll text))))))

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
		    (transclude-spans
		     (doc-spans source) start length target-spans insert-point)))))

(defun delete-leaf (name)
  (let ((exists (leaf-exists name)))
    (when exists
      (delete-file (name-to-path (resolve-doc-name name)))
      (delete-doc-name name))
    exists))

(defun import-file (path)
  (let ((new-leaf (save-leaf (create-content-from-file path))))
    (new-doc-name
     (leaf-name
      (save-leaf (new-doc-leaf (list (span (leaf-name new-leaf)
					   0
					   (length (content-leaf-contents new-leaf))))))))))

(defun export-text (name &optional output-filename)
  (let* ((doc (safe-load-doc name))
	 (text (generate-concatatext (doc-spans doc))))
    (if output-filename
	(with-open-file (s output-filename :direction :output :if-exists :supersede)
	  (princ text s))
	text)))

(defun doc-length (name) (reduce #'+ (doc-spans (safe-load-doc name)) :key #'len))

(defun add-link (doc-name link-or-spec)
  (let ((link (if (consp link-or-spec) (create-link-from-spec link-or-spec) link-or-spec)))
    (save-leaf link)
    (with-safely-loaded-doc doc-name (add-link-to-end doc link))))

(defun safe-load-doc (name)
  (let ((hash (if (hash-name-p name) name (resolve-doc-name name))))
    (if (leaf-missing hash) (error "No leaf has the name ~A" name))
    (load-and-parse hash)))

(defun f-with-safely-loaded-doc (name fn)
  (let* ((doc (safe-load-doc name)))
    (funcall fn doc)
    (update-doc-name name (leaf-name (save-leaf doc)))))

(defun modify-spans (name fn)
  (with-safely-loaded-doc name
    (setf (doc-spans doc) (funcall fn (doc-spans doc)))))

(defun leaf-exists (name) (not (leaf-missing (resolve-doc-name name))))
