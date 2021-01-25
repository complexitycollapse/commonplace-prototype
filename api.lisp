;;;; api.lisp

(in-package :commonplace)

(defmacro with-safely-loaded-doc (doc-or-doc-name  &body body)
  `(f-with-safely-loaded-doc ,doc-or-doc-name (lambda (,(intern "DOC")) ,@body)))

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
  (build (exists (leaf-exists name))
    (when exists
      (delete-file (name-to-path (resolve-doc-name name)))
      (delete-doc-name name))))

(defun import-file (path)
  (let ((new-leaf (save-leaf (create-content-from-file path))))
    (new-doc-name
     (leaf-name
      (save-leaf (new-doc-leaf (list (span (leaf-name new-leaf)
					   0
					   (length (content-leaf-contents new-leaf))))))))))

(defun export-text (name &optional output-filename)
  (let* ((doc (safe-load-doc name)))
    (build (text (generate-concatatext (doc-spans doc)))
      (if output-filename
	  (with-open-file (s output-filename :direction :output :if-exists :supersede)
	    (princ text s))))))

(defun doc-length (name) (reduce #'+ (doc-spans (safe-load-doc name)) :key #'len))

(defun add-link (doc-or-doc-name link-or-spec)
  (let ((link (coerce-to-link link-or-spec T)))
    (with-safely-loaded-doc doc-or-doc-name (pushend link (doc-links doc)))))

(defun insert-link (doc-or-doc-name link-or-spec n)
  (let ((link (coerce-to-link link-or-spec T)))
    (with-safely-loaded-doc doc-or-doc-name
      (setf (doc-links doc) (insert-at link (doc-links doc) n)))))

(defun remove-link (doc-or-doc-name n)
  (with-safely-loaded-doc doc-or-doc-name
    (let ((links (doc-links doc)))
      (setf (doc-links doc) (remove (nth n links) links :test #'equalp)))))

(defun safe-load-doc (name)
  (let ((hash (if (hash-name-p name) name (resolve-doc-name name))))
    (if (leaf-missing hash) (error "No leaf has the name ~A" name))
    (load-and-parse hash)))

(defun f-with-safely-loaded-doc (doc-or-doc-name fn)
  (let* ((doc (typecase doc-or-doc-name
		(doc doc-or-doc-name)
		(otherwise (safe-load-doc doc-or-doc-name)))))
    (funcall fn doc)
    (if (stringp doc-or-doc-name)
	(update-doc-name doc-or-doc-name (leaf-name (save-leaf doc))))))

(defun modify-spans (name fn)
  (with-safely-loaded-doc name
    (setf (doc-spans doc) (funcall fn (doc-spans doc)))))

(defun leaf-exists (name) (not (leaf-missing (resolve-doc-name name))))

(defun coerce-to-link (link-or-spec create)
  (build (link
	  (if (consp link-or-spec) (create-link-from-spec link-or-spec) link-or-spec))
    (if create (save-leaf link))))
