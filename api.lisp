;;;; api.lisp

(in-package :commonplace)

(defmacro with-safely-loaded-doc (doc-or-doc-name  &body body)
  (let ((sdoc (intern "DOC")) (sdoc-length (intern "DOC-LENGTH")))
    `(f-with-safely-loaded-doc ,doc-or-doc-name (lambda (,sdoc ,sdoc-length)
						  (declare (ignorable ,sdoc-length))
						  ,@body))))

(defmacro modify-spans (name &body body)
  `(with-safely-loaded-doc ,name
     (setf (doc-spans doc) (funcall (lambda (spans doc-length)
				      (declare (ignorable doc-length)) ,@body)
				    (doc-spans doc)
				    doc-length))))

(defmacro test-oob (val format-string &rest format-args)
  `(if (> ,val doc-length) (error ,format-string ,@format-args)))

(defun new-doc (&optional name) (new-doc-name (leaf-name (save-leaf (new-doc-leaf))) name))

(defun append-text (name text)
  (modify-spans name (merge-span-lists spans (list (append-to-local-scroll text)))))

(defun insert-text (name point text)
  (modify-spans name
    (test-oob point "The insert point (~A) is too large (max ~A for document ~A)"
	      point doc-length name)
    (insert-spans spans (list (append-to-local-scroll text)) point)))

(defun delete-text (name start length)
  (modify-spans name
    (check-start-and-length-bounds start length doc-length name)
    (delete-spans spans start length)))

(defun move-text (name start length new-pos)
  (modify-spans name
    (test-oob start "The start index (~A) is too large (max ~A for document ~A)"
	      start doc-length name)
    (move-spans spans start length new-pos)))

(defun transclude (destination-name insert-point source-name start length)
  (modify-spans destination-name
    (test-oob insert-point "The insert-point (~A) is too large (max ~A for document ~A)"
	      insert-point doc-length destination-name)
    (let ((source (safe-load-doc source-name)))
      (check-start-and-length-bounds start length (doc-length source) source-name)
      (transclude-spans (doc-spans source) start length spans insert-point))))

(defun delete-leaf (name)
  (build (exists (leaf-exists name))
    (when exists
      (delete-file (name-to-path (resolve-doc-name name)))
      (delete-doc-name name))))

(defun import-file (path &optional name)
  (let ((contents (load-contents-of-file path)))
    (build (doc-name (new-doc name))
      (append-text doc-name contents))))

(defun export-text (name &optional output-filename)
  (let* ((doc (safe-load-doc name)))
    (build (text (generate-concatatext (doc-spans doc)))
      (cond
	((eq T output-filename) (princ text)) ; write to standard output
	(output-filename
	     (with-open-file (s output-filename :direction :output :if-exists :supersede)
	       (princ text s)))))))

(defun doc-length (doc-or-doc-name)
  (let ((doc (if (stringp doc-or-doc-name) (safe-load-doc doc-or-doc-name) doc-or-doc-name)))
    (length-sum (doc-spans doc))))

(defun add-link (doc-or-doc-name link-or-spec)
  (let ((link (coerce-to-link link-or-spec T)))
    (with-safely-loaded-doc doc-or-doc-name (pushend link (doc-links doc)))))

(defun insert-link (doc-or-doc-name link-or-spec n)
  (let ((link (coerce-to-link link-or-spec T)))
    (with-safely-loaded-doc doc-or-doc-name
      (check-link-bounds doc doc-or-doc-name n)
      (setf (doc-links doc) (insert-at link (doc-links doc) n)))))

(defun remove-link (doc-or-doc-name n)
  (with-safely-loaded-doc doc-or-doc-name
    (check-link-bounds doc doc-or-doc-name (1- n))
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
    (funcall fn doc (doc-length doc))
    (if (stringp doc-or-doc-name)
	(update-doc-name doc-or-doc-name (leaf-name (save-leaf doc))))))

(defun leaf-exists (name) (not (leaf-missing (resolve-doc-name name))))

(defun coerce-to-link (link-or-spec create)
  (build (link
	  (if (consp link-or-spec) (create-link-from-spec link-or-spec) link-or-spec))
    (if create (save-leaf link))))

(defun check-start-and-length-bounds (start length doc-length name)
  (test-oob start "The start index (~A) is too large (max ~A for document ~A)"
	    start doc-length name)
  (test-oob (+ start length)
	    "The lenth to delete (~A) is too long (only ~A characters afer start ~A)"
	    length (- doc-length start) start))

(defun check-link-bounds (doc name n)
  (if (> n (length (doc-links doc)))
      (error "Link index (~A) is out of bounds (max ~A for document ~A)"
	n (length (doc-links doc)) name)))
