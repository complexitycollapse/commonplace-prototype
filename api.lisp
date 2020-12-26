;;;; api.lisp

(in-package #:cp-api)

(defun new-doc (name)
  (setf name (ensure-parsed name))
  (if (leaf-exists name) (error "There already exists a leaf called ~A" (serialize-name name)))
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

(defun new-version (existing-name &optional new-name)
  (let* ((actual-name
	  (ensure-parsed (or new-name (get-next-version-name (ensure-parsed existing-name)))))
	 (already-exists (leaf-exists actual-name)))
    (if (and (not new-name) already-exists)
	(error "Leaf ~A already exists. Please specify a name for the new version explicitly."
	       (serialize-name actual-name)))
    (if already-exists (error "Leaf with name ~A already exists." (serialize-name actual-name)))
    (save-leaf (new-version (ensure-parsed existing-name) actual-name))))

(defun delete-leaf (name)
  (let ((exists (leaf-exists name))) ; TODO similar problem to below
    (if exists (delete-file (name-to-path (ensure-parsed name))))
					; TODO This cannot delete non-doc leaves
					; due to ENSURE-PARSED implementation. Fix that.
    exists))

(defun import-file (new-leaf-name path &optional doc-to-add-to insert-point start length)
  (save-leaf (create-content-from-file (ensure-parsed new-leaf-name "content") path))
  (if doc-to-add-to (include-from-leaf doc-to-add-to insert-point new-leaf-name start length)))

(defun include-from-leaf (doc-name insert-point leaf-name start length)
  (modify-spans doc-name
		(lambda (spans)
		  (insert-spans spans (list (span leaf-name start length)) insert-point))))

(defun export-text (name output-filename)
  (let ((doc (safe-load-doc name)))
      (with-open-file (s output-filename :direction :output :if-exists :supersede)
	(princ (generate-concatatext (doc-spans doc)) s))))

(defun fetch-doc (name)
  (let ((missing (download-folio (ensure-parsed name))))
    (if missing (warn "The following leaves could not be located: ~A"
		      (mapcar #'serialize-name missing)))))

(defun doc-length (name)
  (reduce #'+ (doc-spans (safe-load-doc name)) :key #'len))

(defun safe-load-doc (name)
  (setf name (ensure-parsed name))
  (if (leaf-missing name) (error "No leaf has the name ~A" (serialize-name name)))
  (load-and-parse name))

(defun modify-spans (name fn)
  (setf name (ensure-parsed name))
  (let ((doc (safe-load-doc name)))
    (if (not (editable-p doc)) (error "Document ~A is immutable." (serialize-name name)))
    (setf (doc-spans doc) (funcall fn (doc-spans doc)))
    (save-leaf doc)))

(defun leaf-exists (name) (not (leaf-missing (ensure-parsed name))))

(defun ensure-parsed (name &optional (type "doc"))
  (if (stringp name) (parse-name name type) name))
