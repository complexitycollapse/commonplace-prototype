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

(defmacro test-oob (val arg-name doc-name)
  `(if (> ,val doc-length)
       (error 'text-position-too-large-error
	      :position ,val :max doc-length :arg-name ,arg-name :doc-name ,doc-name)))

(defun new-doc (&optional name) (new-doc-name (leaf-name (save-leaf (new-doc-leaf))) name))

(defun append-text (name text)
  (modify-spans name (merge-span-lists spans (list (append-to-local-scroll text)))))

(defun insert-text (name point text)
  (modify-spans name
    (test-oob point "insert point" name)
    (insert-spans spans (list (append-to-local-scroll text)) point)))

(defun delete-text (name start length)
  (modify-spans name
    (check-start-and-length-bounds start length doc-length name "delete")
    (delete-spans spans start length)))

(defun move-text (name start length new-pos)
  (modify-spans name
    (test-oob start "start point" name)
    (move-spans spans start length new-pos)))

(defun transclude (destination-name insert-point source-name start length)
  (modify-spans destination-name
    (test-oob insert-point "insert point" destination-name)
    (let ((source (safe-load-doc source-name)))
      (check-start-and-length-bounds
       start length (doc-length source) source-name "transclude")
      (transclude-spans (doc-spans source) start length spans insert-point))))

(defun delete-leaf (name)
  (build (exists (leaf-exists name))
    (when exists
      (delete-file (name-to-path (resolve-doc-name name)))
      (delete-doc-name name))))

(defun import-file (path &optional name)
  (let ((contents (alexandria:read-file-into-string path)))
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

(defun doc-length (doc-or-doc-name) (length-sum (doc-spans (safe-load-doc doc-or-doc-name))))

(defun new-link (link-or-spec)
  (hash-name-hash (leaf-name (make-span-space-link link-or-spec))))

(defun add-link (doc-or-doc-name link-designator)
  (let ((link (make-span-space-link link-designator))
	(index 0))
    (with-safely-loaded-doc doc-or-doc-name
      (pushend link (doc-links doc))
      (setf index (1- (length (doc-links doc)))))
    (values link index)))

(defun insert-link (doc-or-doc-name link-designator n)
  (let ((link (make-span-space-link link-designator)))
    (with-safely-loaded-doc doc-or-doc-name
      (let* ((links (doc-links doc))
	     (max (length (doc-links doc))))
	(if (> n max) (error 'link-index-out-of-bounds-error :index n :max max))
	(setf (doc-links doc) (insert-at link links n))))))

(defun remove-link (doc-or-doc-name n)
  (with-safely-loaded-doc doc-or-doc-name
    (let* ((links (doc-links doc))
	   (max (1- (length (doc-links doc)))))
      (if (> n max) (error 'link-index-out-of-bounds-error :index n :max max))
      (setf (doc-links doc) (remove (nth n links) links :test #'equalp)))))

(defun safe-load-doc (name)
  (if (doc-p name) name
      (let ((hash
	     (handler-case (if (hash-name-p name) name (resolve-doc-name name))
	       (file-error () (error 'no-doc-with-that-name :name name)))))
	(if (leaf-missing hash) (error 'leaf-not-found :type :doc :name hash))
	(load-and-parse hash))))

(defun f-with-safely-loaded-doc (doc-or-doc-name fn)
  (let* ((doc (typecase doc-or-doc-name
		(doc doc-or-doc-name)
		(otherwise (safe-load-doc doc-or-doc-name)))))
    (funcall fn doc (doc-length doc))
    (if (stringp doc-or-doc-name)
	(update-doc-name doc-or-doc-name (leaf-name (save-leaf doc))))))

(defun leaf-exists (name) (not (leaf-missing (resolve-doc-name name))))

(defun make-span-space-link (link-designator &optional do-not-save)
  (build (link (coerce-to-link (process-link-designator link-designator)))
    (if (not do-not-save) (save-leaf link))))

(defun process-link-designator (link-designator)
  (typecase link-designator
    (link link-designator)
    (concatalink link-designator)
    (string (handler-case (load-and-parse (make-hash-name :hash link-designator))
	      (file-error () (error 'leaf-not-found :name link-designator :type :link))))
    (cons (create-cclink-from-spec link-designator))
    (T (error "Invalid link designator ~S" link-designator))))

(defun check-start-and-length-bounds (start length doc-length name action)
  (test-oob start "start" name)
  (if (> (+ start length) doc-length)
      (error 'excessive-length-error :length (+ start length) :max (- doc-length start)
	     :start start :action action)))
