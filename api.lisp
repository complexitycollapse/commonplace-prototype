;;;; api.lisp

(in-package :commonplace)

(defmacro with-safely-loaded-doc (doc-or-doc-name &body body)
  "Macro for use in API functions that need to load a leaf. Will report the necessary errors
if the leaf doesn't exist, otherwise will execute BODY with DOC bound to the doc objects and
DOC-LENGTH bound its concatatext length."
  (let ((sdoc (intern "DOC")) (sdoc-length (intern "DOC-LENGTH")))
    `(f-with-safely-loaded-doc ,doc-or-doc-name (lambda (,sdoc ,sdoc-length)
						  (declare (ignorable ,sdoc-length))
						  ,@body))))

(defmacro modify-spans (name &body body)
  "Utility macro for API functions. Safely loads a document, executes BODY with SPANS bound to
the document's spans, and updates the document with the spans returned by BODY."
  `(with-safely-loaded-doc ,name
     (setf (doc-spans doc) (funcall (lambda (spans doc-length)
				      (declare (ignorable doc-length)) ,@body)
				    (doc-spans doc)
				    doc-length))
     doc))

(defmacro test-oob (val arg-name doc-name)
  "Check whether a position VAL is out of bounds for the given document. If so, a
TEXT-POSITIOH-TO-LARGE error is signalled, referening the given ARG-NAME."
  `(if (> ,val doc-length)
       (error 'text-position-too-large-error
	      :position ,val :max doc-length :arg-name ,arg-name :doc-name ,doc-name)))

(defun new-doc (&optional name)
  "Create a new document with a randomly generated name. The name is returned. (NOTE: the
NAME parameter should not be used. It is strictly for testing purposes.)"
  (new-doc-name (leaf-name (save-leaf (new-doc-leaf))) name))

(defun fork-doc (existing-doc-name &optional new-doc-name)
  "Create a new name for an existing document version. (NOTE: the NAME parameter should not be
used. It is strictly for testing purposes."
  (with-safely-loaded-doc existing-doc-name (new-doc-name (leaf-name doc) new-doc-name)))

(defun append-text (name text)
  "Append the given TEXT to the end of a document."
  (modify-spans name (merge-span-lists spans (list (append-to-local-scroll text)))))

(defun insert-text (name point text)
  "Insert the given TEXT to a document at the given POINT."
  (modify-spans name
    (test-oob point "insert point" name)
    (insert-spans spans (list (append-to-local-scroll text)) point)))

(defun delete-text (name start length)
  "Delete a section of text from a document."
  (modify-spans name
    (check-start-and-length-bounds start length doc-length name "delete")
    (delete-spans spans start length)))

(defun move-text (name start length new-pos)
  "Move text from one position in a document to another."
  (modify-spans name
    (test-oob start "start point" name)
    (move-spans spans start length new-pos)))

(defun transclude (destination-name insert-point source-name start length)
  "Transclude a section of one document into another. Transclusion means the two documents
share the same contents, rather than it being copied from one to the other. The content to be
transcluded is referenced by SOURCE-NAME, START and LENGTH. It transcluded into
DESTINATION-NAME at INSERT-POINT."
  (modify-spans destination-name
    (test-oob insert-point "insert point" destination-name)
    (let ((source (safe-load-doc source-name)))
      (check-start-and-length-bounds
       start length (doc-length source) source-name "transclude")
      (transclude-spans (doc-spans source) start length spans insert-point))))

(defun delete-leaf (name)
  "Delete a leaf from the repo. Note that this is just a delete from the local cache. The leaf
may exist in other repos."
  (build (exists (leaf-exists name))
    (when exists
      (delete-file (name-to-path (resolve-doc-name name)))
      (delete-doc-name name))))

(defun import-file (path &optional name)
  "Take the contents of the text file named by PATH and import it into Commonplace as a new
document, whose name is returned."
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

(defun doc-length (doc-or-doc-name)
  "Get the length of the document's concatatext."
  (length-sum (doc-spans (safe-load-doc doc-or-doc-name))))

(defun new-link (link-or-spec)
  "Create a new link leaf as specified by the spec."
  (hash-name-hash (leaf-name (make-span-space-link link-or-spec))))

(defun add-link (doc-or-doc-name link-designator)
  "Add a link to the given document. If the LINK-DESIGNATOR is an existing link then that will
be added. Otherwise a new link is created as specified by the designator. The link will be
appended to the end of the document's links."
  (let ((link (make-span-space-link link-designator)))
    (with-safely-loaded-doc doc-or-doc-name
      (pushend link (doc-links doc))
      (values link (1- (length (doc-links doc)))))))

(defun insert-link (doc-or-doc-name link-designator n)
  "Same behaviour as ADD-LINK, except the link is added to the document's link list at the
specified index."
  (let ((link (make-span-space-link link-designator)))
    (with-safely-loaded-doc doc-or-doc-name
      (let* ((links (doc-links doc))
	     (max (length (doc-links doc))))
	(if (> n max) (error 'link-index-out-of-bounds-error :index n :max max))
	(setf (doc-links doc) (insert-at link links n))))))

(defun remove-link (doc-or-doc-name n)
  "Remove a link from a document."
  (with-safely-loaded-doc doc-or-doc-name
    (let* ((links (doc-links doc))
	   (max (1- (length (doc-links doc)))))
      (if (> n max) (error 'link-index-out-of-bounds-error :index n :max max))
      (setf (doc-links doc) (remove (nth n links) links :test #'equalp)))))

;;; Internal functions

(defun safe-load-doc (name)
  "Attempt to load a document by name. The relevant errors as signalled if this causes any
problems."
  (if (doc-p name) name
      (let ((hash
	     (handler-case (if (hash-name-p name) name (resolve-doc-name name))
	       (file-error () (error 'no-doc-with-that-name :name name)))))
	(if (leaf-missing hash) (error 'leaf-not-found :type :doc :name hash))
	(load-and-parse hash))))

(defun f-with-safely-loaded-doc (doc-or-doc-name fn)
  "See WITH-SAFELY-LOADED-DOC."
  (let* ((doc (typecase doc-or-doc-name
		(doc doc-or-doc-name)
		(otherwise (safe-load-doc doc-or-doc-name))))
	 (result (multiple-value-list (funcall fn doc (doc-length doc)))))
    (if (stringp doc-or-doc-name)
	(update-doc-name doc-or-doc-name (leaf-name (save-leaf doc))))
    (values-list result)))

(defun leaf-exists (name) (not (leaf-missing (resolve-doc-name name))))

(defun make-span-space-link (link-designator &optional do-not-save)
  "Make a new link from the given LINK-DESIGNATOR. Link will be converted from a concatalink
to an ordinary link if required."
  (build (link (coerce-to-link (process-link-designator link-designator)))
    (if (not do-not-save) (save-leaf link))))

(defun process-link-designator (link-designator)
  "Convert a link designator to the link it represents, creating it if necessary."
  (typecase link-designator
    (link link-designator)
    (concatalink link-designator)
    (string (handler-case (load-and-parse (make-hash-name :hash link-designator))
	      (file-error () (error 'leaf-not-found :name link-designator :type :link))))
    (cons (create-cclink-from-spec link-designator))
    (T (error "Invalid link designator ~S" link-designator))))

(defun check-start-and-length-bounds (start length doc-length name action)
  "Check that the START and LENGTH parameters are not out of bounds in the document named by
NAME. IF they are, the relevant error is signalled and ACTION is used to describe the
operation that was being attempted."
  (test-oob start "start" name)
  (if (> (+ start length) doc-length)
      (error 'excessive-length-error :length (+ start length) :max (- doc-length start)
	     :start start :action action)))
