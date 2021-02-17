;;;; CLI

(in-package :commonplace)

(defvar current-cli-verb*)

(defun make-exe ()
  (sb-ext:save-lisp-and-die "commonplace" :toplevel #'executable-start :executable T))

(defun executable-start ()
  (handler-case
      (progn
	(setf repo-path* (sb-posix:getcwd))
	(process-command-line (cdr sb-ext:*posix-argv*)))
    (error (e) (cli-out "Internal error: ~A" e))))

(defun process-command-line (args)
  (if (endp args) (cli-out "Missing command line arguments.")
      (let ((verb (make-keyword (car args)))
	    (rest (cdr args)))
	(let ((current-cli-verb* verb))
	  (declare (special current-cli-verb*))
	  (handler-case
	      (case verb
		(:init (cli-init rest))
		(:new (check-argn args nil "type")
		      (setf verb (concatenate 'string "new " (cadr rest)))
		      (cli-new rest))
		(:append (cli-append rest))
		(:insert (cli-insert rest))
		(:delete (cli-delete rest))
		(:move (cli-move rest))
		(:transclude (cli-transclude rest))
		(:import (cli-import rest))
		(:export (cli-export rest))
		(:link (cli-link rest))
		(:unlink (cli-unlink rest))
		(otherwise (cli-verb-not-recognised (car args))))
	    (commonplace-error (e)
	      (let ((*print-escape* nil)) (cli-out "~A" e))))))))

;;; CLI verbs

(defun cli-init (args)
  "CLI: initialise a new repo. Arguments: new repository name."
  (check-argn args 1 "repository name")
  (cli-out "Created new repository in ~A." (init (car args))))

(defun cli-new (args)
  "CLI: create a new named document or link as specified by the first argument. The name is
written to STDOUT."
  (cond ((string= (car args) "doc") (cli-new-doc (cdr args)))
	((string= (car args) "link") (cli-new-link (cdr args)))
	(T (error 'unrecognised-argument-error :verb current-cli-verb*))))

(defun cli-fork (args)
  "CLI: create a fork of an existing document (i.e. a new name for an existing document that
can be edited independently). Arguments: the document to fork. The new name is written to
STDOUT."
  (check-argn args 1 "name")
  (cli-out (fork-doc (car args))))

(defun cli-new-doc (args)
  "CLI: Create a new named document. The name is written to STDOUT. No arguments."
  (check-argn args 0)
  (cli-out "Created ~A" (new-doc)))

(defun cli-append (args)
  "CLI: append some new text to a document. Arguments: document name, text. If text is not
specified then the text is read from STDIN."
  (check-argn args 2 "name")
  (let ((name (car args))
	(text (cadr args)))
    (if (null text) (setf text (drain *standard-input*)))
    (cli-out (append-text name text))))

(defun cli-insert (args)
  "CLI: insert some new text into a document at a given position. Arguments: document name,
position, text. If text is not specified then the text is read from STDIN."
  (check-argn args 3 "name" "position" "text")
  (let ((name (car args))
	(position (cadr args))
	(text (caddr args)))
    (if (null text) (setf text (drain *standard-input*)))
    (cli-out (insert-text name (safe-integer position "position") text))))

(defun cli-delete (args)
  "CLI: delete a section from a document. Arguments: document, start point, length"
  (check-argn args 3 "name" "start" "length")
  (destructuring-bind (name start length) args
    (cli-out
     (delete-text name (safe-integer start "start") (safe-integer length "length")))))

(defun cli-move (args)
  "CLI: move some content from one point in a document to another. Arguments: document, start,
length, new position."
  (check-argn args 4 "name" "start" "length" "new position")
  (destructuring-bind (name start length new-pos) args
    (move-text name (safe-integer start "start") (safe-integer length "length")
	       (safe-integer new-pos "new position"))))

(defun cli-transclude (args)
  "CLI: Transclude some content from one document into another. The content is not copied, but
instead the two documents share the same content. Argumnents: destination document, insert
point in destination, source document, start point in source of contents to transclude, length
of contents."
  (check-argn args 5 "destination" "insert point" "source name" "source start point" "length")
  (destructuring-bind (dest point src start length) args
    (cli-out (transclude
	      dest
	      (safe-integer point "insert point") src
	      (safe-integer start "source start point") (safe-integer length "length")))))

(defun cli-import (args)
  "CLI: import a text file into Commonplace as a new document. The document will have the
same contents as the file. Arguments: path of file."
  (check-argn args 1 "path")
  (cli-out (import-file (car args))))

(defun cli-export (args)
  "CLI: export the concatatext of a document to a file. Arguments: document name, path of
output file. If no output file is specified then the text is written to STDOUT."
  (check-argn args 2 "name")
  (let ((path (cadr args)))
    (export-text (car args) (or path T))))

(defun cli-new-link (args)
  "Create a new link. Arguments: link text. If no link text argument is specified then it is
read from STDIN. The link name will be printed to STDOUT."
  (check-argn args 1)
  (let ((leaf (coerce-to-link (read-cclink (car args)))))
    (save-leaf leaf)
    (cli-out (hash-name-hash (leaf-name leaf)))))

(defun cli-link (args)
  "Add a link to a document. The link is appended to the end of the document's link list.
Arguments: document name, link name."
  (check-argn args 3 "document name" "link name")
  (cond ((caddr args) (cli-insert-link args))
	(T (cli-out (stringify (nth-value 1 (add-link (car args) (cadr args))))))))

(defun cli-insert-link (args)
  "Add a link to a document. The link is inserted into the document's link list at the
specified position. Arguments: document name, link name, position."
  (check-argn args 3 "document name" "link name" "position")
  (insert-link (car args) (cadr args) (safe-integer (caddr args) "position")))

(defun cli-unlink (args)
  "Remove a link from a document. Arguments: document name, index of link in the documents's
link list."
  (check-argn args 2 "document name" "link index")
  (remove-link (car args) (safe-integer (cadr args) "link index")))

;;; Internal

(defun cli-verb-not-recognised (verb)
  (cli-out "~A is not a recognised verb." (string-downcase verb)))

(defun cli-out (format-string &rest format-args)
  (apply #'format T format-string format-args)
  (fresh-line))

(defun stringify (thing) (format nil "~A" thing))

(defun check-argn (args max &rest names)
  (let ((len (length args)))
    (if (and max (> len max)) (error 'unrecognised-argument-error :verb current-cli-verb*))
    (let ((missing (car (drop len names))))
      (if missing
	  (error 'missing-argument-error :argument missing :verb current-cli-verb*)))))

(defun safe-integer (value arg-name)
  (multiple-value-bind (val length) (parse-integer value :junk-allowed T)
    (cond ((or (null val) (not (eq (length value) length)))
	   (error 'not-an-integer-error :argument arg-name :actual value))
	  ((< val 0) (error 'negative-integer-error :argument arg-name :actual val))
	  (T val))))

(defun read-cclink (link-arg) (parse-cclink (or link-arg (drain *standard-input*))))
