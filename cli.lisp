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
  (check-argn args 1 "repository name")
  (cli-out "Created new repository in ~A." (init (car args))))

(defun cli-new (args)
  (cond ((string= (car args) "doc") (cli-new-doc (cdr args)))
	((string= (car args) "link") (cli-new-link (cdr args)))
	(T (error 'unrecognised-argument-error :verb current-cli-verb*))))

(defun cli-new-doc (args)
  (check-argn args 0)
  (cli-out "Created ~A" (new-doc)))

(defun cli-append (args)
  (check-argn args 2 "name")
  (let ((name (car args))
	(text (cadr args)))
    (if (null text) (setf text (drain *standard-input*)))
    (cli-out (append-text name text))))

(defun cli-insert (args)
  (check-argn args 3 "name" "position" "text")
  (let ((name (car args))
	(position (cadr args))
	(text (caddr args)))
    (if (null text) (setf text (drain *standard-input*)))
    (cli-out (insert-text name (safe-integer position "position") text))))

(defun cli-delete (args)
  (check-argn args 3 "name" "start" "length")
  (destructuring-bind (name start length) args
    (cli-out
     (delete-text name (safe-integer start "start") (safe-integer length "length")))))

(defun cli-move (args)
  (check-argn args 4 "name" "start" "length" "new position")
  (destructuring-bind (name start length new-pos) args
    (move-text name (safe-integer start "start") (safe-integer length "length")
	       (safe-integer new-pos "new position"))))

(defun cli-transclude (args)
  (check-argn args 5 "destination" "insert point" "source name" "source start point" "length")
  (destructuring-bind (dest point src start length) args
    (cli-out (transclude
	      dest
	      (safe-integer point "insert point") src
	      (safe-integer start "source start point") (safe-integer length "length")))))

(defun cli-import (args)
  (check-argn args 1 "path")
  (cli-out (import-file (car args))))

(defun cli-export (args)
  (check-argn args 2 "name")
  (let ((path (cadr args)))
    (export-text (car args) (or path T))))

(defun cli-new-link (args)
  (check-argn args 1)
  (let ((leaf (coerce-to-link (read-cclink (car args)))))
    (save-leaf leaf)
    (cli-out (hash-name-hash (leaf-name leaf)))))

(defun cli-link (args)
  (check-argn args 3 "document name" "link name")
  (cond ((caddr args) (cli-insert-link args))
	(T (cli-out (stringify (nth-value 1 (add-link (car args) (cadr args))))))))

(defun cli-insert-link (args)
  (check-argn args 3 "document name" "link name" "position")
  (insert-link (car args) (cadr args) (safe-integer (caddr args) "position")))

(defun cli-unlink (args)
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
