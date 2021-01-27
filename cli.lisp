;;;; CLI

(in-package :commonplace)

(defun make-exe ()
  (sb-ext:save-lisp-and-die "commonplace" :toplevel #'executable-start :executable T))

(defun executable-start ()
  (setf repo-path* (sb-posix:getcwd))
  (process-command-line (cdr sb-ext:*posix-argv*)))

(defun process-command-line (args)
  (if (endp args) (api-out "Missing command line arguments.")
      (case (make-keyword (car args))
	(:init (api-init (cdr args)))
	(:new (api-new (cdr args)))
	(:append (api-append (cdr args)))
	(otherwise (api-verb-not-recognised (car args))))))

(defun api-init (args)
  (cond ((cdr args) (api-out "Unrecognised arguments to init."))
	((endp args) (api-out "You must specify a repository name."))
	(T (api-out "Created new repository in ~A." (init (car args))))))

(defun api-new (args)
  (if args (api-out "Unrecognised arguments to new.")
      (api-out "Created ~A" (new-doc))))

(defun api-append (args)
  (let ((name (car args))
	(text (cadr args)))
    (cond ((null name) (api-out "Missing name argument to append"))
	  ((null text) (api-out "Missing text argument to append"))
	  ((cddr args) (api-out "Unrecognised arguments to append."))
	  (T (append-text name text)))))

(defun api-verb-not-recognised (verb)
  (api-out "~A is not a recognised verb." (string-downcase verb)))

(defun api-out (format-string &rest format-args)
  (apply #'format T format-string format-args)
  (fresh-line))
