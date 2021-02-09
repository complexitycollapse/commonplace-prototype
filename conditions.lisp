;;;; conditions.lisp

(in-package :commonplace)

(defmacro def-cpl-error (name fields report-string &rest report-arg-slots)
  (with-gensyms (condition stream)
    `(define-condition ,name (commonplace-error)
       ,(mapcar (lambda (f) `(,f :initarg ,(make-keyword f))) fields)
       (:report (lambda (,condition ,stream)
		  (format ,stream ,report-string
			  ,@ (mapcar (lambda (s) `(slot-value ,condition ',s))
				     report-arg-slots)))))))

(define-condition commonplace-error (error) ())

(def-cpl-error link-index-out-of-bounds-error (index max)
  "Link index (~A) is out of bounds (max ~A)." index max)

(def-cpl-error text-position-too-large-error (position max arg-name doc-name)
   "The ~A (~A) is too large (max ~A for document ~A)." arg-name position max doc-name)

(def-cpl-error excessive-length-error (length max start action)
  "The length to ~A (~A) is too long (only ~A characters afer start ~A)."
  action length max start)

(def-cpl-error unrecognised-argument-error (verb)
  "Unrecognised argument to ~A." verb)

(def-cpl-error missing-argument-error (argument verb)
  "Missing ~A argument to ~A." argument verb)

(def-cpl-error not-an-integer-error (argument actual)
  "~A argument must be an integer, was ~A." argument actual)

(def-cpl-error negative-integer-error (argument actual)
  "~A argument must be non-negative, was ~A" argument actual)

(def-cpl-error no-doc-with-that-name (name)
  "No document could be found with the name ~A." name)

(define-condition leaf-not-found (commonplace-error)
  ((name :initarg :name)
   (type :initarg :type))
  (:report (lambda (c s)
	     (let ((name (slot-value c 'name))
		   (type (ecase (slot-value c 'type) (:doc "document") (:link "link"))))
	       (format s "Could not find a ~A leaf with name ~A."
		       type
		       (if (name-p name) (serialize-name name) name))))))
