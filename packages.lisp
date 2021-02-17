;;;; package.lisp

(defpackage #:commonplace
  (:use #:cl #:cl-utilities)
  (:export :new-doc
	   :fork-doc
	   :append-text
	   :insert-text
	   :delete-text
	   :move-text
	   :transclude
	   :delete-leaf
	   :import-file
	   :export-text
	   :doc-length
	   :new-link
	   :add-link
	   :insert-link
	   :remove-link
	   :make-exe))
