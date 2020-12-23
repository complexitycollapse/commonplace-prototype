;;;; package.lisp

(defpackage #:xanadu
  (:use #:cl #:cl-utilities)
  (:export
   :leaf-missing :save-leaf :new-doc-leaf :pushend :append-to-local-scroll :insert-spans
   :doc-spans :load-and-parse :delete-spans :move-spans :editable-p :serialize-name
   :parse-name :transclude :get-next-version-name :name-to-path :create-content-from-file
   :new-content-leaf :generate-concatatext :download-folio :len))

(defpackage #:cla
  (:use #:xanadu #:cl)
  (:export
   :new-doc :append-text :insert-text :delete-text :move-text :transclude))
