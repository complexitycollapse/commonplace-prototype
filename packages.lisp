;;;; package.lisp

(defpackage #:commonplace
  (:use #:cl #:cl-utilities)
  (:export
   :leaf-missing :save-leaf :new-doc-leaf :pushend :append-to-local-scroll :insert-spans
   :doc-spans :load-and-parse :delete-spans :move-spans :editable-p :serialize-name
   :parse-name :transclude-spans :get-next-version-name :name-to-path :create-content-from-file
   :new-content-leaf :generate-concatatext :download-folio :len :span :leaf-name
   :make-new-version :merge-span-lists :content-leaf-contents :new-doc-name :update-doc-name
   :resolve-doc-name :hash-name-p :delete-doc-name))

(defpackage #:cp-api
  (:use #:commonplace #:cl)
  (:export
   :new-doc :append-text :insert-text :delete-text :move-text :transclude))
