;;;; commonplace.asd

(asdf:defsystem #:commonplace
  :description "An implementation of a transliterature system"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:cl-utilities :osicat :hunchentoot :drakma :cl-fad)
  :components ((:file "packages")
               (:file "commonplace")
	       (:file "api")))
