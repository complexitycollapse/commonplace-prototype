;;;; xanadu.asd

(asdf:defsystem #:xanadu
  :description "Describe xanadu here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:cl-utilities :osicat :hunchentoot :drakma :cl-fad)
  :components ((:file "packages")
               (:file "xanadu")
	       (:file "api")))
