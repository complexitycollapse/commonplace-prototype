;;;; xanadu.asd

(asdf:defsystem #:xanadu
  :description "Describe xanadu here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:cl-utilities :osicat)
  :components ((:file "package")
               (:file "xanadu")))
