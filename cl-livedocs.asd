;;;; cl-livedocs.asd

(asdf:defsystem #:cl-livedocs
  :description "Describe cl-livedocs here"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "cl-livedocs"))
  :depends-on (:alexandria :anaphora :assoc-utils :webinfo))
