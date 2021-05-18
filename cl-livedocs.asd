;;;; cl-livedocs.asd

(asdf:defsystem #:cl-livedocs
  :description "Documentation browser based on Webinfo for Common Lisp."
  :author "Mariano Montone <marianomontone@gmail.com>"
  :license  "GPL"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "cl-livedocs"))
  :depends-on (:alexandria :anaphora :assoc-utils :webinfo))
