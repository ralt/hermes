(asdf:defsystem #:hermes
  :description "CLI helper for hermes"
  :author "Florian Margaine <florian@margaine.com>"
  :license "MIT License"
  :serial t
  :depends-on (:sb-posix)
  :components ((:file "package")
               (:file "error")
               (:file "command")
               (:file "help")
               (:file "write")
               (:file "hermes")))
