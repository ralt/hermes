(asdf:defsystem #:hermes
  :description "CLI helper for hermes"
  :author "Florian Margaine <florian@margaine.com>"
  :license "MIT License"
  :serial t
  :components ((:file "package")
               (:file "command")
               (:file "help")
               (:file "write")
               (:file "hermes")))
