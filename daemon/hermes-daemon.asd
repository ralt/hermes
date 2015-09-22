(asdf:defsystem #:hermes-daemon
  :description "The hermes daemon"
  :author "Florian Margaine <florian@margaine.com>"
  :license "MIT License"
  :serial t
  :depends-on (:iolib :sb-posix)
  :components ((:file "package")
               (:file "hermes-daemon")))
