(asdf:defsystem #:hermes-daemon
  :description "The hermes daemon"
  :author "Florian Margaine <florian@margaine.com>"
  :license "MIT License"
  :serial t
  :depends-on (:iolib :sb-posix :cl-fad :cl-syslog)
  :components ((:file "package")
               (:file "variables")
               (:file "macros")
               (:file "can-login")
               (:file "regenerate-token")
               (:file "hermes-daemon")))
