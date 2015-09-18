(in-package #:hermes)

(defcommand *root-commands* write (args)
  "transforms a device in an hermes device"
  (if (not args)
      (format t "Missing argument: device~%")))
