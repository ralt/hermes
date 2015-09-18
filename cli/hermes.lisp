(in-package #:hermes)

(defun main (args)
  (handler-case
      (if (command-exists *root-commands* (rest args))
          (handle-command *root-commands* (rest args))
          (handle-command *root-commands* '("help")))
    (argument-error (err) (progn
                            (format t "~A~%" (slot-value err 'text))
                            -2))
    (error () (progn
                (format t "fatal error~%")
                -1))))
