(in-package #:hermes)

(defun main ()
  (handler-case
      (if (command-exists *root-commands* (rest sb-ext:*posix-argv*))
          (handle-command *root-commands* (rest sb-ext:*posix-argv*))
          (handle-command *root-commands* '("help")))
    (argument-error (err)
      (progn
        (format t "~A~%" (slot-value err 'text))
        -2))
    (privilege-error (err)
      (progn
        (format t "~A~%" (slot-value err 'text))
        -3))
    (error (err)
      (progn
        (format t "fatal error: ~A~%" err)
        -1))))
