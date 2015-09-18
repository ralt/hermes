(in-package #:hermes)

(defun main (args)
  (if (command-exists *root-commands* (rest args))
      (handle-command *root-commands* (rest args))
      (handle-command *root-commands* '("help")))
  0)
