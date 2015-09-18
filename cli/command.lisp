(in-package #:hermes)

(defvar *root-commands* nil)
(defvar *help-commands* nil)

(defmacro defcommand (container name args &body body)
  `(setf (getf ,container ,(intern (symbol-name name) :keyword))
         (list :function (lambda ,args
                           ,@body)
               :documentation ,(first body))))

(defun command-exists (container args)
  "Finds out if a command exists in the container."
  (and args (getf container (intern (string-upcase (first args)) :keyword))))

(defun handle-command (container args)
  "Executes a command. Checking if it exists is outside the scope
of this function."
  (funcall (getf (getf container (intern (string-upcase (first args)) :keyword)) :function)
           (rest args)))

(defun command-help (command command-object)
  (format nil "	~A	~A"
          (string-downcase (symbol-name command))
          (getf command-object :documentation)))

(defcommand *root-commands* help (args)
  "prints this help"
  (if args
      (handle-command *help-commands* args)
      (format t "usage: hermes [--help | -h] <command> [<args>]

A CLI utility to manage hermes devices.

This is the list of available commands:

~{~A~^~%~}

You can type \"hermes help <command>\" to get more help about it.

hermes online help: <https://github.com/ralt/hermes/issues>
" (loop for (k v) on *root-commands* by #'cddr
     collect (command-help k v)))))

(defcommand *help-commands* write (args)
  "writes a hermes device"
  (declare (ignore args))
  (format t "usage: hermes write <device>

Transform a device in an hermes device.

/!\\ WARNING /!\\

This command will delete ALL the data on the device!

/!\\ WARNING /!\\
"))

(defcommand *root-commands* write (args)
  "transforms a device in an hermes device"
  (if (not args)
      (format t "Missing argument: device~%")))
