(in-package #:hermes)

(defvar *root-commands* nil)

(defmacro defcommand (container name args &body body)
  `(setf (getf ,container ,(intern (symbol-name name) :keyword))
         (list :function (lambda ,args
                           ,@body)
               :documentation ,(if (stringp (first body)) (first body) ""))))

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
