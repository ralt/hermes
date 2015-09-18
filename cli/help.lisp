(in-package #:hermes)

(defvar *help-commands* nil)

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
  (declare (ignore args))
  (format t "usage: hermes write <device> <user>

Transform a device in an hermes device for a user.

/!\\ WARNING /!\\

This command will delete ALL the data on the device!

/!\\ WARNING /!\\
"))
