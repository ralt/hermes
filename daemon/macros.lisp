(in-package #:hermes-daemon)

(defmacro loop-with-unix-socket (vars &body body)
  (let ((socket (first vars))
        (server (gensym)))
    `(progn
       (when (probe-file *socket-path*)
         (sb-posix:unlink *socket-path*))
       (sockets:with-open-socket (,server :address-family :local
                                          :local-filename (namestring *socket-path*)
                                          :connect :passive)
         (sb-posix:chown *socket-path*
                         0
                         (sb-posix:group-gid (sb-posix:getgrnam "hermes")))
         (sb-posix:chmod *socket-path* #o660)
         (loop do (let ((,socket (sockets:accept-connection ,server)))
                    ,@body
                    (close ,socket)))))))

(defmacro with-sync-open-file (vars &body body)
  (let ((stream (first vars)))
    `(with-open-file ,vars
       ,@body
       (finish-output ,stream)
       (sb-posix:fsync (sb-sys:fd-stream-fd ,stream)))))

(defmacro with-retries ((tries-count delay-in-seconds) &body body)
  (let ((result (gensym))
        (i (gensym))
        (tries-max (1- tries-count)))
    `(loop for ,i from 0 upto ,tries-max
        do (let ((,result (progn ,@body)))
             (when ,result (return ,result)))
          ;; Don't sleep the last time, it's pointless
        do (unless (= ,i ,tries-max)
             (sleep ,delay-in-seconds)))))

(defmacro make-overrideable-variables (&body alists)
  `(progn
     ,@(loop for pair in alists
          collect (let ((var-name (cdr pair)))
                    `(when (uiop:getenvp ,var-name)
                       (setf ,(car pair) (uiop:getenv ,var-name)))))))
