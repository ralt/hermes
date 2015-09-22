(in-package #:hermes-daemon)

(defvar *max-username-length* 33)
(defvar *socket-path* #p"/var/run/hermes.sock")

(defun main (&rest args)
  (declare (ignore args))
  (let ((user-buffer (make-array *max-username-length*
                                 :element-type '(unsigned-byte 8))))
    (when (probe-file *socket-path*)
      (sb-posix:unlink *socket-path*))
    (sockets:with-open-socket (server :address-family :local
                                      :local-filename (namestring *socket-path*)
                                      :connect :passive)
      (sb-posix:chown *socket-path* 0 (sb-posix:group-gid (sb-posix:getgrnam "hermes")))
      (sb-posix:chmod *socket-path* #o660)
      (loop
         do (let ((socket (sockets:accept-connection server :wait t)))
              (when (> (read-sequence user-buffer socket) 0)
                (write-byte (if (can-login-p (buffer-to-string user-buffer))
                                1
                                0)
                            socket)
                (close socket)))))))

(defun buffer-to-string (buffer)
  (coerce (loop for byte across buffer
             until (= byte 0)
             collect (code-char byte))
          'string))

(defun can-login-p (user)
  t)
