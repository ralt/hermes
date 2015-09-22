(in-package #:hermes-daemon)

(defvar *max-username-length* 33)
(defvar *socket-path* #p"/var/run/hermes.sock")
(defvar *fingerprint-length* 5)
(defvar *fingerprint* #(82 111 98 105 110))

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
  (if (find-hermes-device)
      t
      nil))

(defun find-hermes-device ()
  (some #'is-hermes-device (remove-if-not #'is-sd-device (cl-fad:list-directory #p"/dev/"))))

(defun is-sd-device (path)
  (let ((file (pathname-name path)))
    (when (and file (> (length file) 2))
      (string= (subseq file 0 2) "sd"))))

(defun is-hermes-device (path)
  (and (is-block-device path)
       (has-hermes-fingerprint path)))

(defun is-block-device (path)
  (sb-posix:s-isblk (sb-posix:stat-mode (sb-posix:stat path))))

(defun has-hermes-fingerprint (path)
  (with-open-file (f path
                     :direction :input
                     :element-type '(unsigned-byte 8))
    (let ((bytes (make-array *fingerprint-length* :element-type '(unsigned-byte 8))))
      (read-sequence bytes f)
      (every #'= bytes *fingerprint*))))
