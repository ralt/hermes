(in-package #:hermes-daemon)

(defun main (&rest args)
  (declare (ignore args))
  (loop-with-unix-socket (socket)
    (let ((user-buffer (make-array *max-username-length*
                                   :element-type '(unsigned-byte 8))))
      (when (> (read-sequence user-buffer socket) 0)
        (log :info "login requested")
        (let ((user (buffer-to-string user-buffer)))
          (write-byte (handler-case
                          (if (and (can-login-p user)
                                   (regenerate-token user))
                              1
                              0)
                        (error (err)
                          (progn (log :crit (format nil "fatal error: ~A" err))
                                 0)))
                      socket))))))

(defun buffer-to-string (buffer)
  (coerce (loop for byte across buffer
             until (= byte 0)
             collect (code-char byte))
          'string))

(defun log (priority text)
  (syslog:log "hermes" :authpriv priority text))
