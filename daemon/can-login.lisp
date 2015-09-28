(in-package #:hermes-daemon)

;;; See "Safe One-Time Tokens" (aka the big-ass comment for the
;;; "regenerate-token" function) for why this function is following a
;;; kinda convoluted process to validate tokens.
(defun can-login-p (user)
  (let ((device (find-hermes-device))
        (user-file (merge-pathnames user *user-tokens-path*)))
    (unless device
      (log :err "no device found")
      (return-from can-login-p nil))
    (unless (probe-file user-file)
      (log :err "no user file found")
      (return-from can-login-p nil))
    (let ((user-token (read-user-token user-file))
          (device-token (read-device-token device)))
      ;; New user token vs new device token, the classic path.
      (when (timing-safe-compare user-token
                                 device-token
                                 *token-length*)
        (log :info "normal login successful")
        (return-from can-login-p t))
      (unless (has-hermes-fingerprint device *safe-ott-device-offset*)
        (log :err "tokens don't match, aborting")
        (return-from can-login-p nil))
      (let ((device-old-token (read-device-old-token device)))
        ;; Another possible path: new user token vs old device token.
        ;; This can happen if writing the new token on device failed.
        (when (timing-safe-compare user-token
                                   device-old-token
                                   *token-length*)
          (log :info "recovered login with the old token on the device")
          (return-from can-login-p t))
        ;; Last possible path: old user token vs old device token.
        ;; This can happen if writing the new token on user file
        ;; failed.
        (unless (user-has-old-token-fingerprint user-file)
          (log :err "device has old token but not user file, aborting")
          (return-from can-login-p nil))
        (if (timing-safe-compare (read-user-old-token user-file)
                                 device-old-token
                                 *token-length*)
            (progn
              (log :info "recovered login with the old tokens")
              t)
            (progn
              (log
               :err
               "device and user file have old tokens that don't match, aborting")
              nil))))))

(defun user-has-old-token-fingerprint (user-file)
  (has-hermes-fingerprint user-file *safe-ott-user-offset*))

(defun read-user-token (user-file)
  (read-token user-file))

(defun read-user-old-token (user-file)
  (read-token user-file (+ *safe-ott-user-offset* *fingerprint-length*)))

(defun read-device-token (device)
  (read-token device *fingerprint-length*))

(defun read-device-old-token (device)
  (read-token device (+ *safe-ott-device-offset* *fingerprint-length*)))

(defun read-token (path &optional (offset 0))
  (with-open-file (f path
                     :direction :input
                     :element-type '(unsigned-byte 8))
    (when (file-position f offset)
      (let ((token (make-array *token-length* :element-type '(unsigned-byte 8))))
        (read-sequence token f)
        token))))

(defun timing-safe-compare (user-token device-token token-length)
  (let ((result 0))
    (loop
       for i from 0 upto (1- token-length)
       do (setf result (logior result (logxor (elt user-token i)
                                              (elt device-token i)))))
    (= result 0)))

(defun find-hermes-device ()
  ;; Retry multiple times over 5 seconds, a device can take some time to be
  ;; available in the OS.
  (with-retries (6 1)
    (find-if #'is-hermes-device
             (remove-if-not #'is-storage-device
                            (cl-fad:list-directory *devices-folder*)))))

(defun starts-with (string other-string)
  (string= (subseq string 0 (length other-string)) other-string))

(defun is-storage-device (path)
  (let ((file (pathname-name path)))
    (and file
         (> (length file) 2)
         (starts-with file *storage-device-prefix*))))

(defun is-hermes-device (path)
  (and (is-block-device path)
       (has-hermes-fingerprint path)))

(defun is-block-device (path)
  (sb-posix:s-isblk (sb-posix:stat-mode (sb-posix:stat path))))

(defun has-hermes-fingerprint (path &optional (offset 0))
  (with-open-file (f path
                     :direction :input
                     :element-type '(unsigned-byte 8))
    (when (file-position f offset)
      (let ((bytes (make-array *fingerprint-length* :element-type '(unsigned-byte 8))))
        (read-sequence bytes f)
        (every #'= bytes *fingerprint*)))))
