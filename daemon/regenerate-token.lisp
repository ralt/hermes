(in-package #:hermes-daemon)

;;; Safe One-Time Tokens
;;;
;;; Getting the new token is simple. Just read /dev/urandom.
;;;
;;; Writing the new tokens, however, is not so simple. If one of the
;;; tokens isn't entirely written (e.g. power cut), the device/user
;;; file combination won't match anymore. So extra care has to be
;;; applied to make sure both are written. This means extra care when
;;; writing, but also in the can-login-p function, that will have to
;;; check the redundancy tokens that are added.
;;;
;;; Before explaining the process for safe one-time tokens, here are
;;; some assumptions:
;;;   - A token is 128 bytes
;;;   - The fingerprint is 5 bytes
;;;   - The user file token starts at position 0
;;;   - The device fingerprint starts at position 0
;;;   - The device token starts at position 5
;;;
;;; The process for safe one-time tokens is the following:
;;;   - On the device:
;;;     - Write the fingerprint followed by the old token at position
;;;       133
;;;     - Write the new token at position 5
;;;   - On the user file:
;;;     - Write the fingerprint followed by the old token at position
;;;       128
;;;     - Write the new token at position 0
;;;   - On the device: fill with zeroes from position 133 for 133
;;;     bytes.
;;;   - On the user file: fill with zeroes from position 128 for 133
;;;     bytes.
;;;
;;; This process makes sure that no matter at which time power is cut,
;;; the can-login-p function will *always* be able to validate a token,
;;; either through the old tokens, or with the new tokens.
(defun regenerate-token (user)
  (let* ((new-token (read-token #p"/dev/urandom"))
         (device (find-hermes-device))
         (user-file (merge-pathnames user *user-tokens-path*))
         ;; I could read it on the user file too, really.
         (old-token (read-device-token device)))
    (unless device
      (log :err "no device found to write new token")
      (return-from regenerate-token nil))
    (unless (probe-file user-file)
      (log :err "no user file to write new token")
      (return-from regenerate-token nil))
    (write-device-old-token device old-token)
    (write-device-token device new-token)
    (write-user-old-token user-file old-token)
    (write-user-token user-file new-token)
    (write-device-zeroes device)
    (write-user-zeroes user-file))
  t)

(defun write-device-token (device token)
  (write-token device token *fingerprint-length*))

(defun write-device-old-token (device token)
  (write-token device *fingerprint* *safe-ott-device-offset*)
  (write-token device token (+ *safe-ott-device-offset*
                               *fingerprint-length*)))

(defun write-user-token (user-file token)
  (write-token user-file token))

(defun write-user-old-token (user-file token)
  (write-token user-file *fingerprint* *safe-ott-user-offset*)
  (write-token user-file token (+ *safe-ott-user-offset*
                                  *fingerprint-length*)))

(defun write-device-zeroes (device)
  (write-zeroes device
                :length (+ *token-length* *fingerprint-length*)
                :offset *safe-ott-device-offset*))

(defun write-user-zeroes (user-file)
  (write-zeroes user-file
                :length (+ *token-length* *fingerprint-length*)
                :offset *safe-ott-user-offset*))

(defun write-zeroes (path &key length offset)
  (let ((zeroes-buffer (make-array length
                                   :element-type '(unsigned-byte 8)
                                   :initial-element 0)))
    (with-sync-open-file (f path
                             :direction :output
                             :if-exists :overwrite
                             :element-type '(unsigned-byte 8))
      (when (file-position f offset)
        (write-sequence zeroes-buffer f)))))

(defun write-token (path token &optional (offset 0))
  (with-sync-open-file (f path
                           :direction :output
                           :if-exists :overwrite
                           :element-type '(unsigned-byte 8))
    (when (file-position f offset)
      (write-sequence token f))))
