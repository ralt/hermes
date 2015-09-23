(in-package #:hermes-daemon)

(defvar *max-username-length* 33)
(defvar *socket-path* #p"/var/run/hermes.sock")
(defvar *fingerprint* #(82 111 98 105 110))
(defvar *fingerprint-length* (length *fingerprint*))
(defvar *token-length* 128)
(defvar *user-tokens-path* #p"/etc/hermes/")
(defvar *storage-device-prefix* "sd")
(defvar *devices-folder* #p"/dev/")

(defvar *safe-ott-device-offset* 133)
(defvar *safe-ott-user-offset* 128)

(defmacro loop-with-unix-socket (vars &body body)
  (let ((socket (first vars))
        (server (gensym)))
    `(progn
       (when (probe-file *socket-path*)
         (sb-posix:unlink *socket-path*))
       (sockets:with-open-socket (,server :address-family :local
                                         :local-filename (namestring *socket-path*)
                                         :connect :passive)
         (sb-posix:chown *socket-path* 0 (sb-posix:group-gid (sb-posix:getgrnam "hermes")))
         (sb-posix:chmod *socket-path* #o660)
         (loop do (let ((,socket (sockets:accept-connection ,server)))
                    ,@body
                    (close ,socket)))))))

(defun main (&rest args)
  (declare (ignore args))
  (loop-with-unix-socket (socket)
    (let ((user-buffer (make-array *max-username-length*
                                   :element-type '(unsigned-byte 8))))
      (when (> (read-sequence user-buffer socket) 0)
        (let ((user (buffer-to-string user-buffer)))
          (write-byte (handler-case
                          (if (and (can-login-p user)
                                   (regenerate-token user))
                              1
                              0)
                        (error () 0))
                      socket))))))

(defun buffer-to-string (buffer)
  (coerce (loop for byte across buffer
             until (= byte 0)
             collect (code-char byte))
          'string))

;;; See "Safe One-Time Tokens" (aka the big-ass comment for the
;;; "regenerate-token" function) for why this function is following a
;;; kinda convoluted process to validate tokens.
(defun can-login-p (user)
  (let ((device (find-hermes-device))
        (user-file (merge-pathnames user *user-tokens-path*)))
    (unless device
      (return-from can-login-p nil))
    (unless (probe-file user-file)
      (return-from can-login-p nil))
    (let ((user-token (read-user-token user-file))
          (device-token (read-device-token device)))
      ;; New user token vs new device token, the classic path.
      (when (timing-safe-compare user-token
                                 device-token
                                 *token-length*)
        (return-from can-login-p t))
      (unless (has-hermes-fingerprint device *safe-ott-device-offset*)
        (return-from can-login-p nil))
      (let ((device-old-token (read-device-old-token device)))
        ;; Another possible path: new user token vs old device token.
        ;; This can happen if writing the new token on device failed.
        (when (timing-safe-compare user-token
                                   device-old-token
                                   *token-length*)
          (return-from can-login-p t))
        ;; Last possible path: old user token vs old device token.
        ;; This can happen if writing the new token on user file
        ;; failed.
        (unless (user-has-old-token-fingerprint user-file)
          (return-from can-login-p nil))
        (timing-safe-compare (read-user-old-token user-file)
                             device-old-token
                             *token-length*)))))

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
    (when (= (file-position f offset) offset)
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
  (find-if #'is-hermes-device (remove-if-not #'is-storage-device (cl-fad:list-directory *devices-folder*))))

(defun is-storage-device (path)
  (let ((file (pathname-name path)))
    (when (and file (> (length file) 2))
      (string= (subseq file 0 2) *storage-device-prefix*))))

(defun is-hermes-device (path)
  (and (is-block-device path)
       (has-hermes-fingerprint path)))

(defun is-block-device (path)
  (sb-posix:s-isblk (sb-posix:stat-mode (sb-posix:stat path))))

(defun has-hermes-fingerprint (path &optional (offset 0))
  (with-open-file (f path
                     :direction :input
                     :element-type '(unsigned-byte 8))
    (when (= (file-position f offset) offset)
      (let ((bytes (make-array *fingerprint-length* :element-type '(unsigned-byte 8))))
        (read-sequence bytes f)
        (every #'= bytes *fingerprint*)))))

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
    (when (and device (probe-file user-file))
      (write-device-old-token device old-token)
      (write-device-token device new-token)
      (write-user-old-token user-file old-token)
      (write-user-token user-file new-token)
      (write-device-zeroes device)
      (write-user-zeroes user-file))))

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
    (with-open-file (f path
                       :direction :output
                       :if-exists :overwrite
                       :element-type '(unsigned-byte 8))
      (when (= (file-position f offset) offset)
        (write-sequence zeroes-buffer f)))))

(defun write-token (path token &optional (offset 0))
  (with-open-file (f path
                     :direction :output
                     :if-exists :overwrite
                     :element-type '(unsigned-byte 8))
    (when (= (file-position f offset) offset)
      (write-sequence token f))))
