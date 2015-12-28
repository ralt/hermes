(in-package #:hermes)

(defun write-bytes (bytes stream)
  (loop for byte across bytes
     do (write-byte byte stream)))

(defcommand *root-commands* write (args)
  "transforms a device into a hermes device"
  (unless args
    (error 'argument-error :text "Missing argument: device"))
  (when (< (length args) 2)
    (error 'argument-error :text "Not enough arguments"))
  (when (> (length args) 2)
    (error 'argument-error :text "Too many arguments"))
  (unless (= (sb-posix:geteuid) 0)
    (error 'privilege-error :text "You need to be root"))
  (let* ((token-length 128)
         (token (make-array token-length :element-type '(unsigned-byte 8)))
         (device (first args))
         (user (second args)))
    (when (yes-or-no-p "Do you really want to erase all data on ~A?" device)
      (when (yes-or-no-p "Are you sure?")
        (when (yes-or-no-p "ALL DATA ON ~A ABOUT TO BE ERASED! Are you sure?" device)
          (with-open-file (f #p"/dev/urandom"
                             :direction :input
                             :element-type '(unsigned-byte 8))
            (read-sequence token f :start 0 :end token-length))
          (with-open-file (f device
                             :direction :output
                             :element-type '(unsigned-byte 8)
                             :if-exists :overwrite
                             :if-does-not-exist :create)
            (write-bytes #(82 111 98 105 110) f)
            (write-bytes token f))
          (with-open-file (f (merge-pathnames user #p"/etc/hermes/")
                             :direction :output
                             :element-type '(unsigned-byte 8)
                             :if-exists :overwrite
                             :if-does-not-exist :create)
            (write-bytes token f))
          (format t "USB key and user file have the token.~%")
          (multiple-value-bind (_ __ code)
              (uiop:run-program (format nil "usermod -a -G hermes ~A" user))
            (declare (ignore _ __))
            (unless (= code 0)
              (error (format nil "Couldn't add ~A to the hermes group" user))))
          (format t "Added ~A to the hermes group.~%" user))))))
