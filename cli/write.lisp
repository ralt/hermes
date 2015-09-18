(in-package #:hermes)

(defun write-bytes (bytes stream)
  (loop for byte across bytes
     do (write-byte byte stream)))

(defcommand *root-commands* write (args)
  "transforms a device in an hermes device"
  (unless args
    (error 'argument-error :text "Missing argument: device"))
  (when (< (length args) 2)
    (error 'argument-error :text "Not enough arguments"))
  (when (> (length args) 2)
    (error 'argument-error :text "Too many arguments"))
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
          (with-open-file (f (merge-pathnames #p".hermes" (format nil "/home/~A/" user))
                             :direction :output
                             :element-type '(unsigned-byte 8)
                             :if-exists :overwrite
                             :if-does-not-exist :create)
            (write-bytes token f)))))))
