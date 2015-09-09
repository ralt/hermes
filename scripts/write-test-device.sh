#!/usr/bin/sbcl --script

(load "/home/florian/.sbclrc")

(ql:quickload :alexandria)
(ql:quickload :cl-ppcre)

(defun write-bytes (bytes stream)
  (dolist (byte bytes)
    (write-byte byte stream)))

(defun string-to-bytes (string)
  (loop for char across string collect (char-code char)))

(defun vector-to-list (vec)
  (loop for el across vec collect el))

(defun get-public-key (string)
  (mapcar #'char-code (vector-to-list (second (cl-ppcre:split " " string)))))

(defun 4-bytes-length (string)
  (let ((integer (length string)))
    (list
     (logand integer #xFF)
     (logand (ash integer -8) #xFF)
     (logand (ash integer -16) #xFF)
     (logand (ash integer -24) #xFF))))

(let ((public-key (get-public-key (alexandria:read-file-into-string "id_rsa.pub")))
      (private-key (vector-to-list (alexandria:read-file-into-byte-vector "id_rsa"))))
 (with-open-file (f #p"/dev/sdb"
                    :direction :output
                    :element-type '(unsigned-byte 8)
                    :if-exists :overwrite
                    :if-does-not-exist :create)
                 (write-bytes (string-to-bytes "Robin") f)
                 (write-byte 1 f) ;; 1 = rsa pair
                 (write-bytes (4-bytes-length public-key) f)
                 (write-bytes public-key f)
                 (write-bytes (4-bytes-length private-key) f)
                 (write-bytes private-key f)))
