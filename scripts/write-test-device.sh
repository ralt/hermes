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

(defun get-key (string)
  (mapcar #'char-code (vector-to-list (second (cl-ppcre:split " " string)))))

(let ((public-key (get-key (alexandria:read-file-into-string "id_rsa.pub")))
      (private-key (vector-to-list (alexandria:read-file-into-byte-vector "id_rsa"))))
 (with-open-file (f #p"test.bin"
                    :direction :output
                    :element-type '(unsigned-byte 8)
                    :if-exists :overwrite
                    :if-does-not-exist :create)
                 (write-bytes (string-to-bytes "Robin") f)
                 (write-byte 1 f) ;; 1 = rsa pair
                 (write-bytes public-key f)
                 (write-bytes private-key f)))
