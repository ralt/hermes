#!/usr/bin/sbcl --script

(load "~/.sbclrc")

(defun write-bytes (stream bytes)
  (dolist (byte bytes)
    (write-byte byte stream)))

(defun string-to-bytes (string)
  (loop for char across string collect (char-code char)))

(with-open-file (f #p"test.bin"
                   :direction :output
                   :element-type '(unsigned-byte 8)
                   :if-exists :overwrite)
  (write-bytes f (string-to-bytes "Robin")))
