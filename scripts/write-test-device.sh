#!/usr/bin/sbcl --script

(load "/home/florian/.sbclrc")

(ql:quickload :alexandria)
(ql:quickload :cl-ppcre)
(ql:quickload :ironclad)

(defpackage #:hermes
  (:use :cl))

(in-package #:hermes)

(defvar *token-length* 128)

(setf ironclad:*prng* (ironclad:make-prng :fortuna))

(defun write-bytes (bytes stream)
  (dolist (byte bytes)
    (write-byte byte stream)))

(defun string-to-bytes (string)
  (loop for char across string collect (char-code char)))

(defun vector-to-list (vec)
  (loop for el across vec collect el))

(defvar *token* (vector-to-list (ironclad:random-data *token-length*)))

(with-open-file (f #p"/dev/sdb"
                   :direction :output
                   :element-type '(unsigned-byte 8)
                   :if-exists :overwrite
                   :if-does-not-exist :create)
  (write-bytes (string-to-bytes "Robin") f)
  (write-bytes *token* f))

(with-open-file (f (merge-pathnames #p".hermes" (user-homedir-pathname))
                   :direction :output
                   :element-type '(unsigned-byte 8)
                   :if-exists :overwrite
                   :if-does-not-exist :create)
  (write-bytes *token* f))
