#!/usr/bin/sbcl --script

(load (merge-pathnames #p".sbclrc" (user-homedir-pathname)))

(ql:quickload :alexandria)
(ql:quickload :cl-ppcre)

(defpackage #:hermes
  (:use :cl))

(in-package #:hermes)

(defun write-bytes (bytes stream)
  (loop for byte across bytes
     do (write-byte byte stream)))

(defvar *token-length* 128)
(defvar *token* (make-array *token-length* :element-type '(unsigned-byte 8)))

(with-open-file (f #p"/dev/urandom"
                   :direction :input
                   :element-type '(unsigned-byte 8))
  (read-sequence *token* f :start 0 :end *token-length*))

(with-open-file (f #p"/dev/sdb"
                   :direction :output
                   :element-type '(unsigned-byte 8)
                   :if-exists :overwrite
                   :if-does-not-exist :create)
  (write-bytes #(82 111 98 105 110) f)
  (write-bytes *token* f))

(with-open-file (f (merge-pathnames #p".hermes" (user-homedir-pathname))
                   :direction :output
                   :element-type '(unsigned-byte 8)
                   :if-exists :overwrite
                   :if-does-not-exist :create)
  (write-bytes *token* f))
