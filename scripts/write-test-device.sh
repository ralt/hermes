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

;;; /dev/sdb is my usb stick with 2 partitions:
;;;   - /dev/sdb1: the hermes device
;;;   - /dev/sdb2: a fat32 partition
;;; I made this with:
;;; # dd if=/dev/zero of=/dev/sdb bs=4k
;;; # sync
;;; # fdisk /dev/sdb
;;; [create empty DOS partition table with "o"]
;;; [create new 2048 bytes partition with "n" (and change the end to be 4096)]
;;; [create new partition with "n" (and attribute rest of blocks)]
;;; [sync changes with "w"]
(with-open-file (f #p"/dev/sdb1"
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
