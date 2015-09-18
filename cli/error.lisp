(in-package #:hermes)

(define-condition argument-error (error)
  ((text :initarg :text :reader :text)))
