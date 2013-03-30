(defpackage #:xzlib-system
  (:use :cl :asdf))

(in-package #:xzlib-system)

(defsystem :xzlib
  :version "0.1"
  :components ((:file "package")
			   (:file "xzlib" :depends-on ("package"))
			   (:static-file TODO)))

