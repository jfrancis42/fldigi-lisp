;;;; package.lisp

(defpackage #:fldigi
  (:use #:cl)
  (:export :get-carrier
	   :set-carrier
	   :get-frequency
	   :set-frequency
	   :get-mode
	   :set-mode))
