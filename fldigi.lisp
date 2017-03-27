;;;; fldigi.lisp

(in-package #:fldigi)

;;; "fldigi" goes here. Hacks and glory await!

(defparameter *rpc-port* 7362)
(defparameter *rpc-host* "localhost")

(defun rpc-get (thing)
  (s-xml-rpc:xml-rpc-call (s-xml-rpc:encode-xml-rpc-call thing) :host *rpc-host* :url "/RPC2" :port *rpc-port*))

(defun rpc-set (thing value)
    (s-xml-rpc:xml-rpc-call (s-xml-rpc:encode-xml-rpc-call thing value) :host *rpc-host* :url "/RPC2" :port *rpc-port*))

(defun get-carrier ()
  (rpc-get "modem.get_carrier"))

(defun set-carrier (value)
  (rpc-set "modem.set_carrier" value)
  (equalp (rpc-get "modem.get_carrier") value))

(defun get-frequency ()
  (rpc-get "main.get_frequency"))

(defun set-frequency (value)
  (rpc-set "main.set_frequency" (* 1.0 value))
  (equalp (rpc-get "main.get_frequency") value))

(defun get-mode ()
  (rpc-get "main.get_sideband"))

(defun set-mode (value)
  (rpc-set "main.set_sideband" value)
  (equalp (rpc-get "main.get_sideband") value))

