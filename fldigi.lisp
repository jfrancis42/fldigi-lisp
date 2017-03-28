;;;; fldigi.lisp

(in-package #:fldigi)

;;; "fldigi" goes here. Hacks and glory await!

(defparameter *rpc-port* 7362)
(defparameter *rpc-host* "localhost")
(defparameter *sweet-spot* 1000)
(defparameter *start-wait* 5)
(defparameter *char-wait* 2)
(defparameter *message* "")
(defparameter *message-lock* (bt:make-lock))

(defun rpc-get (thing)
  (s-xml-rpc:xml-rpc-call (s-xml-rpc:encode-xml-rpc-call thing) :host *rpc-host* :url "/RPC2" :port *rpc-port*))

(defun rpc-set (thing value)
    (s-xml-rpc:xml-rpc-call (s-xml-rpc:encode-xml-rpc-call thing value) :host *rpc-host* :url "/RPC2" :port *rpc-port*))

(defun get-carrier-frequency ()
  (rpc-get "modem.get_carrier"))

(defun set-carrier-frequency (value)
  (rpc-set "modem.set_carrier" value))

(defun get-dial-frequency ()
  (rpc-get "main.get_frequency"))

(defun set-dial-frequency (value)
  (rpc-set "main.set_frequency" (* 1.0 value)))

(defun get-modem ()
  (rpc-get "modem.get_name"))

(defun set-modem (name)
  (rpc-set "modem.set_by_name" name))

(defun get-spot ()
  (rpc-get "spot.get_auto"))

(defun set-spot (value)
  (rpc-set "spot.set_auto" value))

(defun get-afc ()
  (rpc-get "main.get_afc"))

(defun set-afc (value)
  (rpc-set "main.set_afc" value))

(defun get-sideband ()
  (rpc-get "main.get_sideband"))

(defun set-sideband (value)
  (rpc-set "main.set_sideband" value))

(defun get-rsid-rx ()
  (rpc-get "main.get_rsid"))

(defun set-rsid-rx (value)
  (rpc-set "main.set_rsid" value))

(defun get-squelch ()
  (rpc-get "main.get_squelch"))

(defun set-squelch (value)
  (rpc-set "main.set_squelch" value))

(defun get-squelch-level ()
  (rpc-get "main.get_squelch_level"))

(defun set-squelch-level (value)
  (rpc-set "main.set_squelch_level" value))

(defun set-rx ()
  (unless (equal "rx" (rpc-get "main.get_trx_status"))
    (rpc-set "main.rx")))

(defun set-tx ()
  (unless (equal "tx" (rpc-get "main.get_trx_status"))
    (rpc-set "main.tx")))

(defun get-transmit-frequency ()
  (+ (get-dial-frequency) (get-carrier-frequency)))

(defun set-transmit-frequency (freq sweet)
  (if sweet
      (progn
	(set-carrier-frequency *sweet-spot*)
	(set-dial-frequency (- freq *sweet-spot*)))
      (set-dial-frequency (- freq (get-carrier-frequency)))))

(defun add-tx-string (text)
  (bt:with-lock-held (*message-lock*)
    (setf *message* (concatenate 'string *message* text))))

(defun get-modem-quality ()
  (rpc-get "modem.get_quality"))

(defun search-up ()
  (rpc-get "modem.search_up"))

(defun search-down ()
  (rpc-get "modem.search_down"))

(defun get-rx-data ()
  (babel:octets-to-string
   (rpc-get "rx.get_data")))

(defun get-tx-data ()
  (babel:octets-to-string
   (rpc-get "tx.get_data")))

(defun clear-rx-data ()
  (rpc-get "text.clear_rx"))

(defun clear-tx-data ()
  (rpc-get "text.clear_tx"))

(defun clear-message ()
    (bt:with-lock-held (*message-lock*)
      (setf *message* "")))

(defun list-modems ()
  (rpc-get "modem.get_names"))

(defun list-apis ()
  (rpc-get "fldigi.list"))







