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

(defun fldigi-rpc (thing &optional value)
  (if value
      (s-xml-rpc:xml-rpc-call (s-xml-rpc:encode-xml-rpc-call thing value) :host *rpc-host* :url "/RPC2" :port *rpc-port*)
      (s-xml-rpc:xml-rpc-call (s-xml-rpc:encode-xml-rpc-call thing) :host *rpc-host* :url "/RPC2" :port *rpc-port*)))

(defun get-carrier-frequency ()
  (fldigi-rpc "modem.get_carrier"))

(defun set-carrier-frequency (value)
  (fldigi-rpc "modem.set_carrier" value))

(defun get-dial-frequency ()
  (fldigi-rpc "main.get_frequency"))

(defun set-dial-frequency (value)
  (fldigi-rpc "main.set_frequency" (* 1.0 value)))

(defun get-modem ()
  (fldigi-rpc "modem.get_name"))

(defun set-modem (name)
  (cond
    ((equal "BPSK31" name)
     (setf *start-wait* 5)
     (setf *char-wait* 1))
    ((equal "BPSK63" name)
     (setf *start-wait* 5)
     (setf *char-wait* 0.5))
    ((equal "BPSK125" name)
     (setf *start-wait* 5)
     (setf *char-wait* 0.5))
    ((equal "BPSK250" name)
     (setf *start-wait* 5)
     (setf *char-wait* 0.5))
    (t
     (setf *start-wait* 5)
     (setf *char-wait* 2)))
  (fldigi-rpc "modem.set_by_name" name))

(defun get-spot ()
  (fldigi-rpc "spot.get_auto"))

(defun toggle-spot (value)
  (fldigi-rpc "spot.toggle_auto"))

(defun get-afc ()
  (fldigi-rpc "main.get_afc"))

(defun toggle-afc (value)
  (fldigi-rpc "main.toggle_afc"))

(defun get-rsid-rx ()
  (fldigi-rpc "main.get_rsid"))

(defun toggle-rsid-rx ()
  (fldigi-rpc "main.toggle_rsid" value))

(defun get-squelch ()
  (fldigi-rpc "main.get_squelch"))

(defun toggle-squelch ()
  (fldigi-rpc "main.toggle_squelch" value))

(defun get-squelch-level ()
  (fldigi-rpc "main.get_squelch_level"))

(defun set-squelch-level (value)
  (fldigi-rpc "main.set_squelch_level" value))

(defun get-trx-status ()
  (fldigi-rpc "main.get_trx_status"))

(defun set-rx ()
  (unless (equal "rx" (get-trx-status))
    (fldigi-rpc "main.rx")))

(defun set-tx ()
  (unless (equal "tx" (get-trx-status))
    (fldigi-rpc "main.tx")))

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

(defun send-buffer (&optional (text nil))
  (when text (add-tx-string text))
  (bt:with-lock-held (*message-lock*)
    (when (> (length *message*) 0)
      (fldigi-rpc "text.add_tx" *message*)
      (set-tx)
      (sleep *start-wait*)
      (loop while (> (length (get-tx-data)) 0)
	 do (sleep *char-wait*))
      (setf *message* "")
      (set-rx))))

(defun get-modem-quality ()
  (fldigi-rpc "modem.get_quality"))

(defun search-up ()
  (fldigi-rpc "modem.search_up"))

(defun search-down ()
  (fldigi-rpc "modem.search_down"))

(defun get-rx-data ()
  (map 'string #'code-char (fldigi-rpc "rx.get_data")))

(defun get-tx-data ()
  (babel:octets-to-string
   (fldigi-rpc "tx.get_data")))

(defun clear-rx-data ()
  (fldigi-rpc "text.clear_rx"))

(defun clear-tx-data ()
  (fldigi-rpc "text.clear_tx"))

(defun clear-message ()
    (bt:with-lock-held (*message-lock*)
      (setf *message* "")))

(defun list-modems ()
  (fldigi-rpc "modem.get_names"))

(defun list-apis ()
  (fldigi-rpc "fldigi.list"))

(defun list-modes ()
  (fldigi-rpc "rig.get_modes"))

(defun get-mode ()
  (fldigi-rpc "rig.get_mode"))

(defun set-mode (mode)
  (fldigi-rpc "rig.set_mode" mode))

(defun get-version ()
  (fldigi-rpc "fldigi.version"))

(defun get-afc-range ()
  (fldigi-rpc "modem.get_afc_search_range"))

(defun set-afc-range (range)
  (fldigi-rpc "modem.get_afc_search_range" range))

(defun get-reverse ()
  (fldigi-rpc "main.get_reverse"))

(defun toggle-reverse ()
  (fldigi-rpc "main.toggle_reverse" value))

(defun tune ()
  (fldigi-rpc "main.tune" value))

(defun abort-tune ()
  (fldigi-rpc "main.abort" value))

(defun get-status-1 ()
  (fldigi-rpc "main.get_status1"))

(defun get-status-2 ()
  (fldigi-rpc "main.get_status2"))

(defun get-modem-bw ()
  (fldigi-rpc "modem.get_bandwidth"))

(defun get-modem-bw (bw)
  (fldigi-rpc "modem.set_bandwidth" bw))

(defun get-olivia-bw ()
  (fldigi-rpc "modem.olivia.get_bandwidth"))

(defun get-olivia-bw (bw)
  (fldigi-rpc "modem.olivia.set_bandwidth" bw))

