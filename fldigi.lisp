;;;; fldigi.lisp

(in-package #:fldigi)

;;; "fldigi" goes here. Hacks and glory await!

(defvar *rpc-port* 7362)
(defvar *rpc-host* "localhost")
(defvar *sweet-spot* 1000)
(defvar *start-wait* 5)
(defvar *char-wait* 2)
(defvar *message* "")
(defvar *message-lock* (bt:make-lock))

(defvar *default-modem* "BPSK31")
(defvar *default-mode* "USB")
(defvar *default-freq* 14070000)
(defvar *default-carrier* *sweet-spot*)

(defun fldigi-rpc (thing &optional value)
  (if value
      (s-xml-rpc:xml-rpc-call
       (s-xml-rpc:encode-xml-rpc-call thing value)
       :host *rpc-host*
       :url "/RPC2"
       :port *rpc-port*)
      (s-xml-rpc:xml-rpc-call
       (s-xml-rpc:encode-xml-rpc-call thing)
       :host *rpc-host*
       :url "/RPC2"
       :port *rpc-port*)))

(defun get-carrier-frequency ()
  "Returns the current modem carrier frequency."
  (fldigi-rpc "modem.get_carrier"))

(defun set-carrier-frequency (value)
  "Sets the current modem carrier frequency, returns the old
frequency."
  (fldigi-rpc "modem.set_carrier" value))

(defun get-dial-frequency ()
  "Returns the current radio dial frequency."
  (fldigi-rpc "main.get_frequency"))

(defun set-dial-frequency (value)
  "Sets the radio dial frequency, returns the old radio dial
frequency."
  (fldigi-rpc "main.set_frequency" (* 1.0 value)))

(defun list-modems ()
  "List available modems by name."
  (fldigi-rpc "modem.get_names"))

(defun get-modem ()
  "Returns the name of the current modem."
  (fldigi-rpc "modem.get_name"))

(defun set-modem (name)
  "Changes to the specified modem name. Returns the old modem
name. For select modems, automatically changes char timeouts (todo: it
should really do this for all modems)."
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
  "Returns the status of the auto-spot."
  (if (eql 1 (fldigi-rpc "spot.get_auto"))
      t
      nil))

(defun toggle-spot ()
  "Toggles the status of auto-spot."
  (fldigi-rpc "spot.toggle_auto"))

(defun set-spot (state)
  "Set Spot to state (t or nil)."
  (unless (eq (state (get-spot)))
    (toggle-spot)))

(defun get-afc ()
  "Returns the status of the AFC setting."
  (if (eql 1 (fldigi-rpc "main.get_afc"))
      t
      nil))

(defun toggle-afc ()
  "Toggles the status of the AFC setting."
  (fldigi-rpc "main.toggle_afc"))

(defun set-afc (state)
  "Set AFC to state (t or nil)."
  (unless (eq (state (get-afc)))
    (toggle-afc)))

(defun get-rsid-rx ()
  "Returns the status of RxID."
  (if (eql 1 (fldigi-rpc "main.get_rsid"))
      t
      nil))

(defun toggle-rsid-rx ()
  "Toggles the status of RxID."
  (fldigi-rpc "main.toggle_rsid" value))

(defun set-rsid-rx (state)
  "Set RxID to state (t or nil)."
  (unless (eq (state (get-rsid-rx)))
    (toggle-rsid-rx)))

(defun get-squelch ()
  "Returns the status of the squelch setting."
  (if (eql 1 (fldigi-rpc "main.get_squelch"))
      t
      nil))

(defun toggle-squelch ()
  "Toggles the status of the squelch setting (not valid for all
modems)."
  (fldigi-rpc "main.toggle_squelch" value))

(defun set-squelch (state)
  "Set squelch to state (t or nil)."
  (unless (eq (state (get-squelch)))
    (toggle-squelch)))

(defun get-squelch-level ()
  "Returns the current squelch level."
  (fldigi-rpc "main.get_squelch_level"))

(defun set-squelch-level (value)
  "Sets the squelch level, returns the old value."
  (fldigi-rpc "main.set_squelch_level" value))

(defun get-trx-status ()
  "Returns the current transmit status ('rx', 'tx', or 'tune')."
  (fldigi-rpc "main.get_trx_status"))

(defun set-rx ()
  "Switches to receive mode, if not already. May take a few hundred
milliseconds, depending on the current modem in use."
  (unless (equal "rx" (get-trx-status))
    (fldigi-rpc "main.rx")))

(defun set-tx ()
  "Switches to transmit mode, if not already. Sends anything that
might happen to be in the internal FLDigi transmit buffer."
  (unless (equal "tx" (get-trx-status))
    (fldigi-rpc "main.tx")))

(defun get-transmit-frequency ()
  "Returns the actual transmit frequency (dial frequency plus carrier
freqency)."
  (+ (get-dial-frequency) (get-carrier-frequency)))

(defun set-transmit-frequency (freq sweet)
  "Sets the current transmit frequency to freq. If the second argument
is true, the carrier is set to the 'sweet spot' (1000 hz for most
radios, determined by the value of the global *sweet-spot*) and the
dial frequency is calculated and set to be 1000hz lower than freq. If
the second argument is nil, the carrier is left at it's current value,
and the dial frequency is set to the specified freq minus the current
carrier frequency."
  (if sweet
      (progn
	(set-carrier-frequency *sweet-spot*)
	(set-dial-frequency (- freq *sweet-spot*)))
      (set-dial-frequency (- freq (get-carrier-frequency)))))

(defun sweet-spot ()
  "Set the carrier to the *sweet-spot* value, and adjust the dial
frequency so that the tx/rx freq is the same as before the call, but
now the carrier is in the 'sweet spot' of the rx audio (typically 1khz)."
  (set-transmit-frequency (get-transmit-frequency) t))

(defun add-tx-string (text)
  "Add a string to the buffer of data to be sent."
  (bt:with-lock-held (*message-lock*)
    (setf *message* (concatenate 'string *message* text))))

(defun send-buffer (&optional (text nil))
  "Send the current buffered transmit data, and optionally any data
specified as an argument concatenated on the end. This can be used two
ways. Text can be added with add-tx-string, and then sent with
send-buffer, or text may be added and sent at the same time with
send-buffer."
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
  "Return the modem quality number (0-100)."
  (fldigi-rpc "modem.get_quality"))

(defun search-up ()
  "Scan the carrier up until it finds a signal."
  (fldigi-rpc "modem.search_up"))

(defun search-down ()
  "Scan the carrier down until it finds a signal."
  (fldigi-rpc "modem.search_down"))

(defun get-rx-data ()
  "Return any received data. Note this may include non-ASCII garbage,
so filter accordingly."
  (map 'string #'code-char (fldigi-rpc "rx.get_data")))

(defun get-tx-data ()
  "Return any text that has been sent (mostly for internal use)."
  (babel:octets-to-string
   (fldigi-rpc "tx.get_data")))

(defun clear-rx-data ()
  "Clear any received data from the FLDigi buffer."
  (fldigi-rpc "text.clear_rx"))

(defun clear-tx-data ()
  "Clear any transmitted data from the FLDigi buffer."
  (fldigi-rpc "text.clear_tx"))

(defun clear-message ()
  "Clear the internal buffer of data to be sent."
  (bt:with-lock-held (*message-lock*)
    (setf *message* "")))

(defun list-apis ()
  "Return a list of all valid FLDigi API calls."
  (fldigi-rpc "fldigi.list"))

(defun list-modes ()
  "Return a list of modes the radio may be switched to."
  (fldigi-rpc "rig.get_modes"))

(defun get-mode ()
  "Get the current radio mode."
  (fldigi-rpc "rig.get_mode"))

(defun set-mode (mode)
  "Set the radio mode. Returns the previous mode."
  (fldigi-rpc "rig.set_mode" mode))

(defun get-version ()
  "Get the version of FLDigi we're talking to."
  (fldigi-rpc "fldigi.version"))

(defun get-afc-range ()
  "Get the current AFC range."
  (fldigi-rpc "modem.get_afc_search_range"))

(defun set-afc-range (range)
  "Set the AFC range. Return the previous range."
  (fldigi-rpc "modem.get_afc_search_range" range))

(defun get-reverse ()
  "Get the current status of the 'Reverse' modem button (not valid for
all modems)."
  (fldigi-rpc "main.get_reverse"))

(defun toggle-reverse ()
  "Toggle the status of the 'Reverse' modem button (not valid for all
modems)."
  (fldigi-rpc "main.toggle_reverse" value))

(defun set-reverse (state)
  "Set Reverse to state (t or nil)."
  (unless (eq (state (get-reverse)))
    (toggle-reverse)))

(defun tune ()
  "Switch the radio to 'tune' mode (ie, transmit a carrier). This
continues until you abort it."
  (fldigi-rpc "main.tune"))

(defun abort-tune ()
  "Abort tuning (also immediately aborts any other type of transmit
immediately)."
  (fldigi-rpc "main.abort"))

(defun auto-tune (&optional (sec 5))
  "Switch to tune mode for the optionally specified number of
seconds (default 5), then revert to receive. Allows for an auto-tuner
to auto-tune."
  (tune)
  (sleep sec)
  (abort-tune))

(defun get-status-1 ()
  "Get status message one (usually SNR)."
  (fldigi-rpc "main.get_status1"))

(defun get-status-2 ()
  "Get status message two."
  (fldigi-rpc "main.get_status2"))

(defun get-modem-bw ()
  "Get the current modem bandwidth (not valid for all modems)."
  (fldigi-rpc "modem.get_bandwidth"))

(defun set-modem-bw (bw)
  "Set modem bandwidth (not valid for all modems)."
  (fldigi-rpc "modem.set_bandwidth" bw))

(defun get-olivia-bw ()
  "Get the current Olivia bandwidth (only valid when an Olivia modem
is selected)."
  (fldigi-rpc "modem.olivia.get_bandwidth"))

(defun set-olivia-bw (bw)
  "Set modem bandwidth (only valid when an Olivia modem is selected)."
  (fldigi-rpc "modem.olivia.set_bandwidth" bw))

(defun set-defaults ()
  "Set everything back to a known good state."
  (set-modem *default-modem*)
  (set-mode *default-mode*)
  (set-dial-frequency *default-freq*)
  (set-carrier-frequency *default-carrier*))
