;;;; package.lisp

(defpackage #:fldigi
  (:use #:cl)
  (:export :*rpc-port*
	   :*rpc-host*
	   :*sweet-spot*
	   :get-trx-status
	   :get-carrier-frequency
	   :set-carrier-frequency
	   :get-dial-frequency
	   :set-dial-frequency
	   :get-modem-name
	   :get-modem
	   :set-modem
	   :get-spot
	   :toggle-spot
	   :get-afc
	   :toggle-afc
	   :get-rsid-rx
	   :toggle-rsid-rx
	   :get-squelch
	   :toggle-squelch
	   :get-squelch-level
	   :set-squelch-level
	   :set-rx
	   :set-tx
	   :get-transmit-frequency
	   :set-transmit-frequency
	   :add-tx-string
	   :send-buffer
	   :get-modem-quality
	   :search-up
	   :search-down
	   :get-rx-data
	   :get-tx-data
	   :clear-rx-data
	   :clear-tx-data
	   :clear-message
	   :list-modems
	   :list-apis
	   :list-modes
	   :get-mode
	   :set-mode
	   :get-version
	   :get-afc-range
	   :set-afc-range
	   :get-reverse
	   :toggle-reverse
	   :tune
	   :abort-tune
	   :get-status-1
	   :get-status-2
	   :get-modem-bw
	   :set-modem-bw
	   :get-olivia-bw
	   :set-olivia-bw))
