;;;; package.lisp

(defpackage #:fldigi
  (:use #:cl)
  (:export :*rpc-port*
	   :*rpc-host*
	   :*sweet-spot*
	   :get-carrier-frequency
	   :set-carrier-frequency
	   :get-dial-frequency
	   :set-dial-frequency
	   :get-modem-name
	   :get-modem
	   :set-modem
	   :get-spot
	   :set-spot
	   :get-afc
	   :set-afc
	   :get-sideband
	   :set-sideband
	   :get-rsid-rx
	   :set-rsid-rx
	   :get-squelch
	   :set-squelch
	   :get-squelch-level
	   :set-squelch-level
	   :set-rx
	   :set-tx
	   :get-transmit-frequency
	   :set-transmit-frequency
	   :add-tx-string
	   :get-modem-quality
	   :search-up
	   :search-down
	   :get-rx-data
	   :get-tx-data
	   :clear-rx-data
	   :clear-tx-data
	   :clear-message
	   :list-modems
	   :list-apis))
