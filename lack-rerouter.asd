(defsystem "lack-rerouter"
  :version "0.1.0"
  :author "Daniel Nussenbaum"
  :license "MIT"
  :depends-on ("lack-app-file"
               "lack-component"
               "local-time"
               "alexandria")
  :components ((:file "lack-rerouter")))

;; (register-system-packages "lack-compression-cache" '(:lack.middleware.static))
