(defsystem "lack-compression-cache"
  :version "0.1.0"
  :author "Daniel Nussenbaum"
  :license "MIT"
  :depends-on ("lack-app-file"
               "lack-component"
               "alexandria")
  :components ((:file "lack-compression-cache")))

;; (register-system-packages "lack-compression-cache" '(:lack.middleware.static))
