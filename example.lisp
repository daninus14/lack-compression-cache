(ql:quickload "clack")

(defvar *app*
  (lambda (env)
    (declare (ignorable env))
    '(200 (:content-type "text/plain") ("Hello, Compression2!"))))

(defvar *compressed-app*
  (funcall lack/middleware/compression.cache:*lack-middleware-compression-cache*
           *app* :cache-path "bin/cache" :static-files-path "/static/"))

(defvar *clack-handler*
  (clack:clackup *compressed-app*))

;; To Stop the App
(clack:stop *clack-handler*)
