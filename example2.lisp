(ql:quickload "clack")
(ql:quickload "lack")

(defvar *app*
  (lambda (env)
    (declare (ignorable env))
    '(200 (:content-type "text/plain") ("Hello, Compression2!"))))

(defvar *compressed-app*
  (lack:builder
   (:compression-cache :cache-path "bin/cache" :static-files-path "static/")
   (:static :path "/public/"
            :root #P"/static-files/")   
   *app*))

(defvar *clack-handler*
  (clack:clackup *compressed-app*))

;; To Stop the App
(clack:stop *clack-handler*)

