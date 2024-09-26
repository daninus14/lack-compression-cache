(defpackage lack/middleware/compression.cache
  (:nicknames :lack.middleware.compression.cache)
  (:use :cl)
  (:import-from :local-time)
  (:import-from :lack.component
   :call)
  (:import-from :lack.app.file
   :lack-app-file)
  (:import-from :alexandria
   :starts-with-subseq
                :if-let)
  (:export :*lack-middleware-compression-cache*
   :call-app-file))
(in-package :lack/middleware/compression.cache)

(defvar *cache-initialized* NIL)

(defun remove-leading-slash (string)
  (if (and (stringp string) (char= #\/ (aref string 0)))
      (subseq string 1)
      string))

(defun apply-middleware (app static-files-path &optional (root #P"./") no-http-cache)
  (etypecase static-files-path
    (null app)
    (string
     (lambda (env)
       (let ((path-info (getf env :path-info)))
         (if (and (uiop:file-exists-p
                   (uiop:merge-pathnames* (remove-leading-slash path-info) root))
                  (starts-with-subseq static-files-path path-info))
             (ensure-optimal-filepath app env root no-http-cache)
             (funcall app env)))))
    ;; (function
    ;;  (lambda (env)
    ;;   (let ((path-info (getf env :path-info)))
    ;;     (if-let (new-path (funcall static-files-path path-info))
    ;;       (progn
    ;;         (setf (getf env :path-info) new-path)
    ;;         (call-app-file root env))
    ;;       (funcall app env)))))
    ))

(defparameter *lack-middleware-compression-cache*
  (lambda (app &key cache-path static-files-path (root #P"./") (no-http-cache NIL))
    (if *cache-initialized* (apply-middleware app static-files-path root no-http-cache)
        (progn
          (compression-cache:initialize-cache (uiop:merge-pathnames* cache-path root))
          (apply-middleware app static-files-path root no-http-cache))))
  "Middleware for serving compressed cached files")

(defvar *already-compressed-file-extensions*
  (list "jpeg" "png" "gif" "webp" "mp3" "aac" "flac" "ogg" "mp4" "avi" "mov"
        "webm" "zip" "rar" "tar" "7z"))

(defun ends-with-p (end s &key (ignore-case NIL))
  "Return t if s ends with the substring 'end', nil otherwise.

  END can be a character or a string."
  (let ((s-length (length s))
        (end-length (length (string end))))
    (when (>= s-length end-length)
      (let ((fn (if ignore-case #'string-equal #'string=)))
        (funcall fn s end :start1 (- s-length end-length))))))

(defun needs-compression (filepath)
  (let ((filename (format NIL "~A" filepath)))
    (dolist (curr-extension *already-compressed-file-extensions* T)
      (when (ends-with-p curr-extension
                         filename)
        (return NIL)))))

(defun ensure-optimal-filepath (app env &optional (root #P"./") no-http-cache)
  "This will call the app function on the environment which has an optimal :path-info.
First we will check if the request has a \"Accept-Encoding: gzip\" header.
If it doesn't, it will maintain the environment as it is.
If it does, then it will check the extension of the file requested to see
if it is beneficial to gzip compress. Please see the readme for a list of
extensions that will not be compressed.
If the file extension is not of the already compressed extensions listed,
then a compressed version of the file will be served. This compressed version
will be cached for future requests."
  (let* ((headers (getf env :headers))
         (accept-encoding (when headers (gethash "accept-encoding" headers)))
         (accepts-gzip (when accept-encoding (search "gzip" accept-encoding)))
         (path-info (getf env :path-info)))
    (if (and accepts-gzip (needs-compression path-info))
        ;; here provide the file copression alternative
        ;; and set the encoding header
        ;; and set the cache header
        ;; there should be an option for the length of the cache header
        (list 200
              (append
               (list :content-encoding "gzip")
               (when (null no-http-cache)
                 (list :cache-control "max-age=31536000, immutable"
                       :expires (local-time:to-rfc1123-timestring
                                 (local-time:timestamp+ (local-time:now) 1 :year)))))
              (compression-cache:ensure-path-to-compressed-file
               (remove-leading-slash path-info)))
        (call-app-file root env))))

(defun call-app-file (root env)
  (lack.component:call (make-instance 'lack.app.file:lack-app-file :root root) env))
