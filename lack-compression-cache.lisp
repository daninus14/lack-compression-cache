(defpackage lack/middleware/compression-cache
  (:nicknames :lack.middleware.compression-cache)
  (:use :cl)
  (:import-from :lack/component
   :call)
  (:import-from :lack/app/file
   :lack-app-file)
  (:import-from :alexandria
   :starts-with-subseq
                :if-let)
  (:export :*lack-middleware-compression-cache*
   :call-app-file))
(in-package :lack/middleware/compression-cache)

(defparameter *lack-middleware-compression-cache*
  (lambda (app &key path (root #P"./"))
    (etypecase path
      (null app)
      (string
       (lambda (env)
         (let ((path-info (getf env :path-info)))
           (if (starts-with-subseq path path-info)
               (progn
                 (setf (getf env :path-info)
                       (subseq path-info (1- (length path))))
                 (call-app-file root env))
               (funcall app env)))))
      (function
       (lambda (env)
        (let ((path-info (getf env :path-info)))
          (if-let (new-path (funcall path path-info))
            (progn
              (setf (getf env :path-info) new-path)
              (call-app-file root env))
            (funcall app env)))))))
  "Middleware for serving compressed cached files")

(defun call-app-file (root env)
  (call (make-instance 'lack-app-file :root root) env))
