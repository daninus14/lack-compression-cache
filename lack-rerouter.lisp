(defpackage lack/middleware/rerouter
  (:nicknames :lack.middleware.rerouter)
  (:use :cl)
  (:import-from :local-time)
  (:import-from :lack.component
   :call)
  (:import-from :lack.app.file
   :lack-app-file)
  (:import-from :alexandria
   :starts-with-subseq
                :if-let)
  (:export :*lack-middleware-rerouter*))
(in-package :lack/middleware/rerouter)

;; (defun remove-leading-slash (string)
;;   (if (and (stringp string) (char= #\/ (aref string 0)))
;;       (subseq string 1)
;;       string))

(defun get-new-app-with-updated-route (app route-prefix static-files-path operation)
  (let ((operation-function
          (cond ((equal :prepend operation)
                 (lambda (path-info)
                   (format NIL "~A~A" static-files-path
                           path-info)))
                ((equal :replace operation)
                 (lambda (path-info)
                   (format NIL "~A~A" static-files-path
                           (subseq path-info
                                   (1- (length route-prefix))))))
                (T operation))))
    (lambda (env)
      (let ((path-info (getf env :path-info)))
        ;; if uri is prefixed by route-prefix, then change the path-info to start with
        ;; static-files-path
        (when (starts-with-subseq route-prefix path-info)
          (setf (getf env :path-info)
                (funcall operation-function path-info)))
        (funcall app env)))))

(defparameter *lack-middleware-rerouter*
  (lambda (app &key route-prefix static-files-path (operation :prepend))
    (etypecase route-prefix
      (null app)
      (string
       (get-new-app-with-updated-route app route-prefix static-files-path operation))))
  "Middleware for rerouting request URIs (routes)")
