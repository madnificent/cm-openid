(dolist (system '(:claymore :bknr.datastore :bknr.indices :cl-openid))
  (asdf:oos 'asdf:load-op system))

;;;;;;;;;;;;;;;;;;
;; define packages
(defpackage :openid-user.site
  (:use :common-lisp
	:claymore
	:claymore.routing
	:claymore.html))

(in-package :openid-user.site)

;;;;;;;;;
;; config
(defparameter *relying-party* (make-instance 'cl-openid:relying-party
					     :root-uri (puri:uri "http://freyr.homelinux.org/openid")
					     :realm (puri:uri "http://freyr.homelinux.org/"))
  "A relying party instance, filled when calling INIT-RELYING-PARTY.")

(defun get-authproc ()
  (cl-openid:handle-indirect-response 
   *relying-party* (hunchentoot:get-parameters*)
   (puri:merge-uris (hunchentoot:request-uri*) (cl-openid:root-uri *relying-party*))))
   
;;;;;;;;;;
;; routing
(claymore.routing:set-routing-table '(("openid"
				       (when indirect-response?
					 (when access-granted? access-granted-page)
					 access-denied-page)
				       (when has-openid-identifier? initiate-authentication)
				       (handles login-form loosely))))

(defwhen has-openid-identifier?
    "Specifies that the current request has an openid-identifier"
  ((&rest args)
   (hunchentoot:log-message :route-when "has-openid-identifier?")
   (when (param "openid_identifier")
     args))
  ((&rest args)
   args)) ;; can't easily enforce this

(defwhen indirect-response?
    "Checks if this is an indirect response of the openid server"
  ((&rest args)
   (hunchentoot:log-message :route-when "indirect-response?")
   (when (param cl-openid:+authproc-handle-parameter+)
     args))
  ((&rest args)
   args)) ;; can't easily enforce this

(defwhen access-granted?
    "Specifies that the access has been granted"
  ((&rest args)
   (hunchentoot:log-message :route-when "access-granted?")
   (when (get-authproc)
     args))
  ((&rest args)
   args)) ;; can't easily enforce this

;;;;;;;;
;; pages
(defun standard-page (title &rest body)
  (html (head (title title))
	(h1 title)
	body))

(defpage login-form
  (standard-page 
   "OpenID login"
   (form :method "post" :action (handler-url 'initiate-authentication)
	 (text-field "openid_identifier" T
		     :style "background-image: url('http://openid.net/wp-content/uploads/2007/10/openid_small_logo.png');background-position: 0px 0px;background-repeat: no-repeat;padding-left: 20px;"
		     :value "")
	 (submit-button :name "openid_action" :value "Login")
	 (label (input :type "checkbox" :name "checkid_immediate") " Immediate request"))))

(defun tableize-alist (alist)
  "Creates an html table from an alist"
  (table (tr (td "var") (td "value"))
	 (loop for (var val) on alist by #'cddr collect
	      (tr (td (format nil "~A" var)) (td (format nil "~A" val))))))

(defpage access-granted-page
  (standard-page "access granted"
		 (p (strong "realm:") (princ-to-string (cl-openid:realm *relying-party*)))
		 (h2 "Response:")
		 (tableize-alist (hunchentoot:get-parameters*))
		 (p :style "text-align:right;" (link-to-page "back" 'login-form))))

(defpage access-denied-page
    (standard-page "access denied"
		   (p (strong "realm:") (princ-to-string (cl-openid:realm *relying-party*)))
		   (h2 "Response:")
		   (tableize-alist (hunchentoot:get-parameters*))
		   (p :style "text-align:right;" (link-to-page "back" 'login-form))))

(defpage initiate-authentication
  (hunchentoot:log-message :info "Running page initiate-authentication")
  (hunchentoot:redirect 
   (princ-to-string 
    (cl-openid:initiate-authentication *relying-party* (param "openid_identifier")
				       :immediate-p (param "checkid_immediate")))))
