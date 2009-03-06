(dolist (system '(:claymore :cl-openid))
  (asdf:oos 'asdf:load-op system))

;;;;;;;;;;;;;;;;;;
;; define packages
(defpackage :openid-user
  (:use :common-lisp
	:claymore
	:claymore.routing
	:claymore.html)
  (:export :openid
	   :indirect-response?
	   :access-granted?
	   :has-openid-identifier?
	   :login-form
	   :login-form-page
	   :*relying-party*
	   :*before-access-granted-page*))
	   

(in-package :openid-user)

;;;;;;;;;
;; config
;; This is something the framework should abstract in some way or another
(defvar *relying-party* nil
  "The relying party")
(defvar *host* nil
  "The hostname that will be used (if specified)")

(defun host ()
  (or *host* 
      (concatenate 'string "http://" (hunchentoot:host) "")))
(defun login-page-uri ()
  "URI of the login page"
  (concatenate 'string (host) (claymore.routing:handler-url 'login-form-page)))

(defun relying-party ()
  "Returns the relying party or sets it when no relying party has been specified before."
  (or *relying-party*
      (setf *relying-party*
	    (make-instance 'cl-openid:relying-party
			   :root-uri (puri:uri (login-page-uri))
			   :realm (puri:uri (host))))))

(defun get-authproc ()
  (cl-openid:handle-indirect-response 
   (relying-party) (hunchentoot:get-parameters*)
   (puri:merge-uris (hunchentoot:request-uri*) (cl-openid:root-uri (relying-party)))))
   
;;;;;;;;;;
;; routing
(setf (claymore.routing:subroute 'openid) '((when indirect-response?
					      (when access-granted? access-granted-page)
					      access-denied-page)
					    (when has-openid-identifier? initiate-authentication)
					    (handles login-form-page loosely)))

(defwhen has-openid-identifier?
    "Specifies that the current request has an openid-identifier"
  ((&rest args)
   (when (param "openid_identifier")
     args))
  ((&rest args)
   args)) ;; can't easily enforce this

(defwhen indirect-response?
    "Checks if this is an indirect response of the openid server"
  ((&rest args)
   (when (param cl-openid:+authproc-handle-parameter+)
     args))
  ((&rest args)
   args)) ;; can't easily enforce this

(defwhen access-granted?
    "Specifies that the access has been granted"
  ((&rest args)
   (when (get-authproc)
     args))
  ((&rest args)
   args)) ;; can't easily enforce this

;;;;;;;;
;; pages
(defun trivial-page (title &rest body)
  "This page is used for the simplest standard views generated here.  You should overwrite the correct pages, as to be able to redirect to the right page after a user-login."
  (html (head (title title))
	(h1 title)
	body))

(defun login-form ()
  (form :method "post" :action (handler-url 'initiate-authentication)
	(text-field "openid_identifier" T
		    :style "background-image: url('http://openid.net/wp-content/uploads/2007/10/openid_small_logo.png');background-position: 0px 0px;background-repeat: no-repeat;padding-left: 20px;"
		    :value "")
	(submit-button :name "openid_action" :value "Login")))


(defpage login-form-page
  (trivial-page
   "OpenID login"
   (login-form)))

(defvar *before-access-granted-page* (lambda ())
  "This is ran before the access-granted-page is rendered.")

(defpage access-granted-page
  (funcall *before-access-granted-page*)
  (trivial-page "success"
		(p "Your id has been accepted")))

(defpage access-denied-page
  (trivial-page "access denied"
		(p "The openid service didn't recognise you, please try again " (link-to-page "here" 'login-form-page))))

(defpage some-error-happened
  (trivial-page "request failed"
		(p "Apologies, the request failed.  This is generally a very bad symptom.  Double-check the openid uri you entered. If all went well, it might be a good idea to inform the the website maintainer about this issue.")))

(defpage initiate-authentication
  (hunchentoot:redirect 
   (princ-to-string 
    (cl-openid:initiate-authentication (relying-party) (param "openid_identifier")
				       :immediate-p (param "checkid_immediate")))))