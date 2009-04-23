(defpackage :cm-openid.sysdef
  (:use :common-lisp :asdf))

(in-package :cm-openid.sysdef)

(defsystem :cm-openid
  :name "ClayMore openid wrapper"
  :author "Aad Versteden <madnificent@gmail.com>"
  :version "0"
  :maintainer "Aad Versteden <madnificent@gmail.com>"
  :licence "BSD"
  :description "A simplified wrapper, to ease the adoption of openid-supportive applications in CLayMore."
  :depends-on (:claymore
	       :hunchentoot
	       :cl-openid) 
 :components ((:file "login")))