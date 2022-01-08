(defpackage :cl-cas
  (:use :cl :alexandria)
  (:nicknames :cas)
  (:export #:cas-login-url
           #:cas-logout-url
           #:cas-validate-url
           #:cas-service-validate-url
           #:cas-proxy-validate-url
           #:cas-proxy-url
           #:cas-ticket
           #:cas-validate
           #:cas-service-validate
           #:cas-proxy-validate
           #:cas-proxy
	   #:format-url
           #:cas-url-invalid-parameters-error))

(defpackage :cl-cas/test
  (:use :cl :cl-cas :parachute))
