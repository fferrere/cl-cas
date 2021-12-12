(defpackage :cl-cas
  (:use :cl :alexandria)
  (:nicknames :cas)
  (:export #:cas-client
           #:cas-server-url
           #:cas-renew
           #:cas-login-url
           #:cas-logout-url
           #:cas-ticket
           #:cas-validate
           #:cas-service-validate))

(defpackage :cl-cas/test
  (:use :cl :cl-cas :parachute))
