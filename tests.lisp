(in-package :cl-cas/test)

(defparameter *cas-url* "https://cas.example.org/cas")
(defparameter *service* "http://www.example.org/foobar")
(defparameter *bad-proxy-service* "http://www.service.com")
(defparameter *proxy-service* "https://www.service.com")
(defparameter *ticket* "ST-1856339-aA5Yuvrxzpv8Tau1cYQ7")
(defparameter *proxy-grating-ticket* "PGT-490649-W81Y9Sa2vTM7hda7xNTkezTbVge4CUsybAr")
(defvar *pgtiou* "PGTIOU-4-B6n7Y5lRhXhaEaEywU9e-cas")
(defvar *proxy-ticket* "PT-1856392-b98xZrQN4p90ASrw96c8")

(defparameter *response-success-xml*
  "<cas:serviceResponse xmlns:cas=\"http://www.yale.edu/tp/cas\">
    <cas:authenticationSuccess>
        <cas:user>username</cas:user>
        <cas:proxyGrantingTicket>PGTIOU-4-B6n7Y5lRhXhaEaEywU9e-cas</cas:proxyGrantingTicket>
     </cas:authenticationSuccess>
   </cas:serviceResponse>")

(defparameter *response-failure-xml*
  "<cas:serviceResponse xmlns:cas=\"http://www.yale.edu/tp/cas\">
    <cas:authenticationFailure code=\"INVALID_TICKET\">Ticket ST-1856339-aA5Yuvrxzpv8Tau1cYQ7 not recognized</cas:authenticationFailure>
   </cas:serviceResponse>")

(defparameter *response-success-json* "{
  \"serviceResponse\" : {
    \"authenticationSuccess\" : {
      \"user\" : \"username\",
      \"proxyGrantingTicket\" : \"PGTIOU-4-B6n7Y5lRhXhaEaEywU9e-cas\"
    }
  }
}")

(defparameter *response-failure-json* "{
  \"serviceResponse\" : {
    \"authenticationFailure\" : {
      \"code\" : \"INVALID_TICKET\",
      \"description\" : \"Ticket ST-1856339-aA5Yuvrxzpv8Tau1cYQ7 not recognized\"
    }
  }
}")

(defvar *proxy-validate-response-success*
  "<cas:serviceResponse xmlns:cas='http://www.yale.edu/tp/cas'>
  <cas:authenticationSuccess>
  <cas:user>username</cas:user>
  <cas:proxyGrantingTicket>PGTIOU-4-B6n7Y5lRhXhaEaEywU9e-cas</cas:proxyGrantingTicket>
  <cas:proxies>
  <cas:proxy>https://proxy2/pgtUrl</cas:proxy>
  <cas:proxy>https://proxy1/pgtUrl</cas:proxy>
  </cas:proxies>
  </cas:authenticationSuccess>
  </cas:serviceResponse>")

(defvar *proxy-validate-response-failure*
  "<cas:serviceResponse xmlns:cas='http://www.yale.edu/tp/cas'>
  <cas:authenticationFailure code=\"INVALID_TICKET\">
      ticket PT-1856376-1HMgO86Z2ZKeByc5XdYD not recognized
  </cas:authenticationFailure>
</cas:serviceResponse>")

(defvar *proxy-response-success*
  "<cas:serviceResponse xmlns:cas='http://www.yale.edu/tp/cas'>
     <cas:proxySuccess>
       <cas:proxyTicket>PT-1856392-b98xZrQN4p90ASrw96c8</cas:proxyTicket>
     </cas:proxySuccess>
   </cas:serviceResponse>")

(defvar *proxy-response-failure*
  "<cas:serviceResponse xmlns:cas='http://www.yale.edu/tp/cas'>
     <cas:proxyFailure code=\"INVALID_REQUEST\">
       'pgt' and 'targetService' parameters are both required
     </cas:proxyFailure>
   </cas:serviceResponse>")

;;
;; Heroku example response
;;
(defparameter *heroku-xml-cas-response-success*
  "<cas:serviceResponse xmlns:cas='http://www.yale.edu/tp/cas'>
    <cas:authenticationSuccess>
        <cas:user>casuser</cas:user>
        <cas:attributes>
            <cas:isFromNewLogin>true</cas:isFromNewLogin>
            <cas:authenticationDate>2021-12-03T08:34:52.910103Z</cas:authenticationDate>
            <cas:successfulAuthenticationHandlers>Static Credentials</cas:successfulAuthenticationHandlers>
            <cas:cn>CAS</cas:cn>
            <cas:first-name>Apereo</cas:first-name>
            <cas:credentialType>UsernamePasswordCredential</cas:credentialType>
            <cas:uid>casuser</cas:uid>
            <cas:display-name>Apereo CAS</cas:display-name>
            <cas:authenticationMethod>Static Credentials</cas:authenticationMethod>
            <cas:longTermAuthenticationRequestTokenUsed>false</cas:longTermAuthenticationRequestTokenUsed>
            <cas:email>info@apereo.org</cas:email>
            <cas:last-name>CAS</cas:last-name>
            <cas:username>casuser</cas:username>
            </cas:attributes>
    </cas:authenticationSuccess>
</cas:serviceResponse>")

(defparameter *heroku-json-cas-response-success*
  "{
  \"serviceResponse\" : {
    \"authenticationSuccess\" : {
      \"user\" : \"casuser\",
      \"attributes\" : {
        \"isFromNewLogin\" : [ true ],
        \"authenticationDate\" : [ 1638524985.903672000 ],
        \"successfulAuthenticationHandlers\" : [ \"Static Credentials\" ],
        \"cn\" : [ \"CAS\" ],
        \"first-name\" : [ \"Apereo\" ],
        \"credentialType\" : [ \"UsernamePasswordCredential\" ],
        \"uid\" : [ \"casuser\" ],
        \"display-name\" : [ \"Apereo CAS\" ],
        \"authenticationMethod\" : [ \"Static Credentials\" ],
        \"longTermAuthenticationRequestTokenUsed\" : [ false ],
        \"email\" : [ \"info@apereo.org\" ],
        \"last-name\" : [ \"CAS\" ],
        \"username\" : [ \"casuser\" ]
      }
    }
  }
}
")

(define-test login
  (is string=
      (cas-login-url *cas-url*)
      (format nil "~a/login" *cas-url*))
  (is string=
      (cas-login-url *cas-url* :service-url *service*)
      (format nil "~a/login?service=~a" *cas-url* (quri:url-encode *service*)))
  (is string=
      (cas-login-url *cas-url* :service-url *service* :renew t)
      (format nil "~a/login?service=~a&renew=true" *cas-url* (quri:url-encode *service*)))
  (is string=
      (cas-login-url *cas-url* :service-url *service* :gateway t)
      (format nil "~a/login?service=~a&gateway=true" *cas-url* (quri:url-encode *service*)))
  (of-type cas-url-invalid-parameters-error
           (handler-case 
               (cas-login-url *cas-url* :service-url *service* :renew t :gateway t)
             (error (err) err))))

(define-test logout
  (let ((redirect-url "https://www.logout.org/logout"))
    (is string=
        (cas-logout-url *cas-url*)
        (format nil "~a/logout" *cas-url*))
    (is string=
        (cas-logout-url *cas-url* :url redirect-url)
        (format nil "~a/logout?url=~a" *cas-url* redirect-url))))

(define-test validate-url
  (is string=
      (cas-validate-url *cas-url* *service* *ticket*)
      (format nil "~a/validate?service=~a&ticket=~a"
              *cas-url* (quri:url-encode *service*) *ticket*))
  (is string=
      (cas-validate-url *cas-url* *service* *ticket* :renew t)
      (format nil "~a/validate?service=~a&ticket=~a&renew=true"
              *cas-url* (quri:url-encode *service*) *ticket*)))

(define-test service-validate-url
  (is string=
      (cas-service-validate-url *cas-url* *service* *ticket*)
      (format nil "~a/serviceValidate?service=~a&ticket=~a"
              *cas-url* (quri:url-encode *service*) *ticket*))
  (is string=
      (cas-service-validate-url *cas-url* *service* *ticket* :pgt-url *proxy-service*)
      (format nil "~a/serviceValidate?service=~a&ticket=~a&pgtUrl=~a"
              *cas-url* (quri:url-encode *service*) *ticket* (quri:url-encode *proxy-service*)))
  (of-type cas-url-invalid-parameters-error
           (handler-case
               (cas-service-validate-url *cas-url* *service* *ticket* :pgt-url *bad-proxy-service*)
             (error (err) err)))
  (is string=
      (cas-service-validate-url *cas-url* *service* *ticket* :renew t :format :json)
      (format nil "~a/serviceValidate?service=~a&ticket=~a&renew=true&format=json"
              *cas-url* (quri:url-encode *service*) *ticket*))
  (is string=
      (cas-service-validate-url *cas-url* *service* *ticket* :renew t)
      (format nil "~a/serviceValidate?service=~a&ticket=~a&renew=true"
              *cas-url* (quri:url-encode *service*) *ticket*)))

(define-test proxy-validate-url
  (is string=
      (cas-proxy-validate-url *cas-url* *service* *ticket*)
      (format nil "~a/proxyValidate?service=~a&ticket=~a"
              *cas-url* (quri:url-encode *service*) *ticket*))
  (is string=
      (cas-proxy-validate-url *cas-url* *service* *ticket* :pgt-url *proxy-service*)
      (format nil "~a/proxyValidate?service=~a&ticket=~a&pgtUrl=~a"
              *cas-url* (quri:url-encode *service*) *ticket* (quri:url-encode *proxy-service*)))
  (of-type cas-url-invalid-parameters-error
           (handler-case
               (cas-proxy-validate-url *cas-url* *service* *ticket* :pgt-url *bad-proxy-service*)
             (error (err) err)))
  (is string=
      (cas-proxy-validate-url *cas-url* *service* *ticket* :renew t)
      (format nil "~a/proxyValidate?service=~a&ticket=~a&renew=true"
              *cas-url* (quri:url-encode *service*) *ticket*))
  (is string=
      (cas-proxy-validate-url *cas-url* *service* *ticket* :renew t :format :json)
      (format nil "~a/proxyValidate?service=~a&ticket=~a&renew=true&format=json"
              *cas-url* (quri:url-encode *service*) *ticket*)))

(define-test proxy-url
  (is string=
      (cas-proxy-url *cas-url* *proxy-service* *proxy-grating-ticket*)
      (format nil "~a/proxy?targetService=~a&pgt=~a"
              *cas-url* (quri:url-encode *proxy-service*) *proxy-grating-ticket*)))

(define-test ticket
  (let ((query-string
          (cadr
           (split-sequence:split-sequence #\?
                                          (format nil "~a/foobar?option1=value1&ticket=~a&option2=value2"
                                                  *service* *ticket*)))))
    (is string= (cas-ticket query-string) *ticket*)))

(define-test response-xml
  ;; (is string= "username" (cl-cas::cas-response-xml-success *response-success-xml*))
  (false (cl-cas::cas-response-xml-success *response-failure-xml*))
  (multiple-value-bind (username attributes pgt proxies)
      (cl-cas::cas-response-xml-success *response-success-xml*)
    (is string= "username" username)
    (false attributes)
    (is string= *pgtiou* pgt)
    (false proxies))
  (multiple-value-bind (username attributes pgt proxies)
      (cl-cas::cas-response-xml-success *proxy-validate-response-success*)
    (is string= "username" username)
    (is string= *pgtiou* pgt)
    (false attributes)
    (true (equalp proxies '("https://proxy2/pgtUrl" "https://proxy1/pgtUrl"))))
  (multiple-value-bind (username attributes pgt proxies)
      (cl-cas::cas-response-xml-success *heroku-xml-cas-response-success*)
    (is string= "casuser" username)
    (false pgt)
    (is string= (cadr (assoc :username attributes)) "casuser")
    (is string= (cadr (assoc :email attributes)) "info@apereo.org")
    (is local-time:timestamp= (cadr (assoc :authentication-date attributes))
        (local-time:parse-timestring "2021-12-03T08:34:52.910103Z"))
    (false proxies)))

(define-test proxy-response-xml
  (is string= *proxy-ticket* (cl-cas::cas-proxy-response-xml-success *proxy-response-success*)))

(define-test response-json
  (is string= "username" (cl-cas::cas-response-json-success *response-success-json*))
  (false (cl-cas::cas-response-json-success *response-failure-json*)))
