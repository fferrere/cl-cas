(in-package :cl-cas/test)

(defparameter *cas-url* "https://cas.example.org/cas")
(defparameter *service* "http://www.example.org/foobar")
(defparameter *cas-client* (make-instance 'cas-client :server-url *cas-url*))
(defparameter *ticket* "ST-1856339-aA5Yuvrxzpv8Tau1cYQ7")

(defparameter *response-success-xml*
  "<cas:serviceResponse xmlns:cas=\"http://www.yale.edu/tp/cas\">
    <cas:authenticationSuccess>
        <cas:user>username</cas:user>
        <cas:proxyGrantingTicket>PGTIOU-84678-8a9d...</cas:proxyGrantingTicket>
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
      \"proxyGrantingTicket\" : \"PGTIOU-84678-8a9d...\"
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
      (cas-login-url *cas-client* *service*)
      (format nil "~a/login?service=~a" *cas-url* (quri:url-encode *service*))))

(define-test logout
  (let ((redirect-url "https://www.logout.org/logout"))
    (is string=
        (cas-logout-url *cas-client*)
        (format nil "~a/logout" *cas-url*))
    (is string=
        (cas-logout-url *cas-client* redirect-url)
        (format nil "~a/logout?url=~a" *cas-url* (quri:url-encode redirect-url)))))

(define-test validate-url
  (setf (cas-renew *cas-client*) t)
  (is string=
      (cl-cas::cas-validate-url *cas-client* *service* *ticket*)
      (format nil "~a/validate?service=~a&ticket=~a&renew=true"
              *cas-url* (quri:url-encode *service*) *ticket*))
  (setf (cas-renew *cas-client*) '())
  (is string=
      (cl-cas::cas-validate-url *cas-client* *service* *ticket*)
      (format nil "~a/validate?service=~a&ticket=~a"
              *cas-url* (quri:url-encode *service*) *ticket*)))

(define-test validate-url
  (setf (cas-renew *cas-client*) t)
  (is string=
      (cl-cas::cas-service-validate-url *cas-client* *service* *ticket* :xml)
      (format nil "~a/serviceValidate?service=~a&ticket=~a&renew=true"
              *cas-url* (quri:url-encode *service*) *ticket*))
  (setf (cas-renew *cas-client*) '())
  (is string=
      (cl-cas::cas-service-validate-url *cas-client* *service* *ticket* :xml)
      (format nil "~a/serviceValidate?service=~a&ticket=~a"
              *cas-url* (quri:url-encode *service*) *ticket*))
  (setf (cas-renew *cas-client*) t)
  (is string=
      (cl-cas::cas-service-validate-url *cas-client* *service* *ticket* :json)
      (format nil "~a/serviceValidate?service=~a&ticket=~a&renew=true&format=json"
              *cas-url* (quri:url-encode *service*) *ticket*))
  (setf (cas-renew *cas-client*) '())
  (is string=
      (cl-cas::cas-service-validate-url *cas-client* *service* *ticket* :json)
      (format nil "~a/serviceValidate?service=~a&ticket=~a&format=json"
              *cas-url* (quri:url-encode *service*) *ticket*)))

(define-test ticket
  (let ((query-string
          (cadr
           (split-sequence:split-sequence #\?
                                          (format nil "~a/foobar?option1=value1&ticket=~a&option2=value2"
                                                  *service* *ticket*)))))
    (is string= (cas-ticket query-string) *ticket*)))

(define-test response-xml
  (is string= "username" (cl-cas::cas-response-xml-success *response-success-xml*))
  (false (cl-cas::cas-response-xml-success *response-failure-xml*)))

(define-test response-json
  (is string= "username" (cl-cas::cas-response-json-success *response-success-json*))
  (false (cl-cas::cas-response-json-success *response-failure-json*)))
