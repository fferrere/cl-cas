(in-package :cl-cas)

;; https://calnetweb.berkeley.edu/calnet-technologists/cas/how-cas-works

;; CAS Protocol V2 Specification

(define-condition cas-error (error)
  ((message :initarg :message
            :reader message))
  (:report (lambda (condition stream)
             (format stream "~a" (message condition)))))

(define-condition cas-url-invalid-parameters-error (cas-error) ())

(define-condition cas-invalid-request-error (cas-error) ()
  (:documentation "INVALID_REQUEST - not all of the required request parameters were present"))
(define-condition cas-invalid-ticket-error (cas-error) ()
  (:documentation "INVALID_TICKET - the ticket provided was not valid, or the ticket did not come from an initial login and “renew” was set on validation."))
(define-condition cas-invalid-service-error (cas-error) ()
  (:documentation "INVALID_SERVICE - the ticket provided was valid, but the service specified did not match the service associated with the ticket."))
(define-condition cas-internal-error (cas-error) ()
  (:documentation "INTERNAL_ERROR - an internal error occurred during ticket validation"))

(defun concatenate-url-path (path-base path)
  (ppcre:regex-replace-all "/+"
			   (format nil "/~@[~a/~]~@[~a~]"
				   (and path-base (string-trim '(#\/) path-base)) 
				   (and path (string-trim '(#\/) path)))
			   "/"))

(defun format-url (cas-server-url path &key query)
  (let ((uri (quri:uri cas-server-url)))
    (quri:render-uri (quri:make-uri :defaults uri
				    :path (concatenate-url-path (quri:uri-path uri) path)
				    :query query))))

(defun cas-login-url (cas-server-url &key service-url renew gateway)
  "Return a CAS Login URL
   Renew : nil (default) or true
   Gateway : nil (default) or true
   Renew and Gateway are mutually exclusive"
  (declare (type boolean gateway renew))
  (and renew gateway (error 'cas-url-invalid-parameters-error
                            :message "renew and gateway are mutually exclusive"))
  (let ((query (when service-url
                 (format nil "service=~a~@[~a~]" (quri:url-encode service-url)
                         (cond
                           (renew "&renew=true")
                           (gateway "&gateway=true"))))))
    (format-url cas-server-url "/login" :query query)))

(defun cas-logout-url (cas-server-url &key url)
  (let ((query (when url (format nil "url=~a" url))))
   (format-url cas-server-url "/logout" :query query)))
      
(defun cas-validate-url (cas-server-url service-url ticket &key renew)
  (let ((query (format nil "service=~a&ticket=~a~@[~a~]"
                       (quri:url-encode service-url)
                       ticket
                       (when renew "&renew=true"))))
    (format-url cas-server-url "/validate" :query query)))

(defun pgt-url-p (pgt-url)
  "Check if pgt-url is a valid Proxy CAS URL, i.e. only https"
  (string= "https" (quri:uri-scheme (quri:uri pgt-url))))

(defun cas-service-validate-url (cas-server-url service-url ticket &key pgt-url renew (format :xml))
  (let ((query (format nil "service=~a&ticket=~a~@[~a~]~@[~a~]~@[~a~]"
                       (quri:url-encode service-url)
                       ticket
                       (when pgt-url
                         (if (pgt-url-p pgt-url)
                             (format nil "&pgtUrl=~a" (quri:url-encode pgt-url))
                             (error 'cas-url-invalid-parameters-error :message
                              (format nil "PGT URL MUST be HTTPS : ~a" pgt-url))))
                       (when renew "&renew=true")
                       (when (eql format :json) "&format=json"))))
    (format-url cas-server-url "/serviceValidate" :query query)))

(defun cas-proxy-validate-url (cas-server-url service-url ticket &key pgt-url renew (format :xml))
  (let ((query (format nil "service=~a&ticket=~a~@[~a~]~@[~a~]~@[~a~]"
                       (quri:url-encode service-url)
                       ticket
                       (when pgt-url
                         (if (pgt-url-p pgt-url)
                             (format nil "&pgtUrl=~a" (quri:url-encode pgt-url))
                             (error 'cas-url-invalid-parameters-error :message
                                    (format nil "This URL MUST be HTTPS : ~a" pgt-url))))
                       (when renew "&renew=true")
                       (when (eql format :json) "&format=json"))))
    (format-url cas-server-url "/proxyValidate" :query query)))

(defun cas-proxy-url (cas-server-url target-service pgt)
  (let ((query (format nil "targetService=~a&pgt=~a"
                       (quri:url-encode target-service)
                       pgt)))
    (format-url cas-server-url "/proxy" :query query)))


;;
;; CAS Response management functions
;;
(defun cas-response-xml-attributes (response-success-node)
  (when-let ((attributes (xmls:xmlrep-find-child-tag "attributes" response-success-node '())))
      (loop for node in (xmls:node-children attributes)
            collect (let ((attribute-name (xmls:node-name node))
                          (attribute-value (car (xmls:node-children node))))
                        (list (intern (cl-json:camel-case-to-lisp attribute-name) :keyword)
                           (cond
                             ((string= attribute-value "true") t)
                             ((string= attribute-value "false") nil)
                             ((handler-case
                                  (local-time:parse-rfc3339-timestring attribute-value)
                                (error () ()))
                              (local-time:parse-rfc3339-timestring attribute-value))
                             (t attribute-value)))))))

(defun cas-response-xml-user (response-success-node)
  (xmls:node-children (xmls:xmlrep-find-child-tag "user" response-success-node)))

(defun cas-response-xml-pgt (response-success-node)
  (handler-case
      (car (xmls:node-children (xmls:xmlrep-find-child-tag "proxyGrantingTicket" response-success-node)))
    (error () ())))

(defun cas-response-xml-proxies (response-success-node)
  (handler-case
      (loop for node in (xmls:node-children (xmls:xmlrep-find-child-tag "proxies" response-success-node))
            collect (car (xmls:node-children node)))
    (error () ())))

(defun cas-response-xml-success (response)
  (when-let ((user-node (xmls:xmlrep-find-child-tag "authenticationSuccess" (xmls:parse response) '())))
    (values (car (cas-response-xml-user user-node))
            (cas-response-xml-attributes user-node)
            (cas-response-xml-pgt user-node)
            (cas-response-xml-proxies user-node))))

(defun cas-proxy-response-xml-ticket (response-success-node)
  (xmls:node-children (xmls:xmlrep-find-child-tag "proxyTicket" response-success-node)))

(defun cas-proxy-response-xml-success (response)
  (when-let ((node (xmls:xmlrep-find-child-tag "proxySuccess" (xmls:parse response) '())))
    (car (cas-proxy-response-xml-ticket node))))

(defun cas-proxy-response-json-success (response)
  (declare (ignore response))
  (error "JSON format not yet implemented"))

(defun cas-response-json-attributes (response-success-node)
  (cdr (assoc :attributes response-success-node)))

(defun cas-response-json-user (response-success-node)
  (cdr (assoc :user response-success-node)))

(defun cas-decode-json (json-string)
  "CAS v2. CAS server may return 'authenticationDate' attribute with epoch-19 format. 
   CL-JSON parse and convert the value into real, not timestamp.
   This function replace cl-json real-handler to check if the value could be a timestamp data."
  (let ((cl-json:*real-handler*
          #'(lambda (str)
              (let ((epoch-19-timestamp (split-sequence:split-sequence #\. str)))
                (if (and (= (length str) 20)
                         (= (length (first epoch-19-timestamp)) 10)
                         (= (length (second epoch-19-timestamp)) 9))
                    (local-time:adjust-timestamp
                        (local-time:unix-to-timestamp (parse-integer (first epoch-19-timestamp)))
                      (offset :nsec (parse-integer (second epoch-19-timestamp))))
                    (cl-json::parse-number str))))))
    (cl-json:decode-json-from-string json-string)))

(defun cas-response-json-success (response)
  (when-let ((user-node (cdr (assoc :authentication-success (cdar (cas-decode-json response))))))
    (values (cas-response-json-user user-node) (cas-response-json-attributes user-node))))

;;
;; CAS Validate Functions
;;
(defun cas-validate (cas-server-url service-url ticket)
  "CAS v1. Verify with CAS that 'ticket' is a legitimate ticket. If so, return username"
  (let* ((url (cas-validate-url cas-server-url service-url ticket))
         (response (babel:octets-to-string (dex:get url :force-binary t))))
    (when (string= (subseq response 0 3) "yes")
      (string-trim '(#\Newline) (subseq response 4)))))

(defun cas-service-validate (cas-server-url service-url ticket &key pgt-url renew (format :xml))
  "Verify with CAS that 'ticket' is a legitimate ticket. Return cas username and its attribues, false elsewhere.

   From Apereo CAS documention :
       - pgtUrl [OPTIONAL] - the URL of the proxy callback. Discussed in Section 2.5.4. As a HTTP request parameter, the “pgtUrl” value MUST be URL-encoded as described in Section 2.2 of RFC 1738 [4].
       - renew [OPTIONAL] - if this parameter is set, ticket validation will only succeed if the service ticket was issued from the presentation of the user’s primary credentials. It will fail if the ticket was issued from a single sign-on session.
       - format [OPTIONAL] - if this parameter is set, ticket validation response MUST be produced based on the parameter value. Supported values are XML and JSON. If this parameter is not set, the default XML format will be used."
  (let ((url (cas-service-validate-url cas-server-url service-url ticket
                                       :format format :renew renew :pgt-url pgt-url))
        (headers '(("Accept" . "application/xml, application/json"))))
    (multiple-value-bind (body status response-headers)
        (dex:get url :headers headers)
      (declare (ignore status))
      (if (ppcre:scan "application/json" (gethash "content-type" response-headers ))
          (cas-response-json-success body)
          (cas-response-xml-success body)))))

(defun cas-ticket (query-string)
  "Return the value of the HTTP query-string 'ticket' parameter as CAS Ticket, if exists"
  (cdr (assoc "ticket"
              (quri:url-decode-params query-string)
              :test #'string-equal)))

;;
;; CAS Proxy functions 
;;

(defun cas-proxy (cas-server-url target-service pgt)
  (let ((url (cas-proxy-url cas-server-url target-service pgt))
        (headers '(("Accept" . "application/xml, application/json"))))
    (multiple-value-bind (body status response-headers)
        (dex:get url :headers headers)
      (declare (ignore status))
      (if (ppcre:scan "application/json" (gethash "content-type" response-headers ))
          (cas-proxy-response-json-success body)
          (cas-proxy-response-xml-success body)))))

(defun cas-proxy-validate (cas-server-url service-url ticket &key pgt-url renew (format :xml))
  "Verify with CAS that 'ticket' is a legitimate ticket. Return cas username and its attribues, false elsewhere."
  (let ((url (cas-proxy-validate-url cas-server-url service-url ticket
                                     :format format :renew renew :pgt-url pgt-url))
        (headers '(("Accept" . "application/xml, application/json"))))
    (multiple-value-bind (body status response-headers)
        (dex:get url :headers headers)
      (declare (ignore status))
      (if (ppcre:scan "application/json" (gethash "content-type" response-headers ))
          (cas-response-json-success body)
          (cas-response-xml-success body)))))
