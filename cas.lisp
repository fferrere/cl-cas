(in-package :cl-cas)

;; https://calnetweb.berkeley.edu/calnet-technologists/cas/how-cas-works

;; CAS Protocol V2 Specification Only

(defclass cas-client ()
  ((server-url :accessor cas-serveur-url
               :initarg :server-url)
   (renew :accessor cas-renew
          :initarg :renew
          :initform '())))

(defun format-url (server-url uri &key service-url suffix)
  (format nil "~A~A~@[~A~]" (string-right-trim '(#\/) server-url) uri
          (if service-url (format nil "?service=~A~@[~A~]" (quri:url-encode service-url) suffix) "")))

(defmethod cas-login-url ((cas cas-client) url)
  (format-url (cas-serveur-url cas) "/login" :service-url url))

(defmethod cas-logout-url ((cas cas-client) &optional redirect-url)
  (format-url (cas-serveur-url cas) (format nil "/logout~@[~a~]"
                                            (when redirect-url
                                              (format nil "?url=~a" (quri:url-encode redirect-url))))))
      

(defmethod cas-validate-url ((cas cas-client) service-url ticket)
  (format-url (cas-serveur-url cas) "/validate" :service-url service-url
              :suffix (format nil "&ticket=~a~a" ticket (if (cas-renew cas) "&renew=true" ""))))

(defmethod cas-service-validate-url ((cas cas-client) service-url ticket format)
  (format-url (cas-serveur-url cas) "/serviceValidate" :service-url service-url
              :suffix (format nil  "&ticket=~a~a~a" ticket
                              (if (cas-renew cas) "&renew=true" "")
                              (if (eq format :json) "&format=json" ""))))


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

(defun cas-response-xml-success (response)
  (when-let ((user-node (xmls:xmlrep-find-child-tag "authenticationSuccess" (xmls:parse response) '())))
    (values (car (cas-response-xml-user user-node)) (cas-response-xml-attributes user-node))))

(defun cas-response-xml-success (response)
  (when-let ((user-node (xmls:xmlrep-find-child-tag "authenticationSuccess" (xmls:parse response) '())))
    (values (car (cas-response-xml-user user-node)) (cas-response-xml-attributes user-node))))

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

(defmethod cas-validate ((cas cas-client) service-url ticket)
  "CAS v1. Verify with CAS that 'ticket' is a legitimate ticket. If so, return username"
  (let* ((url (cas-validate-url cas service-url ticket))
         (response (babel:octets-to-string (dex:get url :force-binary t))))
    (when (string= (subseq response 0 3) "yes")
      (string-trim '(#\Newline) (subseq response 4)))))

(defmethod cas-service-validate ((cas cas-client) service-url ticket &key pgt-url (format :xml))
  "Verify with CAS that 'ticket' is a legitimate ticket. Return cas username and its attribues, false elsewhere.

   From Apereo CAS documention :
       - pgtUrl [OPTIONAL] - the URL of the proxy callback. Discussed in Section 2.5.4. As a HTTP request parameter, the “pgtUrl” value MUST be URL-encoded as described in Section 2.2 of RFC 1738 [4].
       - renew [OPTIONAL] - if this parameter is set, ticket validation will only succeed if the service ticket was issued from the presentation of the user’s primary credentials. It will fail if the ticket was issued from a single sign-on session.
       - format [OPTIONAL] - if this parameter is set, ticket validation response MUST be produced based on the parameter value. Supported values are XML and JSON. If this parameter is not set, the default XML format will be used."
  (declare (ignore pgt-url))
  (let ((url (cas-service-validate-url cas service-url ticket format))
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

;; todo

(defun cas-proxy ())

(defun cas-proxy-validate ())
