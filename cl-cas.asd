(defsystem "cl-cas"
  :version "0.1.0"
  :author "Frédéric FERRERE"
  :license "Apache-2.0"
  :depends-on ("alexandria" "dexador" "xmls" "cl-json" "quri")
  :components ((:file "package")
               (:file "cas"))
  :in-order-to ((asdf:test-op (asdf:test-op "cl-cas/test"))))

(defsystem "cl-cas/test"
  :defsystem-depends-on ("parachute")
  :author "Frédéric FERRERE"
  :depends-on ("cl-cas" "quri" "parachute")
  :components ((:file "tests"))
  :perform (asdf:test-op (op c)
                         (uiop:symbol-call :parachute :test :cl-cas/test)))
