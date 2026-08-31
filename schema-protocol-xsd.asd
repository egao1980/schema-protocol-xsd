(defsystem "schema-protocol-xsd"
  :version "0.1.0"
  :description "XSD 1.0 parse/generate/validate for schema-protocol"
  :author "egao1980"
  :license "MIT"
  :depends-on ("schema-protocol" "closer-mop" "cl-ppcre")
  :properties (:cl-repo (:ci (:sources (("schema-protocol" :oci)))))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "xml")
               (:file "document")
               (:file "emit")
               (:file "compile")
               (:file "validate")
               (:file "protocol"))
  :in-order-to ((test-op (test-op "schema-protocol-xsd/tests"))))

(defsystem "schema-protocol-xsd/tests"
  :depends-on ("schema-protocol-xsd" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "xsd-schema-test")
               (:file "validate-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
