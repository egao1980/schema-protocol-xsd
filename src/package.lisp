(defpackage #:schema-protocol-xsd.generated
  (:use))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; 0.1.0 OCI pin has no XSD-SCHEMA yet — intern so :import-from works.
  (let* ((pkg (find-package '#:schema-protocol))
         (sym (intern "XSD-SCHEMA" pkg)))
    (export sym pkg)))

(defpackage #:schema-protocol-xsd
  (:use #:cl #:xml-protocol)
  (:nicknames #:stack-schema-xsd)
  (:import-from #:closer-mop
                #:ensure-class
                #:slot-definition-name
                #:slot-definition-type)
  (:import-from #:schema-protocol
                #:schema-of
                #:schema-slots
                #:schema-class
                #:schema-object
                #:schema-class-extra
                #:schema-extra-policy
                #:schema-class-key-style
                #:schema-class-computes
                #:schema-class-tag
                #:schema-error
                #:find-schema
                #:schema-slot
                #:schema-tag
                #:schema-variants
                #:variant-tag-values
                #:enum-of
                #:enum-members
                #:finalize-schema
                #:type-kind
                #:type-args
                #:sequence-element-type
                #:slot-is-required-p
                #:slot-wire-p
                #:slot-dump-p
                #:slot-wire-key
                #:slot-min-length
                #:slot-max-length
                #:slot-minimum
                #:slot-maximum
                #:slot-format
                #:slot-description
                #:style-key
                #:xsd-schema
                #:schema-validation-error
                #:schema-validation-error-issues
                #:make-schema-issue
                #:schema-issue-path
                #:schema-issue-message)
  (:export #:xsd-schema-error
           #:xsd-schema-error-message
           #:xsd-schema-ref-error
           #:xsd-schema-ref-error-ref
           #:xsd-schema-document
           #:xsd-schema-document-p
           #:xsd-schema-root
           #:xsd-schema-version
           #:parse-document
           #:serialize
           #:emit
           #:compile-schema
           #:compile-validator
           #:validate-instance
           #:valid-instance-p
           #:xsd-schema-validator
           #:xsd-schema-validator-p
           #:xsd-schema-validation-error
           #:xsd-schema
           #:decode-validating
           #:+xsd-ns+
           #:+xsd-vc-ns+))

(in-package #:schema-protocol-xsd)

(unless (and (fboundp 'xsd-schema)
             (typep (symbol-function 'xsd-schema) 'generic-function))
  (defgeneric xsd-schema (schema &key version)
    (:documentation "Emit an XSD 1.0 or 1.1 document (XML string).")))
