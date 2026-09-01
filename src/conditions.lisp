(in-package #:schema-protocol-xsd)

(define-condition xsd-schema-error (schema-error)
  ((message :initarg :message :reader xsd-schema-error-message :initform nil))
  (:report (lambda (c s)
             (format s "XSD error~@[: ~A~]" (xsd-schema-error-message c)))))

(define-condition xsd-schema-ref-error (xsd-schema-error)
  ((ref :initarg :ref :reader xsd-schema-ref-error-ref))
  (:report (lambda (c s)
             (format s "Unresolved XSD type ~S~@[: ~A~]"
                     (xsd-schema-ref-error-ref c)
                     (xsd-schema-error-message c)))))

(define-condition xsd-schema-validation-error (schema-validation-error)
  ()
  (:documentation "Instance failed XSD validation. ISSUES use schema-issue loc paths."))
