(in-package #:schema-protocol-xsd)

(defclass xsd-schema-backend (schema-format-backend) ()
  (:documentation "schema-protocol format backend for :xsd."))

(defmethod backend-emit-schema ((backend xsd-schema-backend) schema
                                &key (version :1.0) (as :xml) &allow-other-keys)
  (declare (ignore backend))
  (emit schema :version version :as as))

(defmethod backend-parse-schema ((backend xsd-schema-backend) source
                                 &key name package version &allow-other-keys)
  (declare (ignore backend))
  (apply #'compile-schema source
         (append (when name (list :name name))
                 (when package (list :package package))
                 (when version (list :version version)))))

(eval-when (:load-toplevel :execute)
  (register-schema-format :xsd (make-instance 'xsd-schema-backend)))

(defmethod xsd-schema ((schema symbol) &key (version :1.0))
  (emit schema :version version))

(defmethod xsd-schema ((schema standard-object) &key (version :1.0))
  (emit schema :version version))

(defmethod xsd-schema ((schema string) &key (version :1.0))
  (emit schema :version version))

(defmethod xsd-schema ((schema xsd-schema-document) &key (version :1.0))
  (emit schema :version version))

(defmethod xsd-schema ((schema xml-element) &key (version :1.0))
  (emit schema :version version))

(defmethod xsd-schema ((schema xml-document) &key (version :1.0))
  (emit schema :version version))
