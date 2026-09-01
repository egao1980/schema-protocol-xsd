(in-package #:schema-protocol-xsd)

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
