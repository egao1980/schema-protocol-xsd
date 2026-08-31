(in-package #:schema-protocol-xsd)

(defclass xsd-schema-document ()
  ((root :initarg :root :reader xsd-schema-root)
   (version :initarg :version :reader xsd-schema-version :initform :1.0))
  (:documentation "Parsed XSD document (xml-elem tree)."))

(defun xsd-schema-document-p (object)
  (typep object 'xsd-schema-document))

(defun serialize (document &key (declaration t))
  "XSD-SCHEMA-DOCUMENT / xml-elem → XML string."
  (serialize-xml (if (xsd-schema-document-p document)
                     (xsd-schema-root document)
                     document)
                 :declaration declaration))

(defun parse-document (source &key (version :1.0))
  "XML string / xml-elem / document → XSD-SCHEMA-DOCUMENT."
  (cond
    ((xsd-schema-document-p source)
     source)
    ((xml-elem-p source)
     (make-instance 'xsd-schema-document :root source :version version))
    ((stringp source)
     (make-instance 'xsd-schema-document
                    :root (parse-xml source)
                    :version version))
    (t
     (error 'xsd-schema-error
            :message (format nil "cannot read XSD from ~S" (type-of source))))))
