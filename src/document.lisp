(in-package #:schema-protocol-xsd)

(defclass xsd-schema-document ()
  ((root :initarg :root :reader xsd-schema-root)
   (version :initarg :version :reader xsd-schema-version :initform :1.0))
  (:documentation "Parsed XSD document (xml-elem tree)."))

(defun xsd-schema-document-p (object)
  (typep object 'xsd-schema-document))

(defun detect-version (root)
  (let ((v (and (xml-elem-p root) (xe-attr root "version")))
        (min (and (xml-elem-p root)
                  (or (xe-attr root "vc:minVersion")
                      (xe-attr root "minVersion")))))
    (if (or (equal v "1.1") (equal min "1.1"))
        :1.1
        :1.0)))

(defun serialize (document &key (declaration t))
  "XSD-SCHEMA-DOCUMENT / xml-elem → XML string."
  (serialize-xml (if (xsd-schema-document-p document)
                     (xsd-schema-root document)
                     document)
                 :declaration declaration))

(defun parse-document (source &key version)
  "XML string / xml-elem / document → XSD-SCHEMA-DOCUMENT."
  (cond
    ((xsd-schema-document-p source)
     source)
    ((xml-elem-p source)
     (make-instance 'xsd-schema-document
                    :root source
                    :version (or version (detect-version source))))
    ((stringp source)
     (let ((root (parse-xml source)))
       (make-instance 'xsd-schema-document
                      :root root
                      :version (or version (detect-version root)))))
    (t
     (error 'xsd-schema-error
            :message (format nil "cannot read XSD from ~S" (type-of source))))))
