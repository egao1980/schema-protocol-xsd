(in-package #:schema-protocol-xsd)

(defclass xsd-schema-document ()
  ((root :initarg :root :reader xsd-schema-root)
   (version :initarg :version :reader xsd-schema-version :initform :1.0))
  (:documentation "Parsed XSD document (xml-element tree)."))

(defun xsd-schema-document-p (object)
  (typep object 'xsd-schema-document))

(defun detect-version (root)
  (let ((v (and (xml-element-p root) (xml-attr root "version")))
        (min (and (xml-element-p root)
                  (or (xml-attr root "vc:minVersion")
                      (xml-attr root "minVersion")))))
    (if (or (equal v "1.1") (equal min "1.1"))
        :1.1
        :1.0)))

(defun %as-root-element (source)
  (cond
    ((xml-document-p source) (document-root-element source))
    ((xml-element-p source) source)
    (t nil)))

(defun serialize (document &key (declaration t))
  "XSD-SCHEMA-DOCUMENT / xml-element / xml-document → XML string."
  (let ((node (cond
                ((xsd-schema-document-p document) (xsd-schema-root document))
                ((xml-document-p document) document)
                (t document))))
    (xml-protocol:encode node :declaration declaration)))

(defun parse-document (source &key version)
  "XML string / xml-element / xml-document / document → XSD-SCHEMA-DOCUMENT."
  (cond
    ((xsd-schema-document-p source)
     source)
    ((xml-element-p source)
     (make-instance 'xsd-schema-document
                    :root source
                    :version (or version (detect-version source))))
    ((xml-document-p source)
     (let ((root (document-root-element source)))
       (make-instance 'xsd-schema-document
                      :root root
                      :version (or version (detect-version root)))))
    ((stringp source)
     (parse-document (xml-protocol:decode source) :version version))
    (t
     (error 'xsd-schema-error
            :message (format nil "cannot read XSD from ~S" (type-of source))))))
