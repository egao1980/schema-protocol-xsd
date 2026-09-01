(in-package #:schema-protocol-xsd)

(defvar *xsd-emit-version* :1.0)
(defvar *emit-alternatives* nil)

(defun xsd-1.1-p (&optional (version *xsd-emit-version*))
  (member version '(:1.1 :xsd-1.1)))

(defun xs (name &optional attrs &rest children)
  (apply #'xe (concatenate 'string "xs:" name) attrs children))

(defun enum-wire (value)
  (etypecase value
    (keyword (string-downcase (symbol-name value)))
    (symbol (string-downcase (symbol-name value)))
    (string value)
    (number (princ-to-string value))
    (character (string value))))

(defun or-null-p (spec)
  (and (eq (type-kind spec) :or)
       (some (lambda (s) (eq (type-kind s) :null)) (type-args spec))))

(defun or-payload (spec)
  (find-if (lambda (s) (not (eq (type-kind s) :null))) (type-args spec)))

(defun integer-bounds (spec slot)
  (let ((args (and (consp spec) (eq (type-kind spec) :integer) (rest spec))))
    (values (or (and slot (slot-minimum slot))
                (and args (not (eq (first args) '*)) (first args)))
            (or (and slot (slot-maximum slot))
                (and (rest args) (not (eq (second args) '*)) (second args))))))

(defun builtin-for (spec slot)
  (let ((kind (type-kind spec))
        (fmt (and slot (slot-format slot))))
    (case kind
      (:string
       (cond
         ((member fmt '(:uri)) "xs:anyURI")
         ((member fmt '(:date-time :datetime)) "xs:dateTime")
         (t "xs:string")))
      (:integer "xs:integer")
      ((:number :real :float) "xs:decimal")
      (:boolean "xs:boolean")
      ((:keyword :symbol) "xs:string")
      (:null "xs:string")
      (:any "xs:anyType")
      (:hash-table "xs:anyType")
      (:lisp "xs:anyType")
      (:satisfies "xs:anyType")
      (t nil))))

(defun restriction-facets (spec slot)
  (let ((kids '()))
    (multiple-value-bind (lo hi) (integer-bounds spec slot)
      (when lo
        (push (xs "minInclusive" `(("value" . ,(princ-to-string lo)))) kids))
      (when hi
        (push (xs "maxInclusive" `(("value" . ,(princ-to-string hi)))) kids)))
    (when (and slot (member (type-kind spec) '(:number :real :float)))
      (when (slot-minimum slot)
        (push (xs "minInclusive" `(("value" . ,(princ-to-string (slot-minimum slot))))) kids))
      (when (slot-maximum slot)
        (push (xs "maxInclusive" `(("value" . ,(princ-to-string (slot-maximum slot))))) kids)))
    (when (and slot (eq (type-kind spec) :string))
      (when (slot-min-length slot)
        (push (xs "minLength" `(("value" . ,(princ-to-string (slot-min-length slot))))) kids))
      (when (slot-max-length slot)
        (push (xs "maxLength" `(("value" . ,(princ-to-string (slot-max-length slot))))) kids))
      (when (eq (slot-format slot) :uuid)
        (push (xs "pattern"
                  `(("value" . "[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}")))
              kids))
      (when (and (xsd-1.1-p) (member (slot-format slot) '(:date-time :datetime)))
        (push (xs "explicitTimezone" '(("value" . "optional"))) kids)))
    (nreverse kids)))

(defun enum-restriction (values base)
  (apply #'xs "simpleType" nil
         (list (apply #'xs "restriction" `(("base" . ,base))
                      (mapcar (lambda (v)
                                (xs "enumeration" `(("value" . ,(enum-wire v)))))
                              values)))))

(defun simple-inline (spec slot)
  (let ((kind (type-kind spec)))
    (case kind
      ((:member)
       (enum-restriction (type-args spec) "xs:string"))
      (:enum
       (enum-restriction (enum-members spec) "xs:string"))
      (:eql
       (enum-restriction (list (second spec)) "xs:string"))
      (t
       (let ((base (or (builtin-for spec slot) "xs:string"))
             (facets (restriction-facets spec slot)))
         (when facets
           (apply #'xs "simpleType" nil
                  (list (apply #'xs "restriction" `(("base" . ,base)) facets)))))))))

(defun ensure-named-type (name defs)
  (let ((key (string-downcase (symbol-name name))))
    (unless (gethash key defs)
      (setf (gethash key defs) :pending)
      (setf (gethash key defs) (object-complex-type (find-schema name) defs)))
    key))

(defun type-payload (spec slot defs)
  "Return (values type-attr inline-elem)."
  (let ((kind (type-kind spec)))
    (case kind
      (:or
       (if (or-null-p spec)
           (type-payload (or-payload spec) slot defs)
           (let ((alts (type-args spec)))
             (if (every (lambda (s) (not (eq (type-kind s) :nested))) alts)
                 (values nil
                         (apply #'xs "simpleType" nil
                                (list (apply #'xs "union" nil
                                             (mapcar (lambda (s)
                                                       (let ((b (builtin-for s slot)))
                                                         (xs "simpleType" nil
                                                             (xs "restriction" `(("base" . ,(or b "xs:string")))))))
                                                     alts)))))
                 (values "xs:anyType" nil)))))
      (:nested
       (values (ensure-named-type spec defs) nil))
      ((:member :enum :eql)
       (values nil (simple-inline spec slot)))
      (t
       (let ((inline (simple-inline spec slot)))
         (if inline
             (values nil inline)
             (values (or (builtin-for spec slot) "xs:anyType") nil)))))))

(defun element-attrs (name &key min max nillable type)
  (let ((attrs `(("name" . ,name))))
    (when type
      (setf attrs (append attrs `(("type" . ,type)))))
    (when (and min (not (eql min 1)))
      (setf attrs (append attrs `(("minOccurs" . ,(princ-to-string min))))))
    (when (and max (not (eql max 1)))
      (setf attrs (append attrs `(("maxOccurs" . ,(if (eq max :unbounded)
                                                      "unbounded"
                                                      (princ-to-string max)))))))
    (when nillable
      (setf attrs (append attrs '(("nillable" . "true")))))
    attrs))

(defun make-element (name spec slot defs &key (min 1) (max 1) nillable)
  (multiple-value-bind (type inline) (type-payload spec slot defs)
    (let ((el (apply #'xs "element"
                     (element-attrs name :min min :max max :nillable nillable :type type)
                     (when inline (list inline)))))
      (when (and slot (slot-description slot))
        (setf (xe-children el)
              (cons (xs "annotation" nil
                        (xs "documentation" nil (slot-description slot)))
                    (xe-children el))))
      el)))

(defun slot-particle (slot class defs)
  (let* ((spec (slot-definition-type slot))
         (kind (type-kind spec))
         (key (slot-wire-key slot class))
         (required (slot-is-required-p slot))
         (nillable (or-null-p spec))
         (payload (if nillable (or-payload spec) spec)))
    (if (member kind '(:vector :list :sequence))
        (make-element key
                      (or (sequence-element-type spec slot) t)
                      slot defs
                      :min (if required 1 0)
                      :max :unbounded)
        (make-element key payload slot defs
                      :min (if required 1 0)
                      :max 1
                      :nillable nillable))))

(defun extra-any (class)
  (when (eq (schema-extra-policy class) :allow)
    (if (xsd-1.1-p)
        (xs "openContent" '(("mode" . "interleave"))
            (xs "any" '(("processContents" . "lax"))))
        (xs "any" '(("minOccurs" . "0")
                    ("maxOccurs" . "unbounded")
                    ("processContents" . "lax"))))))

(defun emit-tagged-1.0 (class defs)
  (let* ((tag (schema-tag class))
         (slot (schema-slot class tag))
         (prop (slot-wire-key slot class))
         (choices '())
         (mappings '()))
    (dolist (v (reverse (schema-variants class)))
      (let* ((vname (class-name v))
             (key (string-downcase (symbol-name vname))))
        (ensure-named-type vname defs)
        (push (xs "element" `(("name" . ,key) ("type" . ,key))) choices)
        (dolist (tv (variant-tag-values v tag))
          (push (xe "mapping" `(("value" . ,(enum-wire tv)) ("type" . ,key))) mappings))))
    (xs "complexType" `(("name" . ,(string-downcase (symbol-name (class-name class)))))
        (xs "annotation" nil
            (xs "appinfo" nil
                (apply #'xe "discriminator" `(("propertyName" . ,prop))
                       (nreverse mappings))))
        (apply #'xs "choice" nil (nreverse choices)))))

(defun emit-tagged-1.1 (class defs)
  (let* ((tag (schema-tag class))
         (slot (schema-slot class tag))
         (prop (slot-wire-key slot class))
         (alts '()))
    (dolist (v (reverse (schema-variants class)))
      (let* ((vname (class-name v))
             (key (string-downcase (symbol-name vname))))
        (ensure-named-type vname defs)
        (dolist (tv (variant-tag-values v tag))
          (push (xs "alternative"
                    `(("test" . ,(format nil "~A = '~A'" prop (enum-wire tv)))
                      ("type" . ,key)))
                alts))))
    (setf *emit-alternatives*
          (append (nreverse alts)
                  (list (xs "alternative" '(("type" . "xs:error"))))))
    (xs "complexType" `(("name" . ,(string-downcase (symbol-name (class-name class)))))
        (xs "sequence" nil
            (xs "element" `(("name" . ,prop) ("type" . "xs:string")))))))

(defun emit-tagged (class defs)
  (if (xsd-1.1-p)
      (emit-tagged-1.1 class defs)
      (emit-tagged-1.0 class defs)))

(defun object-complex-type (class defs)
  (finalize-schema class)
  (when (and (schema-tag class) (schema-variants class))
    (return-from object-complex-type (emit-tagged class defs)))
  (let ((particles '()))
    (dolist (slot (schema-slots class))
      (when (and (slot-wire-p slot) (slot-dump-p slot))
        (push (slot-particle slot class defs) particles)))
    (dolist (cname (schema-class-computes class))
      (let ((key (style-key cname (schema-class-key-style class))))
        (push (xs "element"
                  `(("name" . ,key) ("type" . "xs:anyType") ("minOccurs" . "0"))
                  (xs "annotation" nil
                      (xs "appinfo" nil
                          (xe "readOnly" nil "true"))))
              particles)))
    (let ((extra (extra-any class))
          (kids (nreverse particles)))
      (apply #'xs "complexType"
             `(("name" . ,(string-downcase (symbol-name (class-name class)))))
             (if (and extra (xsd-1.1-p))
                 (list* extra (apply #'xs "sequence" nil kids) nil)
                 (list (apply #'xs "sequence" nil
                              (append kids (and extra (not (xsd-1.1-p)) (list extra))))))))))

(defun schema-attrs (version)
  (if (xsd-1.1-p version)
      `(("xmlns:xs" . ,+xsd-ns+)
        ("xmlns:vc" . ,+xsd-vc-ns+)
        ("vc:minVersion" . "1.1")
        ("version" . "1.1")
        ("elementFormDefault" . "qualified"))
      `(("xmlns:xs" . ,+xsd-ns+)
        ("elementFormDefault" . "qualified"))))

(defun emit-class (schema &key (version :1.0))
  (let* ((*xsd-emit-version* version)
         (*emit-alternatives* nil)
         (class (schema-of schema))
         (defs (make-hash-table :test #'equal))
         (root-name (string-downcase (symbol-name (class-name class))))
         (root-type (object-complex-type class defs)))
    (setf (gethash root-name defs) root-type)
    (let ((types '()))
      (maphash (lambda (k v)
                 (declare (ignore k))
                 (when (xml-elem-p v)
                   (push v types)))
               defs)
      (apply #'xs "schema"
             (schema-attrs version)
             (apply #'xs "element"
                    (if *emit-alternatives*
                        `(("name" . ,root-name))
                        `(("name" . ,root-name) ("type" . ,root-name)))
                    *emit-alternatives*)
             (nreverse types)))))

(defun emit (schema &key (version :1.0) (as :xml))
  "SCHEMA-CLASS / name / instance / document → XML string (default) or document."
  (let ((doc (cond
               ((xsd-schema-document-p schema) schema)
               ((xml-elem-p schema)
                (make-instance 'xsd-schema-document :root schema :version version))
               ((and (stringp schema)
                     (plusp (length schema))
                     (char= (char schema 0) #\<))
                (parse-document schema :version version))
               (t
                (make-instance 'xsd-schema-document
                               :root (emit-class schema :version version)
                               :version version)))))
    (ecase as
      (:xml (serialize doc))
      (:document doc)
      (:elem (xsd-schema-root doc)))))
