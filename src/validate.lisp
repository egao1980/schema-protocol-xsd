(in-package #:schema-protocol-xsd)

;;; XSD 1.0 instance validator. CLOS compile-schema stays a separate job.
;;; Local named types only. No xs:import / xs:include.

(defstruct (xsd-schema-validator (:conc-name validator-))
  root
  version
  types
  elements
  root-element
  default-open)

(defstruct vctx
  validator
  (path nil)
  (issues nil))

(defun vfail (ctx message &optional value)
  (push (make-schema-issue :path (reverse (vctx-path ctx))
                           :message message
                           :value value)
        (vctx-issues ctx))
  nil)

(defun with-path (ctx token fn)
  (push token (vctx-path ctx))
  (unwind-protect (funcall fn)
    (pop (vctx-path ctx))))

(defun json-null-p (value)
  (eq value :null))

(defun object-p (value)
  (hash-table-p value))

(defun lookup-named (validator name)
  (or (gethash (local-name name) (validator-types validator))
      (gethash name (validator-types validator))))

(defun lookup-element (validator name)
  (or (gethash (local-name name) (validator-elements validator))
      (gethash name (validator-elements validator))))

(defun instance-table (source)
  (cond
    ((hash-table-p source)
     (let ((out (make-hash-table :test #'equal)))
       (maphash (lambda (k v) (setf (gethash (stringify-key k) out) (normalize-instance v)))
                source)
       out))
    ((xml-elem-p source)
     (normalize-instance (elem-to-value source)))
    ((and (stringp source) (plusp (length source)) (char= (char source 0) #\<))
     (normalize-instance (elem-to-value (parse-xml source))))
    ((and (listp source) (keywordp (first source)))
     (let ((out (make-hash-table :test #'equal)))
       (loop for (k v) on source by #'cddr
             do (setf (gethash (stringify-key k) out) (normalize-instance v)))
       out))
    ((and (listp source) (consp (first source)))
     (let ((out (make-hash-table :test #'equal)))
       (dolist (pair source out)
         (setf (gethash (stringify-key (car pair)) out) (normalize-instance (cdr pair))))))
    (t source)))

(defun normalize-instance (value)
  (cond
    ((hash-table-p value) (instance-table value))
    ((and (vectorp value) (not (stringp value)))
     (map 'vector #'normalize-instance value))
    ((and (listp value) (or (keywordp (first value)) (consp (first value))))
     (instance-table value))
    (t value)))

(defun unwrap-xml-root (source validator)
  "If SOURCE is an XML document, return (values payload root-name)."
  (cond
    ((xml-elem-p source)
     (values (elem-to-value source) (xe-local source)))
    ((and (stringp source) (plusp (length source)) (char= (char source 0) #\<))
     (unwrap-xml-root (parse-xml source) validator))
    (t (values (instance-table source) nil))))

(defun integer-string-p (value)
  (or (integerp value)
      (and (stringp value)
           (plusp (length value))
           (let ((start (if (find (char value 0) "+-") 1 0)))
             (and (< start (length value))
                  (every #'digit-char-p (subseq value start)))))))

(defun number-string-p (value)
  (or (realp value)
      (and (stringp value)
           (let* ((*read-eval* nil)
                  (n (ignore-errors (read-from-string value))))
             (realp n)))))

(defun boolean-value-p (value)
  (or (eq value t) (eq value nil)
      (and (stringp value)
           (member value '("true" "false" "1" "0") :test #'string-equal))))

(defun type-matches (xsd-type value)
  (let ((n (local-name xsd-type)))
    (when (and (>= (length xsd-type) 3) (string= xsd-type "xs:" :end1 3))
      (setf n (subseq xsd-type 3)))
    (cond
      ((member n '("integer" "int" "long" "short" "byte") :test #'string-equal)
       (integer-string-p value))
      ((member n '("decimal" "float" "double") :test #'string-equal)
       (number-string-p value))
      ((string-equal n "boolean") (boolean-value-p value))
      ((string-equal n "error") nil)
      ((string-equal n "anyType") t)
      ((string-equal n "anySimpleType")
       (or (stringp value) (realp value) (boolean-value-p value)))
      ((member n '("string" "token" "normalizedString" "NMTOKEN" "NCName"
                   "language" "ID" "IDREF" "anyURI" "dateTime" "date" "time"
                   "hexBinary" "base64Binary")
               :test #'string-equal)
       (or (stringp value) (keywordp value)))
      (t (stringp value)))))

(defun check-pattern (pattern string)
  (handler-case
      (not (null (cl-ppcre:scan pattern string)))
    (error () nil)))

(defun check-facets (ctx restriction value)
  (unless restriction
    (return-from check-facets t))
  (let ((str (as-string value))
        (num (as-number value))
        (minl (facet-int restriction "minLength"))
        (maxl (facet-int restriction "maxLength"))
        (mini (xe-kid restriction "minInclusive"))
        (maxi (xe-kid restriction "maxInclusive"))
        (pat (xe-kid restriction "pattern"))
        (tz (xe-kid restriction "explicitTimezone"))
        (enums (xe-kids restriction "enumeration"))
        (assertions (xe-kids restriction "assertion")))
    (when (and minl str (< (length str) minl))
      (vfail ctx (format nil "minLength ~A" minl) value))
    (when (and maxl str (> (length str) maxl))
      (vfail ctx (format nil "maxLength ~A" maxl) value))
    (when (and mini num)
      (let ((bound (as-number (xe-attr mini "value"))))
        (when (and bound (< num bound))
          (vfail ctx (format nil "minInclusive ~A" bound) value))))
    (when (and maxi num)
      (let ((bound (as-number (xe-attr maxi "value"))))
        (when (and bound (> num bound))
          (vfail ctx (format nil "maxInclusive ~A" bound) value))))
    (when (and pat str (not (check-pattern (xe-attr pat "value") str)))
      (vfail ctx (format nil "pattern ~S" (xe-attr pat "value")) value))
    (when enums
      (unless (some (lambda (e)
                      (let ((ev (xe-attr e "value")))
                        (or (equal ev value)
                            (and str (string-equal ev str)))))
                    enums)
        (vfail ctx "enumeration" value)))
    (when (and tz str)
      (check-explicit-timezone ctx (xe-attr tz "value") str))
    (dolist (a assertions)
      (unless (xpath-true-p (xe-attr a "test") value)
        (vfail ctx (format nil "assertion ~S" (xe-attr a "test")) value)))))

(defun datetime-has-timezone-p (string)
  (let ((n (length string)))
    (and (>= n 1)
         (or (find (char string (1- n)) "Zz")
             (and (>= n 6)
                  (find (char string (- n 6)) "+-"))))))

(defun check-explicit-timezone (ctx policy string)
  (let ((has (datetime-has-timezone-p string)))
    (cond
      ((string-equal policy "required")
       (unless has (vfail ctx "explicitTimezone required" string)))
      ((string-equal policy "prohibited")
       (when has (vfail ctx "explicitTimezone prohibited" string))))))

(defun silent-valid-p (ctx type value)
  (let ((saved (vctx-issues ctx)))
    (setf (vctx-issues ctx) nil)
    (check-type-ref ctx type value)
    (let ((ok (null (vctx-issues ctx))))
      (setf (vctx-issues ctx) saved)
      ok)))

(defun check-simple (ctx elem value)
  (let ((restriction (restriction-of elem))
        (union (or (xe-kid elem "union")
                   (and (xe-kid elem "simpleType")
                        (xe-kid (xe-kid elem "simpleType") "union")))))
    (cond
      (union
       (let ((members (xe-attr union "memberTypes"))
             (nested (xe-kids union "simpleType")))
         (unless (or (and members
                          (some (lambda (tok)
                                  (silent-valid-p ctx tok value))
                                (remove "" (uiop:split-string members :separator " ")
                                        :test #'string=)))
                     (some (lambda (st) (silent-valid-p ctx st value)) nested))
           (vfail ctx "union" value))))
      (t
       (when restriction
         (let ((base (xe-attr restriction "base")))
           (when (and base (xsd-builtin-p base) (not (type-matches base value)))
             (vfail ctx (format nil "type ~A" base) value)))
         (check-facets ctx restriction value))
       (unless restriction
         (let ((type (xe-attr elem "type")))
           (when (and type (xsd-builtin-p type) (not (type-matches type value)))
             (vfail ctx (format nil "type ~A" type) value))))))))

(defun check-type-ref (ctx type value)
  (cond
    ((null type)
     t)
    ((stringp type)
     (cond
       ((xsd-error-type-p type)
        (vfail ctx "xs:error" value))
       ((xsd-builtin-p type)
        (unless (type-matches type value)
          (vfail ctx (format nil "type ~A" type) value)))
       (t
        (let ((node (lookup-named (vctx-validator ctx) type)))
          (unless node
            (vfail ctx (format nil "unresolved type ~S" type))
            (return-from check-type-ref nil))
          (check-node ctx node value)))))
    ((xml-elem-p type)
     (check-node ctx type value))
    (t t)))

(defun check-node (ctx node value)
  (cond
    ((xe-named-p node "simpleType")
     (check-simple ctx node value))
    ((xe-named-p node "complexType")
     (check-complex ctx node value))
    ((xe-named-p node "element")
     (check-element ctx node value))
    (t t)))

(defun check-element (ctx elem value)
  (multiple-value-bind (min max) (parse-occurs elem)
    (let ((nillable (nillable-p elem))
          (type (xe-attr elem "type"))
          (inline (or (xe-kid elem "simpleType") (xe-kid elem "complexType"))))
      (labels ((one (v)
                 (let ((alts (alternatives-of elem)))
                   (cond
                     ((and (json-null-p v) nillable) t)
                     (alts
                      (let ((alt (find-if (lambda (a)
                                            (xpath-true-p (xe-attr a "test") v))
                                          alts)))
                        (if alt
                            (check-type-ref ctx (xe-attr alt "type") v)
                            (vfail ctx "no type alternative" v))))
                     (inline (check-node ctx inline v))
                     (type (check-type-ref ctx type v))
                     (t t)))))
        (cond
          ((or (eq max :unbounded) (and (numberp max) (> max 1)))
           (let ((seq (cond
                        ((array-p value) value)
                        ((and (listp value) (not (keywordp (first value))))
                         (coerce value 'vector))
                        ((null value) #())
                        (t (vector value)))))
             (when (< (length seq) min)
               (vfail ctx (format nil "minOccurs ~A" min) value))
             (when (and (numberp max) (> (length seq) max))
               (vfail ctx (format nil "maxOccurs ~A" max) value))
             (loop for item across (coerce seq 'vector)
                   for i from 0
                   do (with-path ctx i (lambda () (one item))))))
          ((and (null value) (< min 1)) t)
          (t (one value)))))))

(defun object-keys (table)
  (let ((acc '()))
    (maphash (lambda (k v) (declare (ignore v)) (push k acc)) table)
    acc))

(defun effective-open-content (node)
  (or (xe-kid node "openContent")
      (and (vctx-validator *vctx*)
           (validator-default-open (vctx-validator *vctx*)))))

(defvar *vctx* nil)

(defun open-wildcard-p (node)
  (let ((oc (effective-open-content node)))
    (cond
      ((and oc (string-equal (xe-attr oc "mode") "none")) nil)
      (oc t)
      (t
       (let ((seq (content-group node)))
         (and seq (xe-kid seq "any")))))))

(defun check-complex (ctx node value)
  (let ((*vctx* ctx)
        (disc (discriminator-of node))
        (alts (alternatives-of node)))
    (when (and alts (object-p value))
      (let ((alt (find-if (lambda (a) (xpath-true-p (xe-attr a "test") value)) alts)))
        (if alt
            (return-from check-complex
              (check-type-ref ctx (xe-attr alt "type") value))
            (return-from check-complex
              (vfail ctx "no type alternative" value)))))
    (when (and disc (object-p value))
      (return-from check-complex (check-tagged ctx node disc value)))
    (unless (object-p value)
      (vfail ctx "expected object" value)
      (return-from check-complex nil))
    (let* ((seq (content-group node))
           (wildcard (open-wildcard-p node))
           (seen (make-hash-table :test #'equal)))
      (when seq
        (dolist (el (xe-kids seq "element"))
          (let ((name (xe-attr el "name")))
            (setf (gethash name seen) t)
            (multiple-value-bind (v present) (gethash name value)
              (multiple-value-bind (min max) (parse-occurs el)
                (declare (ignore max))
                (cond
                  ((not present)
                   (when (>= min 1)
                     (with-path ctx name (lambda () (vfail ctx "required")))))
                  (t
                   (with-path ctx name (lambda () (check-element ctx el v))))))))))
      (unless wildcard
        (maphash (lambda (k v)
                   (unless (gethash k seen)
                     (with-path ctx k (lambda () (vfail ctx "unexpected element" v)))))
                 value))
      (dolist (a (xe-kids node "assert"))
        (unless (xpath-true-p (xe-attr a "test") value)
          (vfail ctx (format nil "assert ~S" (xe-attr a "test")) value))))))

(defun check-tagged (ctx node disc value)
  (declare (ignore node))
  (let* ((prop (xe-attr disc "propertyName"))
         (raw (and prop (gethash prop value)))
         (tag (as-string raw)))
    (unless tag
      (with-path ctx (or prop "type") (lambda () (vfail ctx "missing tag")))
      (return-from check-tagged nil))
    (let ((mapped (find-if (lambda (m)
                             (string-equal (xe-attr m "value") tag))
                           (xe-kids disc "mapping"))))
      (unless mapped
        (with-path ctx prop (lambda () (vfail ctx "unknown tag" raw)))
        (return-from check-tagged nil))
      (check-type-ref ctx (xe-attr mapped "type") value))))

(defun compile-validator (source &key version)
  "XSD document → reusable validator (named type catalog)."
  (when (xsd-schema-validator-p source)
    (return-from compile-validator source))
  (let* ((doc (parse-document source))
         (root (xsd-schema-root doc))
         (types (make-hash-table :test #'equal))
         (elements (make-hash-table :test #'equal))
         (root-el nil)
         (default-open nil))
    (unless (xe-named-p root "schema")
      (error 'xsd-schema-error
             :message (format nil "expected xs:schema, got ~S" (xe-name root))))
    (dolist (c (xe-children root))
      (when (xml-elem-p c)
        (let ((n (xe-attr c "name")))
          (cond
            ((xe-named-p c "defaultOpenContent")
             (setf default-open c))
            ((and (xe-named-p c "complexType") n)
             (setf (gethash n types) c))
            ((and (xe-named-p c "simpleType") n)
             (setf (gethash n types) c))
            ((and (xe-named-p c "element") n)
             (setf (gethash n elements) c)
             (unless root-el (setf root-el c)))))))
    (make-xsd-schema-validator :root root
                               :version (or version (xsd-schema-version doc))
                               :types types
                               :elements elements
                               :root-element root-el
                               :default-open default-open)))

(defun root-type-node (validator)
  (let ((el (validator-root-element validator)))
    (or (and el (lookup-named validator (xe-attr el "type")))
        (and el (or (xe-kid el "complexType") (xe-kid el "simpleType")))
        (let ((count 0) (only nil))
          (maphash (lambda (k v)
                     (declare (ignore k))
                     (incf count)
                     (setf only v))
                   (validator-types validator))
          (and (= count 1) only)))))

(defun validate-instance (schema instance)
  "Validate INSTANCE (hash-table / plist / XML string) against an XSD document.
   Signals XSD-SCHEMA-VALIDATION-ERROR. Returns INSTANCE on success."
  (let* ((validator (compile-validator schema))
         (ctx (make-vctx :validator validator)))
    (multiple-value-bind (payload root-name) (unwrap-xml-root instance validator)
      (when root-name
        (let ((expected (and (validator-root-element validator)
                             (xe-attr (validator-root-element validator) "name"))))
          (when (and expected (not (string-equal expected root-name)))
            (vfail ctx (format nil "root element ~S, expected ~S" root-name expected)))))
      (let ((root-el (validator-root-element validator)))
        (cond
          ((and root-el (alternatives-of root-el))
           (check-element ctx root-el payload))
          (t
           (let ((node (root-type-node validator)))
             (unless node
               (error 'xsd-schema-error :message "no root type to validate against"))
             (check-node ctx node payload))))))
    (when (vctx-issues ctx)
      (error 'xsd-schema-validation-error
             :issues (nreverse (vctx-issues ctx))))
    instance))

(defun valid-instance-p (schema instance)
  (handler-case
      (progn (validate-instance schema instance) t)
    (xsd-schema-validation-error () nil)
    (xsd-schema-ref-error () nil)
    (xsd-schema-error () nil)))
