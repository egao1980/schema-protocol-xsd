(in-package #:schema-protocol-xsd)

(defvar *generated-package* (find-package '#:schema-protocol-xsd.generated))

(defstruct compile-ctx
  (package *generated-package*)
  (root-name nil)
  (types (make-hash-table :test #'equal))
  (elements (make-hash-table :test #'equal))
  (filled (make-hash-table :test #'eq)))

(defun %sanitize (string)
  (let ((s (substitute #\- #\_ (substitute #\- #\Space (string string)))))
    (if (plusp (length s))
        (string-upcase s)
        "SCHEMA")))

(defun %name-symbol (name ctx)
  (etypecase name
    (symbol
     (if (eq (symbol-package name) (compile-ctx-package ctx))
         name
         (intern (symbol-name name) (compile-ctx-package ctx))))
    (string (intern (%sanitize (local-name name)) (compile-ctx-package ctx)))))

(defun xsd-builtin-p (type)
  (let ((n (local-name type)))
    (or (and (>= (length type) 3) (string= type "xs:" :end1 3))
        (member n '("string" "integer" "int" "long" "short" "byte"
                    "decimal" "float" "double" "boolean" "anyURI"
                    "dateTime" "date" "time" "anyType" "anySimpleType"
                    "token" "normalizedString" "NMTOKEN" "NCName"
                    "language" "ID" "IDREF" "hexBinary" "base64Binary")
                :test #'string-equal))))

(defun register-schema (ctx root)
  (dolist (c (xe-children root))
    (when (xml-elem-p c)
      (let ((n (xe-attr c "name")))
        (cond
          ((xe-named-p c "complexType")
           (when n (setf (gethash n (compile-ctx-types ctx)) c)))
          ((xe-named-p c "simpleType")
           (when n (setf (gethash n (compile-ctx-types ctx)) c)))
          ((xe-named-p c "element")
           (when n (setf (gethash n (compile-ctx-elements ctx)) c))))))))

(defun lookup-type (ctx name)
  (or (gethash (local-name name) (compile-ctx-types ctx))
      (gethash name (compile-ctx-types ctx))))

(defun %ensure-shell (ctx name)
  (let ((sym (%name-symbol name ctx)))
    (or (find-class sym nil)
        (ensure-class sym
                      :metaclass (find-class 'schema-class)
                      :direct-superclasses (list (find-class 'schema-object))
                      :direct-slots '()))))

(defun parse-occurs (elem)
  (let* ((min (xe-attr elem "minOccurs" "1"))
         (max (xe-attr elem "maxOccurs" "1"))
         (min-n (or (ignore-errors (parse-integer min :junk-allowed t)) 1))
         (unbounded (string-equal max "unbounded"))
         (max-n (if unbounded :unbounded
                    (or (ignore-errors (parse-integer max :junk-allowed t)) 1))))
    (values min-n max-n)))

(defun nillable-p (elem)
  (string-equal (xe-attr elem "nillable") "true"))

(defun extra-from (elem)
  (if (or (xe-kid elem "any")
          (and (xe-kid elem "sequence")
               (xe-kid (xe-kid elem "sequence") "any"))
          (and (xe-kid elem "choice")
               (xe-kid (xe-kid elem "choice") "any")))
      :allow
      :forbid))

(defun restriction-of (elem)
  (or (xe-kid elem "restriction")
      (let ((st (xe-kid elem "simpleType")))
        (and st (xe-kid st "restriction")))))

(defun enum-values (restriction)
  (mapcar (lambda (e) (parse-enum-value (xe-attr e "value")))
          (xe-kids restriction "enumeration")))

(defun parse-enum-value (string)
  (cond
    ((null string) nil)
    ((and (plusp (length string))
          (every (lambda (c) (or (alphanumericp c) (find c "-_"))) string)
          (alpha-char-p (char string 0)))
     (intern (string-upcase (substitute #\- #\_ string)) :keyword))
    ((ignore-errors (parse-integer string :junk-allowed nil)))
    (t string)))

(defun facet-int (restriction name)
  (let ((el (xe-kid restriction name)))
    (and el (ignore-errors (parse-integer (xe-attr el "value") :junk-allowed t)))))

(defun builtin-lisp-type (type restriction)
  (let ((n (local-name type)))
    (when (and (>= (length type) 3) (string= type "xs:" :end1 3))
      (setf n (subseq type 3)))
    (cond
      ((member n '("integer" "int" "long" "short" "byte") :test #'string-equal)
       (let ((lo (and restriction (facet-int restriction "minInclusive")))
             (hi (and restriction (facet-int restriction "maxInclusive"))))
         (if (or lo hi)
             `(integer ,(or lo '*) ,(or hi '*))
             'integer)))
      ((member n '("decimal" "float" "double") :test #'string-equal)
       'number)
      ((string-equal n "boolean") 'boolean)
      ((string-equal n "anyURI") 'string)
      ((member n '("dateTime" "date" "time") :test #'string-equal)
       'string)
      ((string-equal n "anyType") t)
      (t 'string))))

(defun simple-type-spec (elem ctx)
  (let* ((restriction (restriction-of elem))
         (union (or (xe-kid elem "union")
                    (and (xe-kid elem "simpleType")
                         (xe-kid (xe-kid elem "simpleType") "union"))))
         (enums (and restriction (enum-values restriction))))
    (cond
      (enums
       (if (= 1 (length enums))
           `(eql ,(first enums))
           `(member ,@enums)))
      (union
       (let ((members (xe-attr union "memberTypes"))
             (nested (xe-kids union "simpleType")))
         `(or ,@(append
                 (when members
                   (mapcar (lambda (tok)
                             (node-type-spec tok ctx))
                           (uiop:split-string members :separator " ")))
                 (mapcar (lambda (st) (simple-type-spec st ctx)) nested)))))
      (restriction
       (builtin-lisp-type (or (xe-attr restriction "base") "xs:string") restriction))
      ((and (stringp elem) (xsd-builtin-p elem))
       (builtin-lisp-type elem nil))
      (t 'string))))

(defun node-type-spec (node ctx &key name-hint)
  (cond
    ((stringp node)
     (if (xsd-builtin-p node)
         (builtin-lisp-type node nil)
         (let ((resolved (lookup-type ctx node)))
           (unless resolved
             (error 'xsd-schema-ref-error
                    :ref node
                    :message "named type not in schema"))
           (if (xe-named-p resolved "simpleType")
               (simple-type-spec resolved ctx)
               (progn
                 (%ensure-shell ctx node)
                 (%fill-class ctx node resolved)
                 (%name-symbol node ctx))))))
    ((xml-elem-p node)
     (cond
       ((xe-named-p node "simpleType")
        (simple-type-spec node ctx))
       ((xe-named-p node "complexType")
        (let ((n (or name-hint (xe-attr node "name")
                     (gentemp "OBJ" (compile-ctx-package ctx)))))
          (%ensure-shell ctx n)
          (%fill-class ctx n node)
          (%name-symbol n ctx)))
       ((xe-named-p node "element")
        (element-type-spec node ctx :name-hint name-hint))
       (t t)))
    (t t)))

(defun element-type-spec (elem ctx &key name-hint)
  (let* ((type (xe-attr elem "type"))
         (inline-simple (xe-kid elem "simpleType"))
         (inline-complex (xe-kid elem "complexType"))
         (spec (cond
                 (inline-simple (simple-type-spec inline-simple ctx))
                 (inline-complex
                  (node-type-spec inline-complex ctx
                                  :name-hint (or name-hint (xe-attr elem "name"))))
                 (type (node-type-spec type ctx :name-hint (or name-hint (xe-attr elem "name"))))
                 (t t))))
    (multiple-value-bind (min max) (parse-occurs elem)
      (declare (ignore min))
      (let ((spec (if (nillable-p elem)
                      (if (and (consp spec) (eq (first spec) 'or))
                          spec
                          `(or :null ,spec))
                      spec)))
        (if (or (eq max :unbounded) (and (numberp max) (> max 1)))
            `(vector ,spec)
            spec)))))

(defun facet-slot-options (elem)
  (let* ((restriction (restriction-of (or (xe-kid elem "simpleType") elem)))
         (opts '()))
    (when restriction
      (let ((minl (facet-int restriction "minLength"))
            (maxl (facet-int restriction "maxLength"))
            (mini (facet-int restriction "minInclusive"))
            (maxi (facet-int restriction "maxInclusive")))
        (when minl (setf opts (list* :min-length minl opts)))
        (when maxl (setf opts (list* :max-length maxl opts)))
        (when mini (setf opts (list* :minimum mini opts)))
        (when maxi (setf opts (list* :maximum maxi opts)))))
    (let* ((type (xe-attr elem "type"))
           (n (and type (local-name type))))
      (cond
        ((and type (or (string-equal n "anyURI") (string-equal type "xs:anyURI")))
         (setf opts (list* :format :uri opts)))
        ((and type (or (string-equal n "dateTime") (string-equal type "xs:dateTime")))
         (setf opts (list* :format :date-time opts)))))
    opts))

(defun property-slot (elem ctx)
  (let* ((name (xe-attr elem "name"))
         (sym (%name-symbol name ctx))
         (spec (element-type-spec elem ctx :name-hint name)))
    (multiple-value-bind (min max) (parse-occurs elem)
      (declare (ignore max))
      (append `(:name ,sym
                :type ,spec
                :initargs (,(intern (symbol-name sym) :keyword))
                :readers (,sym)
                :writers ((setf ,sym))
                :key ,(stringify-key name)
                :required ,(>= min 1)
                :optional ,(< min 1))
              (facet-slot-options elem)))))

(defun discriminator-of (elem)
  (let* ((ann (xe-kid elem "annotation"))
         (app (and ann (xe-kid ann "appinfo")))
         (disc (and app (xe-kid app "discriminator"))))
    disc))

(defun %specialize-tag-slots (ctx vnode prop tag-value)
  (let* ((seq (or (xe-kid vnode "sequence") (xe-kid vnode "choice")))
         (slots '()))
    (when seq
      (dolist (el (xe-kids seq "element"))
        (let ((slot (property-slot el ctx)))
          (when (and tag-value (string-equal (xe-attr el "name") prop))
            (setf (getf slot :type) `(eql ,(parse-enum-value tag-value))))
          (push slot slots))))
    (nreverse slots)))

(defun fill-tagged (ctx name elem)
  (let ((sym (%name-symbol name ctx)))
    (when (gethash sym (compile-ctx-filled ctx))
      (return-from fill-tagged (find-class sym)))
    (setf (gethash sym (compile-ctx-filled ctx)) t)
    (let* ((disc (discriminator-of elem))
           (prop (or (and disc (xe-attr disc "propertyName")) "type"))
           (tag-sym (%name-symbol prop ctx))
           (choice (xe-kid elem "choice"))
           (slots `((:name ,tag-sym
                     :type t
                     :initargs (,(intern (symbol-name tag-sym) :keyword))
                     :readers (,tag-sym)
                     :writers ((setf ,tag-sym))
                     :key ,(stringify-key prop)
                     :required t))))
      (let ((base (ensure-class sym
                                :metaclass (find-class 'schema-class)
                                :direct-superclasses (list (find-class 'schema-object))
                                :direct-slots slots
                                :extra (extra-from elem)
                                :tag tag-sym)))
        (flet ((add-variant (vname vnode &optional tag-value)
                 (when vnode
                   (if tag-value
                       (let ((vsym (%name-symbol vname ctx)))
                         (when (gethash vsym (compile-ctx-filled ctx))
                           (return-from add-variant (find-class vsym)))
                         (setf (gethash vsym (compile-ctx-filled ctx)) t)
                         (ensure-class vsym
                                       :metaclass (find-class 'schema-class)
                                       :direct-superclasses (list base)
                                       :direct-slots (%specialize-tag-slots ctx vnode prop tag-value)
                                       :extra (extra-from vnode)))
                       (%fill-class ctx vname vnode :supers (list base))))))
          (when disc
            (dolist (m (xe-kids disc "mapping"))
              (let ((type (xe-attr m "type")))
                (when type
                  (add-variant type (lookup-type ctx type) (xe-attr m "value"))))))
          (when choice
            (dolist (el (xe-kids choice "element"))
              (let ((type (or (xe-attr el "type") (xe-attr el "name"))))
                (when (and type (not (gethash (%name-symbol type ctx)
                                              (compile-ctx-filled ctx))))
                  (add-variant type (or (lookup-type ctx type)
                                        (xe-kid el "complexType"))))))))
        (find-class sym)))))

(defun %fill-class (ctx name node &key supers)
  (when (discriminator-of node)
    (return-from %fill-class (fill-tagged ctx name node)))
  (let ((sym (%name-symbol name ctx)))
    (when (gethash sym (compile-ctx-filled ctx))
      (return-from %fill-class (find-class sym)))
    (setf (gethash sym (compile-ctx-filled ctx)) t)
    (let* ((seq (or (xe-kid node "sequence") (xe-kid node "choice")))
           (slots '()))
      (when seq
        (dolist (el (xe-kids seq "element"))
          (push (property-slot el ctx) slots)))
      (ensure-class sym
                    :metaclass (find-class 'schema-class)
                    :direct-superclasses (or supers (list (find-class 'schema-object)))
                    :direct-slots (nreverse slots)
                    :extra (extra-from node))
      (find-class sym))))

(defun compile-schema (source &key name (package *generated-package*) version)
  "XSD document → schema-class. NAME defaults to the root element."
  (declare (ignore version))
  (let* ((doc (parse-document source))
         (root (xsd-schema-root doc))
         (ctx (make-compile-ctx :package package)))
    (unless (xe-named-p root "schema")
      (error 'xsd-schema-error
             :message (format nil "expected xs:schema, got ~S" (xe-name root))))
    (register-schema ctx root)
    (let* ((root-el (find-if (lambda (c) (xe-named-p c "element"))
                             (xe-children root)))
           (root-name (or name
                          (and root-el (xe-attr root-el "name"))
                          (gentemp "SCHEMA" package))))
      (setf (compile-ctx-root-name ctx) (%name-symbol root-name ctx))
      (%ensure-shell ctx (compile-ctx-root-name ctx))
      (maphash (lambda (k v)
                 (declare (ignore v))
                 (%ensure-shell ctx k))
               (compile-ctx-types ctx))
      (let ((node (or (and root-el
                           (or (lookup-type ctx (xe-attr root-el "type"))
                               (xe-kid root-el "complexType")
                               (xe-kid root-el "simpleType")))
                      (lookup-type ctx (string-downcase (symbol-name (compile-ctx-root-name ctx)))))))
        (unless node
          (error 'xsd-schema-error :message "no root complexType"))
        (%fill-class ctx (compile-ctx-root-name ctx) node))
      (maphash (lambda (k v)
                 (let ((sym (%name-symbol k ctx)))
                   (unless (gethash sym (compile-ctx-filled ctx))
                     (%fill-class ctx k v))))
               (compile-ctx-types ctx))
      (find-class (compile-ctx-root-name ctx)))))
