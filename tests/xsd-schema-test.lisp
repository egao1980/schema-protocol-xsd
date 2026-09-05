(in-package #:schema-protocol-xsd/tests)

(defun %ht (&rest plist)
  (let ((ht (make-hash-table :test #'equal)))
    (loop for (k v) on plist by #'cddr
          do (setf (gethash k ht) v))
    ht))

(defun %slot (obj name)
  (slot-value obj (intern (string name) (symbol-package (class-name (class-of obj))))))

(deftest emit-object
  (defschema %xsd-addr ()
    (city string))
  (defschema %xsd-user ()
    (name string :min-length 1)
    (age integer :minimum 0 :optional t)
    (address %xsd-addr)
    (tags (vector string))
    (:compute label (self)
      (slot-value self 'name))
    (:extra :forbid))
  (let* ((xml (emit '%xsd-user))
         (doc (parse-document xml))
         (root (xsd-schema-root doc)))
    (ok (search "xs:schema" xml))
    (ok (search "http://www.w3.org/2001/XMLSchema" xml))
    (ok (equal "schema" (xml-local-name root)))
    (ok (xsd-schema-document-p doc))
    (ok (search "minLength" xml))
    (ok (search "minOccurs=\"0\"" xml))
    (ok (search "maxOccurs=\"unbounded\"" xml))
    (ok (search "type=\"%xsd-addr\"" xml))
    (ng (search "<xs:any " xml))
    (ok (stringp (xsd-schema '%xsd-user)))))

(deftest emit-union-enum
  (defschema %xsd-opt ()
    (color (member :red :blue))
    (note (or :null string) :optional t))
  (let ((xml (emit '%xsd-opt)))
    (ok (search "enumeration" xml))
    (ok (search "value=\"red\"" xml))
    (ok (search "nillable=\"true\"" xml))))

(deftest compile-and-parse
  (let* ((xml "<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\" elementFormDefault=\"qualified\">
                 <xs:element name=\"person\" type=\"person\"/>
                 <xs:complexType name=\"person\">
                   <xs:sequence>
                     <xs:element name=\"name\">
                       <xs:simpleType>
                         <xs:restriction base=\"xs:string\">
                           <xs:minLength value=\"1\"/>
                         </xs:restriction>
                       </xs:simpleType>
                     </xs:element>
                     <xs:element name=\"age\" type=\"xs:integer\" minOccurs=\"0\"/>
                   </xs:sequence>
                 </xs:complexType>
               </xs:schema>")
         (class (compile-schema xml :name 'compiled-xsd-person))
         (obj (schema-protocol:parse class (%ht "name" "Ada" "age" 36))))
    (ok (equal "Ada" (%slot obj "NAME")))
    (ok (signals (schema-protocol:parse class (%ht "age" 1))
                 'schema-validation-error))
    (ok (signals (schema-protocol:parse class (%ht "name" "Ada" "x" 1))
                 'schema-validation-error))))

(deftest emit-compile-roundtrip
  (defschema %rt-xsd-note ()
    (title string)
    (body (or :null string) :optional t)
    (:extra :forbid))
  (let* ((xml (emit '%rt-xsd-note))
         (class (compile-schema xml :name 'rt-xsd-note))
         (obj (schema-protocol:parse class (%ht "title" "hi" "body" :null))))
    (ok (equal "hi" (%slot obj "TITLE")))
    (ok (eq :null (%slot obj "BODY")))))

(deftest parse-document-and-named-type
  (let* ((xml "<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\">
                 <xs:element name=\"has-home\" type=\"has-home\"/>
                 <xs:complexType name=\"addr\">
                   <xs:sequence>
                     <xs:element name=\"city\" type=\"xs:string\"/>
                   </xs:sequence>
                 </xs:complexType>
                 <xs:complexType name=\"has-home\">
                   <xs:sequence>
                     <xs:element name=\"home\" type=\"addr\"/>
                   </xs:sequence>
                 </xs:complexType>
               </xs:schema>")
         (parsed (parse-document xml))
         (class (compile-schema parsed :name 'has-xsd-home))
         (obj (schema-protocol:parse class (%ht "home" (%ht "city" "London")))))
    (ok (xsd-schema-document-p parsed))
    (let ((home (%slot obj "HOME")))
      (ok (equal "London" (%slot home "CITY"))))))

(deftest emit-tagged-union
  (defschema %xsd-shape ()
    (kind keyword)
    (:tag kind))
  (defschema %xsd-circ (%xsd-shape)
    (kind (eql :circ) :default :circ)
    (r number))
  (defschema %xsd-rect (%xsd-shape)
    (kind (eql :rect) :default :rect)
    (w number))
  (let ((xml (emit '%xsd-shape)))
    (ok (search "discriminator" xml))
    (ok (search "propertyName=\"kind\"" xml))
    (ok (search "type=\"%xsd-circ\"" xml))
    (ok (search "xs:choice" xml))))

(deftest compile-discriminator
  (let* ((xml "<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\">
                 <xs:element name=\"shape\" type=\"shape\"/>
                 <xs:complexType name=\"circ\">
                   <xs:sequence>
                     <xs:element name=\"kind\" type=\"xs:string\"/>
                     <xs:element name=\"r\" type=\"xs:decimal\"/>
                   </xs:sequence>
                 </xs:complexType>
                 <xs:complexType name=\"rect\">
                   <xs:sequence>
                     <xs:element name=\"kind\" type=\"xs:string\"/>
                     <xs:element name=\"w\" type=\"xs:decimal\"/>
                   </xs:sequence>
                 </xs:complexType>
                 <xs:complexType name=\"shape\">
                   <xs:annotation>
                     <xs:appinfo>
                       <discriminator propertyName=\"kind\">
                         <mapping value=\"circ\" type=\"circ\"/>
                         <mapping value=\"rect\" type=\"rect\"/>
                       </discriminator>
                     </xs:appinfo>
                   </xs:annotation>
                   <xs:choice>
                     <xs:element name=\"circ\" type=\"circ\"/>
                     <xs:element name=\"rect\" type=\"rect\"/>
                   </xs:choice>
                 </xs:complexType>
               </xs:schema>")
         (class (compile-schema xml :name 'compiled-xsd-shape))
         (obj (schema-protocol:parse class (%ht "kind" "circ" "r" 1.5))))
    (ok (schema-tag class))
    (ok (equal "circ" (string-downcase (symbol-name (class-name (class-of obj))))))
    (ok (signals (schema-protocol:parse class (%ht "kind" "nope"))
                 'schema-validation-error))))

(deftest format-registry
  (defschema %reg-xsd ()
    (name string))
  (let ((via-gf (xsd-schema '%reg-xsd))
        (via-reg (emit-schema '%reg-xsd :format :xsd)))
    (ok (stringp via-gf))
    (ok (stringp via-reg))
    (ok (search "xs:schema" via-reg)))
  (let* ((xml "<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\" elementFormDefault=\"qualified\">
                 <xs:element name=\"person\" type=\"person\"/>
                 <xs:complexType name=\"person\">
                   <xs:sequence>
                     <xs:element name=\"name\" type=\"xs:string\"/>
                   </xs:sequence>
                 </xs:complexType>
               </xs:schema>")
         (class (parse-schema xml :format :xsd :name 'reg-compiled-xsd)))
    (ok (schema-class-p class))
    (ok (equal "Ada" (%slot (schema-protocol:parse class (%ht "name" "Ada")) "NAME")))))
