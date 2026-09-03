(in-package #:schema-protocol-xsd/tests)

(defun %person-xsd ()
  "<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\" elementFormDefault=\"qualified\">
     <xs:element name=\"person\" type=\"person\"/>
     <xs:complexType name=\"person\">
       <xs:sequence>
         <xs:element name=\"msg\" type=\"xs:string\"/>
       </xs:sequence>
     </xs:complexType>
   </xs:schema>")

(deftest validate-object-schema
  (let ((schema (%person-xsd)))
    (ok (valid-instance-p schema (%ht "msg" "ok")))
    (ng (valid-instance-p schema (%ht)))
    (ng (valid-instance-p schema (%ht "msg" 1)))
    (ng (valid-instance-p schema (%ht "msg" "ok" "x" 1)))
    (ok (valid-instance-p schema '(:msg "ok")))
    (ok (signals (validate-instance schema (%ht "msg" 1))
                 'xsd-schema-validation-error))))

(deftest validate-facets
  (let ((schema "<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\">
                   <xs:element name=\"n\" type=\"n\"/>
                   <xs:simpleType name=\"n\">
                     <xs:restriction base=\"xs:integer\">
                       <xs:minInclusive value=\"0\"/>
                     </xs:restriction>
                   </xs:simpleType>
                 </xs:schema>"))
    (ok (valid-instance-p schema 3))
    (ng (valid-instance-p schema -1))
    (ok (valid-instance-p schema "3")))
  (let ((schema "<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\">
                   <xs:element name=\"s\" type=\"s\"/>
                   <xs:simpleType name=\"s\">
                     <xs:restriction base=\"xs:string\">
                       <xs:pattern value=\"^a+$\"/>
                     </xs:restriction>
                   </xs:simpleType>
                 </xs:schema>"))
    (ok (valid-instance-p schema "aaa"))
    (ng (valid-instance-p schema "ab")))
  (let ((schema "<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\">
                   <xs:element name=\"c\" type=\"c\"/>
                   <xs:simpleType name=\"c\">
                     <xs:restriction base=\"xs:string\">
                       <xs:enumeration value=\"a\"/>
                       <xs:enumeration value=\"b\"/>
                     </xs:restriction>
                   </xs:simpleType>
                 </xs:schema>"))
    (ok (valid-instance-p schema "a"))
    (ng (valid-instance-p schema "z"))))

(deftest validate-vector-and-nillable
  (let ((schema "<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\">
                   <xs:element name=\"bag\" type=\"bag\"/>
                   <xs:complexType name=\"bag\">
                     <xs:sequence>
                       <xs:element name=\"tags\" type=\"xs:string\" maxOccurs=\"unbounded\"/>
                       <xs:element name=\"note\" type=\"xs:string\" nillable=\"true\" minOccurs=\"0\"/>
                     </xs:sequence>
                   </xs:complexType>
                 </xs:schema>"))
    (ok (valid-instance-p schema (%ht "tags" #("a" "b"))))
    (ng (valid-instance-p schema (%ht)))
    (ok (valid-instance-p schema (%ht "tags" #("a") "note" :null)))
    (ng (valid-instance-p schema (%ht "tags" #(1))))))

(deftest validate-xml-instance
  (let ((schema (%person-xsd)))
    (ok (valid-instance-p schema "<person><msg>ok</msg></person>"))
    (ng (valid-instance-p schema "<person></person>"))
    (ng (valid-instance-p schema "<nope><msg>ok</msg></nope>"))))

(deftest decode-validating-dom
  (let* ((schema (%person-xsd))
         (doc (decode-validating "<person><msg>ok</msg></person>" schema)))
    (ok (xml-document-p doc))
    (ok (string= "person" (xml-local-name (document-root-element doc))))
    (ok (string= "ok" (xml-element-text (xml-child (document-root-element doc) "msg"))))
    (ok (signals (decode-validating "<person></person>" schema)
                 'xsd-schema-validation-error))
    (ok (signals (decode-validating "<a></b>" schema)
                 'xml-parse-error))))

(deftest validate-named-ref
  (let ((schema "<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\">
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
                 </xs:schema>"))
    (ok (valid-instance-p schema (%ht "home" (%ht "city" "London"))))
    (ng (valid-instance-p schema (%ht "home" (%ht))))))

(deftest compile-validator-reuse
  (let ((v (compile-validator
            "<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\">
               <xs:element name=\"s\" type=\"s\"/>
               <xs:simpleType name=\"s\">
                 <xs:restriction base=\"xs:string\">
                   <xs:minLength value=\"2\"/>
                 </xs:restriction>
               </xs:simpleType>
             </xs:schema>")))
    (ok (xsd-schema-validator-p v))
    (ok (valid-instance-p v "ab"))
    (ng (valid-instance-p v "a"))))

(deftest validate-tagged
  (defschema %v-shape ()
    (kind keyword)
    (:tag kind))
  (defschema %v-circ (%v-shape)
    (kind (eql :circ) :default :circ)
    (r number))
  (let ((xml (emit '%v-shape)))
    (ok (valid-instance-p xml (%ht "kind" "circ" "r" 1.5)))
    (ng (valid-instance-p xml (%ht "kind" "nope")))))
