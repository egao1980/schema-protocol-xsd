(in-package #:schema-protocol-xsd/tests)

(defun %attr-person-xsd ()
  "<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\" elementFormDefault=\"qualified\">
     <xs:element name=\"person\" type=\"person\"/>
     <xs:complexType name=\"person\">
       <xs:sequence>
         <xs:element name=\"name\" type=\"xs:string\"/>
       </xs:sequence>
       <xs:attribute name=\"id\" type=\"xs:string\" use=\"required\"/>
     </xs:complexType>
   </xs:schema>")

(defun %ext-person-xsd ()
  "<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\" elementFormDefault=\"qualified\">
     <xs:element name=\"person\" type=\"person\"/>
     <xs:complexType name=\"base\">
       <xs:sequence>
         <xs:element name=\"name\" type=\"xs:string\"/>
       </xs:sequence>
     </xs:complexType>
     <xs:complexType name=\"person\">
       <xs:complexContent>
         <xs:extension base=\"base\">
           <xs:sequence>
             <xs:element name=\"age\" type=\"xs:integer\" minOccurs=\"0\"/>
           </xs:sequence>
           <xs:attribute name=\"id\" type=\"xs:string\" use=\"required\"/>
         </xs:extension>
       </xs:complexContent>
     </xs:complexType>
   </xs:schema>")

(deftest emit-attribute-slot
  (defschema %xsd-attr-user ()
    (id string :key "@id")
    (name string))
  (let ((xml (emit '%xsd-attr-user)))
    (ok (search "xs:attribute" xml))
    (ok (search "name=\"id\"" xml))
    (ok (search "use=\"required\"" xml))
    (ng (search "name=\"@id\"" xml))
    (ok (search "name=\"name\"" xml))))

(deftest emit-extension-from-supers
  (defschema %xsd-base-person ()
    (name string))
  (defschema %xsd-ext-person (%xsd-base-person)
    (id string :key "@id")
    (age integer :optional t))
  (let ((xml (emit '%xsd-ext-person)))
    (ok (search "complexContent" xml))
    (ok (search "xs:extension" xml))
    (ok (search "base=\"%xsd-base-person\"" xml))
    (ok (search "xs:attribute" xml))
    (ok (search "name=\"id\"" xml))
    (ok (search "name=\"age\"" xml))
    (let* ((doc (parse-document xml))
           (types (xml-children-named (xsd-schema-root doc) "complexType"))
           (child (find-if (lambda (c) (string= (xml-attr c "name") "%xsd-ext-person"))
                           types))
           (base (find-if (lambda (c) (string= (xml-attr c "name") "%xsd-base-person"))
                          types)))
      (ok (xml-element-p child))
      (ok (xml-element-p base))
      (ok (xml-child (xml-child child "complexContent") "extension"))
      (ng (search "<xs:element name=\"name\""
                  (xml-protocol:encode child :declaration nil))))))

(deftest tagged-union-not-extension
  (defschema %xsd-attr-shape ()
    (kind keyword)
    (:tag kind))
  (defschema %xsd-attr-circ (%xsd-attr-shape)
    (kind (eql :circ) :default :circ)
    (r number))
  (let ((xml (emit '%xsd-attr-shape)))
    (ok (search "discriminator" xml))
    (ng (search "complexContent" xml))))

(deftest compile-attribute-slots
  (let* ((class (compile-schema (%attr-person-xsd) :name 'compiled-xsd-attr-person))
         (id (schema-slot class
                          (intern "ID" (symbol-package (class-name class))))))
    (ok (equal "@id" (slot-wire-key id class)))
    (let ((obj (schema-protocol:parse class (%ht "@id" "p1" "name" "Ada"))))
      (ok (equal "p1" (%slot obj "ID")))
      (ok (equal "Ada" (%slot obj "NAME"))))
    (ok (signals (schema-protocol:parse class (%ht "name" "Ada"))
                 'schema-validation-error))))

(deftest compile-extension-super
  (let* ((class (compile-schema (%ext-person-xsd) :name 'compiled-xsd-ext-person))
         (supers (closer-mop:class-direct-superclasses class)))
    (ok (some (lambda (c) (string-equal "BASE" (symbol-name (class-name c))))
              supers))
    (let ((obj (schema-protocol:parse class (%ht "@id" "p1" "name" "Ada" "age" 36))))
      (ok (equal "p1" (%slot obj "ID")))
      (ok (equal "Ada" (%slot obj "NAME")))
      (ok (eql 36 (%slot obj "AGE"))))))

(deftest emit-compile-attribute-extension-roundtrip
  (defschema %rt-xsd-base ()
    (name string)
    (:extra :forbid))
  (defschema %rt-xsd-child (%rt-xsd-base)
    (id string :key "@id")
    (age integer :optional t)
    (:extra :forbid))
  (let* ((xml (emit '%rt-xsd-child))
         (class (compile-schema xml :name 'rt-xsd-child))
         (obj (schema-protocol:parse class (%ht "@id" "7" "name" "Ada" "age" 1))))
    (ok (search "complexContent" xml))
    (ok (equal "7" (%slot obj "ID")))
    (ok (equal "Ada" (%slot obj "NAME")))
    (ok (eql 1 (%slot obj "AGE")))))

(deftest validate-attributes
  (let ((schema (%attr-person-xsd)))
    (ok (valid-instance-p schema (%ht "@id" "p1" "name" "Ada")))
    (ok (valid-instance-p schema (%ht "id" "p1" "name" "Ada")))
    (ng (valid-instance-p schema (%ht "name" "Ada")))
    (ng (valid-instance-p schema (%ht "@id" 1 "name" "Ada")))
    (ok (valid-instance-p schema "<person id=\"p1\"><name>Ada</name></person>"))
    (ng (valid-instance-p schema "<person><name>Ada</name></person>"))))

(deftest validate-extension
  (let ((schema (%ext-person-xsd)))
    (ok (valid-instance-p schema (%ht "@id" "p1" "name" "Ada")))
    (ok (valid-instance-p schema (%ht "@id" "p1" "name" "Ada" "age" 2)))
    (ng (valid-instance-p schema (%ht "@id" "p1" "age" 2)))
    (ng (valid-instance-p schema (%ht "name" "Ada")))
    (ok (valid-instance-p schema
                         "<person id=\"p1\"><name>Ada</name><age>2</age></person>"))))

(deftest validate-unique-key-keyref
  (let ((schema "<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\">
                   <xs:element name=\"bag\" type=\"bag\">
                     <xs:unique name=\"skuUnique\">
                       <xs:selector xpath=\"item\"/>
                       <xs:field xpath=\"@sku\"/>
                     </xs:unique>
                     <xs:key name=\"itemKey\">
                       <xs:selector xpath=\"item\"/>
                       <xs:field xpath=\"id\"/>
                     </xs:key>
                     <xs:keyref name=\"itemRef\" refer=\"itemKey\">
                       <xs:selector xpath=\"order\"/>
                       <xs:field xpath=\"@item\"/>
                     </xs:keyref>
                   </xs:element>
                   <xs:complexType name=\"item\">
                     <xs:sequence>
                       <xs:element name=\"id\" type=\"xs:string\"/>
                     </xs:sequence>
                     <xs:attribute name=\"sku\" type=\"xs:string\"/>
                   </xs:complexType>
                   <xs:complexType name=\"order\">
                     <xs:attribute name=\"item\" type=\"xs:string\" use=\"required\"/>
                   </xs:complexType>
                   <xs:complexType name=\"bag\">
                     <xs:sequence>
                       <xs:element name=\"item\" type=\"item\" minOccurs=\"0\" maxOccurs=\"unbounded\"/>
                       <xs:element name=\"order\" type=\"order\" minOccurs=\"0\" maxOccurs=\"unbounded\"/>
                     </xs:sequence>
                   </xs:complexType>
                 </xs:schema>"))
    (ok (valid-instance-p schema
                         (%ht "item" (vector (%ht "@sku" "a" "id" "1")
                                             (%ht "@sku" "b" "id" "2"))
                              "order" (vector (%ht "@item" "1")))))
    (ng (valid-instance-p schema
                         (%ht "item" (vector (%ht "@sku" "a" "id" "1")
                                             (%ht "@sku" "a" "id" "2")))))
    (ng (valid-instance-p schema
                         (%ht "item" (vector (%ht "@sku" "a")))))
    (ng (valid-instance-p schema
                         (%ht "item" (vector (%ht "@sku" "a" "id" "1"))
                              "order" (vector (%ht "@item" "9")))))
    (ok (valid-instance-p schema
                         "<bag><item sku=\"a\"><id>1</id></item></bag>"))))

(deftest skip-complex-identity-xpath
  (let ((schema "<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\">
                   <xs:element name=\"bag\" type=\"bag\">
                     <xs:unique name=\"deep\">
                       <xs:selector xpath=\"item/child\"/>
                       <xs:field xpath=\"@sku\"/>
                     </xs:unique>
                   </xs:element>
                   <xs:complexType name=\"bag\">
                     <xs:sequence>
                       <xs:element name=\"item\" type=\"xs:string\" minOccurs=\"0\" maxOccurs=\"unbounded\"/>
                     </xs:sequence>
                   </xs:complexType>
                 </xs:schema>"))
    (ok (valid-instance-p schema (%ht "item" #("a" "a"))))))
