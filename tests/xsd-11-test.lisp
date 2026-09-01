(in-package #:schema-protocol-xsd/tests)

(deftest xpath-subset
  (ok (schema-protocol-xsd::xpath-true-p "kind = 'circ'" (%ht "kind" "circ" "r" 1)))
  (ng (schema-protocol-xsd::xpath-true-p "kind = 'circ'" (%ht "kind" "rect")))
  (ok (schema-protocol-xsd::xpath-true-p "count(tags) ge 1" (%ht "tags" #("a"))))
  (ng (schema-protocol-xsd::xpath-true-p "count(tags) ge 1" (%ht "tags" #())))
  (ok (schema-protocol-xsd::xpath-true-p "$value mod 2 = 0" 4))
  (ng (schema-protocol-xsd::xpath-true-p "$value mod 2 = 0" 3))
  (ok (schema-protocol-xsd::xpath-true-p "not(exists(x)) and r > 0" (%ht "r" 1.5))))

(deftest emit-1.1-schema-header
  (defschema %x11-plain ()
    (name string)
    (:extra :allow))
  (let ((xml (emit '%x11-plain :version :1.1)))
    (ok (search "version=\"1.1\"" xml))
    (ok (search "vc:minVersion=\"1.1\"" xml))
    (ok (search +xsd-vc-ns+ xml))
    (ok (search "openContent" xml))
    (ok (search "mode=\"interleave\"" xml))
    (ng (search "<xs:any minOccurs" xml))))

(deftest emit-1.1-alternatives
  (defschema %x11-shape ()
    (kind keyword)
    (:tag kind))
  (defschema %x11-circ (%x11-shape)
    (kind (eql :circ) :default :circ)
    (r number))
  (defschema %x11-rect (%x11-shape)
    (kind (eql :rect) :default :rect)
    (w number))
  (let ((xml (emit '%x11-shape :version :1.1)))
    (ok (search "xs:alternative" xml))
    (ok (search "kind = 'circ'" xml))
    (ok (search "type=\"xs:error\"" xml))
    (ng (search "discriminator" xml))
    (let ((class (compile-schema xml :name 'compiled-x11-shape)))
      (ok (schema-tag class))
      (let ((obj (schema-protocol:parse class (%ht "kind" "circ" "r" 1.5))))
        (ok (equal "%x11-circ"
                   (string-downcase (symbol-name (class-name (class-of obj)))))))))))

(deftest validate-assert
  (let ((schema "<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\" version=\"1.1\">
                   <xs:element name=\"bag\" type=\"bag\"/>
                   <xs:complexType name=\"bag\">
                     <xs:sequence>
                       <xs:element name=\"tags\" type=\"xs:string\" minOccurs=\"0\" maxOccurs=\"unbounded\"/>
                     </xs:sequence>
                     <xs:assert test=\"count(tags) ge 1\"/>
                   </xs:complexType>
                 </xs:schema>"))
    (ok (eq :1.1 (xsd-schema-version (parse-document schema))))
    (ok (valid-instance-p schema (%ht "tags" #("a"))))
    (ng (valid-instance-p schema (%ht "tags" #())))
    (ng (valid-instance-p schema (%ht)))))

(deftest validate-assertion-and-mod
  (let ((schema "<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\" version=\"1.1\">
                   <xs:element name=\"n\" type=\"even\"/>
                   <xs:simpleType name=\"even\">
                     <xs:restriction base=\"xs:integer\">
                       <xs:assertion test=\"$value mod 2 = 0\"/>
                     </xs:restriction>
                   </xs:simpleType>
                 </xs:schema>"))
    (ok (valid-instance-p schema 4))
    (ok (valid-instance-p schema "8"))
    (ng (valid-instance-p schema 3))))

(deftest validate-alternative
  (let ((schema "<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\" version=\"1.1\">
                   <xs:element name=\"shape\">
                     <xs:alternative test=\"kind = 'circ'\" type=\"circ\"/>
                     <xs:alternative test=\"kind = 'rect'\" type=\"rect\"/>
                     <xs:alternative type=\"xs:error\"/>
                   </xs:element>
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
                 </xs:schema>"))
    (ok (valid-instance-p schema (%ht "kind" "circ" "r" 1.5)))
    (ok (valid-instance-p schema (%ht "kind" "rect" "w" 2)))
    (ng (valid-instance-p schema (%ht "kind" "circ")))
    (ng (valid-instance-p schema (%ht "kind" "nope")))))

(deftest validate-open-content
  (let ((schema "<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\" version=\"1.1\">
                   <xs:element name=\"user\" type=\"user\"/>
                   <xs:complexType name=\"user\">
                     <xs:openContent mode=\"interleave\">
                       <xs:any processContents=\"lax\"/>
                     </xs:openContent>
                     <xs:sequence>
                       <xs:element name=\"name\" type=\"xs:string\"/>
                     </xs:sequence>
                   </xs:complexType>
                 </xs:schema>"))
    (ok (valid-instance-p schema (%ht "name" "Ada" "x" 1)))
    (ng (valid-instance-p schema (%ht)))))

(deftest validate-default-open-content
  (let ((schema "<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\" version=\"1.1\">
                   <xs:defaultOpenContent mode=\"interleave\">
                     <xs:any processContents=\"lax\"/>
                   </xs:defaultOpenContent>
                   <xs:element name=\"user\" type=\"user\"/>
                   <xs:complexType name=\"user\">
                     <xs:sequence>
                       <xs:element name=\"name\" type=\"xs:string\"/>
                     </xs:sequence>
                   </xs:complexType>
                 </xs:schema>"))
    (ok (valid-instance-p schema (%ht "name" "Ada" "note" "x")))))

(deftest validate-explicit-timezone
  (let ((req "<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\" version=\"1.1\">
                <xs:element name=\"t\" type=\"t\"/>
                <xs:simpleType name=\"t\">
                  <xs:restriction base=\"xs:dateTime\">
                    <xs:explicitTimezone value=\"required\"/>
                  </xs:restriction>
                </xs:simpleType>
              </xs:schema>")
        (pro "<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\" version=\"1.1\">
                <xs:element name=\"t\" type=\"t\"/>
                <xs:simpleType name=\"t\">
                  <xs:restriction base=\"xs:dateTime\">
                    <xs:explicitTimezone value=\"prohibited\"/>
                  </xs:restriction>
                </xs:simpleType>
              </xs:schema>"))
    (ok (valid-instance-p req "2020-01-01T12:00:00Z"))
    (ok (valid-instance-p req "2020-01-01T12:00:00+01:00"))
    (ng (valid-instance-p req "2020-01-01T12:00:00"))
    (ok (valid-instance-p pro "2020-01-01T12:00:00"))
    (ng (valid-instance-p pro "2020-01-01T12:00:00Z"))))

(deftest validate-xs-all
  (let ((schema "<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\" version=\"1.1\">
                   <xs:element name=\"pair\" type=\"pair\"/>
                   <xs:complexType name=\"pair\">
                     <xs:all>
                       <xs:element name=\"a\" type=\"xs:string\"/>
                       <xs:element name=\"b\" type=\"xs:integer\"/>
                     </xs:all>
                   </xs:complexType>
                 </xs:schema>"))
    (ok (valid-instance-p schema (%ht "b" 1 "a" "x")))
    (ng (valid-instance-p schema (%ht "a" "x")))))

(deftest emit-1.1-roundtrip-allow
  (defschema %x11-bag ()
    (name string)
    (:extra :allow))
  (let* ((xml (emit '%x11-bag :version :1.1))
         (class (compile-schema xml :name 'x11-bag)))
    (ok (eq :allow (schema-extra-policy class)))
    (ok (valid-instance-p xml (%ht "name" "Ada" "extra" t)))))
