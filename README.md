# schema-protocol-xsd

XSD 1.0 **parse / generate / validate** for [`schema-protocol`](https://github.com/egao1980/schema-protocol) — the **xsd** format implementor.

| System | Role | OCI |
|--------|------|-----|
| `schema-protocol-xsd` (`stack-schema-xsd`) | XSD 1.0 emit + load + compile → CLOS schema-class | **0.1.0** |

`schema-protocol` owns models / validate / dump. This package owns **XSD documents**.

```lisp
(asdf:load-system "schema-protocol-xsd")

(stack-schema-xsd:emit 'user)
(stack-schema:xsd-schema 'user)   ; same, after this system is loaded

(let ((class (stack-schema-xsd:compile-schema
              "<?xml version=\"1.0\"?>
               <xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\"
                          elementFormDefault=\"qualified\">
                 <xs:element name=\"person\" type=\"person\"/>
                 <xs:complexType name=\"person\">
                   <xs:sequence>
                     <xs:element name=\"name\" type=\"xs:string\"/>
                   </xs:sequence>
                 </xs:complexType>
               </xs:schema>")))
  (stack-schema:parse class '(:name "Ada")))
```

## Prior art

No Common Lisp library validates **XSD documents**. What exists:

| Source | Take | Leave |
|--------|------|-------|
| **schema-protocol-json** | emit / parse-document / compile-schema / validate-instance shape | JSON Schema keywords |
| **cxml-rng** | — | Relax NG. “XSD type library” = datatypes inside RNG, not `xs:schema` |
| **cl-libxml2** | — | libxml2 FFI + native overlay (Windows-primary stack stays Lisp) |
| **Trang / Xerces** | checklist (named types, facets, nillable) | JVM / codegen |
| **W3C XSD 1.0** | `xs:element` / `complexType` / `simpleType` / `sequence` / `choice` / facets | identity constraints, `xs:import`, 1.1 assertions |

Wave-1 is a **closed subset** that round-trips `defschema`:

- objects → `xs:complexType` + `xs:sequence` of elements
- nested schemas → named types (no `xs:import` / `xs:include`)
- optional → `minOccurs="0"`; `:null` unions → `nillable`
- vectors → `maxOccurs="unbounded"`
- enums / `member` / `eql` → `xs:enumeration`
- tagged unions → `xs:choice` + `xs:annotation/appinfo` discriminator (compile rebuilds `:tag`)
- `:extra :allow` → `xs:any processContents="lax"`
- instance validate accepts hash-tables / plists **or** XML strings

**Not in wave-1:** attributes, `xs:extension`, substitution groups, keys/keyref, remote schemas, XSD 1.1.

XML I/O is a small sexp reader/writer in this package (declaration, elements, attrs, comments, predefined entities, CDATA). Not a general XML stack.

## License

MIT — see [LICENSE](LICENSE).
