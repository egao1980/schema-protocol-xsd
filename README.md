# schema-protocol-xsd

XSD 1.0 / **1.1** **parse / generate / validate** for [`schema-protocol`](https://github.com/egao1980/schema-protocol) — the **xsd** format implementor.

| System | Role | OCI |
|--------|------|-----|
| `schema-protocol-xsd` (`stack-schema-xsd`) | XSD emit + load + compile → CLOS schema-class | **0.1.1** |

`schema-protocol` owns models / validate / dump. This package owns **XSD documents**.

```lisp
(asdf:load-system "schema-protocol-xsd")

(stack-schema-xsd:emit 'user)                 ; XSD 1.0
(stack-schema-xsd:emit 'user :version :1.1)   ; alternatives + openContent + vc
(stack-schema:xsd-schema 'user)               ; same, after this system is loaded

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
| **W3C XSD 1.0** | `xs:element` / `complexType` / `simpleType` / `sequence` / `choice` / facets | identity constraints, `xs:import` |
| **W3C XSD 1.1** | `xs:assert` / `xs:assertion`, `xs:alternative`, `xs:openContent`, `explicitTimezone`, `xs:all` | full XPath 2.0, `xs:override`, inheritable attrs |

Wave-1 is a **closed subset** that round-trips `defschema`:

- objects → `xs:complexType` + `xs:sequence` of elements
- nested schemas → named types (no `xs:import` / `xs:include`)
- optional → `minOccurs="0"`; `:null` unions → `nillable`
- vectors → `maxOccurs="unbounded"`
- enums / `member` / `eql` → `xs:enumeration`
- tagged unions → 1.0: `xs:choice` + `appinfo` discriminator; **1.1:** `xs:alternative` + `xs:error`
- `:extra :allow` → 1.0: `xs:any`; **1.1:** `xs:openContent mode="interleave"`
- instance validate accepts hash-tables / plists **or** XML strings

**XSD 1.1** (`:version :1.1` on emit; parse detects `@version` / `vc:minVersion`):

- `xs:assert` / `xs:assertion` — closed XPath 2.0 (`$value`, child/`@` names, `count`/`string-length`/`exists`/`not`, `and`/`or`, `eq`/`ne`/`lt`/`le`/`gt`/`ge`/`mod`)
- `xs:alternative` on elements (first matching `test`)
- `xs:openContent` / `xs:defaultOpenContent` (`mode="none"` closes)
- `xs:explicitTimezone` (`required` / `optional` / `prohibited`)
- `xs:all` (hash-table order already free)
- `xs:error` always fails

**Not in wave-1:** attributes, `xs:extension`, substitution groups, keys/keyref, remote schemas, full XPath, `xs:override`.

XML I/O is a small sexp reader/writer in this package (declaration, elements, attrs, comments, predefined entities, CDATA). Not a general XML stack.

## License

MIT — see [LICENSE](LICENSE).
