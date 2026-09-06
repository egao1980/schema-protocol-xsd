(in-package #:schema-protocol-xsd)

(defvar +xsd-ns+ "http://www.w3.org/2001/XMLSchema")
(defvar +xsd-vc-ns+ "http://www.w3.org/2007/XMLSchema-versioning")

(defun local-name (name)
  (xml-local-name name))

(defun whitespace-char-p (c)
  (or (char= c #\Space) (char= c #\Tab) (char= c #\Newline) (char= c #\Return)))

(defun stringify-key (key)
  (etypecase key
    (string key)
    (symbol (string-downcase (symbol-name key)))
    (character (string key))))

(defun attribute-key (name)
  (concatenate 'string "@" (stringify-key name)))

(defun attribute-key-p (key)
  (and (stringp key) (plusp (length key)) (char= (char key 0) #\@)))

(defun bare-attribute-name (key)
  (if (attribute-key-p key)
      (subseq key 1)
      (stringify-key key)))

(defun array-p (value)
  (and (vectorp value) (not (stringp value))))

(defun as-number (value)
  (cond
    ((realp value) value)
    ((stringp value)
     (let ((*read-eval* nil))
       (multiple-value-bind (n pos)
           (ignore-errors (read-from-string value))
         (and (realp n) (numberp pos) (= pos (length value)) n))))
    (t nil)))

(defun as-string (value)
  (cond
    ((stringp value) value)
    ((keywordp value) (string-downcase (symbol-name value)))
    ((symbolp value) (string-downcase (symbol-name value)))
    ((numberp value) (princ-to-string value))
    (t nil)))

(defun skip-instance-attribute-p (attr)
  (let ((q (xml-qname attr))
        (local (xml-attribute-local-name attr))
        (prefix (xml-attribute-prefix attr)))
    (or (string= q "xmlns")
        (and prefix (string-equal prefix "xmlns"))
        (and (>= (length q) 6) (string= q "xmlns:" :end1 6))
        (string-equal q "xsi:nil")
        (and (string-equal local "nil")
             (or (null prefix) (string-equal prefix "xsi"))))))

(defun instance-attributes (elem)
  (loop for a in (xml-element-attributes elem)
        unless (skip-instance-attribute-p a)
          collect (cons (attribute-key (xml-attribute-local-name a))
                        (xml-attribute-value a))))

(defun elem-to-value (elem)
  "XML element → hash-table / string / :null / vector of repeating children.
   Attributes become @-prefixed keys."
  (when (or (string-equal (xml-attr elem "nil") "true")
            (string-equal (xml-attr elem "xsi:nil") "true"))
    (return-from elem-to-value :null))
  (let ((elems (remove-if-not #'xml-element-p (xml-element-children elem)))
        (attrs (instance-attributes elem)))
    (if (and (null elems) (null attrs))
        (xml-element-text elem)
        (let ((out (make-hash-table :test #'equal))
              (seen (make-hash-table :test #'equal)))
          (dolist (pair attrs)
            (setf (gethash (car pair) out) (cdr pair)))
          (dolist (c elems)
            (incf (gethash (xml-local-name c) seen 0)))
          (dolist (c elems)
            (let* ((k (xml-local-name c))
                   (v (elem-to-value c)))
              (if (> (gethash k seen) 1)
                  (let ((acc (gethash k out)))
                    (setf (gethash k out)
                          (if acc
                              (concatenate 'vector acc (vector v))
                              (vector v))))
                  (setf (gethash k out) v))))
          out))))
