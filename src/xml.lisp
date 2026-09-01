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

(defun elem-to-value (elem)
  "XML element → hash-table / string / :null / vector of repeating children."
  (when (or (string-equal (xml-attr elem "nil") "true")
            (string-equal (xml-attr elem "xsi:nil") "true"))
    (return-from elem-to-value :null))
  (let ((elems (remove-if-not #'xml-element-p (xml-element-children elem))))
    (if (null elems)
        (xml-element-text elem)
        (let ((out (make-hash-table :test #'equal))
              (seen (make-hash-table :test #'equal)))
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
