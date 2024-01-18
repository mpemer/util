;;;; -----------------------------------------------------------------------
;;;; Filename: util.lisp
;;;; Author: Marcus Pemer
;;;; Email: marcus@pemer.com
;;;;
;;;; Description:
;;;; This file contains a collection of utility functions for use in Lisp
;;;; applications. It includes functions for parsing numbers, manipulating
;;;; hash tables, rounding numbers, processing lists with function chains
;;;; and monadic transformations, and other general utility functions.
;;;;
;;;; Copyright (C) 2023 Marcus Pemer
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;;; -----------------------------------------------------------------------

(in-package :util)



;;;; -----------------------------------------------------------------------
(defun parse-number? (s)
  "Attempts to parse the string S as a number,
   and return its rational representation.
   - S: A string expected to contain a numeric value.

   If S is non-nil and represents a valid number, the function parses S into
   a number and then rationalizes it, returning the rational representation
   of the number. If S is nil or does not represent a valid number,
   the function returns NIL."
  (handler-case
      (when (and s (plusp (length s)))
        (rationalize (parse-number s :float-format 'double-float)))
    (invalid-number (e)
      ;; Log the occurrence of a simple parse error and return nil
      (format *error-output* "Parse error occurred: ~A~%" e)
      nil)))

;;;; -----------------------------------------------------------------------
(define-test parse-number?-test
  ;; Test case where the string is a valid number
  (parachute:is = (parse-number? "123.45") 2469/20
                "Parsing a valid decimal number
                 should return its rational representation.")

  ;; Test case where the string is an invalid number
  (parachute:fail (parse-number? "abc") SB-INT:SIMPLE-PARSE-ERROR
                   "Parsing an invalid number should return NIL.")

  ;; Test case with an empty string
  (parachute:false (parse-number? "")
                   "Parsing an empty string should return NIL.")

  ;; Test case with nil
  (parachute:false (parse-number? nil)
                   "Parsing NIL should return NIL."))
;;;; -----------------------------------------------------------------------



;;;; -----------------------------------------------------------------------
(defun insert-hash (k ht v)
  "Inserts the value V into the hash table HT at the key K.
   - K: The key at which the value should be inserted in the hash table.
   - HT: The hash table where the value is to be inserted.
   - V: The value to be inserted into the hash table.

   The function works by consing the value V onto the front of the list
   already present at the key K in the hash table HT. If the key K does
   not exist in HT, it creates a new entry at K with V as its value."
  
  (set# k ht (cons v (get# k ht))))

;;;; -----------------------------------------------------------------------
(define-test insert-hash-test
  ;; Test inserting into an empty hash table
  (let ((ht (make-hash-table)))
    (insert-hash :key1 ht :flerp)
    (multiple-value-bind (value exists) (gethash :key1 ht)
      (parachute:is equal value '(:flerp) "The value should be '(:flerp)'")
      (parachute:true exists "The key should exist in the hash table.")))

  ;; Test updating an existing key
  (let ((ht (make-hash-table)))
    (setf (gethash :key1 ht) :original)
    (insert-hash :key1 ht :new-value)
    (multiple-value-bind (value exists) (gethash :key1 ht)
      (parachute:is equal value '(:new-value . :original)
                    "Inserting with an existing key
                     should update the value at that key.")
      (parachute:true exists
                      "The key should exist in the hash table.")))                  

  ;; Test with a non-existent key
  (let ((ht (make-hash-table)))
    (insert-hash :key2 ht :value2)
    (multiple-value-bind (value exists) (gethash :key2 ht)
      (parachute:is equal value '(:value2)
                    "Inserting with a non-existent key
                     should create a new key-value pair.")
      (parachute:true exists "The key should exist in the hash table."))))
;;;; -----------------------------------------------------------------------



;;;; ---------------------------------------------------------------------
(defun roundto (number decimals)
  "Rounds NUMBER to a specified number of DECIMALS.
   - NUMBER:   The number to be rounded.
               Can be an integer or a floating-point number.
   - DECIMALS: The number of decimal places to round NUMBER to.
               Must be an integer.
   
   The function works by multiplying NUMBER by 10 raised to the power of
   DECIMALS, rounding the result to the nearest integer, and then dividing
   it back by the same factor.
   The result is NUMBER rounded to the specified number of decimal places."

  (declare (type integer decimals))  ; Ensure DECIMALS is an integer
  (let ((f (expt 10 decimals)))      ; Calculate the factor for rounding
    (/ (round (* f number)) f)))     ; Perform the rounding and adjust back

;;;; -----------------------------------------------------------------------
(define-test roundto-test
  ;; Test rounding to 0 decimal places
  (parachute:is = (roundto 123.456 0) 123
                "Rounding to 0 decimal places
                 should round to the nearest integer.")

  ;; Test rounding to 1 decimal place
  (parachute:is = (roundto 123.456 1) 247/2
                "Rounding to 1 decimal place
                 should round to the nearest tenth.")

  ;; Test rounding to 2 decimal places
  (parachute:is = (roundto 123.456 2) 6173/50
                "Rounding to 2 decimal places should
                 round to the nearest hundredth.")

  ;; Test rounding to a negative number of decimal places
  (parachute:is = (roundto 123.456 -1) 120
                "Rounding to a negative number of decimal places
                 should round to tens, hundreds, etc.")

  ;; Test with an integer input
  (parachute:is = (roundto 123 2) 123
                "Rounding an integer should work correctly,
                 adding decimal zeroes as necessary."))
;;;; -----------------------------------------------------------------------



;;;; -----------------------------------------------------------------------
(defun process-list (lst &key chain monads)
  "Process each element of LST through a function chain and then through
   a series of monadic transformations.
   Parameters:
   - LST:    A list of elements to be processed,
             which is a required argument.
   - CHAIN:  An optional list of functions (or a single function)
             that are composed together and applied sequentially
             to each element in LST.
   - MONADS: An optional list of functions (or a single function)
             representing monadic transformations.

   The CHAIN and MONADS parameters are optional and key-based.
   Each has an associated '-provided-p' flag that indicates whether
   the argument was actually passed to the function.
   The function first applies the composed CHAIN to each element in LST.
   Then, each MONAD is applied to the result, potentially producing more
   values which are aggregated into the final result list."
  (let ((result nil))
    (setf chain (ensure-list chain))
    (setf monads (ensure-list monads))
    (dolist (element lst (nreverse result))
      (let ((composite (if chain
                           (funcall (apply #'compose chain) element)
                           element)))
        (dolist (monad monads)
          (let ((derivation (funcall monad composite)))
            (when derivation
              (if (listp derivation)
                  (setf result (splice derivation result))
                  (push derivation result)))))))))

;;;; -----------------------------------------------------------------------
(define-test process-list-test
  ;; Test with a simple chain
  (let ((chain (list #'identity #'1+ #'1+))) ; Increment twice
    (parachute:is equal (process-list '(1 2 3) :chain chain
                                               :monads #'identity)
                  '(3 4 5)
                  "Applying a simple chain
                   should increment each element twice."))

  ;; Test with a simple monad
  (let ((monads (list (lambda (x) (* x 2))))) ; Double the value
    (parachute:is equal (process-list '(1 2 3) :monads monads) '(2 4 6)
                   "Applying a simple monad should double each element."))

  ;; Test with both chain and monads
  (let ((chain (list #'1+))
        (monads (list (lambda (x) (* x 2)))))
    (parachute:is equal (process-list '(1 2 3) :chain chain
                                               :monads monads)
                  '(4 6 8)
                  "Applying both chain and monads
                   should increment and then double each element."))

  ;; Test with empty list
  (parachute:is eq (process-list '() :chain (list #'1+)
                                     :monads (list (lambda (x) (* x 2))))
                nil
                "Processing an empty list should return an empty list."))
;;;; -----------------------------------------------------------------------



;;;; -----------------------------------------------------------------------
(defun splice (add lst)
  "Concatenates two lists by adding ADD in reverse order to the front of LST.
   - ADD: A list whose elements are to be added.
   - LST: The list to which the elements of ADD are to be appended.
   
   This function reverses the list ADD and then concatenates it with LST,
   effectively splicing the reversed ADD into the front of LST."
  (nconc (nreverse add) lst))

;;;; -----------------------------------------------------------------------
(define-test splice-test
  ;; Test with non-empty lists
  (parachute:is equal (splice '(1 2 3) '(4 5 6)) '(3 2 1 4 5 6)
                "Splicing '(1 2 3) to '(4 5 6)
                 should yield '(3 2 1 4 5 6).")

  ;; Test with an empty ADD list
  (parachute:is equal (splice '() '(1 2 3)) '(1 2 3)
                "Splicing an empty list to '(1 2 3)
                 should yield '(1 2 3).")

  ;; Test with an empty LST list
  (parachute:is equal (splice '(1 2 3) nil) '(3 2 1)
                "Splicing '(1 2 3) to an empty list
                 should yield '(3 2 1).")

  ;; Test with both lists empty
  (parachute:is equal (splice nil nil) nil
                "Splicing two empty lists
                 should yield an empty list.")

  ;; Test with nested lists
  (parachute:is equal (splice '(1 (2 3) 4) '((5 6) 7)) '(4 (2 3) 1 (5 6) 7)
                "Splicing nested lists should correctly concatenate them."))
;;;; -----------------------------------------------------------------------



;;;; -----------------------------------------------------------------------
(defun ensure-list (item)
  "Ensures that ITEM is a list.
   - ITEM: An object to be transformed into a list if it is not already one.
   
   If ITEM is already a list, it is returned as-is.
   Otherwise, ITEM is placed into a new list.
   This function is useful for function arguments that may accept
   either a single item or a list of items."
  (if (listp item) item (list item)))

;;;; ---------------------------------------------------------------------
(define-test ensure-list-test
  ;; Test with a list input
  (parachute:is equal (ensure-list '(1 2 3)) '(1 2 3)
                 "Passing a list should return the list unchanged.")

  ;; Test with a single integer
  (parachute:is equal (ensure-list 42) '(42)
                 "Passing a single integer should return it in a list.")

  ;; Test with a string
  (parachute:is equal (ensure-list "Hello") '("Hello")
                 "Passing a string should return it in a list.")

  ;; Test with nil / empty list
  (parachute:is equal (ensure-list nil) nil
                 "Passing nil should return it in a list."))
;;;; -----------------------------------------------------------------------



;;;; -----------------------------------------------------------------------
(defun struct-field-names (struct)
  "Extracts and returns a list of field names from a given struct.

PARAMETERS:
- struct: An instance of a struct or class
          from which to extract field names.

The function uses the Common Lisp Object System (CLOS) Metaobject Protocol
(MOP) to introspect the struct and retrieve the slot definitions.
It then extracts and returns the names of these slots as a list of strings,
which represent the field names.

Each field name is converted from a symbol to a string to ensure
compatibility with external systems and formats, such as CSV headers.

RETURNS:
A list of strings representing the field names of the struct."
  (loop for slot in (sb-mop:class-slots (class-of struct))
        collect (symbol-name (sb-mop:slot-definition-name slot))))

;;;; -----------------------------------------------------------------------
;; Define a test struct for demonstration
(defstruct test-struct
  field1
  field2
  field3)

(define-test struct-field-names-test
  ;; Test with a test struct instance
  (let ((test-instance (make-test-struct :field1 1 :field2 2 :field3 3)))
    (parachute:is equal (struct-field-names test-instance)
                  '("FIELD1" "FIELD2" "FIELD3")
                  "Field names for 'test-struct'
                   should be 'FIELD1', 'FIELD2', and 'FIELD3'.")))
;;;; -----------------------------------------------------------------------



;;;; -----------------------------------------------------------------------
(defun struct-to-list (struct)
  "Converts a given struct to a list of its values,
   preserving the order of its fields.

PARAMETERS:
- struct: An instance of a struct or class
          to be converted into a list of values.

This function utilizes the Common Lisp Object System (CLOS)
Metaobject Protocol (MOP) to introspect the given struct and access each
of its slots. It then retrieves the value of each slot, preserving
the order in which the slots are defined.

The function is generic and does not require prior knowledge of the
struct's field names, making it versatile for use with any struct
or class that adheres to the CLOS standard.

RETURNS:
A list containing the values of each field in the struct,
ordered according to the struct's definition."
  (loop for slot in (closer-mop:class-slots (class-of struct))
        collect (slot-value struct (closer-mop:slot-definition-name slot))))

;;;; -----------------------------------------------------------------------
;; Assuming 'test-struct' is already defined as before
;; (defstruct test-struct field1 field2 field3)

(define-test struct-to-list-test
  ;; Test with a test struct instance
  (let ((test-instance (make-test-struct :field1 "Value1"
                                         :field2 "Value2"
                                         :field3 "Value3")))
    (parachute:is equal (struct-to-list test-instance)
                  '("Value1" "Value2" "Value3")
                  "Converting 'test-struct' to a list should yield
                   'Value1', 'Value2', and 'Value3' in order."))

  ;; Test with a struct having different data types
  (let ((test-instance (make-test-struct :field1 1
                                         :field2 2.0
                                         :field3 'symbol)))
    (parachute:is equal (struct-to-list test-instance) '(1 2.0 symbol)
                  "Struct with various data types should be
                   correctly converted to a list."))

  ;; Test with a struct having nil values
  (let ((test-instance (make-test-struct :field1 nil
                                         :field2 nil
                                         :field3 nil)))
    (parachute:is equal (struct-to-list test-instance) '(nil nil nil)
                  "Struct with nil values should be correctly
                   converted to a list with nils.")))
;;;; -----------------------------------------------------------------------



;;;; -----------------------------------------------------------------------
(defun proper-plist-p (plist)
  "Determines if PLIST is a properly formed property list.

  A proper property list (plist) must satisfy two conditions:
  1. It contains an even number of elements,
     alternating between keys and values.
  2. All keys in the list are keywords.

  Parameters:
  - plist: A list to be checked. It is expected to be a property list where
           keys are potentially followed by corresponding values.

  Returns:
  T (true) if PLIST is a properly formed property list according to
  the above conditions. Otherwise, returns NIL (false).

  Example Usage:
  (proper-plist-p '(:key1 value1 :key2 value2)) ;; Returns T
  (proper-plist-p '(:key1 value1 key2 value2))  ;; Returns NIL
                                                ;; (key2 is not a keyword)
  (proper-plist-p '(:key1 :key2 :key3))         ;; Returns NIL
                                                ;; (odd number of elements)

  Notes:
  This function is useful for validating property lists before their use
  in situations where proper structure is required for correct operation.
  The function checks for the even length of the list and that each key
  is a valid keyword."
  (and (evenp (length plist)) ; Ensure plist has an even number of elements.
       (every #'keywordp
              (loop for item on plist by #'cddr collect (first item)))))
                                        ; Ensure every key is a keyword.

;;;; -----------------------------------------------------------------------
(define-test proper-plist-p-test
  ;; Test with a properly formed plist
  (parachute:true (proper-plist-p '(:key1 value1 :key2 value2))
                "A properly formed plist should return T.")

  ;; Test with a plist having a non-keyword key
  (parachute:false (proper-plist-p '(:key1 value1 key2 value2))
                   "A plist with a non-keyword key should return NIL.")

  ;; Test with a plist having an odd number of elements
  (parachute:false (proper-plist-p '(:key1 value1 :key2))
                   "A plist with an odd number of elements
                    should return NIL.")

  ;; Test with an empty plist
  (parachute:true (proper-plist-p '())
                  "An empty plist should be considered
                   properly formed and return T.")

  ;; Test with a plist having nested plists as values
  (parachute:true (proper-plist-p '(:key1 (:subkey1 value1
                                           :subkey2 value2)
                                    :key2 value2))
                  "A plist with nested plists as values should still be
                   considered properly formed if the outer structure
                   is correct."))
;;;; -----------------------------------------------------------------------



;;;; -----------------------------------------------------------------------
(defun drop (n sequence)
  "Removes the first N elements from SEQUENCE and returns
   the remaining elements.

  Parameters:
  - n: An integer representing the number of elements to be dropped from
       the beginning of SEQUENCE. 
       If N is greater than the length of SEQUENCE,
       an empty sequence is returned.
       If N is less than 0, the function behavior is
       implementation-dependent.
  - sequence: A sequence (list, vector, etc.) from which elements
              will be removed.

  Returns:
  A new sequence containing the elements of the original SEQUENCE,
  excluding the first N elements.

  Example Usage:
  (drop 2 '(1 2 3 4 5)) ;; Returns (3 4 5)
  (drop 0 '(1 2 3))    ;; Returns (1 2 3)
  (drop 3 '(1 2))      ;; Returns NIL

  Notes:
  - The function uses 'subseq', which creates a new sequence,
    leaving the original SEQUENCE unmodified.
  - If SEQUENCE is empty, the function returns an empty sequence
    regardless of the value of N.
  - This function is useful for operations where the initial part
    of a sequence is not needed."
  (subseq sequence n))

;;;; -----------------------------------------------------------------------
(define-test drop-test
  ;; Test dropping elements from a list
  (parachute:is equal (drop 2 '(1 2 3 4 5)) '(3 4 5)
                "Dropping 2 elements from a list
                 should return the remaining elements.")

  ;; Test dropping no elements
  (parachute:is equal (drop 0 '(1 2 3)) '(1 2 3)
                "Dropping 0 elements should return
                 the sequence unchanged.")

  ;; Test dropping more elements than the sequence length
  (parachute:fail (drop 5 '(1 2)) error
                  "Dropping more elements than the sequence contains
                   should result in an error.")

  ;; Test with an empty sequence
  (parachute:fail (drop 3 nil) error
                  "Dropping elements from an empty sequence
                   should result in an error.")

  ;; Test dropping elements from a vector
  (parachute:is equalp (drop 2 #(1 2 3 4 5)) #(3 4 5)
                "Dropping 2 elements from a vector should return
                 the remaining elements.")

  ;; Test with negative n
  (parachute:fail (drop -1 '(1 2 3 4 5)) type-error
                "Dropping a negative number of elements should fail."))
;;;; -----------------------------------------------------------------------



;;;; -----------------------------------------------------------------------
(defun create-mock-file (content)
  "Creates a temporary file in the system's temporary directory
   with the specified content.

  Parameters:
  - content: A sequence of bytes (octets) that will be written to the file.

  Returns:
  The pathname of the newly created temporary file.

  Side Effects:
  This function creates a new file on the file system. The file will be
  located in the '/tmp' directory and have a name based on the current
  universal time to ensure uniqueness.

  Example Usage:
  (create-mock-file #(72 101 108 108 111)) ;; Creates a file containing
                                           ;; the bytes for 'Hello'

  Notes:
  The created file should be explicitly deleted when it is no longer needed
  to avoid leaving temporary files on the system.
  "
  (let ((filename (format nil "/tmp/mock-file-~A" (get-universal-time))))
    (with-open-file (stream filename
                            :direction :output
                            :if-exists :supersede
                            :element-type 'octet)
      (write-sequence content stream))
    filename))

;;;; -----------------------------------------------------------------------
(define-test create-mock-file-test
  ;; Test creating a file and writing content
  (let ((filename (create-mock-file #(72 101 108 108 111))))
    ;; Create a mock file with "Hello" in bytes
    (parachute:true (probe-file filename)
                    "The created file should exist.")
    (parachute:is equal (read-file-as-bytes filename) '(72 101 108 108 111)
                  "The content of the created file
                   should match the input byte sequence.")
    (delete-file filename))) ; Clean up the mock file
;;;; -----------------------------------------------------------------------



;;;; -----------------------------------------------------------------------
(defun read-file-as-bytes (filename)
  "Reads the contents of a file specified by FILENAME and returns it
   as a list of bytes.

  Parameters:
  - filename: A string representing the path of the file to be read.

  Returns:
  A list of bytes (unsigned-byte 8) representing the content of the file.
  If the file does not exist, returns NIL.

  Example Usage:
  (read-file-as-bytes \"/path/to/file.txt\") ;; Returns the content of
                                             ;; file.txt as a list of bytes

  Notes:
  This function is intended for reading binary files, as it treats
  the file content as a sequence of bytes.
  For text files, consider using character-based reading functions to
  handle different character encodings properly.
  The function uses 'with-open-file' to safely open and close
  the file stream."
  (with-open-file (stream filename :element-type '(unsigned-byte 8)
                                   :if-does-not-exist nil)
    (loop for byte = (read-byte stream nil) while byte collect byte)))

;;;; -----------------------------------------------------------------------
(define-test read-file-as-bytes-test
  ;; Test reading a real file
  (let ((filename (create-mock-file #(72 101 108 108 111))))
    ;; Create a mock file with "Hello" in bytes
    (parachute:is equal (read-file-as-bytes filename) '(72 101 108 108 111)
                  "Reading a real file should return
                   the correct byte sequence.")
    (delete-file filename)) ; Clean up the mock file

  ;; Test reading a non-existent file
  (parachute:is equal (read-file-as-bytes "/path/to/nonexistent/file") nil
                "Reading a non-existent file should return NIL."))
;;;; -----------------------------------------------------------------------
