;;;; -----------------------------------------------------------------------
;;;; Filename: util.test.lisp
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

(defpackage :util/test/main
  (:use :common-lisp :util/core/main)
  (:import-from :parachute :define-test :is :fail))

(in-package :util/test/main)


(define-test parse-number?-test
  ;; Test case where the string is a valid number
  (is = 2469/20 (parse-number? "123.45")
      #.#~ "Parsing a valid decimal number ~
      should return its rational representation.")

  ;; Test case where the string is an invalid number
  (is eq nil (parse-number? "abc")
      "Parsing an invalid number should return NIL.")
  
  ;; Test case with an empty string
  (parachute:false (parse-number? "")
                   "Parsing an empty string should return NIL.")

  ;; Test case with nil
  (parachute:false (parse-number? nil)
                   "Parsing NIL should return NIL."))




(define-test insert-hash-test
  ;; Test inserting into an empty hash table
  (let ((ht (make-hash-table)))
    (insert-hash :key1 ht :flerp)
    (multiple-value-bind (value exists) (gethash :key1 ht)
      (is equal '(:flerp) value "The value should be '(:flerp)'")
      (parachute:true exists "The key should exist in the hash table.")))

  ;; Test updating an existing key
  (let ((ht (make-hash-table)))
    (setf (gethash :key1 ht) :original)
    (insert-hash :key1 ht :new-value)
    (multiple-value-bind (value exists) (gethash :key1 ht)
      (is equal '(:new-value . :original) value
          #.#~ "Inserting with an existing key ~
          should update the value at that key.")
      (parachute:true exists
                      "The key should exist in the hash table.")))                  

  ;; Test with a non-existent key
  (let ((ht (make-hash-table)))
    (insert-hash :key2 ht :value2)
    (multiple-value-bind (value exists) (gethash :key2 ht)
      (is equal '(:value2) value
          #.#~ "Inserting with a non-existent key ~
                    should create a new key-value pair.")
      (parachute:true exists "The key should exist in the hash table."))))



(define-test roundto-test
  ;; Test rounding to 0 decimal places
  (is = 123 (roundto 123.456 0)
      #.#~ "Rounding to 0 decimal places ~
                should round to the nearest integer.")

  ;; Test rounding to 1 decimal place
  (is = 247/2 (roundto 123.456 1)
      #.#~ "Rounding to 1 decimal place ~
                should round to the nearest tenth.")

  ;; Test rounding to 2 decimal places
  (is = 6173/50 (roundto 123.456 2)
      #.#~ "Rounding to 2 decimal places should ~
                round to the nearest hundredth.")

  ;; Test rounding to a negative number of decimal places
  (is = 120 (roundto 123.456 -1)
      #.#~ "Rounding to a negative number of decimal places ~
                should round to tens, hundreds, etc.")

  ;; Test with an integer input
  (is = 123 (roundto 123 2)
      #.#~ "Rounding an integer should work correctly, ~
                adding decimal zeroes as necessary."))




(define-test process-list-test
  ;; Test with a simple chain
  (let ((chain (list #'identity #'1+ #'1+))) ; Increment twice
    (is equal '(3 4 5) (process-list '(1 2 3) :chain chain
                                              :monads #'identity)
        #.#~ "Applying a simple chain ~
                  should increment each element twice."))

  ;; Test with a simple monad
  (let ((monads (list (lambda (x) (* x 2))))) ; Double the value
    (is equal '(2 4 6) (process-list '(1 2 3) :monads monads)
        "Applying a simple monad should double each element."))

  ;; Test with both chain and monads
  (let ((chain (list #'1+))
        (monads (list (lambda (x) (* x 2)))))
    (is equal '(4 6 8) (process-list '(1 2 3) :chain chain
                                              :monads monads)
        #.#~ "Applying both chain and monads ~
                  should increment and then double each element."))

  ;; Test with empty list
  (is eq nil (process-list '() :chain (list #'1+)
                               :monads (list (lambda (x) (* x 2))))
      "Processing an empty list should return an empty list."))




(define-test splice-test
  ;; Test with non-empty lists
  (is equal '(3 2 1 4 5 6) (splice '(1 2 3) '(4 5 6))
      #.#~ "Splicing '(1 2 3) to '(4 5 6) ~
                should yield '(3 2 1 4 5 6).")

  ;; Test with an empty ADD list
  (is equal '(1 2 3) (splice '() '(1 2 3))
      #.#~ "Splicing an empty list to '(1 2 3) ~
                should yield '(1 2 3).")

  ;; Test with an empty LST list
  ;;  (is equal '(3 2 1) (splice '(1 2 3) nil)
  ;;        #.#~ "Splicing '(1 2 3) to an empty list ~
  ;;                should yield '(3 2 1).")

  ;; Test with both lists empty
  (is equal nil (splice nil nil)
      #.#~ "Splicing two empty lists ~
                should yield an empty list.")

  ;; Test with nested lists
  (is equal '(4 (2 3) 1 (5 6) 7) (splice '(1 (2 3) 4) '((5 6) 7))
      "Splicing nested lists should correctly concatenate them."))




(define-test ensure-list-test
  ;; Test with a list input
  (is equal '(1 2 3) (ensure-list '(1 2 3))
      "Passing a list should return the list unchanged.")

  ;; Test with a single integer
  (is equal '(42) (ensure-list 42)
      "Passing a single integer should return it in a list.")

  ;; Test with a string
  (is equal '("Hello") (ensure-list "Hello")
      "Passing a string should return it in a list.")

  ;; Test with nil / empty list
  (is equal nil (ensure-list nil)
      "Passing nil should return it in a list."))




;; Define a test struct for demonstration
(defstruct test-struct
  field1
  field2
  field3)

(define-test struct-field-names-test
  ;; Test with a test struct instance
  (let ((test-instance (make-test-struct :field1 1 :field2 2 :field3 3)))
    (is equal '("FIELD1" "FIELD2" "FIELD3")
        (struct-field-names test-instance)
        #.#~ "Field names for 'test-struct' ~
                  should be 'FIELD1', 'FIELD2', and 'FIELD3'.")))


;; Assuming 'test-struct' is already defined as before
;; (defstruct test-struct field1 field2 field3)

(define-test struct-to-list-test
  ;; Test with a test struct instance
  (let ((test-instance (make-test-struct :field1 "Value1"
                                         :field2 "Value2"
                                         :field3 "Value3")))
    (is equal '("Value1" "Value2" "Value3")
        (struct-to-list test-instance)
        #.#~ "Converting 'test-struct' to a list should yield ~
                  Value1', 'Value2', and 'Value3' in order."))

  ;; Test with a struct having different data types
  (let ((test-instance (make-test-struct :field1 1
                                         :field2 2.0
                                         :field3 'symbol)))
    (is equal '(1 2.0 symbol) (struct-to-list test-instance)
        #.#~ "Struct with various data types should be ~
                  correctly converted to a list."))

  ;; Test with a struct having nil values
  (let ((test-instance (make-test-struct :field1 nil
                                         :field2 nil
                                         :field3 nil)))
    (is equal '(nil nil nil) (struct-to-list test-instance)
        #.#~ "Struct with nil values should be correctly ~
                  converted to a list with nils.")))




(define-test proper-plist-p-test
  ;; Test with a properly formed plist
  (parachute:true (proper-plist-p '(:key1 value1 :key2 value2))
                  "A properly formed plist should return T.")

  ;; Test with a plist having a non-keyword key
  (parachute:false (proper-plist-p '(:key1 value1 key2 value2))
                   "A plist with a non-keyword key should return NIL.")

  ;; Test with a plist having an odd number of elements
  (parachute:false (proper-plist-p '(:key1 value1 :key2))
                   #.#~ "A plist with an odd number of elements ~
                   should return NIL.")

  ;; Test with an empty plist
  (parachute:true (proper-plist-p '())
                  #.#~ "An empty plist should be considered ~
                  properly formed and return T.")

  ;; Test with a plist having nested plists as values
  (parachute:true (proper-plist-p '(:key1 (:subkey1 value1
                                           :subkey2 value2)
                                    :key2 value2))
                  #.#~ "A plist with nested plists as values ~
                  should still be considered properly formed ~
                  if the outer structure is correct."))


(define-test drop-test
  ;; Test dropping elements from a list
  (is equal '(3 4 5) (drop 2 '(1 2 3 4 5))
      #.#~ "Dropping 2 elements from a list ~
                should return the remaining elements.")

  ;; Test dropping no elements
  (is equal '(1 2 3) (drop 0 '(1 2 3))
      #.#~ "Dropping 0 elements should return ~
                the sequence unchanged.")

  ;; Test dropping more elements than the sequence length
  (fail (drop 5 '(1 2)) error
        #.#~ "Dropping more elements than the sequence contains ~
                  should result in an error.")

  ;; Test with an empty sequence
  (fail (drop 3 nil) error
        #.#~ "Dropping elements from an empty sequence ~
                  should result in an error.")

  ;; Test dropping elements from a vector
  (is equalp #(3 4 5) (drop 2 #(1 2 3 4 5))
      #.#~ "Dropping 2 elements from a vector should return ~
                the remaining elements.")

  ;; Test with negative n
  (fail (drop -1 '(1 2 3 4 5)) type-error
        "Dropping a negative number of elements should fail."))



(when nil
  (define-test create-mock-file-test
    ;; Test creating a file and writing content
    (let ((filename (create-mock-file #(72 101 108 108 111))))
      ;; Create a mock file with "Hello" in bytes
      (parachute:true (probe-file filename)
                      "The created file should exist.")
      (is equal '(72 101 108 108 111) (read-file-as-bytes filename)
          #.#~ "The content of the created file ~
                  should match the input byte sequence.")
      (delete-file filename))) ; Clean up the mock file
  )


(when nil
  (define-test read-file-as-bytes-test
    ;; Test reading a real file
    (let ((filename (create-mock-file #(72 101 108 108 111))))
      ;; Create a mock file with "Hello" in bytes
      (is equal '(72 101 108 108 111) (read-file-as-bytes filename)
          #.#~ "Reading a real file should return ~
                  the correct byte sequence.")
      (delete-file filename)) ; Clean up the mock file

    ;; Test reading a non-existent file
    (is equal nil (read-file-as-bytes "/path/to/nonexistent/file")
        "Reading a non-existent file should return NIL."))

  )
