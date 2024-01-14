;;;; util.lisp
;;
;; Author: Marcus Pemer
;; Created: 2023-12-16
;;
;; Description:
;; This file contains a collection of utility functions for use in Lisp applications.
;; It includes functions for parsing numbers, manipulating hash tables, rounding numbers,
;; processing lists with function chains and monadic transformations, and other general utility functions.
;;
;; Contents:
;; - parse-number?: Parses a string into a rational number.
;; - insert-hash: Inserts a value into a hash table at a specified key.
;; - roundto: Rounds a number to a specified number of decimal places.
;; - process-list: Processes a list through a function chain and monadic transformations.
;; - splice: Concatenates two lists.
;; - ensure-list: Ensures that an object is a list.
;;
;; Usage:
;; These utility functions are designed to be generic and reusable across various Lisp applications.
;; Each function is documented with a docstring describing its purpose and usage.

(in-package :util)

(defun parse-number? (s)
  "Attempts to parse the string S as a number and return its rational representation.
   - S: A string expected to contain a numeric value.

   If S is non-nil and represents a valid number, the function parses S into a number and then rationalizes it, returning the rational representation of the number. If S is nil or does not represent a valid number, the function returns NIL."
  (when s (rationalize (parse-number s :float-format 'double-float))))


(defun insert-hash (k ht v)
  "Inserts the value V into the hash table HT at the key K.
   - K: The key at which the value should be inserted in the hash table.
   - HT: The hash table where the value is to be inserted.
   - V: The value to be inserted into the hash table.

   The function works by consing the value V onto the front of the list already present at the key K in the hash table HT. If the key K does not exist in HT, it creates a new entry at K with V as its value."
  
  (set# k ht (cons v (get# k ht))))


(defun roundto (number decimals)
  "Rounds NUMBER to a specified number of DECIMALS.
   - NUMBER: The number to be rounded. Can be an integer or a floating-point number.
   - DECIMALS: The number of decimal places to round NUMBER to. Must be an integer.
   
   The function works by multiplying NUMBER by 10 raised to the power of DECIMALS, 
   rounding the result to the nearest integer, and then dividing it back by the same factor.
   The result is NUMBER rounded to the specified number of decimal places."

  (declare (type integer decimals))  ; Ensure DECIMALS is an integer
  (let ((f (expt 10 decimals)))      ; Calculate the factor for rounding
    (/ (round (* f number)) f)))     ; Perform the rounding and adjust back


(defun process-list (lst &key chain monads)
  "Process each element of LST through a function chain and then through a series of monadic transformations.
   Parameters:
   - LST: A list of elements to be processed, which is a required argument.
   - CHAIN: An optional list of functions (or a single function) that are composed together and applied sequentially to each element in LST.
   - MONADS: An optional list of functions (or a single function) representing monadic transformations.

   The CHAIN and MONADS parameters are optional and key-based. Each has an associated '-provided-p' flag that indicates whether the argument was actually passed to the function. The function first applies the composed CHAIN to each element in LST. Then, each MONAD is applied to the result, potentially producing more values which are aggregated into the final result list."
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


(defun splice (add lst)
  "Concatenates two lists by adding ADD in reverse order to the front of LST.
   - ADD: A list whose elements are to be added.
   - LST: The list to which the elements of ADD are to be appended.
   
   This function reverses the list ADD and then concatenates it with LST, effectively splicing the reversed ADD into the front of LST."
  (nconc (nreverse add) lst))


(defun ensure-list (item)
  "Ensures that ITEM is a list.
   - ITEM: An object to be transformed into a list if it is not already one.
   
   If ITEM is already a list, it is returned as-is. Otherwise, ITEM is placed into a new list. This function is useful for function arguments that may accept either a single item or a list of items."
  (if (listp item) item (list item)))



(defun struct-field-names (struct)
  "Extracts and returns a list of field names from a given struct.

PARAMETERS:
- struct: An instance of a struct or class from which to extract field names.

The function uses the Common Lisp Object System (CLOS) Metaobject Protocol (MOP) to introspect the struct and retrieve the slot definitions. It then extracts and returns the names of these slots as a list of strings, which represent the field names.

Each field name is converted from a symbol to a string to ensure compatibility with external systems and formats, such as CSV headers.

RETURNS:
A list of strings representing the field names of the struct."
  (loop for slot in (sb-mop:class-slots (class-of struct))
        collect (symbol-name (sb-mop:slot-definition-name slot))))



(defun struct-to-list (struct)
  "Converts a given struct to a list of its values, preserving the order of its fields.

PARAMETERS:
- struct: An instance of a struct or class to be converted into a list of values.

This function utilizes the Common Lisp Object System (CLOS) Metaobject Protocol (MOP) to introspect the given struct and access each of its slots. It then retrieves the value of each slot, preserving the order in which the slots are defined.

The function is generic and does not require prior knowledge of the struct's field names, making it versatile for use with any struct or class that adheres to the CLOS standard.

RETURNS:
A list containing the values of each field in the struct, ordered according to the struct's definition."
  (loop for slot in (closer-mop:class-slots (class-of struct))
        collect (slot-value struct (closer-mop:slot-definition-name slot))))


(defun proper-plist-p (plist)
  "Checks if a plist is properly formed with an even number of elements and all keys are keywords."
  (and (evenp (length plist)) ; Ensure plist has an even number of elements.
       (every #'keywordp (loop for item on plist by #'cddr collect (first item))))) ; Ensure every key is a keyword.


(defun drop (n sequence)
  "Drops the first n elements from a sequence."
  (subseq sequence n))

