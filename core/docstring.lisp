;;;; -----------------------------------------------------------------------
;;;; Filename: docstring.lisp
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

(defpackage :util/core/docstring
  (:use :common-lisp)
  (:import-from :split-sequence :split-sequence))

(in-package :util/core/docstring)

;;;; -----------------------------------------------------------------------
(defun add-newline-formatting (input-string)
  "Process each line of INPUT-STRING.
   If a line does not end with ~, ~@, or ~:, append ~@ to it.
   This allows for the definition of multi-line docstrings
   using arbitrary indentation. When a line should be continued,
   simply append ~ to the end of that line."
  (with-output-to-string (out)
    (loop for line in (split-sequence:split-sequence #\Newline input-string)
          do (let ((line-end (subseq line (max 0 (- (length line) 2)))))
               (write-string 
                (if (or (string= line-end "~@")
                        (string= line-end "~:")
                        (string-right-trim "~" line-end))
                    line
                    (concatenate 'string line "~@"))
                out))
             (write-char #\Newline out))))

(defun multiline-docstring (stream sub-char arg)
  "Custom dispatch function that processes the following string
and returns a form for read-time evaluation.

Used to define multi-line docstrings with arbitrary indentation."
  (declare (ignore sub-char arg))
  ;; Read the next form (expected to be a string)
  (let ((next-form (read stream t nil t)))
    ;; Return a form to be evaluated at read time
    `(format nil ,(add-newline-formatting next-form))))

(in-package :cl-user)
;; Set up the dispatch macro character #~
(set-dispatch-macro-character #\# #\~ #'util/core/docstring::multiline-docstring)
