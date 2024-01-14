(defpackage :util
  (:use :common-lisp :rutil :parse-number)
  (:import-from :alexandria :compose)
  (:export :process-list :parse-number? :insert-hash :roundto
           :struct-field-names :struct-to-list
           :proper-plist-p :drop))
