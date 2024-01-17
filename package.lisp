(defpackage :util
  (:use :common-lisp :rutil :parse-number)
  (:import-from :alexandria :compose)
  (:import-from :parachute :define-test)
  (:export :process-list :parse-number? :insert-hash :roundto
           :struct-field-names :struct-to-list
           :proper-plist-p :drop :read-file-as-bytes
           :create-mock-file))
