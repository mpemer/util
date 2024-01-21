;;; UTIL system definition
;;; see LICENSE for permissoins

(defsystem "util"
  :class :package-inferred-system
  :name "Utilities"
  :version (:read-file-line "version.txt" :at 0)
  :author "Marcus Pemer <marcus@pemer.com>"
  :license "GPL-3.0"
  :depends-on ("rutils" "local-time" "parse-number" "str" "alexandria" "split-sequence" "util/core/docstring" "util/core/main")
  :in-order-to ((test-op (load-op "util/test/main")))
  :perform (test-op (o c) (symbol-call :parachute 'test 'util/test/main))
  :description "Local utility functions."
  :long-description "Utility functions that are shared between many different lisp systems."
  :maintainer "marcus@pemer.com"
  :homepage "https://your-project-homepage-url.com/")

(register-system-packages :rutils '(:rtl))


