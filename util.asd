;;;; util.asd

(defsystem "util"
  :version "0.1"
  :author "Marcus Pemer"
  :license "GPLv3"
  :depends-on ("parachute" "local-time" "parse-number" "str" "alexandria")
  :components ((:file "package")
               (:file "util" :depends-on ("package")))
  :description "Local utility functions."
  :long-description "Utility functions that are shared between many different lisp systems."
  :maintainer "marcus@pemer.com"
  :homepage "https://your-project-homepage-url.com/")
