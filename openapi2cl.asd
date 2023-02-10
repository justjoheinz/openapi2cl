#-asdf3.1 (error "openapi2cl requires >= ASDF 3.1")

(asdf:defsystem :openapi2cl
  :description "A library to generate a common-lisp client from OpenAPI files."
  :author "Katherine Cox-Buday <cox.katherine.e@gmail.com>"
  :license  "GNU GPL v3"
  :version "0.0.1"
  :depends-on (:cl-strings :yason :cl-yaml :kebab :log4cl)
  :serial t
  :pathname "src/"
  :components ((:file "package")
               (:file "internal")
               (:file "core"))
  :in-order-to ((test-op (test-op "openapi2cl/tests"))))

(asdf:defsystem :openapi2cl/tests
  :depends-on (:rove
               :openapi2cl)
  :pathname "t/"
  :serial t
  :components ((:file "core"))
  :perform (test-op (op c) (symbol-call :rove '#:run c)))
