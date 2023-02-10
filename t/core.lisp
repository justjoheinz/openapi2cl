(in-package :cl-user)
(uiop:define-package :openapi2cl/tests
  (:use :cl :rove :openapi2cl)
  (:local-nicknames (#:i #:openapi2cl/internal)))
(in-package :openapi2cl/tests)


(deftest media-type-subtype-tests
  (testing "should recognize subtype from `application/json`"
    (ok (string= (i:media-type-subtype "application/json") "json")))
    (ok (string= (i:media-type-subtype "application/json+user") "user"))
    (ok (string= (i:media-type-subtype "application/*") "*")))
