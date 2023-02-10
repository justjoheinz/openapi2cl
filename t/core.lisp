(in-package :cl-user)
(uiop:define-package :openapi2cl/tests
  (:use :cl :rove :openapi2cl)
  (:local-nicknames (#:i #:openapi2cl/internal)))
(in-package :openapi2cl/tests)


(deftest media-type-subtype-tests
  (testing "should recognize subtype from application/json"
    (ok (string= (i:media-type-subtype "application/json") "json")))
    (ok (string= (i:media-type-subtype "application/json+user") "user"))
    (ok (string= (i:media-type-subtype "application/*") "*")))

(deftest parameter-tests
        (testing "should read a parameter correctly"
          (let* ((param (yaml:parse "
                              name: username
                              in: path
                              description: username to fetch
                              required: true
                              schema:
                                type: string"))
                 (schema (gethash "schema" param)))
            (ok (string= (i:parameter-name param) "username"))
            (ok (string= (i:parameter-description param) "username to fetch"))
            (ok (string= (i:parameter-type schema) "string"))
            (ok (i:parameter-required-p param)))))
