(in-package :cl-user)
(uiop:define-package :openapi2cl/internal
  (:use #:cl)
  (:import-from #:yason)
  (:import-from #:cl-yaml)
  (:import-from #:cl-strings)
  (:import-from #:kebab)
  (:export
   #:resolve-ref
   #:resolve-object
   #:parameter-name
   #:kebab-symbol-from-string
   #:lisp-parameter-name
   #:parameter-location
   #:parameter-type
   #:parameter-required-p
   #:parameter-location-schema-p
   #:parameter-location-query-p
   #:parameter-location-header-p
   #:parameter-location-path-p
   #:parameter-location-body-p
   #:parameter-location-form-p
   #:openapi-schemes-p
   #:select-scheme
   #:media-type-form-p
   #:media-type-subtype
   #:select-media-type
   #:generate-package-clauses
   #:generate-path-methods
   #:generate-client
   #:security-schemas-from-hash-table  ))

(uiop:define-package :openapi2cl
  (:use #:cl)
  (:import-from #:yason)
  (:import-from #:cl-yaml)
  (:import-from #:cl-strings)
  (:import-from #:kebab)
  (:local-nicknames (#:i #:openapi2cl/internal))
  (:export
   #:with-directory-generate-files
   #:with-directory-generate
   #:with-yaml-generate
   #:with-json-generate))
