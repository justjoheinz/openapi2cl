(in-package :openapi2cl)


;; NOTE: The generation occurs by building up sexps. Any symbols which
;; are written into these sexps must be prepended by the cl-user
;; namespace; otherwise the symbols in the generated code will be
;; prepended by this package's namespace.


(defun generate (root-path openapi client-name)
  "Generates a client and a list of its methods. These are returned in
a 2-tuple.

openapi: the sexp representation of a openapi document.
client-name: the name of the class that hosts all the client methods."
  (check-type client-name symbol)
  (let* ((host (gethash "host" openapi))
         (base-path (gethash "basePath" openapi))
         (consumes-media-types (gethash "consumes" openapi))
         (produces-media-types (gethash "produces" openapi))
         (schemes (or (gethash "schemes" openapi)
                      '("https")))
         (security-schemas (i:security-schemas-from-hash-table
                            (i:resolve-object root-path (gethash "securityDefinitions" openapi))))
         (client (i:generate-client client-name schemes host base-path
                                  consumes-media-types produces-media-types
                                  security-schemas)))
    (values
     client
     (loop for path-name being the hash-keys of (gethash "paths" openapi)
             using (hash-value path-operation)
           nconc (i:generate-path-methods client-name path-name path-operation
                                        consumes-media-types security-schemas)))))

(defun with-yaml-generate (root-path openapi-yaml client-name)
  "Generate a client and a list of its methods based on a YAML file.

root-path: The root path of the Yaml file.
openapi-yaml: a string containing the YAML representation of a openapi document, or a pathname to a YAML document.
client-name: the name of the class that hosts all the client methods."
  (check-type root-path (or string pathname))
  (check-type openapi-yaml (or string pathname))
  (check-type client-name symbol)
  (generate root-path (yaml:parse openapi-yaml) client-name))

(defun with-json-generate (root-path openapi-json client-name)
  "Generate a client and a list of its methods based on a YAML file.

root-path: The root path of the Json file.
openapi-json: a string containing the JSON representation of a openapi document, or a pathname to a JSON document.
client-name: the name of the class that hosts all the client methods."
  (check-type root-path (or string pathname))
  (check-type openapi-json (or string pathname))
  (check-type client-name symbol)
  (generate root-path (yason:parse openapi-json) client-name))

(defun with-directory-generate (path process-results-fn)
  "Given a pathname, process the YAML and JSON files within, and
call `process-results-fn'. This function should take these
arguments: (1) The file-path (2) the client definition (3) the list of
methods for this client."
  (check-type process-results-fn function)
  (flet ((with-file-generate (file-path)
           (let* ((type (pathname-type file-path))
                  (name (i:kebab-symbol-from-string (pathname-name file-path)))
                  (output-path (make-pathname :type "lisp" :defaults file-path)))
             (multiple-value-bind (client-def methods-list)
                 (cond ((string= type "yaml")
                        (with-yaml-generate (uiop:pathname-directory-pathname file-path) file-path name))
                       ((string= type "json")
                        (with-json-generate (uiop:pathname-directory-pathname file-path) file-path name)))
               (funcall process-results-fn output-path client-def methods-list)))))
    (mapcar #'with-file-generate (directory path))))

(defun with-directory-generate-files (input-path output-path package-root &optional preamble)
  "Given a pathname, generate lisp files for every specification
encountered and processed.

package-root will be used as the root of each file's package. The
file's name will be appended to this forming the fully qualified
package name."
  (check-type package-root symbol)
  (flet ((write-defs-out (file-path client-def methods-list)
           (let ((file-path (make-pathname :defaults file-path
                                           :directory output-path))
                 (*package* (find-package :cl-user)))
             (with-open-file (stream file-path :direction :output :if-exists :supersede)
               (format t "Output path: ~a~%" file-path)
               (when preamble (format stream "~a~%~%" preamble))
               (when package-root
                 (dolist (pkg-clause (i:generate-package-clauses
                                      (intern (string-upcase (format nil "~a/~a" package-root (pathname-name file-path))))
                                      :packages-using '(#:cl)
                                      :packages-import '(#:cl-strings)))
                   (format stream "~s~%" pkg-clause))
                 (format stream "~%~%"))
               (format stream "~s~%" client-def)
               (dolist (m methods-list) (format stream "~%~%~s~%" m))))))
    (with-directory-generate input-path #'write-defs-out)))
