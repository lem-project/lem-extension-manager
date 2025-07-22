(defpackage :lem-extension-manager/quicklisp
  (:use :cl :uiop :lem-extension-manager/source)
  (:export :make-quicklisp))

(in-package :lem-extension-manager/quicklisp)

(defun url-not-suitable-error-p (condition)
  (<= 400 (ql-http:unexpected-http-status-code condition) 499))

(defun fetch-gzipped-version (url file &key quietly)
  (let ((gzipped-temp (merge-pathnames "gzipped.tmp" file)))
    (ql-http:fetch url gzipped-temp :quietly quietly)
    (ql-gunzipper:gunzip gzipped-temp file)
    (delete-file-if-exists gzipped-temp)
    (probe-file file)))

(defun maybe-fetch-tgzipped (url file &key quietly)
  (handler-case
      (fetch-gzipped-version url file :quietly quietly)
    (ql-http:unexpected-http-status (condition)
      (cond ((url-not-suitable-error-p condition)
             (ql-http:fetch url file :quietly quietly)
             (probe-file file))
            (t
             (error condition))))))

(defstruct (quicklisp (:include source)))

(defvar *quicklisp-system-list*
  (remove-duplicates
   (mapcar #'ql-dist:release (ql:system-list))))

(defmethod download-source ((source quicklisp) (output-location String))
  (let* ((ql:*local-project-directories* (list *packages-directory*))
         (output-dir (concatenate 'string
                      (namestring *packages-directory*) output-location))
         (release (find (source-name source)
                        *quicklisp-system-list*
                        :key #'ql-dist:project-name
                        :test #'string=))
         (url (ql-dist:archive-url release))
         (name (source-name source))
         (tarfile (concatenate 'string name ".tar")))
    (if release
        (prog1 output-dir
          (uiop:with-current-directory (*packages-directory*)
            (maybe-fetch-tgzipped url tarfile :quietly t)
            (ql-minitar:unpack-tarball tarfile)
            (delete-file tarfile)
            (uiop/cl:rename-file (ql-dist:prefix release) output-location)))
        (error "Package ~a not found!." (source-name source)))))

(defun %register-maybe-quickload (name)
  (uiop:symbol-call :quicklisp :register-local-projects)
  (ql:quickload (alexandria:make-keyword name) :silent t))

