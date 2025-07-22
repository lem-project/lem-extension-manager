(uiop:define-package :lem-extension-manager
  (:use :cl :uiop :lem-extension-manager/source)
  #+quicklisp (:use-reexport :lem-extension-manager/quicklisp)
  (:export :*installed-packages*
           :*packages-directory*
           :lem-use-package
           :load-packages
           ;; For user commands
           :package-test
           :package-remove
           :packages-list

           :extension
           :extension-name
           :extension-source
           :extension-directory))

(in-package :lem-extension-manager)

(defvar *installed-packages* nil)

(defun default-home ()
  (let ((xdg-lem (uiop:xdg-config-home "lem/"))
        (dot-lem (merge-pathnames ".lem/" (user-homedir-pathname))))
    (or (uiop:getenv "LEM_HOME")
        (and (probe-file dot-lem) dot-lem)
        xdg-lem)))

(defvar *packages-directory*
  (pathname (concatenate 'string
             (directory-namestring (default-home))
             "packages"
             (string  (uiop:directory-separator-for-host)))))

(defvar *git-base-arglist* (list "git")
  "The git program, to be appended command-line options.")

(defun run-git (arglist)
  (uiop:wait-process
   (uiop:launch-program (concatenate 'list *git-base-arglist* arglist)
                        :ignore-error-status t)))

(defstruct (git (:include source)) url branch commit)

(defmethod download-source ((source git) (output-location String))
  (let ((output-dir (concatenate 'string
                     (namestring *packages-directory*) output-location)))
    (run-git (list "clone" (git-url source) output-dir))
    (when (git-branch source)
      (uiop:with-current-directory (output-dir)
        (run-git (list "checkout" "-b" (git-branch source)))))

    (when (git-commit source)
      (uiop:with-current-directory (output-dir)
        (run-git (list "checkout" (git-commit source)))))
    output-dir))


(defmethod download-source (source output-location)
  (error "Source ~a not available." source))

(defclass extension ()
  ((name :initarg :name
         :accessor extension-name)
   (source :initarg :source
           :accessor extension-source)
   (directory :initarg :directory
              :accessor extension-directory)))

(defgeneric package-remove (package))

(defmethod package-remove ((package extension))
  (uiop:delete-directory-tree
   (uiop:truename* (extension-directory package)) :validate t)
  (delete package *installed-packages*))

(defgeneric package-test (package))

(defmethod package-test ((package extension))
  (let* ((*packages-directory* (uiop:temporary-directory))
         (name (extension-name package))
         (source (extension-source package)))
    (%download-package source name)
    #+quicklisp
    (%register-maybe-quickload (extension-name package))))

(defun packages-list ()
  (remove-duplicates
   (mapcar (lambda (d) (pathname (directory-namestring d)))
           (directory (merge-pathnames "**/*.asd" *packages-directory*)))))

(defun insert-package (package)
  (pushnew package *installed-packages*
           :test (lambda (a b)
                   (string=
                    (extension-name a)
                    (extension-name b)))))

(defun define-source (source-list name)
  (let ((s (getf source-list :type)))
    (ecase s
      (:git
       (destructuring-bind (&key type url branch commit)
           source-list
         (declare (ignore type))
         (make-git :name name
                   :url url
                   :branch branch
                   :commit commit)))
      #+quicklisp (:quicklisp
       (destructuring-bind (&key type)
           source-list
         (declare (ignore type))
         (make-quicklisp :name name)))
      (t (error "Source ~a not available." s)))))

#+quicklisp
(defun %register-maybe-quickload (name)
  (uiop:symbol-call :quicklisp :register-local-projects)
  (ql:quickload (alexandria:make-keyword name) :silent t))

(defun %download-package (source name)
  (format t "Downloading ~a..." name)
  (download-source source name)
  (format t "Done downloading ~a!" name))

#|

git source: (list :type type :url url :branch branch :commit commit :dependecies dependencie-list)

dependency: (("versioned-objects" :source

(lem-use-package "versioned-objects"
:source (:type :git
:url "https://github.com/smithzvk/Versioned-Objects.git"
:branch "advance-versioning"))


Quicklisp can take care of the dependencies
(lem-use-package "fiveam" :source (:type :quicklisp))

|#
(defmacro lem-use-package (name &key source after
                                     bind hooks
                                     force dependencies)
  (declare (ignore hooks bind after))
  #+sbcl
  (ensure-directories-exist *packages-directory*)
  (alexandria:with-gensyms (spackage rsource pdir)
    `(let* ((asdf:*central-registry*
              (union (packages-list)
                     asdf:*central-registry*
                     :test #'equal))
            #+quicklisp
            (ql:*local-project-directories*
              (nconc (list *packages-directory*)
                     ql:*local-project-directories*))
            (,rsource (define-source ',source ,name))
            (,pdir (merge-pathnames *packages-directory* ,name))
            (,spackage (make-instance 'extension
                                      :name ,name
                                      :source ,rsource
                                      :directory ,pdir)))
       (when (or ,force
                 (not (uiop:directory-exists-p ,pdir)))
         (%download-package ,rsource ,name))
       ,(when dependencies
          (loop for dep in dependencies
                do (eval `(lem-use-package ,@dep))))
       (insert-package ,spackage)
       (and #+quicklisp (%register-maybe-quickload ,name) t))))


;; Package util functions/commands

#+quicklisp
(defun load-packages ()
  (let ((ql:*local-project-directories* (list *packages-directory*)))
    (loop for dpackage in (directory (merge-pathnames "*/" *packages-directory*))
          for spackage = (car
                          (last
                           (pathname-directory
                            (uiop:directorize-pathname-host-device dpackage))))
          do (insert-package
              (make-instance 'extension
                             :name spackage
                             :source (make-local :name spackage)
                             :directory dpackage))
          do (ql:quickload (alexandria:make-keyword spackage) :silent t))))
