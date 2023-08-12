(defpackage :lem-extension-manager
  (:use :cl :lem))
(in-package :lem-extension-manager)

(defclass project ()
  ((data :initarg :data
         :reader project-data)))

(defmethod print-object ((object project) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~S ~S" (project-name object) (project-description object))))

(defmethod project-name ((project project))
  (ultralisp-client/lowlevel:project2-name (project-data project)))

(defmethod project-description ((project project))
  (ultralisp-client/lowlevel:project2-description (project-data project)))

(defmethod project-updated-at ((project project))
  (ultralisp-client/lowlevel:project2-updated-at (project-data project)))

(defun make-project (project)
  (make-instance 'project :data project))

(defun list-projects ()
  (mapcar #'make-project
          (ultralisp-client:get-projects-by-tag "lem-addon")))

;;; list projects
(define-attribute header-attribute
  (t :bold t :foreground :base07))

(define-attribute description-attribute
  (t :foreground :base04))

(define-attribute install-button-attribute
  (t :foreground :base01 :background :base0C :bold t))

(defun make-projects-buffer ()
  (let ((buffer (make-buffer "*Project List*")))
    (setf (buffer-read-only-p buffer) t)
    (setf (variable-value 'highlight-line :buffer buffer) nil)
    buffer))

(defun update-list-buffer (buffer projects)
  (let ((*inhibit-read-only* t))
    (flet ((insert-header (point project)
             (insert-string point " ")
             (insert-string point (project-name project) :attribute 'header-attribute))
           (insert-description (point project)
             (insert-string point "   ")
             (insert-string point 
                            (project-description project)
                            :attribute 'description-attribute)))
      (erase-buffer buffer)
      (with-point ((point (buffer-point buffer) :left-inserting))
        (dolist (project projects)
          (insert-header point project)

          (move-to-column point 40 t)
          (lem/button:insert-button point
                                    "Install"
                                    (let ((project project))
                                      (lambda () (install-project project)))
                                    :attribute 'install-button-attribute)
        
          (insert-character point #\newline)
          (insert-description point project)
          (insert-character point #\newline)
          (insert-character point #\newline))))))

(defun install-project (project)
  (declare (ignore project))
  (editor-error "Install is not implemented"))
