(defpackage :lem-extension-manager
  (:use :cl :lem))
(in-package :lem-extension-manager)

(defclass project ()
  ((data :initarg :data
         :reader project-data)))

(defmethod print-object ((object project) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~S ~S" (project-name object) (project-description object))))

(defun make-project (project)
  (make-instance 'project :data project))

(defmethod project-name ((project project))
  (ultralisp-client/lowlevel:project2-name (project-data project)))

(defmethod project-description ((project project))
  (ultralisp-client/lowlevel:project2-description (project-data project)))

(defmethod project-updated-at ((project project))
  (ultralisp-client/lowlevel:project2-updated-at (project-data project)))

(defmethod project-system-name ((project project))
  (let ((parts (split-sequence:split-sequence #\/ (project-name project))))
    (alexandria:length= 2 parts)
    (second parts)))

(defmethod already-installed-p ((project project))
  (not (null (ql:where-is-system (project-system-name project)))))

(defun list-projects ()
  (mapcar #'make-project
          (ultralisp-client:get-projects-by-tag "lem-addon")))

;;;
(define-attribute header-attribute
  (t :bold t :foreground :base07))

(define-attribute description-attribute
  (t :foreground :base04))

(define-attribute install-button-attribute
  (t :foreground :base01 :background :base0A :bold t))

(define-attribute installed-button-attribute
  (t :foreground :base01 :background :base03 :bold t))

(define-attribute installation-succeeded-attribute
  (t :foreground "green" :bold t))

(define-attribute installation-failed-attribute
  (t :foreground "red" :bold t))

(defun make-projects-buffer ()
  (let ((buffer (make-buffer "*Project List*")))
    (setf (buffer-read-only-p buffer) t)
    (setf (variable-value 'highlight-line :buffer buffer) nil)
    buffer))

(defun try-quickload (system-name)
  (let* ((buffer (make-buffer "*Project install*"))
         (point (buffer-point buffer))
         (*inhibit-read-only* t))
    (erase-buffer buffer)
    (pop-to-buffer buffer)
    (with-open-stream (output (make-buffer-output-stream point t))
      (let ((*standard-output* output)
            (*error-output* output))
        (handler-case (ql:quickload system-name)
          (:no-error (&rest *)
            (insert-string point 
                           "Installation succeeded."
                           :attribute 'installation-succeeded-attribute))
          (error ()
            (insert-string point 
                           "Installation failed."
                           :attribute 'installation-failed-attribute)))))))

(defun install-project (project buffer)
  (try-quickload (project-system-name project))
  (update-list-buffer buffer))

(defun update-list-buffer (buffer &key (projects (list-projects)))
  (let ((*inhibit-read-only* t))
    (flet ((insert-header (point project)
             (insert-string point " ")
             (insert-string point (project-name project) :attribute 'header-attribute))
           (insert-description (point project)
             (insert-string point "   ")
             (insert-string point
                            (project-description project)
                            :attribute 'description-attribute))
           (insert-install-button (point project)
             (lem/button:insert-button point
                                       "Install"
                                       (lambda () (install-project project buffer))
                                       :attribute 'install-button-attribute)))
      (erase-buffer buffer)
      (with-point ((point (buffer-point buffer) :left-inserting))
        (dolist (project projects)
          (insert-header point project)
          (move-to-column point 40 t)
          (insert-install-button point project)
          (insert-character point #\newline)
          (insert-description point project)
          (insert-character point #\newline)
          (insert-character point #\newline)))
      (buffer-start (buffer-point buffer)))))

(define-command extension-manager-list-projects () ()
  (let ((buffer (make-projects-buffer)))
    (update-list-buffer buffer)
    (switch-to-buffer buffer)))
