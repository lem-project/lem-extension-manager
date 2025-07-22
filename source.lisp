(defpackage :lem-extension-manager/source
  (:use :cl)
  (:export :source
           :download-source
           :local))

(in-package :lem-extension-manager/source)

(defstruct source name)

(defstruct (local (:include source)))

(defgeneric download-source (source output-location)
  (:documentation "It downloads the SOURCE to the desired location."))
