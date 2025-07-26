(defsystem "lem-extension-manager"
  :description "Configuration library for managing Lem packages."
  :license "MIT"
  :depends-on (:alexandria)
  :serial t
  :components ((:file "source")
               (:file "quicklisp" :if-feature :quicklisp)
               (:file "main")))
