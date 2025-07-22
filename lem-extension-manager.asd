(defsystem "lem-extension-manager"
  :depends-on (:alexandria)
  :serial t
  :components ((:file "source")
               (:file "quicklisp" :if-feature :quicklisp)
               (:file "main")))
