(defsystem #:ck-lit/cpp
  :components ((:module "source"
                :components ((:file "cpp-transpiler/transpiler"))))
  :depends-on (#:ck-clle))

(defsystem #:ck-lit
  :depends-on (#:ck-lit/cpp))
