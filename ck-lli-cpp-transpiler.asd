(defsystem #:ck-lli-cpp-transpiler
  :components ((:module "source"
                :components ((:file "transpiler"))))
  :depends-on (#:ck-clle))
