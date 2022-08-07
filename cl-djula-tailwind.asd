(defsystem "cl-djula-tailwind"
  :version "0.1.0"
  :author "Rajasegar Chandran"
  :license ""
  :depends-on ("cl-css"
               "cl-ppcre"
               "djula")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "cl-djula-tailwind/tests"))))

(defsystem "cl-djula-tailwind/tests"
  :author "Rajasegar Chandran"
  :license ""
  :depends-on ("cl-djula-tailwind"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cl-djula-tailwind"
  :perform (test-op (op c) (symbol-call :rove :run c)))
