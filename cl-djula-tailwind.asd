(defsystem "cl-djula-tailwind"
  :version "0.1.0"
  :author "Rajasegar Chandran"
  :license "MIT"
  :depends-on ("cl-css"
               "cl-ppcre"
               "djula"
							 "cl-minify-css")
  :components ((:module "src"
                :components
                ((:file "main" :depends-on (
																						"tailwind-layout"
																						"tailwind-spacing"
																						"tailwind-typography"
																						"tailwind-flexbox-grid"
																						"tailwind-backgrounds"
																						"tailwind-colors"
																						"tailwind-sizing"
																						"tailwind-borders"
																						"tailwind-effects"
																						"utils"
																						))
                 (:file "tailwind-layout")
                 (:file "tailwind-spacing")
                 (:file "tailwind-typography" :depends-on ("tailwind-colors"))
                 (:file "tailwind-flexbox-grid")
                 (:file "tailwind-backgrounds" :depends-on ("tailwind-colors"))
                 (:file "tailwind-colors")
                 (:file "tailwind-sizing" :depends-on ("utils"))
                 (:file "tailwind-borders")
                 (:file "tailwind-effects")
                 (:file "utils")
                 )))
  :description ""
  :in-order-to ((test-op (test-op "cl-djula-tailwind/tests"))))

(defsystem "cl-djula-tailwind/tests"
  :author "Rajasegar Chandran"
  :license "MIT"
  :depends-on ("cl-djula-tailwind"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cl-djula-tailwind"
  :perform (test-op (op c) (symbol-call :rove :run c)))
