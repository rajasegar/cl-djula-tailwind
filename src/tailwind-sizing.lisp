(defpackage cl-djula-tailwind.sizing
  (:use :cl)
  (:export :*sizing*))

(in-package cl-djula-tailwind.sizing)

(defvar *width* '(
									("w-0" . ((".w-0" :width "0px")))
								  ("w-px" . ((".w-px" :width "1px" )))
								  ("w-48" . ((".w-48" :width "12rem")))
								  ("w-52" . ((".w-52" :width "13rem")))
								  ("w-56" . ((".w-56" :width "14rem")))
								  ("w-60" . ((".w-60" :width "15rem")))
								  ("w-64" . ((".w-64" :width "16rem")))
								  ("w-72" . ((".w-72" :width "18rem")))
								  ("w-80" . ((".w-80" :width "20rem")))
								  ("w-96" . ((".w-96" :width "24rem")))
                    ))




(defvar *sizing* (append *width* ))
