(defpackage cl-djula-tailwind.borders
  (:use :cl)
  (:export :*borders*))

(in-package cl-djula-tailwind.borders)

(defvar *border-radius* '(
									("rounded-none" . ((".rounded-none" :border-radius "0px")))
									("rounded-sm" . ((".rounded-sm" :border-radius "0.125rem")))
									("rounded" . ((".rounded" :border-radius "0.25rem")))
									("rounded-md" . ((".rounded-md" :border-radius "0.375rem")))
									("rounded-lg" . ((".rounded-lg" :border-radius "0.5rem")))
									("rounded-xl" . ((".rounded-xl" :border-radius "0.75rem")))
									("rounded-2xl" . ((".rounded-2xl" :border-radius "1rem")))
									("rounded-3xl" . ((".rounded-3xl" :border-radius "1.5rem")))
									("rounded-full" . ((".rounded-full" :border-radius "9999px")))
                    ))




(defvar *borders* (append *border-radius* ))
