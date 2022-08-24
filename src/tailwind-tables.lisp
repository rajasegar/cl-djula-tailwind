(defpackage cl-djula-tailwind.tables
  (:use :cl)
  (:export :*tables*))

(in-package cl-djula-tailwind.tables)

(defvar *border-collapse* '(("border-collapse" . ((".border-collapse" :border-collapse "collapse")))
									("border-separate" . ((".border-separate" :border-collapse "separate")))))

(defvar *table-layout* '(("table-auto" . ((".table-auto" :table-layout "auto")))
												 ("table-fixed" . ((".table-fixed" :table-layout "fixed")))))


(defvar *tables* (append *border-collapse* *table-layout*))
