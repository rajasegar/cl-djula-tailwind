(defpackage cl-djula-tailwind.utils
  (:use :cl)
  (:export :get-single
					 :get-double))

(in-package cl-djula-tailwind.utils)

(defvar *values* '(
												 "0px"
												 "1px"
												 "0.125rem"
												 "0.25rem"
												 "0.375rem"
												 "0.5rem"
												 "0.625rem"
												 "0.75rem"
												 "0.875rem"
												 "1rem"
												 "1.25rem"
												 "1.5rem"
												 "1.75rem"
												 "2rem"
												 "2.25rem"
												 "2.5rem"
												 "2.75rem"
												 "3rem"
												 "3.5rem"
												 "4rem"
												 "5rem"
												 "6rem"
												 "7rem"
												 "8rem"
												 "9rem"
												 "10rem"
												 "11rem"
												 "12rem"
												 "13rem"
												 "14rem"
												 "15rem"
												 "16rem"
												 "18rem"
												 "20rem"
												 "24rem"
												 ))

(defvar *units* '("0" "px" "0.5" "1" "1.5" "2" "2.5" "3" "3.5" "4" "5" "6" "7" "8" "9" "10" "11" "12" "14" "16" "20" "24" "28" "32" "36" "40" "44" "48" "52" "56" "60" "64" "72" "80" "96"))

(defun get-single (prop prefix)
	"Get single or all side values for margin or padding"
	(loop for val in *values*
						 for unit in *units*
						 for key = (concatenate 'string prefix unit)
						 for classname = (concatenate 'string "." prefix unit)
			collect `(,key . ((,classname ,prop ,val)))))

(defun get-double (prop-left prop-right prefix)
	"Get vertical or horizontal side values for margin or padding"
	(loop for val in *values*
						 for unit in *units*
						 for key = (concatenate 'string prefix unit)
						 for classname = (concatenate 'string "." prefix unit)
			collect `(,key . ((,classname ,prop-left ,val ,prop-right ,val)))))

