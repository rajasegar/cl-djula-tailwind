(defpackage cl-djula-tailwind.utils
  (:use :cl)
  (:export :get-single
            :get-double
            :get-quadruple
            :get-translate
           :get-percentage-widths
           :get-translate-ratios))

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

(defun get-quadruple (prop-1 prop-2 prop-3 prop-4 prefix)
	"Get top, right, bottom and left values"
	(loop for val in *values*
						 for unit in *units*
						 for key = (concatenate 'string prefix unit)
						 for classname = (concatenate 'string "." prefix unit)
			collect `(,key . ((,classname ,prop-1 ,val ,prop-2 ,val ,prop-3 ,val ,prop-4 ,val)))))


(defun get-translate (prop prefix fun)
	"Get translate x and y utils"
	(loop for val in *values*
						 for unit in *units*
						 for key = (concatenate 'string prefix unit)
						 for classname = (concatenate 'string "." prefix unit)
			collect `(,key . ((,classname ,prop ,(concatenate 'string fun "(" val ")"))))))

(defvar *percentages* '("50%" "33.333333%" "66.666666%" "25%" "50%" "75%" "20%" "40%" "60%" "80%" "16.666667%" "33.333333%" "50%" "66.666667%" "83.333333%" "8.333333%" "16.666667%" "25%" "33.333333%" "41.666667%" "50%" "58.333333%" "66.666667%" "75%" "83.333333%" "91.666667%"))

(defvar *ratios* '("1/2" "1/3" "2/3" "1/4" "2/4" "3/4" "1/5" "2/5" "3/5" "4/5" "1/6" "2/6" "3/6" "4/6" "5/6" "1/12"
												 "2/12" "3/12" "4/12" "5/12" "6/12" "7/12" "8/12" "9/12" "10/12" "11/12"))

(defun get-percentage-widths (prop prefix)
	"Get percentage width utils"
	(loop for val in *percentages*
						 for unit in *ratios*
						 for key = (concatenate 'string prefix unit)
						 for classname = (concatenate 'string "." prefix unit)
			collect `(,key . ((,classname ,prop ,val)))))

(defun get-translate-ratios (prop prefix fun)
	"Get percentage width utils"
	(loop for val in *percentages*
						 for unit in *ratios*
						 for key = (concatenate 'string prefix unit)
						 for classname = (concatenate 'string "." prefix unit)
			collect `(,key . ((,classname ,prop ,(concatenate 'string fun "(" val ")"))))))
