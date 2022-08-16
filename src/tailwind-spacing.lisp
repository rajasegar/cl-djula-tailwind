(defpackage cl-djula-tailwind.spacing
  (:use :cl)
  (:export :*spacing*))

(in-package cl-djula-tailwind.spacing)


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


(defvar *margin-all* (get-single :margin "m-"))
(defvar *margin-top* (get-single :margin-top "mt-"))
(defvar *margin-right* (get-single :margin-right "mr-"))
(defvar *margin-bottom* (get-single :margin-bottom "mb-"))
(defvar *margin-left* (get-single :margin-left "ml-"))
(defvar *margin-horizontal* (get-double :margin-left :margin-right "mx-"))
(defvar *margin-vertical* (get-double :margin-top :margin-bottom "my-"))

(defvar *margin-auto* '(("m-auto" . ((".m-auto" :margin "auto")))
												("mx-auto" . ((".mx-auto" :margin-left "auto" :margin-right "auto")))
												("my-auto" . ((".my-auto" :margin-top "auto" :margin-bottom "auto")))
												("mt-auto" . ((".mt-auto" :margin-top "auto" )))
												("mb-auto" . ((".mb-auto" :margin-bottom "auto" )))
												("mr-auto" . ((".mr-auto" :margin-right "auto" )))
												("ml-auto" . ((".ml-auto" :margin-left "auto" )))))

(defvar *padding-all* (get-single :padding "p-"))
(defvar *padding-top* (get-single :padding-top "pt-"))
(defvar *padding-right* (get-single :padding-right "pr-"))
(defvar *padding-bottom* (get-single :padding-bottom "pb-"))
(defvar *padding-left* (get-single :padding-left "pl-"))
(defvar *padding-horizontal* (get-double :padding-left :padding-right "px-"))
(defvar *padding-vertical* (get-double :padding-top :padding-bottom "py-"))


(defvar *margin* (append *margin-all* *margin-top* *margin-right* *margin-bottom* *margin-left* *margin-horizontal* *margin-vertical* *margin-auto*))

(defvar *padding* (append *padding-all* *padding-top* *padding-right* *padding-bottom* *padding-left* *padding-horizontal* *padding-vertical*))

(defvar *space-between* '(("space-x-0" .((".space-x-0" :margin-left "0px")))
													("space-y-0" .((".space-y-0" :margin-top "0px")))
													("space-x-0.5" .((".space-x-0.5" :margin-left "0.125rem")))
													("space-y-0.5" .((".space-y-0.5" :margin-top "0.125rem")))
													("space-x-1" .((".space-x-1" :margin-left "0.25rem")))
													("space-y-1" .((".space-y-1" :margin-top "0.25rem")))
													("space-x-1.5" .((".space-x-1.5" :margin-left "0.375rem")))
													("space-y-1.5" .((".space-y-1.5" :margin-top "0.375rem")))
													("space-x-2" .((".space-x-2" :margin-left "0.5rem")))
													("space-y-2" .((".space-y-2" :margin-top "0.5rem")))
													("space-x-2.5" .((".space-x-2.5" :margin-left "0.625rem")))
													("space-y-2.5" .((".space-y-2.5" :margin-top "0.625rem")))
													("space-x-3" .((".space-x-3" :margin-left "0.75rem")))
													("space-y-3" .((".space-y-3" :margin-top "0.75rem")))
													("space-x-3.5" .((".space-x-3.5" :margin-left "0.875rem")))
													("space-y-3.5" .((".space-y-3.5" :margin-top "0.875rem")))
													("space-x-4" .((".space-x-4" :margin-left "1rem")))
													("space-y-4" .((".space-y-4" :margin-top "1rem")))))

(defvar *spacing* (append *padding* *margin* *space-between*))
