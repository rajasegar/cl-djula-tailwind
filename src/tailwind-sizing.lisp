(defpackage cl-djula-tailwind.sizing
  (:use :cl
				:cl-djula-tailwind.utils)
  (:export :*sizing*))

(in-package cl-djula-tailwind.sizing)

(defvar *width* (get-single :width "w-"))
(defvar *height* (get-single :height "h-"))
(defvar *max-height* (get-single :max-height "max-h-"))

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

(defvar *width-percentages* (get-percentage-widths :width "width-"))

(defvar *width-viewport* '(("w-full" . ((".w-full" :width "100%")))
											("w-screen" . ((".w-screen" :width "100vw")))
											("w-min" . ((".w-min" :width "min-content")))
											("w-max" . ((".w-max" :width "max-content")))
											("w-fit" . ((".w-fit" :width "fit-content")))))

(defvar *height-viewport* '(("h-full" . ((".h-full" :height "100%")))
											("h-screen" . ((".h-screen" :height "100vh")))
											("h-min" . ((".h-min" :height "min-content")))
											("h-max" . ((".h-max" :height "max-content")))
											("h-fit" . ((".h-fit" :height "fit-content")))))

(defvar *min-width* '(("min-w-0" . ((".min-w-0" :min-width "0px")))
											("min-w-full" . ((".min-w-full" :min-width "100%")))
											("min-w-min" . ((".min-w-min" :min-width "min-content")))
											("min-w-max" . ((".min-w-max" :min-width "max-content")))
											("min-w-fit" . ((".min-w-fit" :min-width "fit-content")))))

(defvar *min-height* '(("min-h-0" . ((".min-h-0" :min-height "0px")))
											("min-h-full" . ((".min-h-full" :min-height "100%")))
											("min-h-screen" . ((".min-h-screen" :min-height "100vh")))
											("min-h-min" . ((".min-h-min" :min-height "min-content")))
											("min-h-max" . ((".min-h-max" :min-height "max-content")))
											("min-h-fit" . ((".min-h-fit" :min-height "fit-content")))))


(defvar *max-height-viewport* '(("max-h-full" . ((".max-h-full" :max-height "100%")))
											("max-h-screen" . ((".max-h-screen" :max-height "100vh")))
											("max-h-min" . ((".max-h-min" :max-height "min-content")))
											("max-h-max" . ((".max-h-max" :max-height "max-content")))
											("max-h-fit" . ((".max-h-fit" :max-height "fit-content")))))

(defvar *max-width* '(("max-w-0" . ((".max-w-0" :max-width "0rem")))
											("max-w-none" . ((".max-w-none" :max-width "none")))
											("max-w-xs" . ((".max-w-xs" :max-width "20rem")))
											("max-w-sm" . ((".max-w-sm" :max-width "24rem")))
											("max-w-md" . ((".max-w-md" :max-width "28rem")))
											("max-w-lg" . ((".max-w-lg" :max-width "32rem")))
											("max-w-xl" . ((".max-w-xl" :max-width "36rem")))
											("max-w-2xl" . ((".max-w-2xl" :max-width "42rem")))
											("max-w-3xl" . ((".max-w-3xl" :max-width "48rem")))
											("max-w-4xl" . ((".max-w-4xl" :max-width "56rem")))
											("max-w-5xl" . ((".max-w-5xl" :max-width "64rem")))
											("max-w-6xl" . ((".max-w-6xl" :max-width "72rem")))
											("max-w-7xl" . ((".max-w-7xl" :max-width "80rem")))
											("max-w-full" . ((".max-w-full" :max-width "100%")))
											("max-w-min" . ((".max-w-min" :max-width "min-content")))
											("max-w-max" . ((".max-w-max" :max-width "max-content")))
											("max-w-fit" . ((".max-w-fit" :max-width "fit-content")))
											("max-w-prose" . ((".max-w-prose" :max-width "65ch")))
											("max-w-screen-sm" . ((".max-w-screen-sm" :max-width "640px")))
											("max-w-screen-md" . ((".max-w-screen-md" :max-width "768px")))
											("max-w-screen-lg" . ((".max-w-screen-lg" :max-width "1024px")))
											("max-w-screen-xl" . ((".max-w-screen-xl" :max-width "1280px")))
											("max-w-screen-2xl" . ((".max-w-screen-2xl" :max-width "1536px")))))


(defvar *sizing* (append *width*
												 *height*
												 *width-percentages*
												 *width-viewport*
												 *height-viewport*
												 *min-width*
												 *min-height*
												 *max-height*
												 *max-height-viewport*
												 *max-width*))
