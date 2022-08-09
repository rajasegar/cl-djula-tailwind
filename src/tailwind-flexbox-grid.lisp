(defpackage cl-djula-tailwind.flexbox-grid
  (:use :cl)
  (:export :*flexbox-grid*))

(in-package cl-djula-tailwind.flexbox-grid)

(defvar *flex-basis* '(
											 ("basis-0" . ((".basis-0" :flex-basis "0px")))
											 ("basis-1" . ((".basis-1" :flex-basis "0.25rem")))
											 ("basis-2" . ((".basis-2" :flex-basis "0.5rem")))
											 ("basis-3" . ((".basis-3" :flex-basis "0.75rem")))
											 ("basis-4" . ((".basis-4" :flex-basis "1rem")))
											 ("basis-5" . ((".basis-5" :flex-basis "1.25rem")))
											 ("basis-6" . ((".basis-6" :flex-basis "1.5rem")))
											 ("basis-7" . ((".basis-7" :flex-basis "1.75rem")))
											 ("basis-8" . ((".basis-8" :flex-basis "2rem")))
											 ("basis-9" . ((".basis-9" :flex-basis "2.25rem")))
											 ("basis-10" . ((".basis-10" :flex-basis "2.5rem")))
											 ("basis-11" . ((".basis-11" :flex-basis "2.75rem")))
											 ("basis-12" . ((".basis-12" :flex-basis "3rem")))
											 ))

(defvar *flex-direction* '(("flex-row" . ((".flex-row" :flex-direction "row")))
													 ("flex-row-reverse" . ((".flex-ro-reversew" :flex-direction "row-reverse")))
													 ("flex-col" . ((".flex-col" :flex-direction "column")))
													 ("flex-col-reverse" . ((".flex-col-reverse" :flex-direction "column-reverse")))))

(defvar *flex-wrap* '(("flex-wrap" . ((".flex-wrap" :flex-wrap "wrap")))
											("flex-wrap-reverse" . ((".flex-wrap-reverse" :flex-wrap "wrap-reverse")))
											("flex-nowrap" . ((".flex-nowrap" :flex-wrap "nowrap")))))

(defvar *flexbox-grid* (append
												*flex-basis*
												*flex-direction*
												*flex-wrap*
												))
