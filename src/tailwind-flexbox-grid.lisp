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

(defvar *justify-content* '(("justify-start" . ((".justify-start" :justify-content "flex-start")))
														("justify-end" . ((".justify-end" :justify-content "flex-end")))
														("justify-center" . ((".justify-center" :justify-content "center")))
														("justify-between" . ((".justify-between" :justify-content "space-between")))
														("justify-around" . ((".justify-around" :justify-content "space-around")))
														("justify-evenly" . ((".justify-evenly" :justify-content "space-evenly")))))

(defvar *justify-items* '(("justify-items-start" . ((".justify-items-start" :justify-items "start")))
													("justify-items-end" . ((".justify-items-end" :justify-items "end")))
													("justify-items-center" . ((".justify-items-center" :justify-items "center")))
													("justify-items-stretch" . ((".justify-items-stretch" :justify-items "stretch")))))

(defvar *justify-self* '(("justify-self-auto" . ((".justify-self-auto" :justify-self "auto")))
												 ("justify-self-start" . ((".justify-self-start" :justify-self "start")))
												 ("justify-self-end" . ((".justify-self-end" :justify-self "end")))
												 ("justify-self-center" . ((".justify-self-center" :justify-self "center")))
												 ("justify-self-stretch" . ((".justify-self-stretch" :justify-self "stretch")))))

(defvar *align-content* '(("content-center" . ((".content-center" :align-content "center")))
													("content-start" . ((".content-start" :align-content "start")))
													("content-end" . ((".content-end" :align-content "end")))
													("content-between" . ((".content-between" :align-content "space-between")))
													("content-around" . ((".content-around" :align-content "space-around")))
													("content-evenly" . ((".content-evenly" :align-content "space-evenly")))))

(defvar *align-items* '(("items-start" . ((".items-start" :align-items "flex-start")))
												("items-end" . ((".items-end" :align-items "flex-end")))
												("items-center" . ((".items-center" :align-items "center")))
												("items-baseline" . ((".items-baseline" :align-items "baseline")))
												("items-stretch" . ((".items-stretch" :align-items "stretch")))))

(defvar *align-self* '(("self-auto" . ((".self-auto" :align-self "auto")))
											 ("self-start" . ((".self-start" :align-self "flex-start")))
											 ("self-end" . ((".self-end" :align-self "flex-end")))
											 ("self-center" . ((".self-center" :align-self "center")))
											 ("self-stretch" . ((".self-stretch" :align-self "stretch")))
											 ("self-baseline" . ((".self-baseline" :align-self "baseline")))))

(defvar *place-content* '(("place-content-center" . ((".place-content-center" :place-content "center")))
													("place-content-start" . ((".place-content-start" :place-content "start")))
													("place-content-end" . ((".place-content-end" :place-content "end")))
													("place-content-between" . ((".place-content-between" :place-content "space-between")))
													("place-content-around" . ((".place-content-around" :place-content "space-around")))
													("place-content-evenly" . ((".place-content-evenly" :place-content "space-evenly")))
													("place-content-stretch" . ((".place-content-stretch" :place-content "stretch")))))
(defvar *place-items* '(("place-items-start" . ((".place-items-start" :place-items "start")))
												("place-items-end" . ((".place-items-end" :place-items "end")))
												("place-items-center" . ((".place-items-center" :place-items "center")))
												("place-items-stretch" . ((".place-items-stretch" :place-items "stretch")))))

(defvar *place-self* '(("place-self-auto" . ((".place-self-auto" :place-self "auto")))
											 ("place-self-start" . ((".place-self-start" :place-self "start")))
											 ("place-self-end" . ((".place-self-end" :place-self "end")))
											 ("place-self-center" . ((".place-self-center" :place-self "center")))
											 ("place-self-stretch" . ((".place-self-stretch" :place-self "stretch")))))

(defvar *flexbox-grid* (append
												*flex-basis*
												*flex-direction*
												*flex-wrap*
												*justify-content*
												*justify-items*
												*justify-self*
												*align-content*
												*align-items*
												*align-self*
												*place-content*
												*place-items*
												*place-self*
												))
