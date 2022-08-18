(defpackage cl-djula-tailwind.flexbox-grid
  (:use :cl
				:cl-djula-tailwind.utils)
  (:export :*flexbox-grid*))

(in-package cl-djula-tailwind.flexbox-grid)

(defvar *flex-basis* '(("basis-0" . ((".basis-0" :flex-basis "0px")))
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

(defvar *flex* '(("flex-1" . ((".flex-1" :flex "1 1 0%")))
								 ("flex-auto" . ((".flex-auto" :flex "1 1 auto")))
								 ("flex-initial" . ((".flex-initial" :flex "0 1 auto")))
								 ("flex-none" . ((".flex-none" :flex "none")))))

(defvar *flex-grow* '(("grow" . ((".grow" :flex-grow "1")))
											("grow-0" . ((".grow-0" :flex-grow "0")))))


(defvar *flex-shrink* '(("shrink" . ((".shrink" :flex-shrink "1")))
											("shrink-0" . ((".shrink-0" :flex-shrink "0")))))

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

(defvar *grid-template-columns* '(("grid-cols-1" . ((".grid-cols-1" :grid-template-columns "repeat(1, minmax(0, 1fr))")))
																 ("grid-cols-2" . ((".grid-cols-2" :grid-template-columns "repeat(2, minmax(0, 1fr))")))
																 ("grid-cols-3" . ((".grid-cols-3" :grid-template-columns "repeat(3, minmax(0, 1fr))")))
																 ("grid-cols-4" . ((".grid-cols-4" :grid-template-columns "repeat(4, minmax(0, 1fr))")))
																 ("grid-cols-5" . ((".grid-cols-5" :grid-template-columns "repeat(5, minmax(0, 1fr))")))
																 ("grid-cols-6" . ((".grid-cols-6" :grid-template-columns "repeat(6, minmax(0, 1fr))")))
																 ("grid-cols-7" . ((".grid-cols-7" :grid-template-columns "repeat(7, minmax(0, 1fr))")))
																 ("grid-cols-8" . ((".grid-cols-8" :grid-template-columns "repeat(8, minmax(0, 1fr))")))
																 ("grid-cols-9" . ((".grid-cols-9" :grid-template-columns "repeat(9, minmax(0, 1fr))")))
																 ("grid-cols-10" . ((".grid-cols-10" :grid-template-columns "repeat(10, minmax(0, 1fr))")))
																 ("grid-cols-11" . ((".grid-cols-11" :grid-template-columns "repeat(11, minmax(0, 1fr))")))
																 ("grid-cols-12" . ((".grid-cols-12" :grid-template-columns "repeat(12, minmax(0, 1fr))")))
																 ("grid-cols-none" . ((".grid-cols-none" :grid-template-columns "none")))))


(defvar *grid-gap* (get-single :gap "gap-"))
(defvar *grid-gap-x* (get-single :column-gap "gap-x-"))
(defvar *grid-gap-y* (get-single :row-gap "gap-y-"))

(defvar *flexbox-grid* (append
												*flex-basis*
												*flex-direction*
												*flex-wrap*
												*flex*
												*flex-grow*
												*flex-shrink*
												*justify-content*
												*justify-items*
												*justify-self*
												*align-content*
												*align-items*
												*align-self*
												*place-content*
												*place-items*
												*place-self*

												*grid-template-columns*
												*grid-gap*
												*grid-gap-x*
												*grid-gap-y*
												))
