(defpackage cl-djula-tailwind.layout
  (:use :cl)
  (:export :*layout*))

(in-package cl-djula-tailwind.layout)

(defvar *aspect-ratio* '(("aspect-auto" . ((".aspect-auto" :aspect-ratio "auto")))
                         ("aspect-square" . ((".aspect-square" :aspect-ratio "1 / 1")))
                         ("aspect-video" . ((".aspect-video" :aspect-ratio "16 / 9")))))

(defvar *display* '(("block" . ((".block" :display "block")))
                    ("inline-block" . ((".inline-block" :display "inline-block")))
                    ("inline" . ((".inline" :display "inline")))
                    ("flex" . ((".flex" :display "flex")))
                    ("inline-flex" . ((".inline-flex" :display "inline-flex")))
                    ("table" . ((".table" :display "table")))
                    ("inline-table" . ((".inline-table" :display "inline-table")))
                    ("table-caption" . ((".table-caption" :display "table-caption")))
                    ("table-cell" . ((".table-cell" :display "table-cell")))
                    ("table-column" . ((".table-column" :display "table-column")))
                    ("table-column-group" . ((".table-column-group" :display "table-column-group")))
                    ("table-footer-group" . ((".table-footer-group" :display "table-footer-group")))
                    ("table-header-group" . ((".table-header-group" :display "table-header-group")))
                    ("table-row-group" . ((".table-row-group" :display "table-row-group")))
                    ("table-row" . ((".table-row" :display "table-row")))
                    ("flow-root" . ((".flow-root" :display "flow-root")))
                    ("grid" . ((".grid" :display "grid")))
                    ("inline-grid" . ((".inline-grid" :display "inline-grid")))
                    ("contents" . ((".contents" :display "contents")))
                    ("list-item" . ((".list-item" :display "list-item")))
                    ("hidden" . ((".hidden" :display "none")))))

(defvar *clear* '(("clear-left" . ((".clear-left" :clear "left")))
									("clear-right" . ((".clear-right" :clear "right")))
									("clear-both" . ((".clear-both" :clear "both")))
									("clear-none" . ((".clear-none" :clear "none")))))

(defvar *overflow* '(("overflow-auto" . ((".overflow-auto" :overflow "auto")))
										 ("overflow-hidden" . ((".overflow-hidden" :overflow "hidden")))
										 ("overflow-clip" . ((".overflow-clip" :overflow "clip")))
										 ("overflow-visible" . ((".overflow-visible" :overflow "visible")))
										 ("overflow-scroll" . ((".overflow-scroll" :overflow "scroll")))
										 ("overflow-x-auto" . ((".overflow-x-auto" :overflow-x "auto")))
										 ("overflow-y-auto" . ((".overflow-y-auto" :overflow-y "auto")))
										 ("overflow-x-hidden" . ((".overflow-x-hidden" :overflow-x "hidden")))
										 ("overflow-y-hidden" . ((".overflow-y-hidden" :overflow-y "hidden")))
										 ("overflow-x-clip" . ((".overflow-x-clip" :overflow-x "clip")))
										 ("overflow-y-clip" . ((".overflow-y-clip" :overflow-y "clip")))
										 ("overflow-x-visible" . ((".overflow-x-visible" :overflow-x "visible")))
										 ("overflow-y-visible" . ((".overflow-y-visible" :overflow-y "visible")))
										 ("overflow-x-scroll" . ((".overflow-x-scroll" :overflow-x "scroll")))
										 ("overflow-y-scroll" . ((".overflow-y-scroll" :overflow-y "scroll")))))

(defvar *position* '(("static" . ((".static" :position "static")))
										 ("fixed" . ((".fixed" :position "fixed")))
										 ("absolute" . ((".absolute" :position "absolute")))
										 ("relative" . ((".relative" :position "relative")))
										 ("sticky" . ((".sticky" :position "sticky")))))

(defvar *visibility* '(("visible" . ((".visible" :visibility "visible")))
											 ("invisible" . ((".invisible" :visibility "hidden")))))

(defvar *z-index* '(
										("z-0" . ((".z-0" :z-index "0")))
										("z-10" . ((".z-10" :z-index "10")))
										("z-20" . ((".z-20" :z-index "20")))
										("z-30" . ((".z-30" :z-index "30")))
										("z-40" . ((".z-40" :z-index "40")))
										("z-50" . ((".z-50" :z-index "50")))
										("z-auto" . ((".z-auto" :z-index "auto")))

										))
(defvar *layout* (append
									*aspect-ratio*
									*display*
									*clear*
									*position*
									*visibility*
									*z-index*
									))
