(defpackage cl-djula-tailwind.layout
  (:use :cl
				:cl-djula-tailwind.utils)
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
										("z-auto" . ((".z-auto" :z-index "auto")))))

(defvar *columns* '(("columns-1" . ((".columns-1" :columns 1)))
                    ("columns-2" . ((".columns-2" :columns 2)))
                    ("columns-3" . ((".columns-3" :columns 3)))
                    ("columns-4" . ((".columns-4" :columns 4)))
                    ("columns-5" . ((".columns-5" :columns 5)))
                    ("columns-6" . ((".columns-6" :columns 6)))
                    ("columns-7" . ((".columns-7" :columns 7)))
                    ("columns-8" . ((".columns-8" :columns 8)))
                    ("columns-9" . ((".columns-9" :columns 9)))
                    ("columns-10" . ((".columns-10" :columns 10)))
                    ("columns-11" . ((".columns-11" :columns 11)))
                    ("columns-12" . ((".columns-12" :columns 12)))
                    ("columns-auto" . ((".columns-auto" :columns auto)))
                    ("columns-3xs" . ((".columns-3xs" :columns "16rem")))
                    ("columns-2xs" . ((".columns-2xs" :columns "18rem")))
                    ("columns-xs" . ((".columns-xs" :columns "20rem")))
                    ("columns-sm" . ((".columns-sm" :columns "24rem")))
                    ("columns-md" . ((".columns-md" :columns "28rem")))
                    ("columns-lg" . ((".columns-lg" :columns "32rem")))
                    ("columns-xl" . ((".columns-xl" :columns "36rem")))
                    ("columns-2xl" . ((".columns-2xl" :columns "42rem")))
                    ("columns-3xl" . ((".columns-3xl" :columns "48rem")))
                    ("columns-4xl" . ((".columns-4xl" :columns "56rem")))
                    ("columns-5xl" . ((".columns-5xl" :columns "64rem")))
                    ("columns-6xl" . ((".columns-6xl" :columns "72rem")))
                    ("columns-7xl" . ((".columns-7xl" :columns "80rem")))))

(defvar *overflow* '(
										 ("overflow-auto" . ((".overflow-auto" :overflow "auto")))
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


(defvar *top* (get-single :top "top-"))
(defvar *right* (get-single :right "right-"))
(defvar *bottom* (get-single :bottom "bottom-"))
(defvar *left* (get-single :left "left-"))


(defvar *inset-x* (get-double :left :right "inset-x-"))
(defvar *inset-y* (get-double :top :bottom "inset-y-"))
(defvar *inset* (get-quadruple :top :right :bottom :left "inset-"))

(defvar *layout* (append
									*aspect-ratio*
									*display*
									*clear*
									*position*
									*visibility*
									*z-index*
									*overflow*

									*top*
									*right*
									*bottom*
									*left*

									*inset*
									*inset-x*
									*inset-y*
									))
