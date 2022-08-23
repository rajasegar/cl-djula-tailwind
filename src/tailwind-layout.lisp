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

(defvar *floats* '(("float-right" . ((".float-right" :float "right")))
									 ("float-left" . ((".float-left" :float "left")))
									 ("float-none" . ((".float-none" :float "none")))))

(defvar *clear* '(("clear-left" . ((".clear-left" :clear "left")))
									("clear-right" . ((".clear-right" :clear "right")))
									("clear-both" . ((".clear-both" :clear "both")))
									("clear-none" . ((".clear-none" :clear "none")))))

(defvar *isolation* '(("isolate" . ((".isolate" :isolation "isolate")))
											("isolation-auto" . ((".isolatation-auto" :isolation "auto")))))

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

(defvar *break-after* '(("break-after-auto" . ((".break-after-auto" :break-after "auto")))
												("break-after-avoid" . ((".break-after-avoid" :break-after "avoid")))
												("break-after-all" . ((".break-after-all" :break-after "all")))
												("break-after-avoid-page" . ((".break-after-avoid-page" :break-after "avoid-page")))
												("break-after-page" . ((".break-after-page" :break-after "page")))
												("break-after-left" . ((".break-after-left" :break-after "left")))
												("break-after-right" . ((".break-after-right" :break-after "right")))
												("break-after-column" . ((".break-after-column" :break-after "column")))))

(defvar *break-before* '(("break-before-auto" . ((".break-before-auto" :break-before "auto")))
												("break-before-avoid" . ((".break-before-avoid" :break-before "avoid")))
												("break-before-all" . ((".break-before-all" :break-before "all")))
												("break-before-avoid-page" . ((".break-before-avoid-page" :break-before "avoid-page")))
												("break-before-page" . ((".break-before-page" :break-before "page")))
												("break-before-left" . ((".break-before-left" :break-before "left")))
												("break-before-right" . ((".break-before-right" :break-before "right")))
												("break-before-column" . ((".break-before-column" :break-before "column")))))

(defvar *break-inside* '(("break-inside-auto" . ((".break-inside-auto" :break-inside "auto")))
												 ("break-inside-avoid" . ((".break-inside-avoid" :break-inside "avoid")))
												 ("break-inside-avoid-page" . ((".break-inside-avoid-page" :break-inside "avoid-page")))
												 ("break-inside-avoid-column" . ((".break-inside-avoid-column" :break-inside "avoid-column")))))

(defvar *box-decoration-break* '(("box-decoration-clone" . ((".box-decoration-clone" :box-decoration-break "clone")))
																 ("box-decoration-slice" . ((".box-decoration-slice" :box-decoration-break "slice")))))

(defvar *box-sizing* '(("box-border" . ((".box-border" :box-sizing "border-box")))
											 ("box-content" . ((".box-content" :box-sizing "border-content")))))

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

(defvar *object-fit* '(("object-contain" . ((".object-contain" :object-fit "contain")))
											 ("object-cover" . ((".object-cover" :object-fit "cover")))
											 ("object-fill" . ((".object-fill" :object-fit "fill")))
											 ("object-none" . ((".object-none" :object-fit "none")))
											 ("object-scale-down" . ((".object-scale-down" :object-fit "scale-down")))))

(defvar *object-position* '(("object-bottom" . ((".object-bottom" :object-position "bottom")))
														("object-center" . ((".object-center" :object-position "center")))
														("object-left" . ((".object-left" :object-position "left")))
														("object-left-bottom" . ((".object-left-bottom" :object-position "left bottom")))
														("object-left-top" . ((".object-left-top" :object-position "left top")))
														("object-right" . ((".object-right" :object-position "right")))
														("object-right-bottom" . ((".object-right-bottom" :object-position "right bottom")))
														("object-right-top" . ((".object-right-top" :object-position "right top")))
														("object-top" . ((".object-top" :object-position "top")))))

(defvar *overscroll-behavior* '(("overscroll-auto" . ((".overscroll-auto" :overscroll-behavior "auto")))
																("overscroll-contain" . ((".overscroll-contain" :overscroll-behavior "contain")))
																("overscroll-none" . ((".overscroll-none" :overscroll-behavior "none")))
																("overscroll-y-auto" . ((".overscroll-y-auto" :overscroll-behavior-y "auto")))
																("overscroll-y-contain" . ((".overscroll-y-contain" :overscroll-behavior-y "contain")))
																("overscroll-y-none" . ((".overscroll-y-none" :overscroll-behavior-y "none")))
																("overscroll-x-auto" . ((".overscroll-x-auto" :overscroll-behavior-x "auto")))
																("overscroll-x-contain" . ((".overscroll-x-contain" :overscroll-behavior-x "contain")))
																("overscroll-x-none" . ((".overscroll-x-none" :overscroll-behavior-x "none")))))

(defvar *layout* (append
									*aspect-ratio*
									*display*
									*floats*
									*clear*
									*position*
									*visibility*
									*z-index*

									*top*
									*right*
									*bottom*
									*left*

									*inset*
									*inset-x*
									*inset-y*

									*columns*
									*break-after*
									*break-before*
									*break-inside*
									*box-decoration-break*
									*box-sizing*

									*isolation*
									*object-fit*
									*object-position*
									*overflow*
									*overscroll-behavior*
									))
