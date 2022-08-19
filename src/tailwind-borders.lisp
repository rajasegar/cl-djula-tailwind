(defpackage cl-djula-tailwind.borders
  (:use :cl
				:cl-djula-tailwind.colors)
  (:export :*borders*))

(in-package cl-djula-tailwind.borders)

(defvar *border-radius* '(
									("rounded-none" . ((".rounded-none" :border-radius "0px")))
									("rounded-sm" . ((".rounded-sm" :border-radius "0.125rem")))
									("rounded" . ((".rounded" :border-radius "0.25rem")))
									("rounded-md" . ((".rounded-md" :border-radius "0.375rem")))
									("rounded-lg" . ((".rounded-lg" :border-radius "0.5rem")))
									("rounded-xl" . ((".rounded-xl" :border-radius "0.75rem")))
									("rounded-2xl" . ((".rounded-2xl" :border-radius "1rem")))
									("rounded-3xl" . ((".rounded-3xl" :border-radius "1.5rem")))
									("rounded-full" . ((".rounded-full" :border-radius "9999px")))
                    ))

(defvar *border-width* '(
												 ("border-0" . ((".border-0" :border-width "0px")))
												 ("border-2" . ((".border-2" :border-width "2px")))
												 ("border-4" . ((".border-4" :border-width "4px")))
												 ("border-8" . ((".border-8" :border-width "8px")))
												 ("border" . ((".border" :border-width "1px")))
												 ("border-x-0" . ((".border-x-0" :border-left-width "0px" :border-right-width "0px")))
												 ("border-x-2" . ((".border-x-2" :border-left-width "2px" :border-right-width "2px")))
												 ("border-x-4" . ((".border-x-4" :border-left-width "4px" :border-right-width "4px")))
												 ("border-x-8" . ((".border-x-8" :border-left-width "8px" :border-right-width "8px")))
												 ("border-x" . ((".border-x" :border-left-width "1px" :border-right-width "1px")))
												 ("border-y-0" . ((".border-y-0" :border-top-width "0px" :border-bottom-width "0px")))
												 ("border-y-2" . ((".border-y-2" :border-top-width "2px" :border-bottom-width "2px")))
												 ("border-y-4" . ((".border-y-4" :border-top-width "4px" :border-bottom-width "4px")))
												 ("border-y-8" . ((".border-y-8" :border-top-width "8px" :border-bottom-width "8px")))
												 ("border-y" . ((".border-y" :border-top-width "1px" :border-bottom-width "1px")))
												 ("border-t-0" . ((".border-t-0" :border-top-width "0px" )))
												 ("border-t-2" . ((".border-t-2" :border-top-width "2px" )))
												 ("border-t-4" . ((".border-t-4" :border-top-width "4px" )))
												 ("border-t-8" . ((".border-t-8" :border-top-width "8px" )))
												 ("border-t" . ((".border-t" :border-top-width "1px" )))
												 ("border-r-0" . ((".border-r-0" :border-right-width "0px" )))
												 ("border-r-2" . ((".border-r-2" :border-right-width "2px" )))
												 ("border-r-4" . ((".border-r-4" :border-right-width "4px" )))
												 ("border-r-8" . ((".border-r-8" :border-right-width "8px" )))
												 ("border-r" . ((".border-r" :border-right-width "1px" )))
												 ("border-b-0" . ((".border-b-0" :border-bottom-width "0px" )))
												 ("border-b-2" . ((".border-b-2" :border-bottom-width "2px" )))
												 ("border-b-4" . ((".border-b-4" :border-bottom-width "4px" )))
												 ("border-b-8" . ((".border-b-8" :border-bottom-width "8px" )))
												 ("border-b" . ((".border-b" :border-bottom-width "1px" )))
												 ("border-l-0" . ((".border-l-0" :border-left-width "0px" )))
												 ("border-l-2" . ((".border-l-2" :border-left-width "2px" )))
												 ("border-l-4" . ((".border-l-4" :border-left-width "4px" )))
												 ("border-l-8" . ((".border-l-8" :border-left-width "8px" )))
												 ("border-l" . ((".border-l" :border-left-width "1px" )))))

(defun get-list-key (prefix key)
	(ppcre:regex-replace-all "\\\"" (concatenate 'string prefix (write-to-string key)) ""))

(defun get-list-value (prefix key value)
	(list (ppcre:regex-replace-all "\\\"" (concatenate 'string "." prefix (write-to-string key)) "") :border-color value))

(defun get-list (prefix key value)
	`(,(get-list-key prefix key) . (,(get-list-value prefix key value))))

(defun collect-colors (colors prefix)
	(loop for i in colors
				collect (get-list prefix (car i) (cdr i))))

(defvar *border-others* '(("border-inherit" . ((".border-inherit" :border-color "inherit")))
											("border-current" . ((".border-current" :border-color "currentColor")))
											("border-transparent" . ((".border-transparent" :border-color "transparent")))
											("border-black" . ((".border-black" :border-color "rgb(0 0 0)")))
											("border-white" . ((".border-white" :border-color "rgb(255 255 255)")))))

(defvar *border-color-slate* (collect-colors *slate-colors* "border-slate-"))
(defvar *border-color-gray* (collect-colors *gray-colors* "border-gray-"))
(defvar *border-color-zinc* (collect-colors *zinc-colors* "border-zinc-"))
(defvar *border-color-neutral* (collect-colors *neutral-colors* "border-neutral-"))
(defvar *border-color-stone* (collect-colors *stone-colors* "border-stone-"))
(defvar *border-color-red* (collect-colors *red-colors* "border-red-"))
(defvar *border-color-orange* (collect-colors *orange-colors* "border-orange-"))
(defvar *border-color-amber* (collect-colors *amber-colors* "border-amber-"))
(defvar *border-color-yellow* (collect-colors *yellow-colors* "border-yellow-"))
(defvar *border-color-lime* (collect-colors *lime-colors* "border-lime-"))
(defvar *border-color-green* (collect-colors *green-colors* "border-green-"))
(defvar *border-color-emerald* (collect-colors *emerald-colors* "border-emerald-"))
(defvar *border-color-teal* (collect-colors *teal-colors* "border-teal-"))
(defvar *border-color-cyan* (collect-colors *cyan-colors* "border-cyan-"))
(defvar *border-color-sky* (collect-colors *sky-colors* "border-sky-"))
(defvar *border-color-blue* (collect-colors *blue-colors* "border-blue-"))
(defvar *border-color-indigo* (collect-colors *indigo-colors* "border-indigo-"))
(defvar *border-color-violet* (collect-colors *violet-colors* "border-violet-"))
(defvar *border-color-purple* (collect-colors *purple-colors* "border-purple-"))
(defvar *border-color-fuchsia* (collect-colors *fuchsia-colors* "border-fuchsia-"))
(defvar *border-color-pink* (collect-colors *pink-colors* "border-pink-"))
(defvar *border-color-rose* (collect-colors *rose-colors* "border-rose-"))

(defvar *ring-width* '(
                       ("ring-0" . ((".ring-0" :box-shadow "var(--tw-ring-inset) 0 0 0 calc(0px + var(--tw-ring-offset-width)) var(--tw-ring-color)")))
                       ))

(defvar *border-style* '(("border-solid" . ((".border-solid" :border-style "solid")))
                         ("border-dashed" . ((".border-dashed" :border-style "dashed")))
                         ("border-dotted" . ((".border-dotted" :border-style "dotted")))
                         ("border-double" . ((".border-double" :border-style "double")))
                         ("border-hidden" . ((".border-hidden" :border-style "hidden")))
                         ("border-none" . ((".border-none" :border-style "none")))))

(defvar *borders* (append *border-radius*
													*border-width*

											;; border colors
											*border-color-slate*
											*border-color-gray*
											*border-color-zinc*
											*border-color-neutral*
											*border-color-stone*
											*border-color-red*
											*border-color-orange*
											*border-color-amber*
											*border-color-yellow*
											*border-color-lime*
											*border-color-green*
											*border-color-emerald*
											*border-color-teal*
											*border-color-cyan*
											*border-color-sky*
											*border-color-blue*
											*border-color-indigo*
											*border-color-violet*
											*border-color-purple*
											*border-color-fuchsia*
											*border-color-pink*
											*border-color-rose*

											*border-others*
                      *border-style*

                      *ring-width*
													))
