(defpackage cl-djula-tailwind.svg
  (:use :cl
				:cl-djula-tailwind.colors)
  (:export :*svg*))

(in-package cl-djula-tailwind.svg)


(defun get-list-key (prefix key)
	(ppcre:regex-replace-all "\\\"" (concatenate 'string prefix (write-to-string key)) ""))

(defun get-list-value (prefix key value)
	(list (ppcre:regex-replace-all "\\\"" (concatenate 'string "." prefix (write-to-string key)) "") :fill value))

(defun get-stroke-value (prefix key value)
	(list (ppcre:regex-replace-all "\\\"" (concatenate 'string "." prefix (write-to-string key)) "") :stroke value))

(defun get-list (prefix key value)
	`(,(get-list-key prefix key) . (,(get-list-value prefix key value))))

(defun get-stroke-list (prefix key value)
	`(,(get-list-key prefix key) . (,(get-stroke-value prefix key value))))

(defun collect-colors (colors prefix)
	(loop for i in colors
				collect (get-list prefix (car i) (cdr i))))

(defun collect-stroke-colors (colors prefix)
	(loop for i in colors
				collect (get-stroke-list prefix (car i) (cdr i))))

(defvar *fill-others* '(("fill-inherit" . ((".fill-inherit" :fill "inherit")))
											("fill-current" . ((".fill-current" :fill "currentColor")))
											("fill-transparent" . ((".fill-transparent" :fill "transparent")))
											("fill-black" . ((".fill-black" :fill "rgb(0 0 0)")))
											("fill-white" . ((".fill-white" :fill "rgb(255 255 255)")))))

(defvar *stroke-others* '(("stroke-inherit" . ((".stroke-inherit" :stroke "inherit")))
											("stroke-current" . ((".stroke-current" :stroke "currentColor")))
											("stroke-transparent" . ((".stroke-transparent" :stroke "transparent")))
											("stroke-black" . ((".stroke-black" :stroke "rgb(0 0 0)")))
											("stroke-white" . ((".stroke-white" :stroke "rgb(255 255 255)")))))

(defvar *fill-color-slate* (collect-colors *slate-colors* "fill-slate-"))
(defvar *fill-color-gray* (collect-colors *gray-colors* "fill-gray-"))
(defvar *fill-color-zinc* (collect-colors *zinc-colors* "fill-zinc-"))
(defvar *fill-color-neutral* (collect-colors *neutral-colors* "fill-neutral-"))
(defvar *fill-color-stone* (collect-colors *stone-colors* "fill-stone-"))
(defvar *fill-color-red* (collect-colors *red-colors* "fill-red-"))
(defvar *fill-color-orange* (collect-colors *orange-colors* "fill-orange-"))
(defvar *fill-color-amber* (collect-colors *amber-colors* "fill-amber-"))
(defvar *fill-color-yellow* (collect-colors *yellow-colors* "fill-yellow-"))
(defvar *fill-color-lime* (collect-colors *lime-colors* "fill-lime-"))
(defvar *fill-color-green* (collect-colors *green-colors* "fill-green-"))
(defvar *fill-color-emerald* (collect-colors *emerald-colors* "fill-emerald-"))
(defvar *fill-color-teal* (collect-colors *teal-colors* "fill-teal-"))
(defvar *fill-color-cyan* (collect-colors *cyan-colors* "fill-cyan-"))
(defvar *fill-color-sky* (collect-colors *sky-colors* "fill-sky-"))
(defvar *fill-color-blue* (collect-colors *blue-colors* "fill-blue-"))
(defvar *fill-color-indigo* (collect-colors *indigo-colors* "fill-indigo-"))
(defvar *fill-color-violet* (collect-colors *violet-colors* "fill-violet-"))
(defvar *fill-color-purple* (collect-colors *purple-colors* "fill-purple-"))
(defvar *fill-color-fuchsia* (collect-colors *fuchsia-colors* "fill-fuchsia-"))
(defvar *fill-color-pink* (collect-colors *pink-colors* "fill-pink-"))
(defvar *fill-color-rose* (collect-colors *rose-colors* "fill-rose-"))


(defvar *stroke-color-slate* (collect-stroke-colors *slate-colors* "stroke-slate-"))
(defvar *stroke-color-gray* (collect-stroke-colors *gray-colors* "stroke-gray-"))
(defvar *stroke-color-zinc* (collect-stroke-colors *zinc-colors* "stroke-zinc-"))
(defvar *stroke-color-neutral* (collect-stroke-colors *neutral-colors* "stroke-neutral-"))
(defvar *stroke-color-stone* (collect-stroke-colors *stone-colors* "stroke-stone-"))
(defvar *stroke-color-red* (collect-stroke-colors *red-colors* "stroke-red-"))
(defvar *stroke-color-orange* (collect-stroke-colors *orange-colors* "stroke-orange-"))
(defvar *stroke-color-amber* (collect-stroke-colors *amber-colors* "stroke-amber-"))
(defvar *stroke-color-yellow* (collect-stroke-colors *yellow-colors* "stroke-yellow-"))
(defvar *stroke-color-lime* (collect-stroke-colors *lime-colors* "stroke-lime-"))
(defvar *stroke-color-green* (collect-stroke-colors *green-colors* "stroke-green-"))
(defvar *stroke-color-emerald* (collect-stroke-colors *emerald-colors* "stroke-emerald-"))
(defvar *stroke-color-teal* (collect-stroke-colors *teal-colors* "stroke-teal-"))
(defvar *stroke-color-cyan* (collect-stroke-colors *cyan-colors* "stroke-cyan-"))
(defvar *stroke-color-sky* (collect-stroke-colors *sky-colors* "stroke-sky-"))
(defvar *stroke-color-blue* (collect-stroke-colors *blue-colors* "stroke-blue-"))
(defvar *stroke-color-indigo* (collect-stroke-colors *indigo-colors* "stroke-indigo-"))
(defvar *stroke-color-violet* (collect-stroke-colors *violet-colors* "stroke-violet-"))
(defvar *stroke-color-purple* (collect-stroke-colors *purple-colors* "stroke-purple-"))
(defvar *stroke-color-fuchsia* (collect-stroke-colors *fuchsia-colors* "stroke-fuchsia-"))
(defvar *stroke-color-pink* (collect-stroke-colors *pink-colors* "stroke-pink-"))
(defvar *stroke-color-rose* (collect-stroke-colors *rose-colors* "stroke-rose-"))

(defvar *stroke-width* '(("stroke-0" . ((".stroke-0" :stroke-width 0)))
                         ("stroke-1" . ((".stroke-1" :stroke-width 1)))
                         ("stroke-2" . ((".stroke-2" :stroke-width 2)))))

(defvar *svg* (append

											;; fill colors
											*fill-color-slate*
											*fill-color-gray*
											*fill-color-zinc*
											*fill-color-neutral*
											*fill-color-stone*
											*fill-color-red*
											*fill-color-orange*
											*fill-color-amber*
											*fill-color-yellow*
											*fill-color-lime*
											*fill-color-green*
											*fill-color-emerald*
											*fill-color-teal*
											*fill-color-cyan*
											*fill-color-sky*
											*fill-color-blue*
											*fill-color-indigo*
											*fill-color-violet*
											*fill-color-purple*
											*fill-color-fuchsia*
											*fill-color-pink*
											*fill-color-rose*

											*fill-others*

											;; stroke colors
											*stroke-color-slate*
											*stroke-color-gray*
											*stroke-color-zinc*
											*stroke-color-neutral*
											*stroke-color-stone*
											*stroke-color-red*
											*stroke-color-orange*
											*stroke-color-amber*
											*stroke-color-yellow*
											*stroke-color-lime*
											*stroke-color-green*
											*stroke-color-emerald*
											*stroke-color-teal*
											*stroke-color-cyan*
											*stroke-color-sky*
											*stroke-color-blue*
											*stroke-color-indigo*
											*stroke-color-violet*
											*stroke-color-purple*
											*stroke-color-fuchsia*
											*stroke-color-pink*
											*stroke-color-rose*

											*stroke-others*

                      *stroke-width*
											))
