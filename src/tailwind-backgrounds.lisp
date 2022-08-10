(defpackage cl-djula-tailwind.backgrounds
  (:use :cl
				:cl-djula-tailwind.colors)
  (:export :*backgrounds*))

(in-package cl-djula-tailwind.backgrounds)


(defun get-list-key (prefix key)
	(ppcre:regex-replace-all "\\\"" (concatenate 'string prefix (write-to-string key)) ""))

(defun get-list-value (prefix key value)
	(list (ppcre:regex-replace-all "\\\"" (concatenate 'string "." prefix (write-to-string key)) "") :background-color value))

(defun get-list (prefix key value)
	`(,(get-list-key prefix key) . (,(get-list-value prefix key value))))

(defun collect-colors (colors prefix)
	(loop for i in colors
				collect (get-list prefix (car i) (cdr i))))

(defvar *bg-color-slate* (collect-colors *slate-colors* "bg-slate-"))
(defvar *bg-color-gray* (collect-colors *gray-colors* "bg-gray-"))
(defvar *bg-color-zinc* (collect-colors *zinc-colors* "bg-zinc-"))
(defvar *bg-color-neutral* (collect-colors *neutral-colors* "bg-neutral-"))
(defvar *bg-color-stone* (collect-colors *stone-colors* "bg-stone-"))
(defvar *bg-color-red* (collect-colors *red-colors* "bg-red-"))
(defvar *bg-color-orange* (collect-colors *orange-colors* "bg-orange-"))
(defvar *bg-color-amber* (collect-colors *amber-colors* "bg-amber-"))
(defvar *bg-color-yellow* (collect-colors *yellow-colors* "bg-yellow-"))
(defvar *bg-color-lime* (collect-colors *lime-colors* "bg-lime-"))
(defvar *bg-color-green* (collect-colors *green-colors* "bg-green-"))
(defvar *bg-color-emerald* (collect-colors *emerald-colors* "bg-emerald-"))
(defvar *bg-color-teal* (collect-colors *teal-colors* "bg-teal-"))
(defvar *bg-color-cyan* (collect-colors *cyan-colors* "bg-cyan-"))
(defvar *bg-color-sky* (collect-colors *sky-colors* "bg-sky-"))
(defvar *bg-color-blue* (collect-colors *blue-colors* "bg-blue-"))
(defvar *bg-color-indigo* (collect-colors *indigo-colors* "bg-indigo-"))
(defvar *bg-color-violet* (collect-colors *violet-colors* "bg-violet-"))
(defvar *bg-color-purple* (collect-colors *purple-colors* "bg-purple-"))
(defvar *bg-color-fuchsia* (collect-colors *fuchsia-colors* "bg-fuchsia-"))
(defvar *bg-color-pink* (collect-colors *pink-colors* "bg-pink-"))
(defvar *bg-color-rose* (collect-colors *rose-colors* "bg-rose-"))


(defvar *backgrounds* (append

											;; background colors
											*bg-color-slate*
											*bg-color-gray*
											*bg-color-zinc*
											*bg-color-neutral*
											*bg-color-stone*
											*bg-color-red*
											*bg-color-orange*
											*bg-color-amber*
											*bg-color-yellow*
											*bg-color-lime*
											*bg-color-green*
											*bg-color-emerald*
											*bg-color-teal*
											*bg-color-cyan*
											*bg-color-sky*
											*bg-color-blue*
											*bg-color-indigo*
											*bg-color-violet*
											*bg-color-purple*
											*bg-color-fuchsia*
											*bg-color-pink*
											*bg-color-rose*
											))
