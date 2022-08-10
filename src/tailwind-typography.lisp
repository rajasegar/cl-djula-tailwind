(defpackage cl-djula-tailwind.typography
  (:use :cl
				:cl-djula-tailwind.colors)
  (:export :*typography*))

(in-package cl-djula-tailwind.typography)

(defvar *font-sizes* '(("text-xl" . ((".text-xl" :font-size "1.25rem" :line-height "1.75rem")))
                         ("text-2xl" . ((".text-2xl" :font-size "1.5rem" :line-height "2rem")))
                    ))


(defvar *font-styles* '(("italic" . ((".italic" :font-style "italic")))
                         ("non-italic" . ((".non-italic" :font-style "normal")))))

(defvar *font-weights* '(("font-bold" . ((".fond-bold" :font-weight "700")))
                         ("font-normal" . ((".font-normal" :font-style "400")))))

(defvar *text-color* '(
											 ("text-inherit" . ((".text-inherit" :color "inherit")))
											 ("text-current" . ((".text-current" :color "currentColor")))
											 ("text-transparent" . ((".text-transparent" :color "transparent")))
											 ("text-black" . ((".text-black" :color "rgb(0,0,0)")))
											 ("text-white" . ((".text-white" :color "rgb(255,255,255)")))
											 ))

(defun get-list-key (prefix key)
	(ppcre:regex-replace-all "\\\"" (concatenate 'string prefix (write-to-string key)) ""))

(defun get-list-value (prefix key value)
	(list (ppcre:regex-replace-all "\\\"" (concatenate 'string "." prefix (write-to-string key)) "") :color value))

(defun get-list (prefix key value)
	`(,(get-list-key prefix key) . (,(get-list-value prefix key value))))

(defun collect-colors (colors prefix)
	(loop for i in colors
				collect (get-list prefix (car i) (cdr i))))


(defvar *text-color-slate* (collect-colors *slate-colors* "text-slate-"))
(defvar *text-color-gray* (collect-colors *gray-colors* "text-gray-"))
(defvar *text-color-zinc* (collect-colors *zinc-colors* "text-zinc-"))
(defvar *text-color-neutral* (collect-colors *neutral-colors* "text-neutral-"))
(defvar *text-color-stone* (collect-colors *stone-colors* "text-stone-"))
(defvar *text-color-red* (collect-colors *red-colors* "text-red-"))
(defvar *text-color-orange* (collect-colors *orange-colors* "text-orange-"))
(defvar *text-color-amber* (collect-colors *amber-colors* "text-amber-"))
(defvar *text-color-yellow* (collect-colors *yellow-colors* "text-yellow-"))
(defvar *text-color-lime* (collect-colors *lime-colors* "text-lime-"))
(defvar *text-color-green* (collect-colors *green-colors* "text-green-"))
(defvar *text-color-emerald* (collect-colors *emerald-colors* "text-emerald-"))
(defvar *text-color-teal* (collect-colors *teal-colors* "text-teal-"))
(defvar *text-color-cyan* (collect-colors *cyan-colors* "text-cyan-"))
(defvar *text-color-sky* (collect-colors *sky-colors* "text-sky-"))
(defvar *text-color-blue* (collect-colors *blue-colors* "text-blue-"))
(defvar *text-color-indigo* (collect-colors *indigo-colors* "text-indigo-"))
(defvar *text-color-violet* (collect-colors *violet-colors* "text-violet-"))
(defvar *text-color-purple* (collect-colors *purple-colors* "text-purple-"))
(defvar *text-color-fuchsia* (collect-colors *fuchsia-colors* "text-fuchsia-"))
(defvar *text-color-pink* (collect-colors *pink-colors* "text-pink-"))
(defvar *text-color-rose* (collect-colors *rose-colors* "text-rose-"))

(defvar *typography* (append
											*font-sizes*
											*font-styles*
											*font-weights*

											;; text colors
											*text-color-slate*
											*text-color-gray*
											*text-color-zinc*
											*text-color-neutral*
											*text-color-stone*
											*text-color-red*
											*text-color-orange*
											*text-color-amber*
											*text-color-yellow*
											*text-color-lime*
											*text-color-green*
											*text-color-emerald*
											*text-color-teal*
											*text-color-cyan*
											*text-color-sky*
											*text-color-blue*
											*text-color-indigo*
											*text-color-violet*
											*text-color-purple*
											*text-color-fuchsia*
											*text-color-pink*
											*text-color-rose*

											))
