(defpackage cl-djula-tailwind.typography
  (:use :cl
				:cl-djula-tailwind.colors)
  (:export :*typography*))

(in-package cl-djula-tailwind.typography)

(defvar *font-sizes* '(("text-xs" . ((".text-xs" :font-size "0.75rem" :line-height "1rem")))
											 ("text-sm" . ((".text-sm" :font-size "0.875rem" :line-height "1.25rem")))
											 ("text-base" . ((".text-base" :font-size "1rem" :line-height "1.5rem")))
											 ("text-lg" . ((".text-lg" :font-size "1.125rem" :line-height "1.75rem")))
											 ("text-xl" . ((".text-xl" :font-size "1.25rem" :line-height "1.75rem")))
                       ("text-2xl" . ((".text-2xl" :font-size "1.5rem" :line-height "2rem")))
                       ("text-3xl" . ((".text-3xl" :font-size "1.875rem" :line-height "2.25rem")))
                       ("text-4xl" . ((".text-4xl" :font-size "2.25rem" :line-height "2.5rem")))
                       ("text-5xl" . ((".text-5xl" :font-size "3rem" :line-height "1")))
                       ("text-6xl" . ((".text-6xl" :font-size "3.75rem" :line-height "1")))
                       ("text-7xl" . ((".text-7xl" :font-size "4.5rem" :line-height "1")))
                       ("text-8xl" . ((".text-8xl" :font-size "6rem" :line-height "1")))
                       ("text-9xl" . ((".text-9xl" :font-size "8rem" :line-height "1")))))


(defvar *font-styles* '(("italic" . ((".italic" :font-style "italic")))
                         ("non-italic" . ((".non-italic" :font-style "normal")))))

(defvar *font-weights* '(("font-thin" . ((".fond-thin" :font-weight "100")))
												 ("font-extralight" . ((".fond-extralight" :font-weight "200")))
												 ("font-light" . ((".fond-light" :font-weight "300")))
                         ("font-normal" . ((".font-normal" :font-weight "400")))
                         ("font-medium" . ((".font-normal" :font-weight "500")))
                         ("font-semibold" . ((".font-semibold" :font-weight "600")))
												 ("font-bold" . ((".fond-bold" :font-weight "700")))
												 ("font-extrabold" . ((".fond-extrabold" :font-weight "800")))
												 ("font-black" . ((".fond-black" :font-weight "900")))))

(defvar *font-family* '(
												("font-sans" . ((".font-sans" :font-family "ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, \"Helvetica Neue\", Arial, \"Noto Sans\", sans-serif, \"Apple Color Emoji\", \"Segoe UI Emoji\", \"Segoe UI Symbol\", \"Noto Color Emoji\"")))
												("font-serif" . ((".font-serif" :font-family "ui-serif, Georgia, Cambria, \"Times New Roman\", Times, serif")))
												("font-mono" . ((".font-mono" :font-family "ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, \"Liberation Mono\", \"Courier New\", monospace")))
												 ))

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
											*font-family*

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
