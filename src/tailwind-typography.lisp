(defpackage cl-djula-tailwind.typography
  (:use :cl
	 :cl-djula-tailwind.colors
	 :cl-djula-tailwind.utils)
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
												("font-sans" . ((".font-sans" :font-family "ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, 'Noto Sans', sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji'")))
												("font-serif" . ((".font-serif" :font-family "ui-serif, Georgia, Cambria, 'Times New Roman', Times, serif")))
												("font-mono" . ((".font-mono" :font-family "ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, 'Liberation Mono', 'Courier New', monospace")))
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

(defvar *text-align* '(("text-left" . ((".text-left" :text-align "left")))
											 ("text-center" . ((".text-center" :text-align "center")))
											 ("text-right" . ((".text-right" :text-align "right")))
											 ("text-justify" . ((".text-justify" :text-align "justify")))
											 ("text-start" . ((".text-start" :text-align "start")))
											 ("text-end" . ((".text-end" :text-align "end")))))

(defvar *line-height* '(("leading-3" . ((".leading-3" :line-height ".75rem")))
												("leading-4" . ((".leading-4" :line-height "1rem")))
												("leading-5" . ((".leading-5" :line-height "1.25rem")))
												("leading-6" . ((".leading-6" :line-height "1.5rem")))
												("leading-7" . ((".leading-7" :line-height "1.75rem")))
												("leading-8" . ((".leading-8" :line-height "2rem")))
												("leading-9" . ((".leading-9" :line-height "2.25rem")))
												("leading-10" . ((".leading-10" :line-height "2.5rem")))
												("leading-none" . ((".leading-none" :line-height "1")))
												("leading-tight" . ((".leading-tight" :line-height "1.25")))
												("leading-snug" . ((".leading-snug" :line-height "1.375")))
												("leading-normal" . ((".leading-normal" :line-height "1.5")))
												("leading-relaxed" . ((".leading-relaxed" :line-height "1.625")))
												("leading-loose" . ((".leading-loose" :line-height "2")))))

(defvar *list-style-type* '(("list-none" . (".list-none" :list-style-type "none"))
														("list-disc" . (".list-disc" :list-style-type "disc"))
														("list-decimal" . (".list-decimal" :list-style-type "decimal"))))

(defvar *list-style-position* '(("list-inside" . ((".list-inside" :list-style-position "inside")))
																("list-outside" . ((".list-outside" :list-style-position "outside")))))

(defvar *text-decoration* '(("underline" . ((".underline" :text-decoration-line "underline")))
														("overline" . ((".overline" :text-decoration-line "overline")))
														("line-through" . ((".line-through" :text-decoration-line "line-through")))
														("no-underline" . ((".no-underline" :text-decoration-line "none")))))

(defvar *text-decoration-style* '(("decoration-solid" . ((".decoration-solid" :text-decoration-style "solid")))
																	("decoration-double" . ((".decoration-double" :text-decoration-style "double")))
																	("decoration-dotted" . ((".decoration-dotted" :text-decoration-style "dotted")))
																	("decoration-dashed" . ((".decoration-dashed" :text-decoration-style "dashed")))
																	("decoration-wavy" . ((".decoration-wavy" :text-decoration-style "wavy")))))

(defvar *text-decoration-thickness* '(("decoration-auto" . ((".decoration-auto" :text-decoration-thickness "auto")))
																			("decoration-from-font" . ((".decoration-from-font" :text-decoration-thickness "from-font")))
																			("decoration-0" . ((".decoration-0" :text-decoration-thickness "0px")))
																			("decoration-1" . ((".decoration-1" :text-decoration-thickness "1px")))
																			("decoration-2" . ((".decoration-2" :text-decoration-thickness "2px")))
																			("decoration-4" . ((".decoration-4" :text-decoration-thickness "4px")))
																			("decoration-8" . ((".decoration-8" :text-decoration-thickness "8px")))))

(defvar *text-transform* '(("uppercase" . ((".uppercase" :text-transform "uppercase")))
													 ("lowercase" . ((".lowercase" :text-transform "lowercase")))
													 ("capitalize" . ((".capitalize" :text-transform "capitalize")))
													 ("normal-case" . ((".normal-case" :text-transform "none")))))

(defvar *text-overflow* '(("truncate" . ((".truncate" :overflow "hidden" :text-overflow "ellipsis" :white-space "no-wrap")))
													("text-ellipsis" . ((".text-ellipsis" :text-overflow "ellipsis")))
													("text-clip" . ((".text-clip" :text-overflow "clip")))))

(defvar *text-indent* (get-single :text-indent "indent-"))

(defvar *vertical-align* '(("align-baseline" . ((".align-baseline" :vertical-align "baseline")))
													("align-top" . ((".align-top" :vertical-align "top")))
													("align-middle" . ((".align-middle" :vertical-align "middle")))
													("align-bottom" . ((".align-bottom" :vertical-align "bottom")))
													("align-text-top" . ((".align-text-top" :vertical-align "text-top")))
													("align-text-bottom" . ((".align-text-bottom" :vertical-align "text-bottom")))
													("align-sub" . ((".align-sub" :vertical-align "sub")))
													("align-super" . ((".align-super" :vertical-align "super")))))

(defvar *whitespace* '(("whitespace-normal" . ((".whitespace-normal" :white-space "normal")))
											 ("whitespace-nowrap" . ((".whitespace-nowrap" :white-space "nowrap")))
											 ("whitespace-pre" . ((".whitespace-pre" :white-space "pre")))
											 ("whitespace-pre-line" . ((".whitespace-pre-line" :white-space "pre-line")))
											 ("whitespace-pre-wrap" . ((".whitespace-pre-wrap" :white-space "pre-wrap")))))

(defvar *word-break* '(("break-normal" . ((".break-normal" :overflow-wrap "normal" :word-break "normal")))
											 ("break-words" . ((".break-words" :overflow-wrap "break-word")))
											 ("break-all" . ((".break-all" :word-break "break-all")))))

(defvar *content* '(("content-none" . ((".content-none" :content "none"))))

(defvar *typography* (append
											*font-sizes*
											*font-styles*
											*font-weights*
											*font-family*

											;; text colors
											*text-color*
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

											*text-align*

											*line-height*
											*list-style-type*
											*list-style-position*
											*text-decoration*
											*text-decoration-style*
											*text-decoration-thickness*
											*text-transform*
											*text-overflow*
											*text-indent*

											*vertical-align*

											*whitespace*
											*word-break*
											*content*
											))
