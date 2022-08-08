(defpackage cl-djula-tailwind.typography
  (:use :cl)
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

(defvar *slate-colors* '(("50" . "#f8fafc")
    ("100" . "#f1f5f9")
    ("200" . "#e2e8f0")
    ("300" . "#cbd5e1")
    ("400" . "#94a3b8")
    ("500" . "#64748b")
    ("600" . "#475569")
    ("700" . "#334155")
    ("800" . "#1e293b")
    ("900" . "#0f172a")))

(defvar *gray-colors*
		'(("50" . "#f9fafb")
    ("100" . "#f3f4f6")
    ("200" . "#e5e7eb")
    ("300" . "#d1d5db")
    ("400" . "#9ca3af")
    ("500" . "#6b7280")
    ("600" . "#4b5563")
    ("700" . "#374151")
    ("800" . "#1f2937")
    ("900" . "#111827")))

(defvar *zinc-colors* '(("50" . "#fafafa")
    ("100" . "#f4f4f5")
    ("200" . "#e4e4e7")
    ("300" . "#d4d4d8")
    ("400" . "#a1a1aa")
    ("500" . "#71717a")
    ("600" . "#52525b")
    ("700" . "#3f3f46")
    ("800" . "#27272a")
    ("900" . "#18181b")))

(defun get-list-key (prefix key)
	(concatenate 'string prefix (write-to-string key)))

(defun get-list-value (prefix key value)
	`(,(concatenate 'string "." prefix (write-to-string key)) :color ,value))

(defun get-list (prefix key value)
	`(,(get-list-key prefix key) . ((,(get-list-value prefix key value)))))

;; (defvar *text-color-slate* '())

(defparameter *text-color-slate* (loop for i in *slate-colors*
				collect (get-list "text-slate-" (car i) (cdr i))))

(defparameter *text-color-gray* (loop for i in *gray-colors*
				collect (get-list "text-gray-" (car i) (cdr i))))

(defparameter *text-color-zinc* (loop for i in *gray-colors*
				collect (get-list "text-zinc-" (car i) (cdr i))))
;; (print (get-list-key "text-slate-" "50"))
;; (print (get-list-value "text-slate-" "50" "#fff"))
;; (print (get-list "text-slate-" "50" "#fff"))
(print *text-color-zinc*)

(defvar *typography* (append *font-sizes* *font-styles* *font-weights*))
