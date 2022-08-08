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
                    ("hidden" . ((".hidden" :display "none")))
                         ))

(defvar *layout* (append *aspect-ratio* *display*))
