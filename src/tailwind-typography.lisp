(defpackage cl-djula-tailwind.typography
  (:use :cl)
  (:export :*font-sizes*
   :*font-styles*
   :*font-weights*))

(in-package cl-djula-tailwind.typography)

(defvar *font-sizes* '(("text-xl" . ((".text-xl" :font-size "1.25rem" :line-height "1.75rem")))
                         ("text-2xl" . ((".text-2xl" :font-size "1.5rem" :line-height "2rem")))
                    ))


(defvar *font-styles* '(("italic" . ((".italic" :font-style "italic")))
                         ("non-italic" . ((".non-italic" :font-style "normal")))))

(defvar *font-weights* '(("font-bold" . ((".fond-bold" :font-weight "700")))
                         ("font-normal" . ((".font-normal" :font-style "400")))))
