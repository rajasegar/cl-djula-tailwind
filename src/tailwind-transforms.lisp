(defpackage cl-djula-tailwind.transforms
  (:use :cl)
  (:export :*transforms*))

(in-package cl-djula-tailwind.transforms)

(defvar *rotate* '(("rotate-0" . ((".rotate-0" :transform "rotate(0deg)")))
                   ("rotate-1" . ((".rotate-1" :transform "rotate(1deg)")))
                   ("rotate-2" . ((".rotate-2" :transform "rotate(2deg)")))
                   ("rotate-3" . ((".rotate-3" :transform "rotate(3deg)")))
                   ("rotate-6" . ((".rotate-6" :transform "rotate(6deg)")))
                   ("rotate-12" . ((".rotate-12" :transform "rotate(12deg)")))
                   ("rotate-45" . ((".rotate-45" :transform "rotate(45deg)")))
                   ("rotate-90" . ((".rotate-90" :transform "rotate(90deg)")))
                   ("rotate-180" . ((".rotate-180" :transform "rotate(180deg)")))))

(defvar *skew* '(("skew-x-0" . ((".skew-x-0" :transform "skewX(0deg)")))
                 ("skew-y-0" . ((".skew-y-0" :transform "skewY(0deg)")))
                 ("skew-x-1" . ((".skew-x-1" :transform "skewX(1deg)")))
                 ("skew-y-1" . ((".skew-y-1" :transform "skewY(1deg)")))
                 ("skew-x-2" . ((".skew-x-2" :transform "skewX(2deg)")))
                 ("skew-y-2" . ((".skew-y-2" :transform "skewY(2deg)")))
                 ("skew-x-3" . ((".skew-x-3" :transform "skewX(3deg)")))
                 ("skew-y-3" . ((".skew-y-3" :transform "skewY(3deg)")))
                 ("skew-x-6" . ((".skew-x-6" :transform "skewX(6deg)")))
                 ("skew-y-6" . ((".skew-y-6" :transform "skewY(6deg)")))
                 ("skew-x-12" . ((".skew-x-12" :transform "skewX(12deg)")))
                 ("skew-y-12" . ((".skew-y-12" :transform "skewY(12deg)")))))

(defvar *transform-origin* '(("origin-center" . ((".origin-center" :transform-origin "center")))
                             ("origin-top" . ((".origin-top" :transform-origin "top")))
                             ("origin-top-right" . ((".origin-top-right" :transform-origin "top right")))
                             ("origin-right" . ((".origin-right" :transform-origin "right")))
                             ("origin-bottom-right" . ((".origin-bottom-right" :transform-origin "bottom right")))
                             ("origin-bottom" . ((".origin-bottom" :transform-origin "bottom")))
                             ("origin-bottom-left" . ((".origin-bottom-left" :transform-origin "bottom left")))
                             ("origin-left" . ((".origin-left" :transform-origin "left")))
                             ("origin-top-left" . ((".origin-top-left" :transform-origin "top left")))))

(defvar *transforms*  (append *transform-origin*
                              *skew*
                              *rotate*
                              ))
