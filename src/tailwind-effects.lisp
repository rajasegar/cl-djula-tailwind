(defpackage cl-djula-tailwind.effects
  (:use :cl)
  (:export :*effects*))

(in-package cl-djula-tailwind.effects)

(defvar *box-shadow* '(
									("shadow-sm" . ((".shadow-sm" :box-shadow "0 1px 2px 0 rgb(0 0 0 / 0.05)")))
									("shadow" . ((".shadow" :box-shadow "0 1px 3px 0 rgb(0 0 0 / 0.1), 0 1px 2px -1px rgb(0 0 0 / 0.1)")))
									("shadow-md" . ((".shadow-md" :box-shadow "0 4px 6px -1px rgb(0 0 0 / 0.1), 0 2px 4px -2px rgb(0 0 0 / 0.1)")))
									("shadow-lg" . ((".shadow-lg" :box-shadow "0 10px 15px -3px rgb(0 0 0 / 0.1), 0 4px 6px -4px rgb(0 0 0 / 0.1)")))
									("shadow-xl" . ((".shadow-xl" :box-shadow "0 20px 25px -5px rgb(0 0 0 / 0.1), 0 8px 10px -6px rgb(0 0 0 / 0.1)")))
									("shadow-2xl" . ((".shadow-2xl" :box-shadow "0 25px 50px -12px rgb(0 0 0 / 0.25);")))
									("shadow-inner" . ((".shadow-inner" :box-shadow "inset 0 2px 4px 0 rgb(0 0 0 / 0.05)")))
									("shadow-none" . ((".shadow-none" :box-shadow "0 0 #0000")))
                    ))




(defvar *effects* (append *box-shadow* ))
