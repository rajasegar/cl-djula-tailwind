(defpackage cl-djula-tailwind.spacing
  (:use :cl)
  (:export :*spacing*))

(in-package cl-djula-tailwind.spacing)

(defvar *padding* '(("p-0" . ((".p-0" :padding "0px")))
                         ("px-0" . ((".px-0" :padding-left "0px" :padding-right "0px")))
                         ("py-0" . ((".py-0" :padding-top "0px" :padding-bottom "0px")))
                    ("p-1" . ((".p-1" :padding "0.25rem")))
                    ("p-2" . ((".p-2" :padding "0.5rem")))
                    ("p-3" . ((".p-3" :padding "0.75rem")))
                    ("p-4" . ((".p-4" :padding "1rem")))
                    ))


(defvar *margin* '(("m-0" . ((".m-0" :margin "0px")))
                         ("mx-0" . ((".mx-0" :margin-left "0px" :margin-right "0px")))
                         ("my-0" . ((".my-0" :margin-top "0px" :margin-bottom "0px")))
                   ("m-1" . ((".m-1" :margin "0.25rem")))
                   ("m-2" . ((".m-2" :margin "0.5rem")))
                   ("m-3" . ((".m-3" :margin "0.75rem")))
                   ("m-4" . ((".m-4" :margin "1rem")))
                   ))

(defvar *spacing* (append *padding* *margin*))
