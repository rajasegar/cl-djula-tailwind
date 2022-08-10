(defpackage cl-djula-tailwind
  (:use :cl
   :cl-djula-tailwind.layout
	 :cl-djula-tailwind.spacing
   :cl-djula-tailwind.typography
	 :cl-djula-tailwind.backgrounds)
	(:export :get-stylesheet))

(in-package :cl-djula-tailwind)

(defun parse-template-string (path dir)
  "Parse Djula template from the given path"
    (djula::parse-template-string (uiop::read-file-string (merge-pathnames path dir))))

(defun get-markup (template)
  "Get the HTML markup from the template content"
    (remove-duplicates
    (loop for l in template 
            for x = (first l)
            for y = (second l)
            when  (eq x :string)
            collect y)
    :test #'string=))

(defun find-class-attrs (html)
  "Find the class attribute values"
(car (loop for l in html
         for x = (ppcre:all-matches-as-strings  "class=\"([^\"]*)\"" l) 
                when x
         collect x)))

(defun replace-class-keyword (props)
  "Remove the class keywords and extra quotes"
  (loop for l in props
        for x = (ppcre:regex-replace-all "class=" l "")
        for y = (ppcre:regex-replace-all "\\\"" x "")
        when y
          collect y))

(defun join-string-list (string-list)
    "Concatenates a list of strings and puts spaces between the elements."
    (format nil "~{~A~^ ~}" string-list))

(defun split-by-one-space (string)
    "Returns a list of substrings of string
    divided by ONE space each.
    Note: Two consecutive spaces will be seen as
    if there were an empty string between them."
  (remove-duplicates 
    (loop for i = 0 then (1+ j)
          as j = (position #\Space string :start i)
          collect (subseq string i j)
          while j) :test #'string=))

(defun get-classnames (markup)
  "Get the list of Tailwind class names as a list"
	;; (print (replace-class-keyword (find-class-attrs markup)))
   (split-by-one-space (join-string-list (replace-class-keyword (find-class-attrs markup)))))
  

(defparameter *tailwind* (append
                          *layout*
                          *spacing*
                          *typography*
													*backgrounds*
													))
;; (print *tailwind*)

(defun get-stylesheet (file dir)
  "Generate the stylesheet based on tailwind class definitions"
  (let ((template (parse-template-string file dir)))
		;; (print template)

    (let ((markup (get-markup template)))
			;; (print markup)

      (cl-minify-css:minify-css (join-string-list (loop for c in  (get-classnames markup)
                              for key = (assoc c *tailwind* :test #'string=)
                              if key
																collect (cl-css:css (cdr key))
																else 
																collect (get-pseudo-class c)
																												))))))



(defun get-pseudo-class (str)
	"Generate class definitions for hover:, focus: and other states"
	(let (result)
		(cl-ppcre:do-register-groups
				(state class)
				("(hover|focus|active|disabled|focus-within|focus-visible):([a-z0-9-]*)" str)
			(let ((classname (concatenate 'string "." state "\\:" class ":" state))
						(props (cdr (cadr (assoc class *tailwind* :test #'string=)))))
				(push (concatenate 'string classname " { " (cl-css:inline-css props) " }") result)))
		(car result)))

;; (print (get-pseudo-class "hover:bg-red-400"))

;; (defparameter *template-directory* (asdf:system-relative-pathname "cl-djula-tailwind" "tests/templates/"))
;; (print (get-stylesheet #P"index.html" *template-directory*))

