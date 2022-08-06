(defpackage cl-djula-tailwind
  (:use :cl))
(in-package :cl-djula-tailwind)

(defun parse-template-string (path)
    (djula::parse-template-string (uiop::read-file-string (merge-pathnames path *template-directory*))))


(defun get-markup (template)
    (remove-duplicates
    (loop for l in template 
            for x = (first l)
            for y = (second l)
            when  (eq x :string)
            collect y)
    :test #'string=))

(defun find-class-attrs (html)
(car (loop for l in html
         for x = (ppcre:all-matches-as-strings  "class=\"([^\"]*)\"" l) 
                when x
         collect x)))

(defun replace-class-keyword (props)
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
   (split-by-one-space (join-string-list (replace-class-keyword (find-class-attrs markup)))))
  

(let ((template (parse-template-string #P"index.html")))

 (let ((markup (get-markup template)))
   ;; (print markup)

   (print (get-classnames markup))
   ;; Match regex pattern
   ;; collect tw utility class names
   ;; generate css for classes
   ;; inject stylesheet into the head

   )
  )