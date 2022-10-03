(defpackage cl-djula-tailwind
  (:use :cl
   :cl-djula-tailwind.layout
	 :cl-djula-tailwind.spacing
   :cl-djula-tailwind.typography
				:cl-djula-tailwind.backgrounds
				:cl-djula-tailwind.sizing
				:cl-djula-tailwind.borders
				:cl-djula-tailwind.effects
				:cl-djula-tailwind.flexbox-grid
				:cl-djula-tailwind.accessibility
				:cl-djula-tailwind.svg
	 :cl-djula-tailwind.transforms
	 :cl-djula-tailwind.tables)
  (:import-from :cl-ppcre
                :all-matches
                :do-register-groups
                :all-matches-as-strings
                :regex-replace-all)
	(:export :get-stylesheet
						:get-plain-class
						:get-pseudo-class
						:get-responsive-class
						:get-darkmode-class
						:get-peer-class
					 :get-form-state-class
					 :get-child-modifier-class))

(in-package :cl-djula-tailwind)

(defvar *util-prop-hash* '(
													 ("p" . :padding)
													 ("pt" . :padding-top)
													 ("pr" . :padding-right)
													 ("pb" . :padding-bottom)
													 ("pl" . :padding-left)
													 ("m" . :margin)
													 ("mt" . :margin-top)
													 ("mr" . :margin-right)
													 ("mb" . :margin-bottom)
													 ("ml" . :margin-left)
													 ("h" . :height)
													 ("min-h" . :min-height)
													 ("max-h" . :max-height)
													 ("w" . :width)
													 ("min-w" . :min-width)
													 ("max-w" . :max-width)
													 ))

(defun parse-template-string (path dir)
  "Parse Djula template from the given path"
    (djula::parse-template-string (uiop::read-file-string (merge-pathnames path dir))))


(defun get-markup (template)
  "Get the HTML markup from the template content"
	(let ((markup '()))
		(dolist (l template)
			(let ((x (first l))
						(y (second l)))
				(when (eq x :string)
						(push y markup))))
		(remove-duplicates (reverse markup) :test #'string=)))

(defun find-class-attrs (html)
  "Find the class attribute values"
 (loop for l in html
         for x = (all-matches-as-strings  "class=\"([^\"]*)\"" l) 
                when x
         collect x))

(defun replace-class-keyword (props)
  "Remove the class keywords and extra quotes"
	(let ((classnames '()))
	(dolist (line props)
		(dolist (word line)
			(let* ((x (regex-replace-all "class=" word ""))
						(y (regex-replace-all "\\\"" x "")))
				(push y classnames))))
		(reverse classnames)))
  
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
  (split-by-one-space
	 (join-string-list
		(replace-class-keyword
		 (find-class-attrs markup)))))
  

(defvar *tailwind* (append
                          *layout*
                          *spacing*
                          *typography*
													*backgrounds*
													*sizing*
													*borders*
													*effects*
													*flexbox-grid*
													*accessibility*
                          *svg*
                          *transforms*
													*tables*
													))
       
(defun is-plain-util (c)
		(assoc c *tailwind* :test #'string=))

(defun is-pseudo-util (str)
		(all-matches "(hover|focus|active|disabled):([a-z0-9-]*)" str))

(defun is-darkmode-util (str)
		(all-matches "dark:([a-z0-9-]*)" str))

(defun is-responsive-util (str)
		(all-matches "(sm|md|lg|xl|2xl):([a-z0-9-]*)" str))

(defun is-peer-util (str)
		(all-matches "peer-(checked|hover|focus|active|disabled):([a-z0-9-]*)" str))

(defvar *form-state-util-regex* "(required|invalid|disabled):([a-z0-9-]*)")


(defun form-state-utilp (str)
  "Is this a form state modifier util"
  (all-matches *form-state-util-regex* str))

(defun get-form-state-class (str)
	"Generate class definitions for required: invalid: and other form states"
	(let (result)
		(do-register-groups
				(state class)
				(*form-state-util-regex* str)
			(let ((classname (concatenate 'string "." state "\\:" class ":" state))
						(props (cdr (cadr (assoc class *tailwind* :test #'string=)))))
				(push (concatenate 'string classname " { " (cl-css:inline-css props) " }") result)))
		(car result)))

(defvar *arbitrary-value-regex* "([a-z]*)-\\[([a-z0-9-]*)\\]")
(defun arbitrary-value-utilp (str)
	"Is this an arbitrary value util"
	(all-matches *arbitrary-value-regex* str))

(defun get-arbitrary-value-class (str)
	"Generate class definitions for arbitrary values"
	(let (result)
		(do-register-groups
				(class value)
				(*arbitrary-value-regex* str)
			(let ((classname (concatenate 'string "." class "-\\[" value "\\] " ))
						(props (cdr (assoc class *util-prop-hash* :test #'string=))))
				(push (concatenate 'string classname " { " (cl-css:inline-css `(,props ,value)) " }") result)))
		(car result)))

(defvar *child-modifier-regex* "(first|last|odd|even):([a-z0-9-]*)")

(defun child-modifier-utilp (str)
  "Is this a child modifier util"
  (all-matches *child-modifier-regex* str))

(defun get-child-modifier-class (str)
	"Generate class definitions for first: last: and other child modifiers"
	(let (result)
		(do-register-groups
				(state class)
				(*child-modifier-regex* str)
			(let ((classname (concatenate 'string "." state "\\:" class ":" state))
						(props (cdr (cadr (assoc class *tailwind* :test #'string=)))))
				(push (concatenate 'string classname " { " (cl-css:inline-css props) " }") result)))
		(car result)))

(defun get-plain-class (str)
	"Generate class definitions for simple plain utilities"
		(cl-css:css (cdr (assoc str *tailwind* :test #'string=))))

(defun get-pseudo-class (str)
	"Generate class definitions for hover: focus: and other states"
	(let (result)
		(do-register-groups
				(state class)
				("(hover|focus|active|disabled|focus-within|focus-visible):([a-z0-9-]*)" str)
			(let ((classname (concatenate 'string "." state "\\:" class ":" state))
						(props (cdr (cadr (assoc class *tailwind* :test #'string=)))))
				(push (concatenate 'string classname " { " (cl-css:inline-css props) " }") result)))
		(car result)))


(defun get-darkmode-class (str)
	"Generate class definitions for darkmode"
	(let (result)
		(do-register-groups
				(state class)
				("(dark):([a-z0-9-]*)" str)
			(let ((classname (concatenate 'string "." state "\\:" class))
						(props (cdr (cadr (assoc class *tailwind* :test #'string=)))))
				(push (concatenate 'string "@media (prefers-color-scheme: dark) { " classname " { " (cl-css:inline-css props) " } }") result)))
		(car result)))


(defvar *media-query* '(("sm" . "@media (min-width: 640px)")
												("md" . "@media (min-width: 768px)")
												("lg" . "@media (min-width: 1024px)")
												("xl" . "@media (min-width: 1280px)")
												("2xl" . "@media (min-width: 1536px)")))

(defun get-media-query (breakpoint)
 (cdr (assoc breakpoint *media-query* :test #'string=)))

(defun get-responsive-class (str)
	"Generate class definitions for responsive utility variants sm: md: lg: etc.,"
	(let (result)
		(do-register-groups
				(breakpoint class)
				("(sm|md|lg|xl|2xl):([a-z0-9-]*)" str)
			(let ((classname (concatenate 'string "." breakpoint "\\:" class ))
						(props (cdr (cadr (assoc class *tailwind* :test #'string=)))))
				(push (concatenate 'string
													 (get-media-query breakpoint) " { " classname " { " (cl-css:inline-css props) " } }") result)))
		(car result)))

(defun get-peer-class (str)
	"Generate class definitions for peer:{{modifier}} states"
	(let (result)
		(do-register-groups
				(state class)
				("peer-(checked|invalid|required|hover|focus|active|disabled|focus-within|focus-visible):([a-z0-9-]*)" str)
			(let ((classname (concatenate 'string ".peer:" state " ~ .peer-" state "\\:" class))
						(props (cdr (cadr (assoc class *tailwind* :test #'string=)))))
				(push (concatenate 'string classname " { " (cl-css:inline-css props) " }") result)))
		(car result)))

(defvar *group-regex* "group-(hover|focus|active):([a-z0-9-]*)")

(defun group-utilp (str)
  "Is this a group modifier util"
  (all-matches *group-regex* str))

(defun get-group-class (str)
	"Generate class definitions for group-*:{{modifier}} states"
	(let (result)
		(do-register-groups
				(state class)
				(*group-regex* str)
			(let ((classname (concatenate 'string ".group:" state " ~ .group-" state "\\:" class))
						(props (cdr (cadr (assoc class *tailwind* :test #'string=)))))
				(push (concatenate 'string classname " { " (cl-css:inline-css props) " }") result)))
		(car result)))

(defun get-templates (root-template)
	"Get the list of templates - extends and includes from the root-template"
	(let ((templates '()))
		(dolist (l root-template)
			(let ((x (first l))
						(y (second l)))
				(when (eq x :unparsed-tag)
					(do-register-groups
							(tag template-name)
							("(extends|include) \"([_a-z\/\.]*)\"" y)
						(push template-name templates)))
				))
		(reverse templates)))

(defun get-stylesheet (file dir)
  "Generate the stylesheet based on tailwind class definitions"
  (let* ((root-template (parse-template-string file dir))
				 (templates (get-templates root-template))
				 (markup '()))

		;; Let get the list of templates first
		;; Then process each template and collect the markup
		;; in a single list 
		(dolist (tmplt templates)
			(let ((template-content (parse-template-string tmplt dir)))
				;; (print tmplt)
				;; (print (join-string-list (get-markup template-content)))
			(push (join-string-list (get-markup template-content)) markup)))

		;; finally push the markup of the root-template (current route template)
			 (push (join-string-list (get-markup root-template)) markup)

			(let ((styles '()))
				(dolist (c (get-classnames markup))
					(cond
						((is-plain-util c) (push (get-plain-class c) styles))
						((is-pseudo-util c) (push (get-pseudo-class c) styles))
						((is-responsive-util c) (push (get-responsive-class c) styles))
						((is-darkmode-util c) (push (get-darkmode-class c) styles))
						((is-peer-util c) (push (get-peer-class c) styles))
						((form-state-utilp c) (push (get-form-state-class c) styles))
						((child-modifier-utilp c) (push (get-child-modifier-class c) styles))
						((group-utilp c) (push (get-group-class c) styles))
						((arbitrary-value-utilp c) (push (get-arbitrary-value-class c) styles))
						(t (print c))))
				(cl-minify-css:minify-css (join-string-list (reverse styles))))))


;; (defparameter *template-directory* (asdf:system-relative-pathname "cl-djula-tailwind" "tests/templates/"))
;; (defparameter *tests-directory* (asdf:system-relative-pathname "cl-djula-tailwind" "tests/"))
;; (print (get-stylesheet #P"index.html" *template-directory*))


