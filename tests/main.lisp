(defpackage cl-djula-tailwind/tests/main
  (:use :cl
        :cl-djula-tailwind
        :rove))
(in-package :cl-djula-tailwind/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-djula-tailwind)' in your Lisp.

(deftest test-plain-util
  (testing "should give css for mx-2"
    (ok (string= (cl-minify-css:minify-css (get-plain-class "mx-2")) ".mx-2{margin-left:0.5rem;margin-right:0.5rem;}"))))

(deftest test-pseudo-util
  (testing "should give css for hover:text-red-500"
    (ok (string= (get-pseudo-class "hover:text-red-500") ".hover\\:text-red-500:hover { color: #ef4444;  }"))))

(deftest test-responsive-util
  (testing "should give css for md:w-32"
    (ok (string= (get-responsive-class "md:w-32") "@media (min-width: 768px) { .md\\:w-32 { width: 8rem;  } }"))))


(deftest test-darkmode-util
  (testing "should give css for dark:bg-black"
    (ok (string= (get-darkmode-class "dark:bg-black") "@media (prefers-color-scheme: dark) { .dark\\:bg-black { background-color: rgb(0 0 0);  } }"))))

(deftest test-peer-util
  (testing "should give css for peer-invalid:visible"
    (ok (string= (get-peer-class "peer-invalid:visible") ".peer:invalid ~ .peer-invalid\\:visible { visibility: visible;  }"))))

(deftest test-invalid-plain-util
  (testing "should give invalid  for abc"
    (ng (is-plain-util "abc"))))
