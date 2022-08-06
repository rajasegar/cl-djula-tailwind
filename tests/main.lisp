(defpackage cl-djula-tailwind/tests/main
  (:use :cl
        :cl-djula-tailwind
        :rove))
(in-package :cl-djula-tailwind/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-djula-tailwind)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
