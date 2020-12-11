(in-package :cl-user)

(py4cl2:defpymodule "math" nil :silent t)
(py4cl2:defpymodule "numpy" nil :lisp-package "NP" :silent t)
(py4cl2:defpymodule "numpy.random" t :silent t)

;;; Do not test on ECL on travis just to save some travis time
#-ecl
(py4cl2:defpymodule "networkx" nil :lisp-package "NX" :silent t)

(defpackage :py4cl2-tests
  (:use :cl :clunit :py4cl2 :iterate)
  (:shadow :deftest)
  (:export :run))

