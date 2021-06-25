(asdf:defsystem "py4cl2-tests"
  :serial t
  :description "Unit tests for the py4cl2 library."
  :author #.(concatenate 'string
                         "py4cl author: Ben Dudson <benjamin.dudson@york.ac.uk>"
                         (string #\newline)
                         "py4cl2 maintainer: Shubhamkar Ayare <shubhamayare@yahoo.co.in>")
  :license "MIT"
  :version "2.8.0"
  :depends-on ("py4cl2"
               #-(or :ecl :abcl)
               "numcl"
               "alexandria"
               "clunit"
               "trivial-garbage"
               "trivial-arguments")
  :components ((:file "package")
               (:file "tests"))
  :perform (test-op (o c) (symbol-call :py4cl2-tests :run)))
