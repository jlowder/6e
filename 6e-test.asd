(defsystem 6e-test
    :depends-on (:lisp-unit :6e)
    :components
    ((:module "test"
              :components ((:file "6e-test")))))

