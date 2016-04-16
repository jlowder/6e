(defsystem 6e
    :name "6e"
    :version "0.1.0"
    :author "Jason Lowdermilk <jlowdermilk@gmail.com>"
    :license "MIT"
    :description "6-element set conversion functions"
    :long-description "Functions to convert Keplerian 6-element sets to/from Cartesian position and velocity vectors"
    :depends-on (:vector)
    :components
    ((:module "src"
              :components ((:file "6e")))))

