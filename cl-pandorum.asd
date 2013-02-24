;;;; cl-pandorum.asd

(asdf:defsystem #:cl-pandorum
  :serial t
  :description "Pandoric macros from let-over-lamdba.
Working correctly, when imported from another package."
  :author "Alexander Popolitov <popolit@gmail.com>"
  :license "GPL"
  :depends-on (#:defmacro-enhance #:anaphora #:iterate)
  :components ((:file "package")
               (:file "cl-pandorum")))

