;;;; cl-event2.asd

(asdf:defsystem #:cl-event2
  :depends-on (#:cffi)
  :serial t
  :components ((:file "bindings")))

