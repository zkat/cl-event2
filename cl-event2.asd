;;;; cl-event2.asd

(asdf:defsystem #:cl-event2
  :depends-on (#:cffi #:trivial-garbage)
  :serial t
  :components ((:file "event-bindings")
               (:file "event")))

