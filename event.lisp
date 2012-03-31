(cl:defpackage #:cl-event2-event
  (:use #:cl #:alexandria)
  (:nicknames #:event)
  (:export
   ;; Event bases
   #:make-event-base
   #:dispatch-events
   #:exit-event-loop))
(cl:in-package #:event)

(defstruct (event-base (:constructor %make-event-base (pointer)))
  pointer)

(defun make-event-base ()
  (let* ((ptr (%event:event-base-new))
         (base (%make-event-base ptr)))
    (tg:finalize base (curry #'%event:event-base-free ptr))
    base))

;; TODO - check errors
(defun dispatch-events (event-base)
  (case (%event:event-base-dispatch (event-base-pointer event-base))
    (0 t)
    (-1 nil)
    (1 nil)))

;; TODO - &key delay
;; TODO - check errors
(defun exit-event-loop (event-base)
  (zerop
   (%event:event-base-loopexit (event-base-pointer event-base)
                               (cffi:null-pointer))))

(defstruct (event (:constructor %make-event))
  event-base
  fd
  events
  callback
  pointer)

(let ((active-events (make-hash-table)))
  (defun find-event (event-id)
    (gethash event-id active-events))
  (defun (setf find-event) (new-event event-id)
    (setf (gethash event-id active-events) new-event)))

(cffi:defcallback event-callback :void ((what :short) (event-pointer :pointer))
  (if-let (event (find-event (cffi:pointer-address event-pointer)))
    (funcall (event-callback event) what)
    (error "No active event found while executing event-callback."))
  (values))

;; TODO - nicer interface for specifying events
;; TODO - timeouts
(defun add-event-handler (event-base fd events callback)
  (let* ((ptr (cffi:foreign-alloc :char :count (%event:event-get-struct-event-size)))
         (event (%make-event :pointer ptr
                             :event-base event-base
                             :fd fd
                             :events events
                             :callback callback)))
    (%event:event-assign ptr
                         (event-base-pointer event-base)
                         fd
                         events
                         (cffi:get-callback 'event-callback)
                         ptr)
    (%event:event-add ptr (cffi:null-pointer))
    (setf (find-event (cffi:pointer-address ptr)) event)
    (tg:finalize event (curry #'%event:event-free ptr))
    event))
