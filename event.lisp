(cl:defpackage #:cl-event2-event
  (:use #:cl #:alexandria)
  (:nicknames #:event)
  (:export
   ;; Event bases
   #:make-event-base
   #:event-base-dispatch
   #:exit-event-loop
   ;; Events
   #:add-event-handler))
(cl:in-package #:event)

(defstruct (event-base (:constructor %make-event-base (pointer)))
  pointer)
(defmethod print-object ((obj event-base) s)
  (print-unreadable-object (obj s :type t :identity t)))

(defun make-event-base ()
  (let* ((ptr (%event:event-base-new))
         (base (%make-event-base ptr)))
    (tg:finalize base (curry #'%event:event-base-free ptr))
    base))

;; TODO - Report errors properly. libevent prints the details to terminal. Maybe try to use that?
;; TODO - figure out how to reset this after we've, for example, interrupted a thread.
(defun event-base-dispatch (event-base &key oncep nonblockp)
  (case (%event:event-base-loop (event-base-pointer event-base)
                                (loop with flags = 0
                                   for keyword in (list (when oncep :once)
                                                        (when nonblockp :nonblock))
                                   when keyword
                                   do (setf flags
                                            (logior flags
                                                    (cffi:foreign-enum-value
                                                     '%event:loop-flag keyword)))
                                   finally (return flags)))
    (0 t)
    (-1 (error "An error occurred while dispatching events."))
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
  callback
  pointer)
(defmethod print-object ((obj event) s)
  (print-unreadable-object (obj s :type t :identity t)))

(let ((active-events (make-hash-table)))
  (defun find-event (event-id)
    (gethash event-id active-events))
  (defun (setf find-event) (new-event event-id)
    (setf (gethash event-id active-events) new-event)))

(cffi:defcallback event-callback :void ((fd :int) (what :short) (event-pointer :pointer))
  (declare (ignore fd))
  (if-let (event (find-event (cffi:pointer-address event-pointer)))
    (funcall (event-callback event) what)
    (error "No active event found while executing event-callback."))
  (values))

(defun seconds-to-timeval-values (secs)
  (multiple-value-bind (secs usecs) (truncate secs)
    (values secs
            (truncate (* usecs 1000000)))))

(defun add-event-handler (event-base fd callback &key
                          timeout readp writep signalp persistentp edge-triggered-p)
  (let* ((flag-enums (list (when timeout :timeout)
                           (when readp :read)
                           (when writep :write)
                           (when signalp :signal)
                           (when persistentp :persist)
                           (when edge-triggered-p :et)))
         (flags (loop with flags = 0
                   for enum in flag-enums
                   when enum
                   do (setf flags (logior flags (cffi:foreign-enum-value '%event:event-flag enum)))
                   finally (return flags))))
    (let* ((ptr (cffi:foreign-alloc :char :count (%event:event-get-struct-event-size)))
           (event (%make-event :pointer ptr
                               :event-base event-base
                               :fd fd
                               :callback callback)))
      (%event:event-assign ptr
                           (event-base-pointer event-base)
                           fd
                           flags
                           (cffi:get-callback 'event-callback)
                           ptr)
      (if (null timeout)
          (%event:event-add ptr (cffi:null-pointer))
          (cffi:with-foreign-object (timeval '%event:timeval)
            (multiple-value-bind (secs usecs)
                (seconds-to-timeval-values timeout)
              (cffi:with-foreign-slots ((%event:tv-sec %event:tv-usec) timeval %event:timeval)
                (setf %event:tv-sec secs
                      %event:tv-usec usecs))
              (%event:event-add ptr timeval))))
      (setf (find-event (cffi:pointer-address ptr)) event)
      (tg:finalize event (curry #'%event:event-free ptr))
      event)))
