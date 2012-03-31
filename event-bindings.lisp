(cl:defpackage #:cl-event2-event-bindings
  (:use #:cl)
  (:nicknames #:%event)
  (:import-from #:cffi
                #:define-foreign-library
                #:use-foreign-library
                #:defcenum
                #:defctype
                #:defcstruct
                #:defcfun)
  (:export
   ;; Event bases
   #:event-base-new
   #:event-base-free
   #:event-base-dispatch
   #:event-base-loopexit
   #:event-base-loopbreak
   ;; Events
   #:event-new
   #:event-assign
   #:event-free
   #:event-add
   #:event-get-struct-event-size))
(cl:in-package #:%event)

(define-foreign-library event2
  (:unix (:or "libevent.so" "libevent.so.5"))
  (:darwin "libevent")
  (t (:default "libevent")))
(use-foreign-library event2)

(defcstruct event)
(defcstruct event-base)
(defcstruct event-config)
(defctype evutil-socket-t :int)

(defcstruct timeval
  (tv-sec :long)
  (tv-usec :long))

(defcenum log-severity
  (:debug 0)
  (:msg 1)
  (:warn 2)
  (:err 3))

(defcenum loop-flag
  (:once #x01)
  (:nonblock #x02))

(defcenum event-flag
  (:timeout #x01)
  (:read #x02)
  (:write #x04)
  (:signal #x08)
  (:persist #x10)
  (:et #x20))

(defcenum event-base-config-flag
  (:nolock #x01)
  (:ignore-env #x02)
  (:startup-iocp #x04)
  (:no-cache-time #x08)
  (:epoll-use-changelist #x10))

(defcenum event-method-feature
  (:et #x01)
  (:o1 #x02)
  (:fds #x04))

;; TODO - some of these return :int, but may be better off returning :boolean?

(defcfun event-active :void
  (event event) (res :int) (ncalls :short))

(defcfun event-add :int
  ;; TODO - wtf is timeout
  (event event) (timeout :pointer))

(defcfun event-assign :int
  ;; TODO - this will need to be wrapped so the `events` arg can be properly ANDed.
  (event event) (event-base event-base) (fd evutil-socket-t)
  (events :short) (callback :pointer) (callback-arg :pointer))

(defcfun event-base-dispatch :int
  (event-base event-base))

(defcfun event-base-dump-events :void
  ;; TODO - no docs on this?
  (event-base event-base) (fd :int))

(defcfun event-base-free :void
  (event-base event-base))

(defcfun event-base-get-features :int
  (event-base event-base))

(defcfun event-base-get-method :string
  (event-base event-base))

(defcfun event-base-gettimeofday-cached :int
  (event-base event-base) (timeval timeval))

(defcfun event-base-got-break :boolean
  (event-base event-base))

(defcfun event-base-got-exit :boolean
  (event-base event-base))

(defcfun event-base-init-common-timeout :pointer
  (event-base event-base) (duration timeval))

(defcfun event-base-loop :int
  (event-base event-base) (flags :int))

(defcfun event-base-loopbreak :int
  (event-base event-base))

(defcfun event-base-loopexit :int
  (event-base event-base) (delay timeval))

(defcfun event-base-new event-base)

(defcfun event-base-new-with-config event-base
  (event-config event-config))

(defcfun event-base-once :int
  (event-base event-base) (fd evutil-socket-t)
  (events :short) (callback :pointer) (callback-arg :pointer)
  (timeout timeval))

(defcfun event-base-priority-init :int
  (event-base event-base) (npriorities :int))

(defcfun event-base-set :int
  (event-base event-base) (event event))

(defcfun event-config-avoid-method :int
  (config event-config) (method :string))

(defcfun event-config-free :void
  (config event-config))

(defcfun event-config-new event-config)

(defcfun event-config-require-features :int
  (config event-config) (feature :int))

(defcfun event-config-set-flag :int
  (config event-config) (flag :int))

(defcfun event-config-set-num-cpus-hint :int
  (config event-config) (num-cpus :int))

(defcfun event-debug-unassign :void
  (event event))

(defcfun event-del :int
  (event event))

(defcfun event-enable-debug-mode :void)

(defcfun event-free :void
  (event event))

(defcfun event-get-assignment :void
  (event event) (base-out event-base) (fd-out evutil-socket-t)
  (events-out :pointer) (callback-out :pointer) (arg-out :pointer))

(defcfun event-get-struct-event-size :int)

(defcfun event-get-supported-methods :pointer)

(defcfun event-get-version :string)

(defcfun event-get-version-number :uint32)

(defcfun event-initialized :boolean
  (event event))

(defcfun event-new event
  (event-base event-base) (fd evutil-socket-t) (events :short)
  (callback :pointer) (callback-arg :pointer))

(defcfun event-pending :boolean
  (event event) (events :short) (timeout :pointer))

(defcfun event-priority-set :int
  (event event) (priority :int))

(defcfun event-reinit :int
  (event-base event-base))

(defcfun event-set-fatal-callback :void
  (callback :pointer))

(defcfun event-set-log-callback :void
  (callback :pointer))

(defcfun event-set-mem-functions :void
  (malloc-fn :pointer)
  (realloc-fn :pointer)
  (free-fn :pointer))
