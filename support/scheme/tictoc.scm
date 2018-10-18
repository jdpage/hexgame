;; Chicken Scheme high-resolution timer bindings
;; Copyright (C) 2018  Jonathan David Page <jonathan@sleepingcyb.org>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
(declare (unit tictoc))

(module tictoc
    (tic
     toc
     timer?
     profile-format
     profile-this
     profile-log
     profiled
     profile-collect
     )
  (import scheme chicken foreign)
  (use srfi-69 data-structures lolevel)

  (foreign-declare "#include <time.h>")
  (foreign-declare "#include <stdint.h>")

  (define-type timer (struct timer))
  (: make-timer (blob -> timer))
  (: timer? (* -> boolean : timer))
  (: timespec-buffer (timer -> blob))
  (define-record-type timer
    (make-timer buffer)
    timer?
    (buffer timer-buffer))
  (define-foreign-type timer scheme-pointer timer-buffer)

  (: sizeof-timespec fixnum)
  (define sizeof-timespec (foreign-value "sizeof(struct timespec)" size_t))

  (: tic (-> timer))
  (define (tic)
    (let ((t (make-timer (make-blob sizeof-timespec))))
      ((foreign-lambda* void ((timer t))
                        "clock_gettime(CLOCK_PROCESS_CPUTIME_ID, t);")
       t)
      t))

  (: toc (timer -> fixnum))
  (define (toc t)
    ((foreign-lambda* integer64 ((timer t))
                      "struct timespec e, *s = t;"
                      "clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &e);"
                      "C_return((e.tv_sec - s->tv_sec) * (int64_t)1e9 + (e.tv_nsec - s->tv_nsec));")
     t))

  (define profile-info '())
  (define profile-counts (make-hash-table #:initial 0))
  (define profile-times (make-hash-table #:initial 0))

  (define (profile-log timer symbol value)
    (set! profile-info (cons (cons symbol (toc timer)) profile-info))
    value)

  (define (profile-format)
    (sort (map (lambda (k)
                 (let ((count (hash-table-ref profile-counts k))
                       (time (/ (hash-table-ref profile-times k) 1000000)))
                   (list k (/ time count) count time)))
               (hash-table-keys profile-counts))
          (lambda (a b)
            (> (cadr a) (cadr b)))))

  (define-syntax profile-this
    (syntax-rules ()
      ((profile-this tag body ...)
       (let ((s (tic)))
         (profile-log s (quote tag) (begin body ...))))))

  (define-syntax profiled
    (syntax-rules ()
      ((profiled (define (name args ...) body ...))
       (define (name args ...)
         (profile-this name body ...)))
      ((profiled a) a)
      ((profiled a b ...)
       (begin
         (profiled a)
         (profiled b ...)))))

  (define (profile-collect)
    (for-each (lambda (e)
                (hash-table-update! profile-counts (car e) add1)
                (hash-table-update! profile-times (car e) (cut + (cdr e) <>)))
              profile-info)
    (set! profile-info '()))
  )
