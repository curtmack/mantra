;;; mantra: Pronounceable password generator in Guile
;;; Copyright (C) 2019  Curtis Mackie
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define-module (choice)
  #:export (emit
            with-list-emitter
            with-vector-emitter
            choose
            choose-from))

(use-modules (srfi srfi-9)
             (srfi srfi-11)
             (srfi srfi-43))

(define emit-target (make-parameter #f))

;; Emit a value to the current emit target.
(define (emit val)
  (if (emit-target)
      (emit-target (cons val (emit-target)))
      (error "No emit target set!")))

;; Set up an emit target that gathers emitted values into a list.
(define-syntax with-list-emitter
  (syntax-rules ()
    ((_ exp exp* ...)
     (parameterize ((emit-target '()))
       exp exp* ...
       (reverse (emit-target))))))

;; Set up an emit target that gathers emitted values into a vector.
(define-syntax with-vector-emitter
  (syntax-rules ()
    ((_ exp exp* ...)
     (parameterize ((emit-target '()))
       exp exp* ...
       (reverse-list->vector (emit-target))))))

;; A generic for-each that works on lists, strings, and vectors.
(define (for-each-seq thunk seq)
  (cond
   ((pair? seq)
    (for-each thunk seq))
   ((string? seq)
    (string-for-each thunk seq))
   ((vector? seq)
    (vector-for-each thunk seq))))

;; For each clause provided, for each value in the from term of that clause,
;; bind var to that value, then choose from each subsequent clause. Once all
;; clauses have a choice bound to their variable, invoke the body in a context
;; which contains those bindings. The body will ultimately be invoked once for
;; each member of the Cartesian product of all the from sequences.
(define-syntax choose
  (syntax-rules ()
    ((choose ((val from) more ...) body ...)
     (for-each-seq
      (lambda (val)
        (choose (more ...) body ...))
      from))
    ((choose () body ...)
     (begin body ...))))

;; Convenient syntax for calling choice with a list of thunks.
(define-syntax choose-from
  (syntax-rules ()
    ((choose-from thunk thunk* ...)
     (choose ((t (list thunk thunk* ...))) (t)))))
