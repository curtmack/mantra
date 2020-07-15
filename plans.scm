;;; mantra: Pronounceable password generator in Guile
;;; Copyright (C) 2019-2020  Curtis Mackie
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

(define-module (plans)
  #:use-module (generator)
  #:use-module (srfi srfi-1)

  #:export (compile-plan
            plan-entropy
            make-plan
            random-password))

(define (compile-plan . args)
  (let recur
      ((accum '())
       (remaining args))
    (if (null? remaining)
        ;; Randomly shuffle the list at the end
        (vector->list (random-shuffle (list->vector accum)))
        (let ((next-phrases (first remaining))
              (next-number (second remaining)))
          (recur
           (append! (make-list next-number next-phrases) accum)
           (drop remaining 2))))))

(define factorial
  (case-lambda
    ((n) (factorial n 1))
    ((n accum)
     (cond
      ((not (integer? n)) (error "Non-integer factorial:" n))
      ((zero? n) accum)
      ((= 1 n) accum)
      (#t (factorial (1- n) (* n accum)))))))

(define (perm-entropy . args)
  (let* ((counts (let recur ((accum '())
                             (remaining args))
                   (if (null? remaining)
                       accum
                       (recur (cons (second remaining) accum)
                              (drop remaining 2)))))
         (total-count (apply + counts))
         ;; Shortcut - when there's only one distinguishable type of phrase,
         ;; there's only one permutation
         (possibilities (if (<= (length counts) 1)
                            1
                            (fold
                             (lambda (current-count accum)
                               (/ accum (factorial current-count)))
                             (factorial total-count)
                             counts))))
    (log2 possibilities)))

(define (plan-entropy . args)
  (let recur
      ((accum 0d0)
       (remaining args))
    (if (null? remaining)
        ;; Add the permutation entropy into the final total
        (+ accum (apply perm-entropy args))
        (let ((next-phrases (first remaining))
              (next-number (second remaining)))
          ;; The entropy per phrase is given by (phrase-entropy next-phrases).
          ;; Because it's a logarithmic measure, the total entropy is that times
          ;; the number to add.
          (recur
           (+ accum (* next-number (phrase-entropy next-phrases)))
           (drop remaining 2))))))

(define (make-plan default-phrases minimum-entropy . starting-args)
  (let recur
      ((current-plan starting-args)
       (current-entropy (apply plan-entropy starting-args))
       (num-defaults 0))
    (if (>= current-entropy minimum-entropy)
        ;; Keep the current arguments
        current-plan
        ;; Otherwise, add another default
        (let* ((next-num-defaults (1+ num-defaults))
               (next-plan (append! (list default-phrases next-num-defaults) starting-args))
               (next-entropy (apply plan-entropy next-plan)))
          (recur next-plan next-entropy next-num-defaults)))))

(define (random-password compiled-plan)
  (string-concatenate
   (map random-phrase compiled-plan)))
