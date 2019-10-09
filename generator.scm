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

(define-module (generator)
  #:export (blocking
            random-phrase
            random-password
            entropy-per-phrase))

(use-modules (rules))
(use-modules (rnrs base)
             (rnrs bytevectors)
             (ice-9 binary-ports)
             (srfi srfi-1))

;; This will be set by the main entrypoint to decide whether we should use the
;; blocking /dev/random or the nonblocking /dev/urandom. In the future, I'd like
;; to use libgcrypt rather than read from /dev/[u]random directly, for more
;; portability.
(define blocking (make-parameter #f))

;; Generate a bytevector of cryptographically secure random bytes.
(define (random-bytes count)
  (call-with-input-file (if (blocking) "/dev/random" "/dev/urandom")
    (lambda (port)
      (get-bytevector-n port count))
    #:binary #t))

;; Generate a random 64-bit unsigned integer.
(define (random-u64)
  (let ((bv (random-bytes 8)))
    (bytevector-u64-native-ref bv 0)))

;; The smallest number larger than every 64-bit unsigned integer.
(define max-u64 (expt 2 64))

;; Generate a random number in the range of
(define (uniform-random num)
  (cond
   ((> num (expt 2 64))
    (error "Too large for u64" num))
   ;; This is a shortcut. If num is a power of two, we can just use a mask.
   ((= (logand num (1+ (lognot num))) num)
    (logand (1- num) (random-u64)))
   (#t
    (let ((rand (random-u64)))
      ;; The bias range consists of (mod max-u64 num) values at the end of the
      ;; range. bias-start will be the first value that is biased.
      (let ((bias-start (- max-u64 (mod max-u64 num))))
        ;; If this roll is in the bias range, then we need to resample to clear
        ;; that bias. Otherwise, we can mod the number and return it.
        (if (>= rand bias-start)
            (uniform-random num)
            (mod rand num)))))))

;; Generate a random phrase.
(define (random-phrase)
  (vector-ref phrases
              (uniform-random (vector-length phrases))))

;; Logarithm base 2.
(define (log2 val)
  (/ (log val) (log 2)))

;; The number of bits of entropy per phrase.
(define entropy-per-phrase (log2 (vector-length phrases)))

;; Generate a random password with the given minimum entropy (specified in
;; bits).
(define (random-password min-entropy)
  (string-concatenate
   ;; Unfold a list of random phrases until we have achieved our minimum
   ;; entropy, then concatenate them into a single string.
   (unfold
    (lambda (remn) (negative? remn))
    (lambda (_) (random-phrase))
    (lambda (remn) (- remn entropy-per-phrase))
    min-entropy)))
