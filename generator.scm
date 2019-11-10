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

;; Generate a bytevector of `count' cryptographically secure random bytes.
(define (random-bytes count)
  (call-with-input-file (if (blocking) "/dev/random" "/dev/urandom")
    (lambda (port)
      (get-bytevector-n port count))
    #:binary #t))

;; Generate a random integer with `count' bytes.
(define (random-uint count)
  (let ((bv (random-bytes count)))
    (bytevector-uint-ref bv 0 (native-endianness) count)))

;; The smallest number larger than every unsigned integer of `count' bytes.
(define (max-uint count)
  (expt 256 count))

;; Compute the size of the bias region when a random number between 0 and
;; `possibilities' minus 1 is used to select from a vector of size `count'.
(define (bias-region possibilities count)
  (mod possibilities count))

;; Compute the resample probability of `bytes' random bytes are used to select
;; from a vector of size `count'.
(define (resample-probability bytes count)
  (let ((possibilities (expt 256 bytes)))
    ;; If there are fewer possibilities than the vector count, we have already
    ;; failed. Return a resample rate of 100% to indicate this.
    (if (< possibilities count)
        1
        ;; Otherwise, the resample probability is the size of the bias region
        ;; divided by the total number of possibilities.
        (/ (bias-region possibilities count)
           possibilities))))

;; Compute the optimal number of bytes for a random integer for selecting from
;; a vector of size count.
(define (optimal-random-bytes count)
  ;; The minimum number of bytes such that the probability of a resample is less
  ;; than 1/10.
  (let ((desired-resample-rate 1/10))
    (do ((bytes 1 (1+ bytes)))
        ((< (resample-probability bytes count) desired-resample-rate)
         bytes))))

;; Generate a random number in the range from 0 to num.
(define* (uniform-random num #:optional (bytes (optimal-random-bytes num)))
  (cond
   ;; This is a shortcut. If num is a power of two, we can just use a mask.
   ((= (logand num (1+ (lognot num))) num)
    (logand (1- num) (random-uint bytes)))
   (#t
    (let ((rand (random-uint bytes)))
      ;; bias-start will be the first value that is biased.
      (let ((bias-start (- (max-uint bytes)
                           (bias-region (max-uint bytes) num))))
        ;; If this roll is in the bias range, then we need to resample to clear
        ;; that bias. Otherwise, we can mod the number and return it.
        (if (>= rand bias-start)
            (uniform-random num)
            (mod rand num)))))))

;; Generate a random phrase.
(define* (random-phrase
          #:optional (bytes (optimal-random-bytes (vector-length phrases))))
  (vector-ref phrases
              (uniform-random (vector-length phrases) bytes)))

;; Logarithm base 2.
(define (log2 val)
  (/ (log val) (log 2)))

;; The number of bits of entropy per phrase.
(define entropy-per-phrase (log2 (vector-length phrases)))

;; Generate a random password with the given minimum entropy (specified in
;; bits).
(define (random-password min-entropy)
  (let ((bytes (optimal-random-bytes (vector-length phrases))))
    (string-concatenate
     ;; Unfold a list of random phrases until we have achieved our minimum
     ;; entropy, then concatenate them into a single string.
     (unfold
      (lambda (remn) (negative? remn))
      (lambda (_) (random-phrase bytes))
      (lambda (remn) (- remn entropy-per-phrase))
      min-entropy))))
