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
  #:use-module (rules)
  #:use-module ((rnrs base)
                #:select (mod))
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 binary-ports)
  #:use-module (srfi srfi-1)

  #:export (blocking
            log2
            random-shuffle
            random-phrase
            phrase-entropy))

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
;; `possibilities' minus 1 is used to select from a vector of size `count'.  In
;; a realistic scenario, `possibilities' will be a power of 2, and `count' will
;; be the size of the choice we actually want to make.
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

;; At worst, we want to resample this proportion of the time.  Used to select an
;; optimal number of random bytes to use.
(define desired-worst-case-resample-rate 1/10)

;; Compute the optimal number of bytes for a random integer for selecting from
;; a vector of size count.
(define (optimal-random-bytes count)
  ;; The minimum number of bytes such that the probability of a resample is less
  ;; than our desired resample rate.  As this is a discrete logarithm, there's
  ;; no closed-form computation we can use to replace this loop.  Luckily, it's
  ;; not anticipated this loop will ever get past 3 or 4 bytes.
  (do ((bytes 1 (1+ bytes)))
      ((< (resample-probability bytes count)
          desired-worst-case-resample-rate)
       bytes)))

;; Is the random number `roll' biased when randomly selecting a number from 0 to
;; `count' minus 1 using `bytes' bytes of randomness?
;;
;; At this point, it's worth clarifying what we mean by "biased."  Suppose we
;; wanted to choose a fair value from 1 to 5, by rolling a standard six-sided
;; die.  We can force the result of the roll into that range by taking the
;; result mod 5 (using 5 instead of 0), but now we have a problem: the numbers 2
;; through 5 can be rolled in only one way, but a 1 can actually be rolled two
;; different ways, since 1 mod 5 = 6 mod 5.  This means our sampling is biased.
;; The only fair way to deal with this problem is to resample: on a 6, we can
;; reroll the die, and keep doing this until we roll a number 1 through 5, which
;; we'll use as our result.
;;
;; It turns out this will be a problem any time we try to generate a random
;; number between 1 and N, if N is not a power of two.  That is, there will
;; always be some residual M such that the first M values are more probable than
;; the rest.  Using more bytes of randomness when we "roll the die" can reduce
;; this difference to be arbitrarily small, but it can never eliminate it
;; entirely.  The only way to truly solve this problem is detect this condition
;; and resample, just like we rerolled the six-sided die.
(define (biased? roll count bytes)
  ;; The bias-start is the start of the bias region; that is, the value which is
  ;; 0 mod 2^bytes, and is part of the final run of values that does not map
  ;; over the full range from 0 to count exclusive.
  (let* ((uint-range (max-uint bytes))
         (bias-start (- uint-range
                        (bias-region uint-range count))))
    ;; We're biased if the roll is in the bias range.
    (>= roll bias-start)))

;; Generate a random index between 0 (inclusive) and `len' (exclusive).
(define random-index
  (case-lambda
    ((len)
     (random-index len (optimal-random-bytes len)))
    ((len bytes)
     (let reroll ((roll (random-uint bytes)))
       (if (biased? roll len bytes)
           (reroll (random-uint bytes))
           (mod roll len))))))

;; Generate a random phrase.
(define random-phrase
  (case-lambda
    ((phrases)
     (vector-ref phrases (random-index (vector-length phrases))))
    ((phrases bytes)
     (vector-ref phrases (random-index (vector-length phrases) bytes)))))

;; Shuffle a vector using secure randomness.
(define (random-shuffle vec)
  ;; Knuth TAoCP Algorithm P
  ;; This is a destructive algorithm, so defensively copy the vector first
  (let ((vec (vector-copy vec)))
    (do ((i (1- (vector-length vec)) (1- i)))
        ((< i 1) vec)                   ; return the final copy vector when done
      (let ((j (random-index (1+ i))))
        ;; Now j is some number 0 <= j <= i
        ;; Swap vec[i] and vec[j], then continue to the next iteration
        (let ((tmp (vector-ref vec i)))
          (vector-set! vec i (vector-ref vec j))
          (vector-set! vec j tmp))))))

;; Logarithm base 2.
(define (log2 val)
  (/ (log val) (log 2)))

;; The number of bits of entropy per phrase.
(define (phrase-entropy phrases)
  (log2 (vector-length phrases)))
