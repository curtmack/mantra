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

(define-module (rules)
  #:export (phrases))

(use-modules (srfi srfi-43)
             (ice-9 control))

;; Generic for-each that works on lists, vectors, and strings
(define (for-each-seq f seq)
  (cond
   ((list? seq) (for-each f seq))
   ;; vector-for-each can't be rewound in a continuation
   ((vector? seq)
    (do ((i 0 (1+ i)))
        ((>= i (vector-length seq)))
      (f (vector-ref seq i))))
   ;; ditto string-for-each
   ((string? seq)
    (do ((i 0 (1+ i)))
        ((>= i (string-length seq)))
      (f (string-ref seq i))))))

;; For each clause provided, for each value in the `from' term of that clause,
;; bind `var' to that value, then choose from each subsequent clause. Once all
;; clauses have a choice bound to their variable, invoke `body' in a context
;; which contains those bindings. The body will ultimately be invoked once for
;; each member of the Cartesian product of all the `from' sequences.
(define-syntax choose
  (syntax-rules ()
    ((choose ((var from) more ...) body ...)
     (for-each-seq
      (lambda (var)
        (choose (more ...) body ...))
      from))
    ((choose () body ...)
     (begin body ...))))

;; Convenient syntax for invoking `choose' with a list of thunks.
(define-syntax choose-from
  (syntax-rules ()
    ((choose-from thunk thunk* ...)
     (choose ((th (list thunk thunk* ...))) (th)))))

(define emit-tag (make-prompt-tag "emit"))

;; Emit a value to an enclosing prompt.
(define (emit x)
  (abort-to-prompt emit-tag x))

;; Create a prompt in which emitted values are collected into a list, which is
;; returned once the entire collection process is complete.  The list contains
;; the values in the reverse of the order they were emitted.
(define (with-reverse-list-emitter thunk)
  (let* ((accum '()))
    (letrec ((handler
              (lambda (k x)
                (set! accum (cons x accum))
                (call-with-prompt
                    emit-tag
                  (lambda () (k x))
                  handler))))
      (call-with-prompt emit-tag thunk handler))
    accum))

;; Create a prompt in which emitted values are collected into a list, which is
;; returned once the entire collection process is complete.  The list contains
;; the values in the order they were emitted.
(define (with-list-emitter thunk)
  (reverse (with-reverse-list-emitter thunk)))

;; Create a prompt in which emitted values are collected into a vector, which is
;; returned once the entire collection process is complete.  The vector contains
;; the values in the order they were emitted.
(define (with-vector-emitter thunk)
  (reverse-list->vector (with-reverse-list-emitter thunk)))

;; These rules are based on very rough, non-scientific experiments I did while
;; writing the original Haskell version of mantra back in 2015. To my ear as an
;; English speaker raised in Nebraska, they produce memorable phrases with
;; obvious pronunciations. Feel free to customize as you wish; the program will
;; automatically adjust to the actual possibility space, so your entropy
;; measurements will still be accurate.

(define lower-hard-consonants "bcdfgjklmnprstvxz")
(define upper-hard-consonants (string-upcase lower-hard-consonants))

(define lower-soft-consonants "hwy")
(define upper-soft-consonants (string-upcase lower-soft-consonants))

(define lower-vowels "aeiou")
(define upper-vowels (string-upcase lower-vowels))

(define digits "0123456789")
(define symbols "!@#$%^&*()[]{}`~-+=_'\";:,.<>/?")

(define (hvs)
  (choose ((h upper-hard-consonants)
           (v lower-vowels)
           (s lower-soft-consonants))
          (emit (string h v s))))

(define (svh)
  (choose ((s upper-soft-consonants)
           (v lower-vowels)
           (h lower-hard-consonants))
          (emit (string s v h))))

(define (hvh)
  (choose ((h1 upper-hard-consonants)
           (v lower-vowels)
           (h2 lower-hard-consonants))
          (emit (string h1 v h2))))

(define (vhv)
  (choose ((v1 upper-vowels)
           (h lower-hard-consonants)
           (v2 lower-vowels))
          (emit (string v1 h v2))))

(define (digsym)
  (choose ((dig digits)
           (sym symbols))
          (emit (string dig sym))
          (emit (string sym dig))))

(define (vvh)
  ;; These are the vowel pairs allowed to begin a phrase, when followed by a
  ;; hard consonant.
  (choose ((vowels '("Ai" "Ae" "Ao" "Au"
                     "Ea" "Ee" "Ei" "Oa"
                     "Oi" "Oo" "Ou"))
           (h lower-hard-consonants))
          (emit (string-append vowels (string h)))))

(define (hvv)
  ;; These are the vowel pairs allowed to end a phrase, when preceded by a hard
  ;; consonant.
  (choose ((vowels '("ai" "ao" "au" "ea"
                     "ee" "ei" "eo" "eu"
                     "ia" "ie" "io" "oa"
                     "oe" "oi" "oo" "ou"
                     "ua" "ue" "uo"))
           (h upper-hard-consonants))
          (emit (string-append (string h) vowels))))

(define (ccv)
  ;; This table consists of:
  ;;  - A possible combination of two consonants that can start a phrase,
  ;;    followed by
  ;;  - A list of each vowel that is allowed to appear after that combination.
  ;; This gives us a little more control to help eliminate phrases that aren't
  ;; obviously pronounceable.
  (choose ((tab '(("Bl" #\a #\e #\i #\o #\u)
                  ("Br" #\a #\e #\i #\o #\u)
                  ("By" #\a #\e     #\o #\u) ; "yi" is not a sensible diphthong
                  ("Ch" #\a #\e #\i #\o #\u)
                  ("Cl" #\a #\e #\i #\o #\u)
                  ("Cr" #\a #\e #\i #\o #\u)
                  ("Dr" #\a #\e #\i #\o #\u)
                  ("Fl" #\a #\e #\i #\o #\u)
                  ("Fr" #\a #\e #\i #\o #\u)
                  ("Gl" #\a #\e #\i #\o #\u)
                  ("Gr" #\a #\e #\i #\o #\u)
                  ("Gy" #\a #\e     #\o #\u)
                  ("Kl" #\a #\e #\i #\o #\u)
                  ("Kr" #\a #\e #\i #\o #\u)
                  ("Ky" #\a #\e     #\o #\u)
                  ("Pl" #\a #\e #\i #\o #\u)
                  ("Pr" #\a #\e #\i #\o #\u)
                  ("Ps" #\a #\e #\i #\o #\u)
                  ("Pw" #\a #\e #\i #\o #\u)
                  ("Py" #\a #\e     #\o #\u)
                  ("Qu" #\a #\e #\i #\o #\u)
                  ("Sc" #\a #\e #\i #\o #\u)
                  ("Sh" #\a #\e #\i #\o #\u)
                  ("Sl" #\a #\e #\i #\o #\u)
                  ("Sm" #\a #\e #\i #\o #\u)
                  ("Sn" #\a #\e #\i #\o #\u)
                  ("Sp" #\a #\e #\i #\o #\u)
                  ("St" #\a #\e #\i #\o #\u)
                  ("Sw" #\a #\e #\i #\o #\u)
                  ("Th" #\a #\e #\i #\o #\u)
                  ("Tl" #\a #\e #\i #\o #\u)
                  ("Tr" #\a #\e #\i #\o #\u)
                  ("Ts" #\a #\e #\i #\o #\u)
                  ("Ty" #\a #\e     #\o #\u)
                  ("Vl" #\a #\e #\i #\o #\u)
                  ("Vr" #\a #\e #\i #\o #\u)
                  ("Wh" #\a #\e #\i #\o #\u))))
          (choose ((vowel (cdr tab)))
                  (emit (string-append (car tab) (string vowel))))))

(define (vcc)
  ;; This works the same as the previous table, but in reverse: it groups
  ;; terminating pairs of consonants with valid preceding vowels.
  (choose ((tab '(("bs" #\A #\E #\I #\O #\U)
                  ("ch" #\A #\E #\I #\O #\U)
                  ("cs" #\A #\E #\I #\O #\U)
                  ("ds" #\A #\E #\I #\O #\U)
                  ("ff" #\A #\E #\I #\O #\U)
                  ("ft" #\A #\E #\I #\O #\U)
                  ("lb" #\A #\E #\I #\O #\U)
                  ("lc" #\A #\E #\I #\O #\U)
                  ("lf" #\A #\E #\I #\O #\U)
                  ("lg" #\A #\E #\I #\O #\U)
                  ("lk" #\A #\E #\I #\O #\U)
                  ("ll" #\A #\E #\I #\O #\U)
                  ("lp" #\A #\E #\I #\O #\U)
                  ("ls" #\A #\E #\I #\O #\U)
                  ("lt" #\A #\E #\I #\O #\U)
                  ("lv" #\A #\E #\I #\O #\U)
                  ("lx" #\A #\E #\I #\O #\U)
                  ("lz" #\A #\E #\I #\O #\U)
                  ("mn" #\A #\E #\I #\O #\U)
                  ("rb" #\A #\E     #\O #\U) ; in most English accents
                  ("rc" #\A #\E     #\O #\U) ; "er" and "ir" sound similar
                  ("rd" #\A #\E     #\O #\U) ; eliminating "i" makes them
                  ("rf" #\A #\E     #\O #\U) ; more sensible IMHO
                  ("rg" #\A #\E     #\O #\U)
                  ("rk" #\A #\E     #\O #\U)
                  ("rl" #\A #\E     #\O #\U)
                  ("rp" #\A #\E     #\O #\U)
                  ("rr" #\A #\E     #\O #\U)
                  ("rs" #\A #\E     #\O #\U)
                  ("rt" #\A #\E     #\O #\U)
                  ("rv" #\A #\E     #\O #\U)
                  ("rx" #\A #\E     #\O #\U)
                  ("rz" #\A #\E     #\O #\U)
                  ("sc" #\A #\E #\I #\O #\U)
                  ("sh" #\A #\E #\I #\O #\U)
                  ("sk" #\A #\E #\I #\O #\U)
                  ("sm" #\A #\E #\I #\O #\U)
                  ("sp" #\A #\E #\I #\O #\U)
                  ("ss" #\A #\E #\I #\O #\U)
                  ("st" #\A #\E #\I #\O #\U)
                  ("th" #\A #\E #\I #\O #\U)
                  ("wb" #\A #\E     #\O    ) ; "iw" and "uw" aren't sensible
                  ("wc" #\A #\E     #\O    ) ; diphthongs in English
                  ("wd" #\A #\E     #\O    )
                  ("wf" #\A #\E     #\O    )
                  ("wg" #\A #\E     #\O    )
                  ("wk" #\A #\E     #\O    )
                  ("wl" #\A #\E     #\O    )
                  ("wp" #\A #\E     #\O    )
                  ("wr" #\A #\E     #\O    )
                  ("ws" #\A #\E     #\O    )
                  ("wt" #\A #\E     #\O    )
                  ("wv" #\A #\E     #\O    )
                  ("ww" #\A #\E     #\O    )
                  ("wx" #\A #\E     #\O    )
                  ("wz" #\A #\E     #\O    ))))
          (choose ((vowel (cdr tab)))
                  (emit (string-append (string vowel) (car tab))))))

(define phrases
  (with-vector-emitter
   (lambda ()
     (choose-from hvs svh hvh vhv vvh hvv ccv vcc digsym))))
