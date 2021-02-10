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

(define-module (mantra-libs main)
  #:use-module (mantra-libs rules)
  #:use-module (mantra-libs generator)
  #:use-module (mantra-libs plans)
  #:use-module (ice-9 format)
  #:use-module (ice-9 getopt-long)

  #:export (main))

(define program "mantra")
(define version '(2 0 0))

;; Option specification for use with getopt-long
(define option-spec
  `((allowed-symbols          (single-char #\s) (value #t))
    (min-letter-phrases       (single-char #\l) (value #t) (predicate ,string->number))
    (min-digit-symbol-phrases (single-char #\d) (value #t) (predicate ,string->number))
    (entropy                  (single-char #\e) (value #t) (predicate ,string->number))
    (print-entropy            (single-char #\p) (value #f))
    (password-count           (single-char #\n) (value #t) (predicate ,string->number))
    (blocking                 (single-char #\b) (value #f))
    (no-nl                    (single-char #\q) (value #f))
    (version                  (single-char #\v) (value #f))
    (help                     (single-char #\h) (value #f))))

;; The blocking parameter is defined in generator.scm
(define entropy        (make-parameter  #f))
(define print-entropy  (make-parameter  #f))
(define password-count (make-parameter  #f))
(define no-nl          (make-parameter  #f))

;; A default password entropy of 40 bits is slightly padded from zxcvbn's
;; requirement of 10^10 possibilities for a "very unguessable" score, which is
;; adequate for most uses while still generating short, easily remembered
;; passwords.
(define default-entropy        "40")
(define default-password-count  "1")

;; These can be tweaked to ensure a certain number of letter phrases
;; (respectively digit-symbol phrases) are added to the password.  This is
;; useful when trying to respect password complexity rules.
(define min-letter-phrases               (make-parameter #f))
(define min-digit-symbol-phrases         (make-parameter #f))
(define default-min-letter-phrases       "0")
(define default-min-digit-symbol-phrases "0")

;; Allowed char-set of symbols
(define allowed-symbols         (make-parameter #f))
(define default-allowed-symbols "!@#$%^&*()[]{}`~-+=_'\";:,.<>/?")

;; Print the version string
(define (print-version)
  (format #t "~a v~a.~a.~a~%"
          program
          (car version)
          (cadr version)
          (caddr version)))

;; Print the usage/help information.
(define (print-usage progname)
  (format #t "\
Generate random pronounceable passwords.
usage: ~a [options]
    -s, --allowed-symbols X           Digit/symbol phrases will only use the
                                      symbols in X
                                      (default ~a)
    -l, --min-letter-phrases X        Generate at least X letter phrases
                                      (default ~a)
    -d, --min-digit-symbol-phrases X  Generate at least X digit/symbol phrases
                                      (default ~a)
    -e, --entropy X                   Set minimum entropy to X bits
                                      (default ~a)

    -p, --print-entropy               Print the actual entropy of each
                                      generated password
    -n, --password-count N            Generate N passwords (default ~a)
    -b, --blocking                    Use /dev/random instead of /dev/urandom
    -q, --no-nl                       Suppress the final terminating newline

    -v, --version                     Display program version
    -h, --help                        Display this help~%"
          (basename progname)
          default-allowed-symbols
          default-min-letter-phrases
          default-min-digit-symbol-phrases
          default-entropy
          default-password-count))

;; Print `(password-count)' passwords of minimum entropy `(entropy)' bits.
(define (print-passwords)
  (call-with-values
      (lambda ()
        (initialize-phrase-vectors (allowed-symbols)))
    (lambda (letter-phrases digit-symbol-phrases phrases)
      (catch 'impossible-plan
        (lambda ()
          (let ((raw-plan (make-plan
                           phrases (entropy)
                           letter-phrases (min-letter-phrases)
                           digit-symbol-phrases (min-digit-symbol-phrases))))
            (when (print-entropy)
              (let* ((header (format #f "Entropy/password: ~,2f"
                                     (apply plan-entropy raw-plan)))
                     (header-underline (make-string (string-length header) #\-)))
                (display header)
                (newline)
                (display header-underline)
                (when (> (password-count) 0)
                  (newline))))
            (do ((i 1 (1+ i)))
                ((> i (password-count)))
              ;; Compile the plan here so that each password re-shuffles the phrase
              ;; layout
              (let ((compiled-plan (apply compile-plan raw-plan)))
                (display (random-password compiled-plan)))
              ;; Don't print newline after final password when no-nl is set
              (unless (and (= i (password-count))
                           (no-nl))
                (newline)))))
        (lambda (key . num)
          (format #t
                  "~a ~a~%"
                  "Arguments are unsatisfiable"
                  "(allowed-symbols is empty and min-digit-symbol-phrases > 0)"))))))

;; Script entrypoint
(define (main args)
  (let ((progname (car args))
        (options (getopt-long args option-spec)))
    ;; Check if the user is requesting version or help. Both can be requested
    ;; simultaneously, but if either is requested, normal output is suppressed.
    (let ((version-wanted (option-ref options 'version #f))
          (help-wanted    (option-ref options 'help    #f)))
      (if (or version-wanted help-wanted)
          (begin
            (when version-wanted (print-version))
            (when help-wanted (print-usage progname)))
          ;; Otherwise, set up to print passwords normally.
          (parameterize ((allowed-symbols
                          (string->char-set
                           (option-ref options 'allowed-symbols
                                       default-allowed-symbols)))
                         (min-letter-phrases
                          (string->number
                           (option-ref options 'min-letter-phrases
                                       default-min-letter-phrases)))
                         (min-digit-symbol-phrases
                          (string->number
                           (option-ref options 'min-digit-symbol-phrases
                                       default-min-digit-symbol-phrases)))
                         (entropy        (string->number
                                          (option-ref options 'entropy
                                                      default-entropy)))
                         (password-count (string->number
                                          (option-ref options 'password-count
                                                      default-password-count)))
                         (print-entropy  (option-ref options 'print-entropy #f))
                         (blocking       (option-ref options 'blocking #f))
                         (no-nl          (option-ref options 'no-nl    #f)))
            (print-passwords))))))
