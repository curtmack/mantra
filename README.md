# mantra

A pronounceable password generator in Guile Scheme.

## Requirements

* A system with /dev/random and /dev/urandom (i.e. all modern POSIX systems,
  including Cygwin)
* GNU Guile 2.0 or later (tested in 2.0.13 and 2.2.4)

## Usage

```
usage: mantra [options]
    -l, --min-letter-phrases X          Generate at least X letter phrases
                                        (default 0)
    -d, --min-digit-symbol-phrases X    Generate at least X letter phrases
                                        (default 0)
    -e, --entropy X                     Set minimum entropy to X bits
                                        (default 40)

    -p, --print-entropy                 Print the actual entropy of each
                                        generated password
    -n, --password-count N              Generate N passwords (default 1)
    -b, --blocking                      Use /dev/random instead of /dev/urandom
    -q, --no-nl                         Suppress the final terminating newline

    -v, --version                       Display program version
    -h, --help                          Display this help
```

Currently there is no real build or install process, and the main script must be
in the same directory as its dependencies. You may symlink the script to a more
convenient directory if you wish.

## Disclaimers

* The current implementation relies on /dev/urandom (or, with `--blocking`,
  /dev/random) providing cryptographically secure randomness. While this is true
  of all modern POSIX systems that I'm aware of, you may want to double-check
  that it's true for _your_ modern POSIX system before using this.
* While the default entropy of 40 bits is sufficient for many use cases, it is
  on the low side for security-critical applications.
* No effort is made to filter profanity or slurs from the output, as this would
  impact password entropy in a way that's difficult to calculate.
* Although I work with security-critical software on a regular basis, I make no
  claim to be an expert on cryptography, and this is a best-effort
  implementation.
