;;; Copyright © 2022 Katherine Cox-Buday <cox.katherine.e@gmail.com>
;;;
;;; This is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this. If not, see <http://www.gnu.org/licenses/>.

(define-module (upstream packages perl)
  #:use-module (guix build-system perl)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)

  #:use-module (gnu packages bash)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages libevent))

(define-public perl-languageserver
  (let ((commit "0fabd96215814f0aad18cc7bbea84dc10a83d67b")
        (revision "0"))
    (package
     (name "perl-languageserver")
     (version (git-version "2.3.0" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/richterger/Perl-LanguageServer")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "13nk1a7r47qpk1c8va711mbqcgdmxff0qhcdap86ar3afj25j8js"))))
     (build-system perl-build-system)
     (propagated-inputs
      (list perl-anyevent perl-anyevent-aio perl-class-refresh perl-compiler-lexer
            perl-coro perl-data-dump perl-io-aio perl-json perl-moose
            perl-padwalker))
     (home-page "https://github.com/richterger/Perl-LanguageServer")
     (synopsis "Language Server and Debug Protocol Adapter for Perl")
     (description
      "Implements the Language Server Protocol which provides syntax-checking, symbol
search, etc. Perl to various editors, for example Visual Studio Code or Atom.")
     ;; MIT license
     (license license:expat))))

(define-public perl-coro
  (package
   (name "perl-coro")
   (version "6.57")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://cpan/authors/id/M/ML/MLEHMANN/Coro-"
                                version ".tar.gz"))
            (sha256
             (base32
              "1ihl2zaiafr2k5jzj46j44j8vxqs23fqcsahypmi23jl6f0f8a0r"))))
   (build-system perl-build-system)
   (native-inputs (list perl-canary-stability))
   (propagated-inputs (list perl-anyevent perl-anyevent-aio perl-anyevent-bdb
                            perl-bdb perl-common-sense perl-ev perl-event
                            perl-guard perl-io-aio))
   (home-page "https://metacpan.org/release/Coro")
   (synopsis "the only real threads in perl")
   (description
    "This module collection manages continuations in general, most often in the form
of cooperative threads (also called coros, or simply \"coro\" in the
documentation). They are similar to kernel threads but don't (in general) run in
parallel at the same time even on SMP machines. ")
   (license license:perl-license)))

(define-public perl-anyevent-aio
  (package
   (name "perl-anyevent-aio")
   (version "1.1")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "mirror://cpan/authors/id/M/ML/MLEHMANN/AnyEvent-AIO-"
                  version ".tar.gz"))
            (sha256
             (base32
              "0svh0mlp17g0ypq8bgs3h3axg8v7h0z45hryacgn6q8mcj65n43b"))))
   (build-system perl-build-system)
   (propagated-inputs (list perl-anyevent perl-io-aio))
   (home-page "https://metacpan.org/release/AnyEvent-AIO")
   (synopsis "Truly asynchronous file and directory I/O")
   (description
    "Loading this module will install the necessary magic to seamlessly integrate
IO::AIO into AnyEvent, i.e. you no longer need to concern yourself with calling
IO::AIO::poll_cb or any of that stuff (you still can, but this module will do it
in case you don't).")
   (license license:perl-license)))

(define-public perl-class-refresh
  (package
   (name "perl-class-refresh")
   (version "0.07")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "mirror://cpan/authors/id/D/DO/DOY/Class-Refresh-" version
                  ".tar.gz"))
            (sha256
             (base32
              "1cxyshgxpk98icwws2jpgix6m53qsn43c8izxqm5mcybam9h7c73"))))
   (build-system perl-build-system)
   (native-inputs (list perl-test-fatal perl-test-requires))
   (propagated-inputs (list perl-class-load perl-class-unload
                            perl-devel-overrideglobalrequire perl-try-tiny))
   (home-page "https://metacpan.org/release/Class-Refresh")
   (synopsis "Refresh your classes during runtime")
   (description
    "This module allows you to reload your application classes on the fly, so that
the code/test cycle becomes a lot easier.")
   (license license:perl-license)))

(define-public perl-compiler-lexer
  (package
   (name "perl-compiler-lexer")
   (version "0.23")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "mirror://cpan/authors/id/G/GO/GOCCY/Compiler-Lexer-"
                  version ".tar.gz"))
            (sha256
             (base32
              "1nwr1bzq2yg47322hf02j5732cl2gfz4jj97594lzymvzr5cwcb0"))))
   (build-system perl-build-system)
   (native-inputs `(("perl-module-build" ,perl-module-build) ("perl-module-build-xsutil" ,perl-module-build-xsutil)))
   (home-page "https://metacpan.org/release/Compiler-Lexer")
   (synopsis "Lexical Analyzer for Perl5")
   (description "Lexical Analyzer for Perl5")
   (license license:perl-license)))

(define-public perl-io-aio
  (package
   (name "perl-io-aio")
   (version "4.76")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "mirror://cpan/authors/id/M/ML/MLEHMANN/IO-AIO-" version
                  ".tar.gz"))
            (sha256
             (base32
              "13r06d1mvp884x8f8lzdl1w3s6l82m37mrxg12wi88ffjd509x9r"))))
   (build-system perl-build-system)
   (arguments
    `(#:phases
      (modify-phases %standard-phases
                     (add-before 'configure 'set-shell
                                 (lambda* (#:key inputs #:allow-other-keys)
                                   (let ((sh (which "sh")))
                                     (setenv "CONFIG_SHELL" sh)))))))
   (native-inputs (list perl-canary-stability))
   (propagated-inputs (list perl-common-sense))
   (home-page "https://metacpan.org/release/IO-AIO")
   (synopsis "Asynchronous/Advanced Input/Output")
   (description
    "Implements asynchronous I/O using whatever means your operating system
supports. It is implemented as an interface to libeio.")
   (license license:perl-license)))

(define-public perl-devel-overrideglobalrequire
  (package
   (name "perl-devel-overrideglobalrequire")
   (version "0.001")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "mirror://cpan/authors/id/D/DA/DAGOLDEN/Devel-OverrideGlobalRequire-"
                  version ".tar.gz"))
            (sha256
             (base32
              "1yayg0838b58dqg8acahhw885s0yvchjyf2fm7s2ladfwcnqk487"))))
   (build-system perl-build-system)
   (home-page "https://metacpan.org/release/Devel-OverrideGlobalRequire")
   (synopsis "Override CORE::GLOBAL::require safely")
   (description
    "Overrides CORE::GLOBAL::require with a code reference in a way that plays nice
with any existing overloading and ensures the right calling package is in
scope.")
   (license license:perl-license)))

(define-public perl-anyevent-bdb
  (package
   (name "perl-anyevent-bdb")
   (version "1.1")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "mirror://cpan/authors/id/M/ML/MLEHMANN/AnyEvent-BDB-"
                  version ".tar.gz"))
            (sha256
             (base32
              "1j7gb94kzwpb5w2ww5mz3a6ys4jysvnzmf9ibxp64r04jh861qwk"))))
   (build-system perl-build-system)
   (arguments
    `(#:tests? #f))
   (propagated-inputs (list perl-anyevent perl-bdb))
   (home-page "https://metacpan.org/release/AnyEvent-BDB")
   (synopsis "truly asynchronous berkeley db access")
   (description
    "Loading this module will install the necessary magic to seamlessly integrate
BDB into AnyEvent, i.e. you no longer need to concern yourself with calling
BDB::poll_cb or any of that stuff (you still can, but this module will do it in
case you don't).")
   (license license:perl-license)))

(define-public perl-bdb
  (package
   (name "perl-bdb")
   (version "1.92")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://cpan/authors/id/M/ML/MLEHMANN/BDB-"
                                version ".tar.gz"))
            (sha256
             (base32
              "0szbvbcvkawfsyxkhzkrglzdlbr9p6f2z2wh82m1mz5f5ffwmwm3"))))
   (build-system perl-build-system)
   (arguments
    `(#:tests? #f))
   (inputs (list bdb))
   (propagated-inputs (list perl-common-sense))
   (home-page "https://metacpan.org/release/BDB")
   (synopsis "Asynchronous Berkeley DB access")
   (description
    "See the BerkeleyDB
documentation (http://www.oracle.com/technology/documentation/berkeley-db/db/index.html).
The BDB API is very similar to the C API (the translation has been very
faithful).")
   (license license:perl-license)))

(define-public perl-event
  (package
   (name "perl-event")
   (version "1.28")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://cpan/authors/id/E/ET/ETJ/Event-"
                                version ".tar.gz"))
            (sha256
             (base32
              "1i0ca4hkp386fq9cxkbzrm7qb8gm8j8f417bir0yzg5254i28105"))))
   (build-system perl-build-system)
   (home-page "https://metacpan.org/release/Event")
   (synopsis "Event loop processing")
   (description
    "The Event module provide a central facility to watch for various types of
events and invoke a callback when these events occur. The idea is to delay the
handling of events so that they may be dispatched in priority order when it is
safe for callbacks to execute.")
   (license license:perl-license)))

(define-public perl-yaml-syck
  (package
   (name "perl-yaml-syck")
   (version "1.34")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "mirror://cpan/authors/id/T/TO/TODDR/YAML-Syck-" version
                  ".tar.gz"))
            (sha256
             (base32
              "0na1wg3d7ykzy5i44w6i1s37ymq6x0cvcc9gzvmri9xxmv65d4fc"))))
   (build-system perl-build-system)
   (home-page "https://metacpan.org/release/YAML-Syck")
   (synopsis "Fast, lightweight YAML loader and dumper")
   (description
    "This module provides a Perl interface to the libsyck data serialization
library. It exports the Dump and Load functions for converting Perl data
structures to YAML strings, and the other way around.")
   (license license:x11)))

(define-public perl-net-ip
  (package
   (name "perl-net-ip")
   (version "1.26")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://cpan/authors/id/M/MA/MANU/Net-IP-"
                                version ".tar.gz"))
            (sha256
             (base32
              "0ffn2xqqbkfi7v303sp5dwgbv36jah3vg8r4nxhxfiv60vric3q4"))))
   (build-system perl-build-system)
   (home-page "https://metacpan.org/release/Net-IP")
   (synopsis "Perl extension for manipulating IPv4/IPv6 addresses")
   (description
    "This is the Net::IP module, designed to allow easy manipulation of IPv4 and
IPv6 addresses.")
   (license license:perl-license)))
