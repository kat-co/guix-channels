;;; Copyright © 2024 Katherine Cox-Buday <cox.katherine.e@gmail.com>
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

(define-module (upstream packages rust)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system cmake)
  #:use-module (guix build utils)
  #:use-module (guix transformations)
  #:use-module (guix utils)

  #:use-module (gnu packages check)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages rust))

(define-public rust-encode-unicode-1
  (package
   (name "rust-encode-unicode")
   (version "1.0.0")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "encode_unicode" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "1h5j7j7byi289by63s3w4a8b3g6l5ccdrws7a67nn07vdxj77ail"))))
   (build-system cargo-build-system)
   (arguments
    `(#:skip-build? #t
      #:cargo-inputs (("rust-ascii" ,rust-ascii-1))))
   (home-page "https://github.com/tormol/encode_unicode")
   (synopsis
    "UTF-8 and UTF-16 character types, iterators and related methods for char, u8 and u16.
")
   (description
    "UTF-8 and UTF-16 character types, iterators and related methods for char, u8 and
u16.")
   (license (list license:asl2.0 license:expat))))

(define-public rust-prettytable-0.10
  (package
   (name "rust-prettytable-rs")
   (version "0.10.0")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "prettytable" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "10g7081qxpzvv17jvihf4mx56cb8z7y3k6fkh8s9lz5ps4h0aj26"))))
   (build-system cargo-build-system)
   (arguments
    `(#:skip-build? #t
      #:cargo-inputs (("rust-csv" ,rust-csv-1)
                      ("rust-encode-unicode" ,rust-encode-unicode-1)
                      ("rust-is-terminal" ,rust-is-terminal-0.4)
                      ("rust-lazy-static" ,rust-lazy-static-1)
                      ("rust-term" ,rust-term-0.7)
                      ("rust-unicode-width" ,rust-unicode-width-0.1))))
   (home-page "https://github.com/phsym/prettytable-rs")
   (synopsis "A library for printing pretty formatted tables in terminal")
   (description
    "This package provides a library for printing pretty formatted tables in terminal")
   (license license:bsd-3)))

(define-public rust-smol-str-0.2
  (package
    (name "rust-smol-str")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "smol_str" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0jca0hyrwnv428q5gxhn2s8jsvrrkyrb0fyla9x37056mmimb176"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-arbitrary" ,rust-arbitrary-1)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/rust-analyzer/smol_str")
    (synopsis "small-string optimized string type with O(1) clone")
    (description "small-string optimized string type with O(1) clone")
    (license (list license:expat license:asl2.0))))

(define-public rust-leb128-0.2
  (package
    (name "rust-leb128")
    (version "0.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "leb128" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0rxxjdn76sjbrb08s4bi7m4x47zg68f71jzgx8ww7j0cnivjckl8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/gimli-rs/leb128")
    (synopsis
     "Read and write DWARF's \"Little Endian Base 128\" (LEB128) variable length integer encoding.")
    (description
     "Read and write DWARF's \"Little Endian Base 128\" (LEB128) variable length integer
encoding.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-itertools-0.12
  (package
    (name "rust-itertools")
    (version "0.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "itertools" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1c07gzdlc6a1c8p8jrvvw3gs52bss3y58cs2s21d9i978l36pnr5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-either" ,rust-either-1))))
    (home-page "https://github.com/rust-itertools/itertools")
    (synopsis
     "Extra iterator adaptors, iterator methods, free functions, and macros.")
    (description
     "Extra iterator adaptors, iterator methods, free functions, and macros.")
    (license (list license:expat license:asl2.0))))

(define-public rust-im-15
  (package
    (name "rust-im")
    (version "15.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "im" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1sg0jy9y0l3lqjpjyclj6kspi027mx177dgrmacgjni8y0zx7b6h"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-arbitrary" ,rust-arbitrary-1)
                       ("rust-bitmaps" ,rust-bitmaps-2)
                       ("rust-proptest" ,rust-proptest-1)
                       ("rust-quickcheck" ,rust-quickcheck-1)
                       ("rust-rand-core" ,rust-rand-core-0.6)
                       ("rust-rand-xoshiro" ,rust-rand-xoshiro-0.6)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-refpool" ,rust-refpool-0.4)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-sized-chunks" ,rust-sized-chunks-0.6)
                       ("rust-typenum" ,rust-typenum-1)
                       ("rust-version-check" ,rust-version-check-0.9))))
    (home-page "http://immutable.rs/")
    (synopsis "Immutable collection datatypes")
    (description "Immutable collection datatypes")
    (license license:mpl2.0)))

(define-public rust-decorum-0.3
  (package
    (name "rust-decorum")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "decorum" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1kz5zlh2j345vgr9fbs223wxgz0hd3jkndj91hzmqkx1r39mj5r8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-approx" ,rust-approx-0.3)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-derive" ,rust-serde-derive-1))))
    (home-page "https://github.com/olson-sean-k/decorum")
    (synopsis
     "Total ordering, equivalence, hashing, and constraints for floating-point types.")
    (description
     "Total ordering, equivalence, hashing, and constraints for floating-point types.")
    (license license:expat)))

(define-public rust-automerge-test-0.4
  (package
    (name "rust-automerge-test")
    (version "0.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "automerge-test" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0rfgk4j44jn999c1cmh053va122qa6lj4ng1bg2bn5vpns14d488"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-automerge" ,rust-automerge-0.5)
                       ("rust-decorum" ,rust-decorum-0.3)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-smol-str" ,rust-smol-str-0.2))))
    (home-page "https://github.com/automerge/automerge")
    (synopsis "Utilities for testing automerge libraries")
    (description "Utilities for testing automerge libraries")
    (license license:expat)))

(define-public rust-automerge-0.5
  (package
   (name "rust-automerge")
   (version "0.5.7")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "automerge" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "10151zd765bddbqj1aa9nf2hzscx0dafl9i9l8nq0czikryp8nf0"))))
   (build-system cargo-build-system)
   (arguments
    `(#:tests? #f ;; TODO(katco): Spend time getting assert_doc macro to resolve
      #:cargo-inputs (("rust-dot" ,rust-dot-0.1)
                      ("rust-flate2" ,rust-flate2-1)
                      ("rust-fxhash" ,rust-fxhash-0.2)
                      ("rust-hex" ,rust-hex-0.4)
                      ("rust-im" ,rust-im-15)
                      ("rust-itertools" ,rust-itertools-0.12)
                      ("rust-js-sys" ,rust-js-sys-0.3)
                      ("rust-leb128" ,rust-leb128-0.2)
                      ("rust-rand" ,rust-rand-0.8)
                      ("rust-serde" ,rust-serde-1)
                      ("rust-sha2" ,rust-sha2-0.10)
                      ("rust-smol-str" ,rust-smol-str-0.2)
                      ("rust-thiserror" ,rust-thiserror-1)
                      ("rust-tinyvec" ,rust-tinyvec-1)
                      ("rust-tracing" ,rust-tracing-0.1)
                      ("rust-unicode-segmentation" ,rust-unicode-segmentation-1)
                      ("rust-uuid" ,rust-uuid-1)
                      ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                      ("rust-web-sys" ,rust-web-sys-0.3))
      #:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.4)
                                  ("rust-maplit" ,rust-maplit-1)
                                  ("rust-pretty-assertions" ,rust-pretty-assertions-1)
                                  ("rust-prettytable" ,rust-prettytable-0.10)
                                  ("rust-proptest" ,rust-proptest-1)
                                  ("rust-serde-json" ,rust-serde-json-1)
                                  ("rust-test-log" ,rust-test-log-0.2)
                                  ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3)
                                  ("rust-automerge-test" ,rust-automerge-test-0.4))))
   (home-page "https://github.com/automerge/automerge")
   (synopsis
    "A JSON-like data structure (a CRDT) that can be modified concurrently by different users, and merged again automatically")
   (description
    "This package provides a JSON-like data structure (a CRDT) that can be modified
concurrently by different users, and merged again automatically")
   (license license:expat)))
