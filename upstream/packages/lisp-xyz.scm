;;; Copyright © 2020, 2021 Katherine Cox-Buday <cox.katherine.e@gmail.com>
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

(define-module (upstream packages lisp-xyz)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix build-system asdf)

  #:use-module (gnu packages lisp)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages lisp-check)

  #:use-module (upstream packages networking))

(define-public sbcl-inotify
  (let ((commit "8ad433f646f0dd2205dbb2ec52663d6e9c0d9afe")
        (revision "1")
        (site "https://github.com/stassats/inotify"))
    (package
     (name "sbcl-inotify")
     (version (git-version "0.0.0" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url site)
             (commit commit)))
       (file-name (git-file-name "sbcl-inotify" version))
       (sha256
        (base32 "0jill05wsa7xbnkycc1ik1a05slv2h34fpyap2rxbnxvfjvyzw98"))))
     (build-system asdf-build-system/sbcl)
     (inputs
      `(("cffi" ,sbcl-cffi)
        ("iolib" ,sbcl-iolib)))
     (home-page site)
     (synopsis "FFI to inotify(7) for Common Lisp")
     (description
      "Interface to linux inotify(7).")
     (license license:public-domain))))

(define-public cl-inotify
  (sbcl-package->cl-source-package sbcl-inotify))

(define-public ecl-inotify
  (sbcl-package->ecl-package sbcl-inotify))

(define-public sbcl-pp-toml
  (let ((commit "dc880368933f5baea1584a9a40fbd171c642a3a3")
        (revision "1"))
    (package
      (name "sbcl-pp-toml")
      (version (git-version "1.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/pnathan/pp-toml")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1ivr0v7skdml6i3sjvb3x7cv7ynjmvpg0f8jm9bmkng6y7bc0xaa"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("cl-ppcre" ,sbcl-cl-ppcre)
         ("generic-comparability" ,sbcl-generic-comparability)
         ("local-time" ,sbcl-local-time)
         ("parse-number" ,sbcl-parse-number)
         ("split-sequence" ,sbcl-split-sequence)
         ("esrap" ,sbcl-esrap)))
      (home-page "https://github.com/pnathan/pp-toml")
      (synopsis "Paul's Parser for Tom's Own Minimal Language")
      (description "A Common Lisp TOML parser which supports the v0.1.0 spec.")
      (license license:llgpl))))

(define-public cl-pp-toml
  (sbcl-package->cl-source-package sbcl-pp-toml))

(define-public ecl-pp-toml
  (sbcl-package->ecl-package sbcl-pp-toml))
