;;; Copyright © 2020 Katherine Cox-Buday <cox.katherine.e@gmail.com>
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

  #:use-module (upstream packages databases)
  #:use-module (upstream packages networking))

;; (define-public sbcl-cl-apache-arrow
;;   (let ((commit "486481a28a9e0056a1826167f65fa5a07fd08499"))
;;     (package
;;       (name "sbcl-cl-apache-arrow")
;;       (version (git-version "1.0.0" "1" commit))
;;       (source
;;        (origin
;;          (method git-fetch)
;;          (uri (git-reference
;;                (url "https://github.com/kat-co/cl-apache-arrow.git")
;;                (commit version)))
;;          (file-name (git-file-name name version))
;;          (sha256
;;           (base32
;;            "0fg7j1pcpk6x620fjsijzvf5sj3igiayf5l7f4qx0hpdz7rqwk5i"))))
;;       (build-system asdf-build-system/sbcl)
;;       ;; (arguments
;;       ;;  ;; Tests use Rove which is a package-inferred system
;;       ;;  `(#:tests? #f))
;;       (inputs
;;        `(("cl-gobject-introspection" ,sbcl-cl-gobject-introspection)
;;          ("closer-mop" ,sbcl-closer-mop)
;;          ("trivial-garbage" ,sbcl-trivial-garbage)
;;          ("apache-arrow" ,apache-arrow "lib")
;;          ("apache-arrow-c-glib" ,apache-arrow-c-glib)))
;;       (native-inputs
;;        `(("rove" ,cl-rove)))
;;       (home-page "https://github.com/kat-co/cl-apache-arrow")
;;       (synopsis
;;        "A library for working with Apache Arrow and Parquet data.")
;;       (description
;;        "This is a library for working with Apache Arrow and Parquet
;; data. It is a wrapper around the official Apache GLib library using
;; GObject Introspection, which in turn is a wrapper around the C++
;; library.")
;;       (license license:asl2.0))))

(define-public sbcl-oneam
  (let ((commit "8b1da94eca4613fd8a20bdf63f0e609e379b0ba5"))
    (package
      (name "sbcl-oneam")
      (version (git-version "0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/lmj/1am.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "05ss4nz1jb9kb796295482b62w5cj29msfj8zis33sp2rw2vmv2g"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       `(#:asd-system-name "1am"))
      (home-page "https://github.com/lmj/1am")
      (synopsis "A minimal testing framework for Common Lisp.")
      (description "A minimal testing framework for Common Lisp.")
      (license license:expat))))

(define-public cl-oneam
  (sbcl-package->cl-source-package sbcl-oneam))

(define-public ecl-oneam
  (sbcl-package->ecl-package sbcl-oneam))

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
