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
  #:use-module (upstream packages networking))

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

(define-public sbcl-cl-rdkafka
  (package
    (name "sbcl-cl-rdkafka")
    (version "1.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/SahilKang/cl-rdkafka.git")
             (commit "4c05c1aaf59648161f2717b944f3c36b42472195")))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1qcgfd4h7syilzmrmd4z2vknbvawda3q3ykw7xm8n381syry4g82"))))
    (build-system asdf-build-system/sbcl)
    (arguments
     `(#:tests? #f ; Attempts to connect to locally running Kafka
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "src/low-level/librdkafka-bindings.lisp"
               (("librdkafka" all)
                (string-append (assoc-ref inputs "librdkafka") "/lib/"
                               all))))))))
    (inputs
     `(("cffi" ,sbcl-cffi)
       ("cffi" ,sbcl-cffi-grovel)
       ("trivial-garbage" ,sbcl-trivial-garbage)
       ("bordeaux-threads" ,sbcl-bordeaux-threads)
       ;; Attempting to refer to the precompiled version gives:
       ;; Error finding package for symbol "DECLARATION-INFORMATION":
       ;; The name "SB-CLTL2" does not designate any package.
       ("lparallel" ,cl-lparallel)
       ("librdkafka" ,librdkafka)))
    (propagated-inputs
     `(("librdkafka" ,librdkafka)))
    (home-page "https://github.com/SahilKang/cl-rdkafka")
    (synopsis "A Common Lisp client library for Apache Kafka.")
    (description "A Common Lisp client library for Apache Kafka.")
    (license license:gpl3)))

(define-public cl-rdkafka
  (sbcl-package->cl-source-package sbcl-cl-rdkafka))

(define-public ecl-cl-rdkafka
  (sbcl-package->ecl-package sbcl-cl-rdkafka))
