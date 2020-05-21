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

(define-public sbcl-quantile-estimator
  (package
    (name "sbcl-quantile-estimator")
    (version "0.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/deadtrickster/quantile-estimator.cl")
             (commit "84d0ea405d793f5e808c68c4ddaf25417b0ff8e5")))
       (sha256
        (base32
         "0rlswkf0siaabsvvch3dgxmg45fw5w8pd9b7ri2w7a298aya52z9"))))
    (build-system asdf-build-system/sbcl)
    ;; (arguments
    ;;  `(#:asd-file "quantile-estimator.asd"
    ;;    #:test-asd-file "quantile-estimator.test.asd"))
    (inputs
     `(("alexandria" ,sbcl-alexandria)))
    (home-page "https://github.com/deadtrickster/quantile-estimator.cl")
    (synopsis
     "Common Lisp implementation of Graham Cormode and S.
Muthukrishnan's Effective Computation of Biased Quantiles over Data
Streams in ICDE’05")
    (description
     "Common Lisp implementation of Graham Cormode and S.
Muthukrishnan's Effective Computation of Biased Quantiles over Data
Streams in ICDE’05")
    (license license:expat)))

(define-public sbcl-prometheus
  (package
    (name "sbcl-prometheus")
    (version "0.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/deadtrickster/prometheus.cl.git")
             (commit "7352b92296996ff383503e19bdd3bcea30409a15")))
       (sha256
        (base32
         "0fzczls2kfgdx18pja4lqxjrz72i583185d8nq0pb3s331hhzh0z"))))
    (build-system asdf-build-system/sbcl)
    ;; (arguments
    ;;  `(#:asd-file "prometheus.asd"))
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("bordeaux-threads" ,sbcl-bordeaux-threads)
       ("cl-ppcre" ,sbcl-cl-ppcre)
       ("local-time" ,sbcl-local-time)
       ("quantile-estimator" ,sbcl-quantile-estimator)))
    (home-page "https://github.com/deadtrickster/prometheus.cl")
    (synopsis "Prometheus.io Common Lisp client.")
    (description "Prometheus.io Common Lisp client.")
    (license license:expat)))

(define-public sbcl-prometheus.collectors.sbcl
  (package
    (inherit sbcl-prometheus)
    (name "sbcl-prometheus.collectors.sbcl")
    (inputs `(("prometheus" ,sbcl-prometheus)))
    (synopsis "Prometheus collector for SBCL metrics.")
    (description "Prometheus collector for SBCL metrics.")))

(define-public sbcl-prometheus.collectors.process
  (package
    (inherit sbcl-prometheus)
    (name "sbcl-prometheus.collectors.process")
    (inputs
     `(("prometheus" ,sbcl-prometheus)
       ("cl-fad" ,sbcl-cl-fad)
       ("split-sequence" ,sbcl-split-sequence)
       ("cffi" ,sbcl-cffi)
       ("cffi-groven" ,sbcl-cffi-grovel)))
    (synopsis "Prometheus collector for process metrics.")
    (description "Prometheus collector for process metrics.")))

(define-public sbcl-prometheus.formats.text
  (package
    (inherit sbcl-prometheus)
    (name "sbcl-prometheus.formats.text")
    (inputs
     `(("prometheus" ,sbcl-prometheus)
       ("alexandria" ,sbcl-alexandria)))
    (synopsis "Prometheus client text format.")
    (description "Prometheus client text format.")))

(define-public sbcl-prometheus.exposers.hunchentoot
  (package
    (inherit sbcl-prometheus)
    (name "sbcl-prometheus.exposers.hunchentoot")
    (inputs
     `(("prometheus" ,sbcl-prometheus)
       ("prometheus.formats.text" ,sbcl-prometheus.formats.text)
       ("hunchentoot" ,sbcl-hunchentoot)
       ("trivial-utf-8" ,sbcl-trivial-utf-8)
       ("salza2" ,sbcl-salza2)))
    (synopsis "Prometheus collector for Hunchentoot metrics")
    (description "Prometheus collector for Hunchentoot metrics")))

(define-public sbcl-prometheus.pushgateway
  (package
    (inherit sbcl-prometheus)
    (name "sbcl-prometheus.pushgateway")
    (inputs
     `(("prometheus" ,sbcl-prometheus)
       ("prometheus.formats.text" ,sbcl-prometheus.formats.text)
       ("drakma" ,sbcl-drakma)))
    (synopsis "Prometheus Pushgateway client.")
    (description "Prometheus Pushgateway client.")))

(define-public sbcl-uuid
  (let ((commit "e7d6680c3138385c0708f7aaf0c96622eeb140e8"))
    (package
      (name "sbcl-uuid")
      (version (git-version "2012.12.26" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/dardoria/uuid.git")
               (commit commit)))
         (sha256
          (base32
           "0jnyp2kibcf5cwi60l6grjrj8wws9chasjvsw7xzwyym2lyid46f"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("ironclad" ,sbcl-ironclad)
         ("trivial-utf-8" ,sbcl-trivial-utf-8)))
      (home-page "https://github.com/dardoria/uuid")
      (synopsis
       "Common Lisp implementation of UUIDs according to RFC4122.")
      (description
       "Common Lisp implementation of UUIDs according to RFC4122.")
      (license license:llgpl))))

;; TODO: Uses ASDF's package-inferred-system which is not supported by
;; asdf-build-system/sbcl as of 2020-05-21. We should fix
;; asdf-build-system/sbcl.
(define-public sbcl-rove
  (package
    (name "sbcl-rove")
    (version "0.9.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fukamachi/rove.git")
             (commit "f3695db08203bf26f3b861dc22ac0f4257d3ec21")))
       (sha256
        (base32
         "07ala4l2fncxf540fzxj3h5mhi9i4wqllhj0rqk8m2ljl5zbz89q"))))
    (build-system asdf-build-system/sbcl)
    (inputs
     `(("dissect" ,sbcl-dissect)
       ("bordeaux-threads" ,sbcl-bordeaux-threads)
       ("trivial-gray-streams" ,sbcl-trivial-gray-streams)))
    (home-page "https://github.com/fukamachi/rove.git")
    (synopsis
     "#1=(yet another . #1#) common lisp testing library")
    (description
     "Rove is a unit testing framework for Common Lisp applications.
This is intended to be a successor of Prove.")
    (license license:bsd-3)))

(define-public sbcl-cl-apache-arrow
  (let ((commit "486481a28a9e0056a1826167f65fa5a07fd08499"))
    (package
      (name "sbcl-cl-apache-arrow")
      (version (git-version "1.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/kat-co/cl-apache-arrow.git")
               (commit commit)))
         (sha256
          (base32
           "0fg7j1pcpk6x620fjsijzvf5sj3igiayf5l7f4qx0hpdz7rqwk5i"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       ;; Tests use Rove which is a package-inferred system
       `(#:tests? #f))
      (inputs
       `(("cl-gobject-introspection" ,sbcl-cl-gobject-introspection)
         ("closer-mop" ,sbcl-closer-mop)
         ("trivial-garbage" ,sbcl-trivial-garbage)
         ("apache-arrow" ,apache-arrow "lib")
         ("apache-arrow-c-glib" ,apache-arrow-c-glib)))
      (home-page "https://github.com/kat-co/cl-apache-arrow")
      (synopsis
       "A library for working with Apache Arrow and Parquet data.")
      (description
       "This is a library for working with Apache Arrow and Parquet
data. It is a wrapper around the official Apache GLib library using
GObject Introspection, which in turn is a wrapper around the C++
library.")
      (license license:asl2.0))))

(define-public sbcl-dissect
  (let ((commit "cffd38479f0e64e805f167bbdb240b783ecc8d45"))
    (package
      (name "sbcl-dissect")
      (version (git-version "1.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Shinmera/dissect.git")
               (commit commit)))
         (sha256
          (base32
           "0rmsjkgjl90gl6ssvgd60hb0d5diyhsiyypvw9hbc0ripvbmk5r5"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("cl-ppcre" ,sbcl-cl-ppcre)))
      (home-page "https://shinmera.github.io/dissect/")
      (synopsis
       "Common Lisp introspection library for the call stack and
restarts.")
      (description
       "Dissect is a small library for introspecting the call stack
and active restarts.")
      (license license:zlib))))

(define-public sbcl-exponential-backoff
  (let ((commit "8d9e8444d8b3184a524c12ce3449f91613ab714f"))
    (package
      (name "sbcl-exponential-backoff")
      (version (git-version "0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/death/exponential-backoff.git")
               (commit commit)))
         (sha256
          (base32
           "1389hm9hxv85s0125ja4js1bvh8ay4dsy9q1gaynjv27ynik6gmv"))))
      (build-system asdf-build-system/sbcl)
      (home-page "https://github.com/death/exponential-backoff")
      (synopsis
       "An implementation of the exponential backoff algorithm in
Common Lisp.")
      (description
       "An implementation of the exponential backoff algorithm in
Common Lisp. Inspired by the implementation found in Chromium. Read
the header file to learn about each of the parameters.")
      (license license:expat))))

(define-public sbcl-sxql
  (let ((commit "5aa8b739492c5829e8623432b5d46482263990e8"))
    (package
      (name "sbcl-sxql")
      (version (git-version "0.1.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/fukamachi/sxql.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0k25p6w2ld9cn8q8s20lda6yjfyp4q89219sviayfgixnj27avnj"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       `(#:test-asd-file "sxql-test.asd"))
      (inputs
       `(("optima" ,sbcl-optima)
         ("iterate" ,sbcl-iterate)
         ("cl-syntax-annot" ,sbcl-cl-syntax-annot)
         ("trivial-types" ,sbcl-trivial-types)
         ("split-sequence" ,sbcl-split-sequence)
         ("alexandria" ,sbcl-alexandria)))
      (native-inputs
       `(("prove" ,sbcl-prove)
         ("prove-asdf" ,sbcl-prove-asdf)))
      (home-page "https://github.com/fukamachi/sxql")
      (synopsis "An SQL generator for Common Lisp.")
      (description "An SQL generator for Common Lisp.")
      (license license:bsd-3))))

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
