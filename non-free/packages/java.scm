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

(define-module (non-free packages java)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages java))

(define-public elasticsearch
  (package
   (name "elasticsearch")
   (version "7.7.0")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://artifacts.elastic.co/downloads/elasticsearch/"
           "elasticsearch-oss-" version
           "-no-jdk-linux-x86_64.tar.gz"))
     (sha256
      (base32
       "1p5svi3i7l6nhxkapcgjlx314n3l7cdd466xq85rg43mnvq0v4qw"))))
   (build-system copy-build-system)
   (arguments
    `(#:install-plan
      ;; This is a pre-built package. It will require a lot of work
      ;; in Guix to become a good-citizen. Since that kind of
      ;; package will probably look quite different than anything I
      ;; do here, I am foregoing any attempt at modifying the
      ;; environment it expects to run in. Therefore, it will live
      ;; in /opt in its entirety.
      `(("." ,(string-append "opt/elasticsearch")))
      #:phases
      (modify-phases
       %standard-phases
       (add-after
        'unpack 'remove-log-files
        (lambda _
          (substitute* "config/jvm.options"
                       ;; It is intended that Shepherd will handle
                       ;; logging.
                       (("file=[^:]+") "stderr")
                       (("^-XX:ErrorFile=.*$") "")
                       ;; Remove the filecount & filesize options
                       ((":filecount=.+$") "")))))))
   (inputs
    `(("openjdk" ,openjdk12)))
   (home-page "https://www.elastic.co/elasticsearch/")
   (synopsis "Open Source, Distributed, RESTful Search Engine")
   (description
    "Elasticsearch is a distributed, RESTful search and analytics
engine capable of addressing a growing number of use cases. As the
heart of the Elastic Stack, it centrally stores your data for
lightning fast search, fine‑tuned relevancy, and powerful analytics
that scale with ease.")
   (license license:asl2.0)))
