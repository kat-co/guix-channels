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

(define-module (non-free packages javascript)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system copy)

  #:use-module (gnu packages node))

(define-public kibana
  (package
   (name "kibana")
   (version "7.7.0")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://artifacts.elastic.co/downloads/kibana/"
           "kibana-oss-" version "-linux-x86_64.tar.gz"))
     (sha256
      (base32
       "0bb8rldgmkrlxy08bnz5yg4gm7rcdfcc7zcm4w16f46kjh8f7g45"))))
   (build-system copy-build-system)
   (arguments
    `(#:install-plan
      ;; This is a pre-built package. It will require a lot of work in
      ;; Guix to become a good-citizen. Since that kind of package
      ;; will probably look quite different than anything I do here, I
      ;; am foregoing any attempt at modifying the environment it
      ;; expects to run in. Therefore, it will live in /opt in its
      ;; entirety.
      ;;
      ;; But the least we can do is avoid embedding an entirely new
      ;; Node.
      `(("." "opt/kibana"
         #:exclude-regexp ("^node/")))
      #:phases
      (modify-phases
       %standard-phases
       (add-after 'unpack 'set-node-location
                  (lambda* (#:key inputs #:allow-other-keys)
                    (substitute* "bin/kibana"
                                 (("^NODE=.*$")
                                  (string-append
                                   "NODE="
                                   (assoc-ref inputs "node")
                                   "/bin/node\n"))))))))
   (inputs
    `(("node" ,node)))
   (home-page "https://www.elastic.co/kibana")
   (synopsis "Your window into the Elastic Stack")
   (description
    "Kibana is a free and open user interface that lets you
visualize your Elasticsearch data and navigate the Elastic Stack. Do
anything from tracking query load to understanding the way requests
flow through your apps.")
   (license license:asl2.0)))
