;;; Copyright © 2019 Katherine Cox-Buday <cox.katherine.e@gmail.com>
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

(define-module (upstream packages logging)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages flex)
  #:use-module (upstream packages c))

(define-public rsyslog
  (package
    (name "rsyslog")
    (version "8.1907.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/"
                           "rsyslog/rsyslog/archive/"
                           "v" version ".tar.gz"))
       (sha256
        (base32
         "14piy5s7nm753ls1aijj221x5ip18phpp89wx62s7bizxp8h548f"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; autogen.sh calls configure at the end of the script.
         (replace 'bootstrap
           (lambda _ (invoke "autoreconf" "-vfi"))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)
       ("bison" ,bison)
       ("flex" ,flex)))
    (inputs
     `(("libestr" ,libestr)
       ("libfastjson" ,libfastjson)
       ("liblogging" ,liblogging)
       ("libz" ,zlib)
       ("libuuid" ,util-linux)
       ("libgcrypt" ,libgcrypt)
       ("libcurl" ,curl)))
    (home-page "https://www.rsyslog.com/")
    (synopsis "RSYSLOG is the rocket-fast system for log processing.")
    (description
     "It offers high-performance, great security features and a
modular design. While it started as a regular syslogd, rsyslog has
evolved into a kind of swiss army knife of logging, being able to
accept inputs from a wide variety of sources, transform them, and
output to the results to diverse destinations.

Rsyslog can deliver over one million messages per second to local
destinations when limited processing is applied (based on v7, December
2013). Even with remote destinations and more elaborate processing the
performance is usually considered \"stunning\".")
    (license (list license:gpl3
                   license:asl2.0
                   license:lgpl3))))
