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

(define-module (upstream packages c)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python-xyz))

(define-public libestr
  (package
    (name "libestr")
    (version "0.1.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://github.com/"
                           "rsyslog/libestr/archive/"
                           "v" version ".tar.gz"))
       (sha256
        (base32
         "1d3bliacvadafsmyya8f3pp1g7fzr7q4aylj3nc85zbmz203pda6"))))
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
       ("pkg-config" ,pkg-config)
       ("libtool" ,libtool)))
    (home-page "https://github.com/rsyslog/libestr")
    (synopsis "Helper functions for handling strings.")
    (description
     "A library which contains some essential string manipulation
functions and more, like escaping special characters.")
    (license license:lgpl2.1)))

(define-public libfastjson
  (package
    (name "libfastjson")
    (version "0.99.8")
    (source
     (origin
       (method url-fetch)

       (uri (string-append "mirror://github.com/"
                           "rsyslog/libfastjson/archive/"
                           "v" version ".tar.gz"))
       (sha256
        (base32
         "1qrk6vy7bm6ni7mhl5drf6si96pxz6aly0iycb3f7ad54rxhajby"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)))
    (home-page "https://github.com/rsyslog/libfastjson")
    (synopsis "A fast json library for C ")
    (description
     "libfastjson is a fork from json-c, and is currently under
development. The aim of this project is not to provide a slightly
modified clone of json-c. It's aim is to provide: a small library with
essential json handling functions, sufficiently good json support (not
100% standards compliant), and be very fast in processing.")
    (license
     (license:non-copyleft
      "https://github.com/rsyslog/libfastjson/blob/master/COPYING"
      "It is a MIT license."))))

(define-public liblogging
  (package
    (name "liblogging")
    (version "1.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://github.com/"
                           "rsyslog/liblogging/archive/"
                           "v" version ".tar.gz"))
       (sha256
        (base32
         "0n1c2hfbamjyiisjg35z8wy6ll4pl50vk9296g8xfaakldymn8sx"))))
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
       ("pkg-config" ,pkg-config)
       ("libtool" ,libtool)
       ;; For rst2man.py
       ("python-docutils" ,python-docutils)))
    (home-page "https://github.com/rsyslog/liblogging")
    (synopsis
     "An easy to use and lightweight signal-safe logging library")
    (description
     "Liblogging is an easy to use library for logging. It offers an
enhanced replacement for the syslog() call, but retains its ease of
use.")
    (license license:bsd-2)))
