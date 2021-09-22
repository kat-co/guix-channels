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
  #:use-module (gnu packages c)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages web))

(define-public liblognorm
  (package
    (name "liblognorm")
    (version "2.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/"
                           "rsyslog/liblognorm/archive/"
                           "v" version ".tar.gz"))
       (sha256
        (base32
         "0awszylrsw1bm08rm6874arkia1mvc9wb0fg45zwn6gliqqs6kjr"))))
    (build-system gnu-build-system)
    (arguments
     ;; Bash scripts interact with the filesystem
     `(#:tests? #f
       #:configure-flags
       (list (string-append "--includedir="
                            (assoc-ref %outputs "dev")
                            "/include"))
       #:phases
       (modify-phases
           %standard-phases
         (add-after 'install 'fix-circular-dependency
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((pkgconfig (string-append (assoc-ref outputs "dev")
                                             "/lib/pkgconfig")))
               (mkdir-p pkgconfig)
               (rename-file (string-append (assoc-ref outputs "lib")
                                           "/lib/pkgconfig")
                            pkgconfig)))))))
    (inputs
     `(("libestr" ,libestr)
       ("libfastjson" ,libfastjson)
       ("json-c" ,json-c)))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)))
    (outputs '("out" "lib" "dev"))
    (home-page "http://www.liblognorm.com")
    (synopsis
     "A fast samples-based log normalization library")
    (description
     "Liblognorm normalizes event data into well-defined name-value
pairs and a set of tags describing the message.")
    (license license:lgpl2.1)))
