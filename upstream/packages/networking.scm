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

(define-module (upstream packages networking)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages pkg-config))

(define-public atheepmgr
  (package
    (name "atheepmgr")
    (version "2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://github.com/rsa9000/atheepmgr/archive/refs/tags/"
         "atheepmgr-" version ".tar.gz"))
       (sha256
        (base32
         "0bgcspnxf9vg8aa2fgm8yyrnvvcxfzk8n9pymdzpypx8ylki4zs8"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; No tests in the codebase
       #:parallel-build? #f             ; Breaks generating config.h
       #:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (replace 'install
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (install-file "atheepmgr"
                                      (string-append out "/bin")))))
                  (add-before 'build 'set-CC
                    (lambda _
                      (setenv "CC" "gcc")
                      #t)))))
    (inputs
     `(("libpciaccess" ,libpciaccess)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://github.com/rsa9000/atheepmgr")
    (synopsis "Atheros EEPROM manager")
    (description
     "A utility to dump and update the EEPROM content of Atheros based
wireless NICs.")
    (license license:isc)))
