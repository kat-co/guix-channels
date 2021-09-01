;;; Copyright © 2021 Katherine Cox-Buday <cox.katherine.e@gmail.com>
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

(define-module (non-free packages security)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system copy)
  #:use-module (nonguix build-system binary)

  #:use-module (gnu packages compression))

(define-public kolide-launcher
  (package
    (name "kolide-launcher")
    (version "0.11.22")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/kolide/launcher/releases/download/"
             "v" version "/linux.amd64_v" version ".zip"))
       (sha256
        (base32
         "1hlh5miblyxgc8n9b3jrl05cv8j66ch9a3qjrxylgazzw1s88jf6"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       '(("." "bin" #:exclude ("package-builder")))))
    (native-inputs
     `(("unzip" ,unzip)))
    (supported-systems '("x86_64-linux"))
    (home-page "https://github.com/kolide")
    (synopsis "Osquery management system with extended tables")
    (description
     "The Kolide Osquery Launcher is a lightweight launcher/manager
which offers a few extra capabilities on top of osquery:
@itemize
@item secure automatic updates of osquery
@item many additional tables
@item tooling to generate deployment packages for a variety of platforms
@end itemize")
    ;; MIT
    (license license:expat)))

(define-public osquery
  (package
    (name "osquery")
    (version "4.9.0")
    (source
     (origin
       (method url-fetch/tarbomb)
       (uri (string-append
             "https://github.com/osquery/osquery/releases/download/"
             version "/osquery-" version "_1.linux_x86_64.tar.gz"))
       (sha256
        (base32
         "1iaqvxgsn42s1sx9wq5pjqsw8227n1ngj2g5cpvrdki1dsvzb1s1"))))
    (build-system binary-build-system)
    (arguments
     `(#:install-plan
       '(("etc" "etc")
         ("usr/local/" "."))
       #:patchelf-plan
       '(("usr/local/bin/osqueryd" ("zlib" "libc")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chmod-to-allow-patchelf
           (lambda _
             (chmod "usr/local/bin/osqueryd" #o755))))))
    (inputs
     `(("zlib" ,zlib)))
    (supported-systems '("x86_64-linux"))
    (home-page "https://osquery.io/")
    (synopsis "SQL powered operating system instrumentation,
monitoring, and analytics")
    (description
     "osquery exposes an operating system as a high-performance
relational database.  This allows you to write SQL-based queries to
explore operating system data.  With osquery, SQL tables represent
abstract concepts such as running processes, loaded kernel modules,
open network connections, browser plugins, hardware events or file
hashes.")
    (license (list license:gpl2 license:asl2.0))))
