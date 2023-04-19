;;; Copyright © 2021, 2023 Katherine Cox-Buday <cox.katherine.e@gmail.com>
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

(define-module (upstream packages python-xyz)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)

  #:use-module (gnu packages check)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt))

(define-public python-graph-cli
  (let ((commit "b69a9cb0514082e65fd262d330865b543e0a248d")
        (revision "1"))
    (package
      (name "python-graph-cli")
      (version (git-version "0.1.17" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/mcastorina/graph-cli")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "006mv362vmbg3cfvxp0jv547gqnw27a5s6hljq9jcdbi3nwjyv1n"))))
      (build-system python-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'chdir-to-src
             ;; src is stored under a sub-directory
             (lambda _ (chdir "graph_cli"))))))
      (propagated-inputs
       `(("python-matplotlib" ,python-matplotlib)
         ("python-numpy" ,python-numpy)
         ("python-pandas" ,python-pandas)
         ;; This should probably be a requirement of `python-matplotlib'
         ("python-pyqt" ,python-pyqt)))
      (home-page "https://github.com/mcastorina/graph-cli")
      (synopsis "A CLI utility to create graphs from CSV files.")
      (description "A CLI utility to create graphs from CSV files.")
      (license #f))))

(define-public python-fido2
  (package
    (name "python-fido2")
    (version "1.1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Yubico/python-fido2")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1icc87nz5i6pq6n4qqb3g4djn5r83wsb80j5j33l0lch08g32xj3"))
              (snippet
               ;; Remove bundled dependency.
               '(delete-file "fido2/public_suffix_list.dat"))))
    (build-system pyproject-build-system)
    (arguments
     `(;; This attempts to access
       ;; /System/Library/Frameworks/IOKit.framework/IOKit
       ;; The recommendation is to use tox for testing.
       #:tests? #false
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'install-public-suffix-list
           (lambda* (#:key inputs #:allow-other-keys)
             (copy-file
              (search-input-file inputs
                                 (string-append
                                  "/share/public-suffix-list-"
                                  ,(package-version public-suffix-list)
                                  "/public_suffix_list.dat"))
              "fido2/public_suffix_list.dat"))))))
    (propagated-inputs
     (list python-cryptography python-six))
    (native-inputs
     (list poetry python-mock python-pyfakefs public-suffix-list))
    (home-page "https://github.com/Yubico/python-fido2")
    (synopsis "Python library for communicating with FIDO devices over USB")
    (description
     "This Python library provides functionality for communicating with a Fast
IDentity Online (FIDO) device over Universal Serial Bus (USB) as well as
verifying attestation and assertion signatures.  It aims to support the FIDO
Universal 2nd Factor (U2F) and FIDO 2.0 protocols for communicating with a USB
authenticator via the Client-to-Authenticator Protocol (CTAP 1 and 2).  In
addition to this low-level device access, classes defined in the
@code{fido2.client} and @code{fido2.server} modules implement higher level
operations which are useful when interfacing with an Authenticator, or when
implementing a Relying Party.")
    ;; python-fido2 contains some derivative files originally from pyu2f
    ;; (https://github.com/google/pyu2f).  These files are licensed under the
    ;; Apache License, version 2.0.  The maintainers have customized these
    ;; files for internal use, so they are not really a bundled dependency.
    (license (list license:bsd-2 license:asl2.0))))
