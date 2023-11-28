;;; Copyright © 2020, 2021, 2022, 2023 Katherine Cox-Buday <cox.katherine.e@gmail.com>
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
  #:use-module (guix build-system gnu)

  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages lisp-check)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages shells)
  ;; sbcl-mcclim
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages pkg-config)

  #:use-module (upstream packages networking))

(define-public sbcl-mcclim
  (let ((commit "099b7f33e63877c829ad6a35311ccee8a5ed1141")
        (revision "4"))
    (package
      (name "sbcl-mcclim")
      (version (git-version "0.9.7" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://codeberg.org/McCLIM/McCLIM")
               (commit commit)))
         (file-name (git-file-name "cl-mcclim" version))
         (sha256
          (base32 "03gpz7z559dbzad3ry4jdsc2kf7va2vf5q3qs6x6amrkqqxp1rig"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-fiveam pkg-config))
      (inputs
       (list fontconfig
             freetype
             harfbuzz
             sbcl-alexandria
             sbcl-babel
             sbcl-bordeaux-threads
             sbcl-cffi
             sbcl-cluffer
             sbcl-cl-base64
             sbcl-cl-dejavu
             sbcl-cl-freetype2
             sbcl-cl-pdf
             sbcl-cl-unicode
             sbcl-cl-vectors
             sbcl-cl-who
             sbcl-closer-mop
             sbcl-clx
             sbcl-flexi-streams
             sbcl-flexichain
             sbcl-log4cl
             sbcl-lorem-ipsum
             sbcl-opticl
             sbcl-slime-swank
             sbcl-spatial-trees
             sbcl-trivial-features
             sbcl-trivial-garbage
             sbcl-trivial-gray-streams
             sbcl-zpb-ttf))
      (arguments
       '(#:asd-systems '("mcclim"
                         "clim-examples"
                         ;; clim-debugger is required by cleavir.
                         "clim-debugger")
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "Extensions/fontconfig/src/functions.lisp"
                 (("libfontconfig\\.so")
                  (search-input-file inputs "/lib/libfontconfig.so")))
               (substitute* "Extensions/harfbuzz/src/functions.lisp"
                 (("libharfbuzz\\.so")
                  (search-input-file inputs "/lib/libharfbuzz.so"))))))))
      (home-page "https://mcclim.common-lisp.dev/")
      (synopsis "Common Lisp GUI toolkit")
      (description
       "McCLIM is an implementation of the @emph{Common Lisp Interface Manager
specification}, a toolkit for writing GUIs in Common Lisp.")
      (license license:lgpl2.1+))))

(define-public sbcl-inotify
  (let ((commit "8ad433f646f0dd2205dbb2ec52663d6e9c0d9afe")
        (revision "1")
        (site "https://github.com/stassats/inotify"))
    (package
      (name "sbcl-inotify")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url site)
               (commit commit)))
         (file-name (git-file-name "sbcl-inotify" version))
         (sha256
          (base32 "0jill05wsa7xbnkycc1ik1a05slv2h34fpyap2rxbnxvfjvyzw98"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("cffi" ,sbcl-cffi)
         ("iolib" ,sbcl-iolib)))
      (home-page site)
      (synopsis "FFI to inotify(7) for Common Lisp")
      (description
       "Interface to linux inotify(7).")
      (license license:public-domain))))

(define-public cl-inotify
  (sbcl-package->cl-source-package sbcl-inotify))

(define-public ecl-inotify
  (sbcl-package->ecl-package sbcl-inotify))

(define-public sbcl-pp-toml
  (let ((commit "dc880368933f5baea1584a9a40fbd171c642a3a3")
        (revision "1"))
    (package
      (name "sbcl-pp-toml")
      (version (git-version "1.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/pnathan/pp-toml")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1ivr0v7skdml6i3sjvb3x7cv7ynjmvpg0f8jm9bmkng6y7bc0xaa"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("cl-ppcre" ,sbcl-cl-ppcre)
         ("generic-comparability" ,sbcl-generic-comparability)
         ("local-time" ,sbcl-local-time)
         ("parse-number" ,sbcl-parse-number)
         ("split-sequence" ,sbcl-split-sequence)
         ("esrap" ,sbcl-esrap)))
      (home-page "https://github.com/pnathan/pp-toml")
      (synopsis "Paul's Parser for Tom's Own Minimal Language")
      (description "A Common Lisp TOML parser which supports the v0.1.0 spec.")
      (license license:llgpl))))

(define-public cl-pp-toml
  (sbcl-package->cl-source-package sbcl-pp-toml))

(define-public ecl-pp-toml
  (sbcl-package->ecl-package sbcl-pp-toml))

(define-public sbcl-snooze
  (let ((commit "a84d44efce5d9f8b0e3916ea137dc3d43a561944")
        (revision "1"))
    (package
     (name "sbcl-snooze")
     (version (git-version "1.0.0" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/joaotavora/snooze")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0y6n5k6yw6183kf00m4wa9fksad9qjy4i5qr027ys48mgw8d23fj"))))
     (build-system asdf-build-system/sbcl)
     (inputs
      (list sbcl-alexandria
            sbcl-cl-ppcre
            sbcl-closer-mop
            sbcl-quri
            sbcl-parse-float
            sbcl-rfc2388))
     (native-inputs
      (list sbcl-fiasco))
     (home-page "https://github.com/joaotavora/snooze")
     (synopsis "A framework for building REST services using CLOS.")
     (description
      "Snooze is an URL router for Common Lisp designed around REST web services.")
     (license license:llgpl))))

(define-public cl-snooze
  (sbcl-package->cl-source-package sbcl-snooze))

(define-public ecl-snooze
  (sbcl-package->ecl-package sbcl-snooze))

(define-public acl2
  (package
    (name "acl2")
    (version "8.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/acl2-devel/acl2-devel")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "12cv5ms1j3vfrq066km020nwxb6x2dzh12g8nz6xxyxysn44wzzi"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         ;; ACL2 expects to be built in the place it will eventually be
         ;; installed to. E.g., It will use those paths to find books.
         (add-before 'build 'copy-to-install-location
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (share (string-append out "/share")))
               (mkdir-p share)
               (copy-recursively (getcwd) share))))
         (replace 'build
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((share (string-append (assoc-ref outputs "out") "/share/")))
               (chdir share)
               ;; Book verification passes $HOME into truename
               (setenv "HOME" (getcwd))
               (invoke "make" (format #f "-j~a" (parallel-job-count))
                       "all"
                       "LISP=sbcl" "ACL2_MAKE_LOG=NONE")
               (patch-shebang (string-append share "saved_acl2"))
               ;; Certify books
               (invoke "make" (format #f "-j~a" (parallel-job-count))
                       ;; Don't discard all books because some don't certify
                       "--ignore-errors"
                       "regression-everything"
                       "LISP=sbcl" "ACL2_MAKE_LOG=NONE"))))
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin/"))
                    (share (string-append out "/share/")))
               ;; Delete files to save space
               (for-each
                delete-file
                (find-files
                 share
                 (lambda (path stat)
                   (string-prefix? path "@useless-runes.lsp"))))
               (mkdir-p bin)
               ;; Symlink things into bin
               (for-each
                (lambda (paths)
                  (symlink (string-append share (car paths))
                           (string-append bin (cdr paths))))
                '(("saved_acl2" . "acl2")
                  ("books/build/cert.pl" . "acl2-cert")
                  ("books/build/clean.pl" . "acl2-clean")))))))
       #:test-target "mini-proveall"))
    (native-inputs (list inetutils minisat perl python ruby sbcl tcsh which z3))
    (home-page "http://www.cs.utexas.edu/users/moore/acl2")
    (synopsis "A logic and programming language for modeling computer systems")
    (description
     "ACL2 is a logic and programming language in which you can model computer
systems, together with a tool to help you prove properties of those models.
\"ACL2\" denotes \"A Computational Logic for Applicative Common Lisp\". ACL2 is
part of the Boyer-Moore family of provers, for which its authors have received
the 2005 ACM Software System Award.")
    (license (list license:bsd-3
                   license:gpl2
                   license:lgpl2.1
                   license:cc0
                   license:public-domain))))
