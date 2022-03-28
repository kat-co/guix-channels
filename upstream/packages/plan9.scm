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
(define-module (upstream packages plan9)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages autogen)
  #:use-module ((gnu packages base) #:prefix base:)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages xorg)

  #:use-module (guix build utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils))

(define %plan9port
  (let ((commit "d0d440860f2000a1560abb3f593cdc325fcead4c")
        (revision "0"))
    (package
      (name "plan9port")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/9fans/plan9port")
               (commit commit)))
         (file-name (git-file-name name commit))
         (sha256
          (base32 "0555323hxvggh4vz4fg9k127y1w14g8hxjzs2jr1gbdhy6l1g9nr"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f
         #:phases
         (modify-phases %standard-phases
           (add-after 'configure 'configure-plan-9
             (lambda* (#:key outputs inputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      ;;(plan9 (format #f "~a/share/plan9" out))
                      )
                 (setenv "PLAN9" (getcwd))
                 ;;(setenv "PLAN9_TARGET" plan9)
                 (setenv "PATH"
                         (format #f "~a/bin:~a" (getcwd) (getenv "PATH")))
                 (setenv "NPROC" (format #f "~a" (parallel-job-count)))))))))
      (home-page "https://github.com/9fans/plan9port")
      (synopsis "A port of many Plan 9 libraries and programs to Unix")
      (description
       "Plan 9 from User Space (aka plan9port) is a port of many Plan 9 programs
from their native Plan 9 environment to Unix-like operating systems.")
      ;; MIT
      (license license:expat))))

(define-public mk
  (package/inherit %plan9port
    (name "mk")
    (arguments
     (substitute-keyword-arguments (package-arguments %plan9port)
       ((#:phases original-phases)
        `(modify-phases ,original-phases
           (replace 'build
             (lambda _
               (with-directory-excursion "src"
                 (invoke "../dist/buildmk"))))
           (replace 'check
             (lambda _
               (stat "./bin/mk")))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (format #f "~a/bin" out)))
                 (install-file "./bin/mk" bin))))))))))

(define-public plan9port
  (let ((install-path "share/plan9"))
    (package/inherit %plan9port
      (name "plan9port")
      (inputs (list fontconfig fuse libx11 libxt xorgproto xorg-server))
      (native-inputs (list gawk mk perl base:which))
      (arguments
       (substitute-keyword-arguments (package-arguments %plan9port)
         ((#:phases original-phases)
          `(modify-phases ,original-phases
             (replace 'build
               (lambda* (#:key outputs #:allow-other-keys)
                 (let* ((out (assoc-ref outputs "out"))
                        (plan9 (format #f "~a/~a" out ,install-path))
                        (src (format #f "~a/src" plan9)))
                   ;; The build process expects to be built in the location it
                   ;; will reside.
                   (copy-recursively "." plan9)
                   (with-directory-excursion src
                     (invoke (which "mk") "all")))))
             (replace 'install
               (lambda* (#:key outputs #:allow-other-keys)
                 (let* ((out (assoc-ref outputs "out"))
                        (bin (format #f "~a/bin" out))
                        (plan9 (format #f "~a/~a" out ,install-path))
                        (src (format #f "~a/src" plan9)))
                   (with-directory-excursion src
                     (setenv "PLAN9" plan9)
                     (invoke (which "mk") "-k" "install"))
                   ;; Since Plan 9 commands sometimes replace their GNU/Linux
                   ;; counterparts, a 9 command is provided to specifically
                   ;; invoke Plan 9 commands. This can be safely put in bin/ for
                   ;; ease of use.
                   (mkdir-p bin)
                   (symlink (format #f "~a/bin/9" plan9)
                            (format #f "~a/9" bin))
                   (copy-file "./bin/quote1" (format #f "~a/bin/\"" plan9))
                   (copy-file "./bin/quote2"
                              (format #f "~a/bin/\"\"" plan9)))))))))
      (native-search-paths
       (list (search-path-specification
              ;; The installation expects this environment variable to be set
              ;; for proper usage of some commands.
              (variable "PLAN9")
              (files (list install-path)))
             ;; Just like the commands, the man pages may clobber their
             ;; GNU/Linux counterparts. So instead of installing them in a
             ;; common location, keep them separate and append to the manpath.
             ;; TODO: Is there a way to set this to 'suffix?
             ;; (search-path-specification
             ;;  (variable "MANPATH")
             ;;  (files (list (format #f "~a/man" install-path))))
             )))))

(define-public diod
    (let ((commit "b4eedf279eeaad9919b7ed794a438154dcc2df3c")
        (revision "0"))
    (package
      (name "diod")
      (version (git-version "1.0.23" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/chaos/diod")
               (commit commit)))
         (file-name (git-file-name name commit))
         (sha256
          (base32 "0xhkbwcgh18j38fp8qb0vyii2pnv2ygy4w588i33gkdnfxdi11pw"))))
      (build-system gnu-build-system)
      (inputs (list autogen popt ncurses lua munge libcap tcp-wrappers))
      (home-page "https://github.com/chaos/diod")
      (synopsis "Distributed I/O Daemon - a 9P file server")
      (description
       "diod is a multi-threaded, user space file server that speaks 9P2000.L
protocol.")
      (license license:gpl2))))
