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

(define-module (upstream packages databases)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages adns)
  #:use-module (gnu packages benchmark)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages logging)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages regex)
  #:use-module (gnu packages rpc)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web))

;; (define-public apache-arrow-c-glib
;;   (package
;;     (inherit apache-arrow)
;;     (name "apache-arrow-c-glib")
;;     (build-system gnu-build-system)
;;     (arguments
;;      `(#:phases
;;        (modify-phases %standard-phases
;;          (add-before 'configure 'enter-source-directory
;;            (lambda _
;;              (chdir "c_glib")
;;              #t))
;;          (add-after 'enter-source-directory 'autogen-sh
;;            (lambda _
;;              (setenv "NOCONFIGURE" "1")
;;              (invoke "sh" "autogen.sh")))
;;          (add-after 'install 'transfer-gir-files
;;            (lambda* (#:key outputs #:allow-other-keys)
;;              ;; The configure file doesn't respect the docdir or
;;              ;; htmldir flags. Specifying the datadir flag works, but
;;              ;; the gir files are also stored in the doc package.
;;              ;; Technically this is correct, but in practice the gir
;;              ;; files should live with the library files.
;;              (let ((out (assoc-ref outputs "out"))
;;                    (doc (assoc-ref outputs "doc")))
;;                (rename-file (string-append doc "/share/gir-1.0")
;;                             (string-append out "/share/gir-1.0"))))))
;;        #:configure-flags
;;        (list
;;         ;; Speeds up one-time build. Guix builds are always built from
;;         ;; scratch effectively making them one-time builds.
;;         "--disable-dependency-tracking"
;;         "--enable-gtk-doc"
;;         (string-append "--datadir=" (assoc-ref %outputs "doc")
;;                        "/share"))))
;;     (propagated-inputs
;;      `(("apache-arrow" ,apache-arrow "lib")))
;;     (native-inputs
;;      `(("autoconf" ,autoconf)
;;        ("autoconf-archive" ,autoconf-archive)
;;        ("automake" ,automake)
;;        ("glib" ,glib "bin")
;;        ("libtool" ,libtool)
;;        ("pkg-config" ,pkg-config)))
;;     (inputs
;;      `(("glib" ,glib)
;;        ("gobject-introspection" ,gobject-introspection)
;;        ("gtk-doc" ,gtk-doc)))
;;     (outputs '("out" "doc"))
;;     (synopsis
;;      "Arrow GLib is a wrapper library for Arrow C++. Arrow GLib
;; provides C API.")
;;     (description
;;      "Columnar in-memory analytics")))
