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

(define-public apache-arrow
  (package
    (name "apache-arrow")
    (version "0.17.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/apache/arrow")
             (commit (string-append "apache-arrow-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "18kigl0mygxybad189wal5z6ncdhfmkgqihndgl7nx4yy5437nsj"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'enter-source-directory
           (lambda _ (chdir "cpp") #t))
         (add-after 'unpack 'set-env
           (lambda _
             (setenv "BOOST_ROOT" (assoc-ref %build-inputs "boost"))
             (setenv "BROTLI_HOME" (assoc-ref %build-inputs "brotli"))
             (setenv "FLATBUFFERS_HOME" (assoc-ref %build-inputs "flatbuffers"))
             (setenv "RAPIDJSON_HOME" (assoc-ref %build-inputs "rapidjson"))
             #t)))
       #:build-type "Release"
       #:configure-flags
       (list "-DARROW_PYTHON=ON"
             "-DARROW_GLOG=ON"
             ;; Parquet options
             "-DARROW_PARQUET=ON"
             "-DPARQUET_BUILD_EXECUTABLES=ON"
             ;; The maintainers disallow using system versions of
             ;; jemalloc:
             ;; https://issues.apache.org/jira/browse/ARROW-3507. This
             ;; is unfortunate because jemalloc increases performance:
             ;; https://arrow.apache.org/blog/2018/07/20/jemalloc/.
             "-DARROW_JEMALLOC=OFF"

             ;; The CMake option ARROW_DEPENDENCY_SOURCE is a global
             ;; option that instructs the build system how to resolve
             ;; each dependency. SYSTEM = Finding the dependency in
             ;; system paths using CMake's built-in find_package
             ;; function, or using pkg-config for packages that do not
             ;; have this feature
             "-DARROW_DEPENDENCY_SOURCE=SYSTEM"

             ;; Install to PREFIX/lib (the default is
             ;; PREFIX/lib64).
             (string-append "-DCMAKE_INSTALL_LIBDIR="
                            (assoc-ref %outputs "lib"))
             (string-append "-DCMAKE_INSTALL_BINDIR="
                            (assoc-ref %outputs "out"))
             (string-append "-DCMAKE_INSTALL_INCLUDEDIR="
                            (assoc-ref %outputs "include"))


             "-DARROW_WITH_SNAPPY=ON"
             "-DARROW_WITH_ZLIB=ON"
             "-DARROW_WITH_ZSTD=ON"
             "-DARROW_WITH_LZ4=ON"


             ;; Building the tests forces on all the
             ;; optional features and the use of static
             ;; libraries.
             "-DARROW_BUILD_TESTS=OFF"
             "-DBENCHMARK_ENABLE_GTEST_TESTS=OFF"
             ;;"-DBENCHMARK_ENABLE_TESTING=OFF"
             "-DARROW_BUILD_STATIC=OFF")))
    (inputs
     `(("boost" ,boost)
       ("brotli" ,google-brotli)
       ("double-conversion" ,double-conversion)
       ("snappy" ,snappy)
       ("gflags" ,gflags)
       ("glog" ,glog)
       ("apache-thrift" ,apache-thrift "lib")
       ("protobuf" ,protobuf)
       ("rapidjson" ,rapidjson)
       ("zlib" ,zlib)
       ("bzip2" ,bzip2)
       ("lz4" ,lz4)
       ("zstd" ,zstd "lib")
       ("re2" ,re2)
       ("grpc" ,grpc)
       ("python-3" ,python)
       ("python-numpy" ,python-numpy)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (outputs '("out" "lib" "include"))
    (home-page "https://arrow.apache.org/")
    (synopsis "Columnar in-memory analytics")
    (description "Apache Arrow is a columnar in-memory analytics layer
designed to accelerate big data. It houses a set of canonical in-memory
representations of flat and hierarchical data along with multiple
language-bindings for structure manipulation. It also provides IPC and common
algorithm implementations.")
    (license license:asl2.0)))

(define-public apache-thrift
  (package
    (name "apache-thrift")
    (version "0.13.0")
    (source
     (origin
       (method url-fetch)
       (uri  (string-append
              "https://github.com/apache/thrift/archive/v"
              version ".tar.gz"))
       (sha256
        (base32
         "1nbsbw2w9n558f58rrr1qv736g0rimifmvhxh3qg9109ws4019jx"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f
       #:configure-flags
       (list (string-append "--with-boost="
                            (assoc-ref %build-inputs "boost")))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("flex" ,flex)
       ("bison" ,bison)
       ("boost" ,boost)
       ("libressl" ,libressl)))
    (outputs '("out" "lib" "include"))
    (home-page "https://thrift.apache.org/")
    (synopsis
     "Thrift is a lightweight, language-independent software stack for
point-to-point RPC implementation.")
    (description
     "Thrift provides clean abstractions and implementations for data
transport, data serialization, and application level processing. The
code generation system takes a simple definition language as input and
generates code across programming languages that uses the abstracted
stack to build interoperable RPC clients and servers.")
    (license license:asl2.0)))

(define-public apache-arrow-c-glib
  (package
    (inherit apache-arrow)
    (name "apache-arrow-c-glib")
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'enter-source-directory
           (lambda _
             (chdir "c_glib")
             #t))
         (add-after 'enter-source-directory 'autogen-sh
           (lambda _
             (setenv "NOCONFIGURE" "1")
             (invoke "sh" "autogen.sh"))))
       #:configure-flags
       (list "--enable-gtk-doc")))
    (propagated-inputs
     `(("apache-arrow" ,apache-arrow)))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("autoconf-archive" ,autoconf-archive)
       ("automake" ,automake)
       ("glib" ,glib "bin")
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("glib" ,glib)
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc)))
    (synopsis
     "Arrow GLib is a wrapper library for Arrow C++. Arrow GLib
provides C API.")
    (description
     "Columnar in-memory analytics")))
