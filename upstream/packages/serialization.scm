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


(define-module (upstream packages serialization)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake))


(define-public flatbuffers
  (package
    (name "flatbuffers")
    (version "1.12.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/google/flatbuffers/archive/v"
                            version ".tar.gz"))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0idwab0ilmwnab6nclhq49smyxvmi5i7a4s571iis78qp4zj5wk2"))))
    (build-system cmake-build-system)
    (arguments
     '(#:build-type "Release"
       #:configure-flags
       (list (string-append "-DCMAKE_INSTALL_LIBDIR="
                            (assoc-ref %outputs "out") "/lib"))))
    (home-page "https://google.github.io/flatbuffers/")
    (synopsis "Memory-efficient serialization library")
    (description "FlatBuffers is a cross-platform serialization library for C++,
C#, C, Go, Java, JavaScript, PHP, and Python.  It was originally created for
game development and other performance-critical applications.")
    (license license:asl2.0)))
