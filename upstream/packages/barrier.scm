;;; Copyright © 2023 Katherine Cox-Buday <cox.katherine.e@gmail.com>
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

(define-module (upstream packages barrier)
  #:use-module (guix build-system cmake)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xorg))

(define-public barrier
  (let ((commit "4b12265ae5d324b942698a3177e1d8b1749414d7")
        (revision "0"))
    (package
      (name "barrier")
      (version (git-version "2.4.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/debauchee/barrier")
               (commit commit)
               (recursive? #t)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "06ibp8bj9ycvjjwywnn4djpa08n07z5wfb18rl2zmsb44p86my4m"))))
      (build-system cmake-build-system)
      (arguments
       '(#:configure-flags
         (list "-DBARRIER_USE_EXTERNAL_GTEST=ON")
         #:tests? #f))                    ;tests require a running x server
      (native-inputs
       (list googletest pkg-config))
      (inputs
       (list avahi curl gulrak-filesystem libx11 libxtst openssl qtbase-5))
      (synopsis "Keyboard Video Mouse switch software")
      (description "@code{Barrier} is software that mimics the functionality of
a KVM switch, which historically would allow you to use a single keyboard and
mouse to control multiple computers by physically turning a dial on the box to
switch the machine you're controlling at any given moment.  Barrier does this
in software, allowing you to tell it which machine to control by moving your
mouse to the edge of the screen, or by using a keypress to switch focus to a
different system.")
      (home-page "https://github.com/debauchee/barrier")
      (license license:gpl2))))
