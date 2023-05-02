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

(define-module (upstream packages xorg)
  #:use-module (guix download)
  #:use-module (guix packages)

  #:use-module (gnu packages gl))

(define-public (xorg-with-mesa xorg-server mesa)
  "Returns an xorg-server package which will wrap the call to Xorg to include the
correct XORG_DRI_DRIVER_PATH for the mesa package provided."
  (package
    (inherit xorg-server)
    (arguments
     (substitute-keyword-arguments
         (package-arguments xorg-server)
       ((#:phases child-phases '%standard-phases)
        #~(modify-phases #$child-phases
            (add-after 'install 'wrap
              (lambda* (#:key outputs #:allow-other-keys)
                (let* ((out (assoc-ref outputs "out"))
                       (bin (string-append out "/bin"))
                       (xorg (string-append bin "/Xorg")))
                  (wrap-program xorg
                    '("XORG_DRI_DRIVER_PATH" ":" = (#$(file-append mesa "/lib/dri")))))))))))))

(define-public mesa-next
  (package
    (inherit mesa)
    (version "22.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (list (string-append "https://mesa.freedesktop.org/archive/"
                                 "mesa-" version ".tar.xz")
                  (string-append "ftp://ftp.freedesktop.org/pub/mesa/"
                                 "mesa-" version ".tar.xz")
                  (string-append "ftp://ftp.freedesktop.org/pub/mesa/"
                                 version "/mesa-" version ".tar.xz")))
       (sha256
        (base32
         "14s557c7k0ncb5wj9lz901pfjzg887zprjc97kp1j6gl0fpxv89p"))))))
