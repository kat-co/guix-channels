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

(define-module (upstream packages games)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages xml)
  #:use-module (nongnu packages wine))


(define-public lutris
  (package
    (name "lutris")
    (version "0.5.8.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lutris/lutris")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0lsblbkchdj2wrxflbqw12zc69ffw19kq8g5pv8x6czj00idfaz6"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((input-paths (list (assoc-ref inputs "psmisc")
                                       (assoc-ref inputs "p7zip")
                                       (assoc-ref inputs "curl")
                                       (assoc-ref inputs "cabextract")
                                       (assoc-ref inputs "winetricks")))
                    (inputs-bin (map (lambda (i) (string-append "\"" i "/bin\"")) input-paths))
                    (path-candidates (string-join inputs-bin ",")))
               (substitute* "lutris/util/linux.py"
                 (("path_candidates = \\[.*\\]")
                  (string-append "path_candidates = [" path-candidates "]")))))))))
    (inputs
     `(("python-astroid" ,python-astroid)
       ("python-certifi" ,python-certifi)
       ("python-chardet" ,python-chardet)
       ("python-dbus-python" ,python-dbus)
       ("python-entrypoints" ,python-entrypoints)
       ("python-evdev" ,python-evdev)
       ("python-idna" ,python-idna)
       ("python-lazy-object-proxy" ,python-lazy-object-proxy)
       ("python-mccabe" ,python-mccabe)
       ("python-pycairo" ,python-pycairo)
       ("python-pyflakes" ,python-pyflakes)
       ("python-pygobject" ,python-pygobject)
       ("python-pyyaml" ,python-pyyaml)
       ("python-requests" ,python-requests)
       ("python-six" ,python-six)
       ("python-urllib3" ,python-urllib3)
       ("python-wrapt" ,python-wrapt)
       ("python-keyring" ,python-keyring)
       ("python-lxml" ,python-lxml)

       ;; ("gobject-introspection" ,gobject-introspection)
       ("mesa" ,mesa)
       ("psmisc" ,psmisc)
       ("p7zip" ,p7zip)
       ("curl" ,curl)
       ("fluid-3" ,fluid-3)
       ("cabextract" ,cabextract)
       ("winetricks" ,winetricks)))
    (propagated-inputs
     `(("gtk+" ,gtk+)
       ("gnome-desktop" ,gnome-desktop)
       ("webkitgtk" ,webkitgtk)
       ("libnotify" ,libnotify)))
    (synopsis "Gaming Platform Interface")
    (description
     "Lutris is a gaming platform for GNU/Linux.
It allows you to gather and manage (install, configure and launch) all
your games acquired from any source, in a single interface. Its features
include:
@itemize
@item Manage your Linux games, Windows games, emulated console games and browser games
@item Launch your Steam games
@item Community-written installers to ease up your games' installation
@item More than 20 emulators installed automatically or in a single click, providing support for most gaming systems from the late 70's to the present day
@item Download and play libre and freeware games
@end itemize")
    (home-page "https://lutris.net")
    (license license:gpl3+)))
