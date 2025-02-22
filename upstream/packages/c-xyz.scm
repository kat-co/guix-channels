;;; Copyright © 2025 Katherine Cox-Buday <cox.katherine.e@gmail.com>
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

(define-module (upstream packages c-xyz)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public webui
  (package
   (name "webui")
   (version "2.5.0-beta.2")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/webui-dev/webui.git")
                  (commit "2e94629304402fdf4b9144397c85cf56e18a35ee")))
            (sha256
             (base32
              "12fksibv35873qv2rym9wbqamxsv1rqs1j4wwl0ib8b3giqz0hg4"))))
   (build-system gnu-build-system)
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (delete 'configure)
        (delete 'check)
        (replace 'install
         (lambda* (#:key outputs #:allow-other-keys)
           (let* ((out (assoc-ref outputs "out"))
                  (lib (string-append out "/lib")))
             (copy-recursively "include" (string-append out "/include"))
             (copy-recursively "src" (string-append out "/src"))
             (install-file "dist/libwebui-2-static.a" lib)
             (install-file "dist/webui-2.so" lib)))))))
   (home-page "https://webui.me/")
   (synopsis "TODO")
   (description "TODO")
   (license license:expat)))
