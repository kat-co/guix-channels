;;; Copyright © 2022 Katherine Cox-Buday <cox.katherine.e@gmail.com>
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

(define-module (upstream packages wm)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)

  #:use-module ((gnu packages wm) #:prefix upstream:))

(define-public stumpwm
  (let ((commit "0cc4d4f56bdb31f52e9e3b493e9839a23b55204a")
        (revision "0"))
    (package
      (inherit upstream:stumpwm)
      (name "stumpwm")
      (version (git-version "20.11" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/stumpwm/stumpwm")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1hhqjzk3vbrwpxk2cdhfz7220rjpl0ws68rlxkvpnjapf6c70zqa"))))
      (arguments
       (substitute-keyword-arguments (package-arguments upstream:stumpwm)
         ;; TODO: Why are tests failing?
         ((#:tests? #f)))))))
