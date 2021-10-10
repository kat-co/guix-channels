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

(define-module (upstream packages python-xyz)
  #:use-module (guix build-system python)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)

  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-xyz))

(define-public python-graph-cli
  (let ((commit "b69a9cb0514082e65fd262d330865b543e0a248d"))
    (package
      (name "python-graph-cli")
      (version "0.1.17")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/mcastorina/graph-cli")
               (commit commit)))
         (file-name (git-file-name name commit))
         (sha256
          (base32 "006mv362vmbg3cfvxp0jv547gqnw27a5s6hljq9jcdbi3nwjyv1n"))))
      (build-system python-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'chdir-to-src
             ;; src is stored under a sub-directory
             (lambda _ (chdir "graph_cli"))))))
      (propagated-inputs
       `(("python-matplotlib" ,python-matplotlib)
         ("python-numpy" ,python-numpy)
         ("python-pandas" ,python-pandas)))
      (home-page "https://github.com/mcastorina/graph-cli")
      (synopsis "A CLI utility to create graphs from CSV files.")
      (description "A CLI utility to create graphs from CSV files.")
      (license #f))))
