;;; Copyright © 2020, 2021 Katherine Cox-Buday <cox.katherine.e@gmail.com>
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

(define-module (upstream packages emacs-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module (gnu packages emacs-xyz))

(define-public emacs-ox-jira
  (package
    (name "emacs-ox-jira")
    (version "20200218.2301")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://melpa.org/packages/ox-jira-"
             version
             ".el"))
       (sha256
        (base32
         "0s5i5wf9dysy4925pgvf3s6msxwxwrvxx9i6i07h8qzyx3y273xf"))))
    (build-system emacs-build-system)
    (propagated-inputs `(("emacs-org" ,emacs-org)))
    (home-page "https://github.com/stig/ox-jira.el")
    (synopsis "JIRA Backend for Org Export Engine")
    (description
     "This module plugs into the regular Org Export Engine and
transforms Org files to JIRA markup for pasting into JIRA tickets &
comments.

In an Org buffer, hit `C-c C-e j j' to bring up *Org Export
Dispatcher* and export it as a JIRA buffer. I usually use `C-x h' to
mark the whole buffer, then `M-w' to save it to the kill ring (and
global pasteboard) for pasting into JIRA issues.
")
    (license #f)))

(define-public emacs-jiralib2
  (package
    (name "emacs-jiralib2")
    (version "20200331.1940")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://melpa.org/packages/jiralib2-"
             version
             ".el"))
       (sha256
        (base32
         "00s8bs6x0xlpm6fq8nrz37m6pjda2kf3vxainjsz2kkm04fqkcfx"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-request" ,emacs-request)
       ("emacs-dash" ,emacs-dash)))
    (home-page
     "https://github.com/nyyManni/jiralib2")
    (synopsis "JIRA REST API bindings to Elisp")
    (description
     "This file provides a programatic interface to JIRA. It provides
access to JIRA from other programs, but no user level functionality.

jiralib2 supports three methods of authentication: cookie, basic and
token. Cookie auth is the same which is used in the browser, and works
by requesting a session id from the API. Basic auth works by including
the Authorization-header in all the requests. Token authentication is
similar to the basic authentication, but uses a server-side generated
token instead of the password, and is only available with JIRA Cloud.
OAuth login is not supported.

Jira References:

Primary reference (on current Jira, only REST is supported):
https://docs.atlassian.com/jira/REST/cloud/
")
    (license #f)))

(define-public emacs-language-detection
  (package
    (name "emacs-language-detection")
    (version "20161123.1813")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://melpa.org/packages/language-detection-"
             version
             ".el"))
       (sha256
        (base32
         "047mywkm5kmpgd9b3x2a8h73iisldians81pfx3dch18d1s4y0nb"))))
    (build-system emacs-build-system)
    (home-page
     "https://github.com/andreasjansson/language-detection.el")
    (synopsis
     "Automatic language detection from code snippets")
    (description
     "Automatic programming language detection using pre-trained
random forest classifier.

Supported languages:

 * ada
 * awk
 * c
 * clojure
 * cpp
 * csharp
 * css
 * dart
 * delphi
 * emacslisp
 * erlang
 * fortran
 * fsharp
 * go
 * groovy
 * haskell
 * html
 * java
 * javascript
 * json
 * latex
 * lisp
 * lua
 * matlab
 * objc
 * perl
 * php
 * prolog
 * python
 * r
 * ruby
 * rust
 * scala
 * shell
 * smalltalk
 * sql
 * swift
 * visualbasic
 * xml

Entrypoints:

 * language-detection-buffer
   - When called interactively, prints the language of the current
     buffer to the echo area
   - When called non-interactively, returns the language of the
     current buffer
 * language-detection-string
   - Non-interactive function, returns the language of its argument
")
    (license #f)))

(define-public emacs-ejira
  (let ((commit "89f7c668caf0e46e929f2c9187b007eed6b5c229")
        (revision "0"))
    (package
     (name "emacs-ejira")
     (version (git-version "0.9" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nyyManni/ejira.git")
             (commit commit)))
       (file-name (git-file-name name commit))
       (sha256
        (base32
         "0a97gx016byiy5fri8jf3x3sfd2h2iw79s6nxv9jigpkgxrkjg7b"))))
     (build-system emacs-build-system)
     (propagated-inputs
      `(("emacs-org" ,emacs-org)
        ("emacs-s" ,emacs-s)
        ("emacs-f" ,emacs-f)
        ("emacs-helm" ,emacs-helm)
        ("emacs-ox-jira" ,emacs-ox-jira)
        ("emacs-dash" ,emacs-dash)
        ("emacs-jiralib2" ,emacs-jiralib2)
        ("emacs-language-detection" ,emacs-language-detection)))
     (home-page "https://github.com/nyyManni/ejira")
     (synopsis "Emacs Jira Integration")
     (description
      "This package provides an extension to org-mode for syncing
issues with JIRA issue servers.")
     (license license:gpl3+))))

(define-public emacs-restclient
  (let ((commit "94d2e8421fa14d0e3307d70e1d1e2db9d43b2f95")
        (version "0")
        (revision "5"))
    (package
     (name "emacs-restclient")
     (version (git-version version revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pashky/restclient.el")
             (commit commit)))
       (sha256
        (base32
         "0c9z6316pdi30w63a4zqn3b84ciqgxfi7mal6rd3micxg6qpv27c"))
       (file-name (git-file-name name version))))
     (build-system emacs-build-system)
     (propagated-inputs
      `(("emacs-helm" ,emacs-helm)
        ("emacs-jq-mode" ,emacs-jq-mode)))
     (home-page "https://github.com/pashky/restclient.el")
     (synopsis "Explore and test HTTP REST webservices")
     (description
      "This tool allows for testing and exploration of HTTP REST Web services
from within Emacs.  Restclient runs queries from a plan-text query sheet,
displays results pretty-printed in XML or JSON with @code{restclient-mode}")
     (license license:public-domain))))

(define-public emacs-ob-restclient
  (let ((commit "bfbc4d8e8a348c140f9328542daf5d979f0993e2"))
    (package
      (name "emacs-ob-restclient")
      (version (git-version "0.02" "3" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/alf/ob-restclient.el")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0nq5w2gankvb7ix8rv33814j7qvhiawd9r15b9i6syn1i5k5pxhj"))))
      (propagated-inputs
       `(("emacs-restclient" ,emacs-restclient)))
      (build-system emacs-build-system)
      (home-page "https://github.com/alf/ob-restclient.el")
      (synopsis "Org-babel functionality for @code{restclient-mode}")
      (description
       "This package integrates @code{restclient-mode} with Org.")
      (license license:gpl3+))))
