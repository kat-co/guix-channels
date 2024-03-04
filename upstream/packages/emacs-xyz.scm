;;; Copyright © 2020, 2021, 2022, 2024 Katherine Cox-Buday <cox.katherine.e@gmail.com>
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
  #:use-module (guix build emacs-utils)
  #:use-module (guix build utils)
  #:use-module (guix build-system emacs)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)

  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages graphviz)

  #:use-module (upstream packages python-xyz))

(define-public emacs-git
  (package
    (name "emacs-git")
    (version "20140128.1041")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/rejeep/git.el.git")
                    (commit "a3396a7027a7d986598c6a2d6d5599bac918f3da")))
              (sha256
               (base32
                "10siqf21ay6vl1r1v2c93rajzyjc67m4wq9q88pbij0z18vkq2f0"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-s emacs-dash emacs-f))
    (home-page "http://github.com/rejeep/git.el")
    (synopsis "An Elisp API for programmatically using Git")
    (description "No description available.")
    (license #f)))

(define-public emacs-org-page
  (package
    (name "emacs-org-page")
    (version "20170807.224")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/emacsorphanage/org-page.git")
                    (commit "b25c3ef41da233306c157634c1f0b019d8b6adc0")))
              (sha256
               (base32
                "06hh1g3rxadscjjn1ym358m2c8qn3s2x7ik0msadvm1zgx7ng4v5"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-ht
                             emacs-simple-httpd
                             emacs-mustache
                             emacs-htmlize
                             emacs-org
                             emacs-dash
                             emacs-git))
    (arguments
     '(#:include '("^[^/]+.el$" "^doc$" "^themes$")
       #:exclude '()))
    (home-page "https://github.com/kelvinh/org-page")
    (synopsis "a static site generator based on org mode")
    (description
     "See documentation at https://github.com/kelvinh/org-page Org-page is a static
site generator based on org mode.  Org-page provides following features: 1) org
sources and html files managed by git 2) incremental publication (according to
=git diff= command) 3) category support 4) tags support (auto generated) 5) RSS
support (auto generated) 6) search engine support (auto generated) 7) a
beautiful theme 8) theme customization support 9) commenting (implemented using
disqus) 10) site visiting tracking (implemented using google analytics) 11)
index/about page support (auto generated if no default provided) 12) site
preview 13) highly customizable")
    (license license:gpl3+)))

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

(define-public emacs-moldable-emacs
  (let ((commit "1c5726761551f9d70269434e3acc6543c0d15459")
        (revision "2"))
    (package
      (name "emacs-moldable-emacs")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ag91/moldable-emacs")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1ki0fk7rc002zazqh779vr6s0aqpj4ms160h7na290rkwx1wmfyv"))))
      (build-system emacs-build-system)
      (arguments
       `(#:include (append '("^molds/" "^tutorial") %default-include)
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch-bin-locations
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let ((file "molds/contrib.el")
                     (graph-bin (which "graph"))
                     (dot-bin (which "dot")))
                 (chmod file #o644)
                 (substitute* file
                   (("\\(executable-find \"graph\"\\)")
                    (string-append "(executable-find \"" graph-bin "\")"))
                   (("\\(executable-find \"dot\"\\)")
                    (string-append "(executable-find \"" dot-bin "\")"))
                   (("\"graph ")
                    (string-append "\"" graph-bin " ")))))))))
      (inputs
       `(("graphviz" ,graphviz)
         ("graph-cli" ,python-graph-cli)))
      (propagated-inputs
       `(("emacs-dash" ,emacs-dash)
         ("emacs-s" ,emacs-s)
         ("emacs-async" ,emacs-async)
         ("emacs-json-mode" ,emacs-json-mode)
         ("emacs-csv-mode" ,emacs-csv-mode)))
      (home-page "https://github.com/ag91/moldable-emacs")
      (synopsis "Adapting Emacs for moldable development ")
      (description
       "This is an extension of Emacs aiming to enable Moldable
Development. Or better still, aiming to make you a better story teller
when you deal with code.")
      (license license:gpl3+))))

(define-public emacs-magit-section
  (package
    (name "emacs-magit-section")
    (version "3.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://stable.melpa.org/packages/magit-section-"
                           version ".tar"))
       (sha256
        (base32 "0l5g0668yhx00k51x5lrsnp4m4696xdy1cv3dz7szh6j3qs0qj8h"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-dash))
    (home-page "https://github.com/magit/magit")
    (synopsis "Sections for read-only buffers")
    (description
     "This package implements the main user interface of Magit — the collapsible
sections that make up its buffers.  This package used to be distributed as part
of Magit but now it can also be used by other packages that have nothing to do
with Magit or Git.")
    (license #f)))

(define-public emacs-kubernetes
  (package
    (name "emacs-kubernetes")
    (version "0.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://stable.melpa.org/packages/kubernetes-"
                           version ".tar"))
       (sha256
        (base32 "0cbw3d47jfcs13i0z30fpf3r85nj9l99053kxdmmjc2k37gcscrh"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-dash
                             emacs-magit-section
                             emacs-magit-popup
                             emacs-with-editor
                             emacs-request
                             emacs-s
                             emacs-transient))
    (home-page "https://github.com/kubernetes-el/kubernetes-el")
    (synopsis "Magit-like porcelain for Kubernetes")
    (description
     "kubernetes-el is a text-based, interactive management interface for managing
Kubernetes clusters within Emacs.")
    (license #f)))

