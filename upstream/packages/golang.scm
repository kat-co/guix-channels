;;; Copyright © 2019, 2021, 2022, 2023 Katherine Cox-Buday <cox.katherine.e@gmail.com>
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

(define-module (upstream packages golang)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system go)
  #:use-module (guix build utils)
  #:use-module (guix transformations)
  #:use-module (guix utils)

  #:use-module (gnu packages golang))

(define with-go-1.19
  (package-input-rewriting/spec `(("go" . ,(const go-1.19)))))

(define-public go-github-com-aybabtme-rgbterm
  (package
    (name "go-github-com-aybabtme-rgbterm")
    (version "0.0.0-20170906152045-cc83f3b3ce59")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/aybabtme/rgbterm")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wvmxvjn64968ikvnxrflb1x8rlcwzpfl53fzbxff2axbx9lq50q"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/aybabtme/rgbterm"))
    (home-page "https://github.com/aybabtme/rgbterm")
    (synopsis "RGB terminal")
    (description
     "Package rgbterm colorizes bytes and strings using RGB colors, for a
full range of pretty terminal strings.
")
    (license license:expat)))

(define-public go-github-com-go-logfmt-logfmt
  (package
    (name "go-github-com-go-logfmt-logfmt")
    (version "0.5.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-logfmt/logfmt")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01fs4x2aqw2qcsz18s4nfvyqv3rcwz5xmgpk3bic6nzgyzsjd7dp"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/go-logfmt/logfmt"))
    (home-page "https://github.com/go-logfmt/logfmt")
    (synopsis "logfmt")
    (description
     "Package logfmt implements utilities to marshal and unmarshal data in the
logfmt format.  The logfmt format records key/value pairs in a way that
balances readability for humans and simplicity of computer parsing.  It is
most commonly used as a more human friendly alternative to JSON for
structured logging.
")
    (license license:expat)))

(define-public go-github-com-kr-logfmt
  (package
    (name "go-github-com-kr-logfmt")
    (version "0.0.0-20210122060352-19f9bcb100e6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kr/logfmt")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1l6322amgy092n30l6br0wzszf3l2a3dkylck3pzpvzr4lqfcyhb"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/kr/logfmt"))
    (home-page "https://github.com/kr/logfmt")
    (synopsis #f)
    (description "Package implements the decoding of logfmt key-value pairs.")
    (license license:expat)))

(define-public humanlog
  (package
    (name "humanlog")
    (version "0.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/aybabtme/humanlog")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0s2ni1d4qqrdybvw8w8q5m500nhs6yz2a73wihmfhlk9hq36037n"))))
    (build-system go-build-system)
    (arguments
     '(#:unpack-path "github.com/aybabtme/humanlog/"
       #:import-path "github.com/aybabtme/humanlog/cmd/humanlog"
       #:install-source? #f))
    (propagated-inputs
     (list
      go-golang-org-x-sys
      go-github-com-urfave-cli
      go-github-com-mattn-go-isatty
      go-github-com-mattn-go-colorable
      go-github-com-kr-logfmt
      go-github-com-go-logfmt-logfmt
      go-github-com-fatih-color
      go-github-com-aybabtme-rgbterm))
    (home-page "https://github.com/aybabtme/humanlog")
    (synopsis "humanlog")
    (description
     "Read logs from @code{stdin} and prints them back to @code{stdout}, but
prettier.")
    (license license:asl2.0)))

(define-public go-github-com-yuin-goldmark
  (package
    (name "go-github-com-yuin-goldmark")
    (version "1.5.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/yuin/goldmark")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0xpgr5zdp49yzrn1mf971ixfin7hbmq9x4bkzz95yw9dfd81dsa8"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/yuin/goldmark"))
    (home-page "https://github.com/yuin/goldmark")
    (synopsis "goldmark")
    (description
     "Package goldmark implements functions to convert markdown text to a desired
format.")
    ;; MIT
    (license license:expat)))

(define-public staticcheck
  (package
    (inherit go-honnef-co-go-tools)
    (name "staticcheck")
    (arguments '(#:unpack-path "honnef.co/go/tools"
                 #:import-path "honnef.co/go/tools/cmd/staticcheck"))))

(define-public go-github-com-cpuguy83-go-md2man-v2
  (package
    (name "go-github-com-cpuguy83-go-md2man-v2")
    (version "2.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cpuguy83/go-md2man")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "051ljpzf1f5nh631lvn53ziclkzmx5lza8545mkk6wxdfnfdcx8f"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/cpuguy83/go-md2man/v2"))
    (propagated-inputs
     (list
      go-github-com-russross-blackfriday-v2))
    (home-page "https://github.com/cpuguy83/go-md2man")
    (synopsis "go-md2man")
    (description "Converts markdown into roff (man pages).")
    (license license:expat)))

(define-public go-github-com-russross-blackfriday-v2
  (package
    (name "go-github-com-russross-blackfriday-v2")
    (version "2.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/russross/blackfriday")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0d1rg1drrfmabilqjjayklsz5d0n3hkf979sr3wsrw92bfbkivs7"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/russross/blackfriday/v2"))
    (home-page "https://github.com/russross/blackfriday")
    (synopsis "Blackfriday")
    (description "Package blackfriday is a markdown processor.
")
    (license license:bsd-2)))

(define-public toxiproxy
  (package
    (name "toxiproxy")
    (version "2.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Shopify/toxiproxy")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "13bhhgvab1m1wyxng729inf036qgj91laxarjc7qga4kzvnrzsjz"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/Shopify/toxiproxy/v2"))
    (propagated-inputs
     (list
      go-golang-org-x-sys
      go-github-com-shurcool-sanitized-anchor-name
      go-github-com-russross-blackfriday-v2
      go-github-com-cpuguy83-go-md2man-v2
      go-gopkg-in-tomb-v1
      go-golang-org-x-term
      go-github-com-urfave-cli-v2
      go-github-com-sirupsen-logrus
      go-github-com-gorilla-mux))
    (home-page "https://github.com/Shopify/toxiproxy")
    (synopsis "Toxiproxy")
    (description
     "Toxiproxy is a framework for simulating network conditions.  It's made
specifically to work in testing, CI and development environments, supporting
deterministic tampering with connections, but with support for randomized chaos
and customization.  We've been
successfully using it in all development and test environments at Shopify since
October, 2014.  See our @url{https://shopifyengineering.myshopify.com/blogs/engineering/building-and-testing-resilient-ruby-on-rails-applications,blog post} on resiliency for more information.")
    (license license:expat)))
