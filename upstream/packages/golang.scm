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
    (description
     "Package implements the decoding of logfmt key-value pairs.
")
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
     `(("go-golang-org-x-sys" ,go-golang-org-x-sys)
       ("go-github-com-urfave-cli" ,go-github-com-urfave-cli)
       ("go-github-com-mattn-go-isatty" ,go-github-com-mattn-go-isatty)
       ("go-github-com-mattn-go-colorable" ,go-github-com-mattn-go-colorable)
       ("go-github-com-kr-logfmt" ,go-github-com-kr-logfmt)
       ("go-github-com-go-logfmt-logfmt" ,go-github-com-go-logfmt-logfmt)
       ("go-github-com-fatih-color" ,go-github-com-fatih-color)
       ("go-github-com-aybabtme-rgbterm" ,go-github-com-aybabtme-rgbterm)))
    (home-page "https://github.com/aybabtme/humanlog")
    (synopsis "humanlog")
    (description
     "Read logs from @code{stdin} and prints them back to @code{stdout}, but prettier.")
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

(define-public go-golang-org-x-tools
  (let ((commit "aee3994bd5f840a71b7b3fd8ce9fa21176e0a9e1")
        (revision "0"))
    (package
      (name "go-golang-org-x-tools")
      (version (git-version "0.4.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://go.googlesource.com/tools")
                      (commit commit)))
                (file-name (string-append "go.googlesource.com-tools-"
                                          version "-checkout"))
                (sha256
                 (base32
                  "0ma6k8jhyl3is96gzyg5mzivwpcnglmpkq4mwyy3d6ml3h37kj3i"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/x/tools"
         ;; Source-only package
         #:tests? #f
         #:phases
         (modify-phases %standard-phases
           ;; Source-only package
           (delete 'build))))
      (propagated-inputs
       (list
        go-github-com-yuin-goldmark
        go-golang-org-x-mod
        go-golang-org-x-net
        go-golang-org-x-sys))
      (synopsis "Tools that support the Go programming language")
      (description "This package provides miscellaneous tools that support the
Go programming language.")
      (home-page "https://go.googlesource.com/tools/")
      (license license:bsd-3))))

(define-public go-github-com-burntsushi-toml
  (package
    (name "go-github-com-burntsushi-toml")
    (version "0.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/BurntSushi/toml")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0bz7q3v4m2rq5z47q81bvb4lw6ss2r1x3phgr43a6nls9x5kcpp1"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/BurntSushi/toml"))
    (home-page "https://github.com/BurntSushi/toml")
    (synopsis "Toml parser and encoder for Go")
    (description "This package is toml parser and encoder for Go.  The interface
is similar to Go's standard library @code{json} and @code{xml} package.")
    (license license:expat)))

(define-public go-honnef-co-go-tools
  (with-go-1.19
   (package
     (name "go-honnef-co-go-tools")
     (version "0.3.3")
     (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/dominikh/go-tools")
              (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "099z04v7vvwwglnps315s9fmal68xvzlc1g8m26iqi980grbwn32"))))
     (build-system go-build-system)
     (arguments
      `(#:import-path "honnef.co/go/tools"
        #:tests? #f
        ;; Source-only package
        #:phases
        (modify-phases %standard-phases
          (delete 'build))))
     (propagated-inputs
      (list
       go-golang-org-x-exp
       go-golang-org-x-tools
       go-golang-org-x-mod
       go-github-com-kisielk-gotool
       go-github-com-burntsushi-toml))
     (home-page "https://honnef.co/go/tools")
     (synopsis "Staticcheck advanced Go linter")
     (description
      "Staticcheck is a state of the art linter for the Go programming language.
Using static analysis, it finds bugs and performance issues, offers
simplifications, and enforces style rules.")
     (license license:expat))))

(define-public staticcheck
  (package
    (inherit go-honnef-co-go-tools)
    (name "staticcheck")
    (arguments '(#:unpack-path "honnef.co/go/tools"
                 #:import-path "honnef.co/go/tools/cmd/staticcheck"))))

(define-public go-gopkg-in-tomb-v1
  (package
    (name "go-gopkg-in-tomb-v1")
    (version "1.0.0-20141024135613-dd632973f1e7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gopkg.in/tomb.v1")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1lqmq1ag7s4b3gc3ddvr792c5xb5k6sfn0cchr3i2s7f1c231zjv"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f
       #:import-path "gopkg.in/tomb.v1" #:unpack-path "gopkg.in/tomb.v1"))
    (home-page "https://gopkg.in/tomb.v1")
    (synopsis "Installation and usage")
    (description
     "The tomb package offers a conventional API for clean goroutine termination.
")
    (license license:bsd-3)))

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
     `(("go-github-com-russross-blackfriday-v2"
        ,go-github-com-russross-blackfriday-v2)))
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
     `(("go-golang-org-x-sys" ,go-golang-org-x-sys)
       ("go-github-com-shurcool-sanitized-anchor-name"
        ,go-github-com-shurcool-sanitized-anchor-name)
       ("go-github-com-russross-blackfriday-v2"
        ,go-github-com-russross-blackfriday-v2)
       ("go-github-com-cpuguy83-go-md2man-v2"
        ,go-github-com-cpuguy83-go-md2man-v2)
       ("go-gopkg-in-tomb-v1" ,go-gopkg-in-tomb-v1)
       ("go-golang-org-x-term" ,go-golang-org-x-term)
       ("go-github-com-urfave-cli-v2" ,go-github-com-urfave-cli-v2)
       ("go-github-com-sirupsen-logrus" ,go-github-com-sirupsen-logrus)
       ("go-github-com-gorilla-mux" ,go-github-com-gorilla-mux)))
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
    (arguments '(#:import-path "github.com/aybabtme/humanlog"))
    (propagated-inputs
      `(("go-golang-org-x-sys" ,go-golang-org-x-sys)
        ("go-github-com-urfave-cli" ,go-github-com-urfave-cli)
        ("go-github-com-mattn-go-isatty" ,go-github-com-mattn-go-isatty)
        ("go-github-com-mattn-go-colorable" ,go-github-com-mattn-go-colorable)
        ("go-github-com-kr-logfmt" ,go-github-com-kr-logfmt)
        ("go-github-com-go-logfmt-logfmt" ,go-github-com-go-logfmt-logfmt)
        ("go-github-com-fatih-color" ,go-github-com-fatih-color)
        ("go-github-com-aybabtme-rgbterm" ,go-github-com-aybabtme-rgbterm)))
    (home-page "https://github.com/aybabtme/humanlog")
    (synopsis "humanlog")
    (description
      "Read logs from @code{stdin} and prints them back to @code{stdout}, but prettier.")
    (license license:asl2.0)))

(define-public go-1.19
  (package
   (inherit go-1.17)
   (name "go")
   (version "1.19.1")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/golang/go")
           (commit (string-append "go" version))))
     (file-name (git-file-name name version))
     (sha256
      (base32
       "1gah4zhbkgbwrrryfmzdv2qwi1rgxk10q2r3hnlcb1dybf9c1i1w"))))
   (arguments
    (substitute-keyword-arguments
     (package-arguments go-1.17)
     ((#:tests? _ #f) #f)
     ((#:phases child-phases '%standard-phases)
      `(modify-phases ,child-phases
        (replace 'install-doc-files
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out")))
              (for-each
               (lambda (file)
                 (install-file file (string-append out "/share/doc/go")))
               '("CONTRIBUTING.md" "LICENSE" "PATENTS" "README.md"
                 "SECURITY.md")))))))))))

;;; gopls

(define-public go-golang-org-x-sync
  (let ((commit "8fcdb60fdcc0539c5e357b2308249e4e752147f1")
        (revision "1"))
    (package
      (name "go-golang-org-x-sync")
      (version (git-version "0.1.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://go.googlesource.com/sync")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "07qrhni6f5hh5p95k1yk6s4wsj341q663irvx6rllrxfsymj6a0z"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/x/sync"
         #:tests? #f
         ;; Source-only package
         #:phases
         (modify-phases %standard-phases
           (delete 'build))))
      (synopsis "Additional Go concurrency primitives")
      (description "This package provides Go concurrency primitives in addition
to the ones provided by the language and “sync” and “sync/atomic”
packages.")
      (home-page "https://go.googlesource.com/sync/")
      (license license:bsd-3))))

(define-public go-golang-org-x-mod
  (let ((commit "7c05a442b7c1d1a107879b4a090bb5a38d3774a1")
        (revision "0"))
    (package
      (name "go-golang-org-x-mod")
      (version (git-version "0.7.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/golang/mod")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "14r24fq3kn84k2y2jvvg8hwpy52a3q429pimrdwl5zwknbr2awmh"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "golang.org/x/mod/"
         #:tests? #f
         #:phases
         (modify-phases %standard-phases
           ;; Source-only package
           (delete 'build))))
      (home-page "https://golang.org/x/mod")
      (synopsis "Tools to work directly with Go module mechanics")
      (description
       "This repository holds packages for writing tools that work directly
with Go module mechanics.  That is, it is for direct manipulation of Go modules
themselves.

The specific case of loading packages should still be done by invoking the
@command{go} command, which remains the single point of truth for package
loading algorithms.")
      (license license:bsd-3))))

(define-public go-golang-org-x-exp
  (package
    (name "go-golang-org-x-exp")
    (version "0.0.0-20221004215720-b9f4876ce741")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://go.googlesource.com/exp")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "030b929xyg8dpp6f4qbyg63msi6zgzj9sqmvnyphfcrjkqf7nr41"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "golang.org/x/exp"
       ;; Source-only package
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'build))))
    (home-page "https://golang.org/x/exp")
    (synopsis "exp")
    (description
     "This subrepository holds experimental and deprecated (in the @code{old}
directory) packages.")
    (license license:bsd-3)))

(define-public go-github-com-jba-printsrc
  (package
    (name "go-github-com-jba-printsrc")
    (version "0.2.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jba/printsrc")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1gyy3kmb5a5i710wkv3b7ah7i7sz5sdc7v3sab5m4rxch1sd2fpj"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/jba/printsrc"
       ;; TODO: Open bug; expecting time.Local, but when local=UTC, we get time.UTC
       #:tests? #f))
    (home-page "https://github.com/jba/printsrc")
    (synopsis "printsrc: Printing Go Values as Source")
    (description
     "Package printsrc prints Go values as Go source.  It strives to render legal Go
source code, and returns an error when detects that it cannot.")
    ;; MIT
    (license license:expat)))

(define-public go-github-com-google-safehtml
  (package
    (name "go-github-com-google-safehtml")
    (version "0.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/google/safehtml")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0j2xjy8xrk9y9k6bqpvimj84i6hg1wwsyvwsb0axhmp49cmnrp86"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/google/safehtml"))
    (propagated-inputs `(("go-golang-org-x-text" ,go-golang-org-x-text)))
    (home-page "https://github.com/google/safehtml")
    (synopsis "Safe HTML for Go")
    (description
     "Package safehtml provides immutable string-like types which represent values
that are guaranteed to be safe, by construction or by escaping or sanitization,
to use in various HTML contexts and with various DOM APIs.")
    (license license:bsd-3)))

(define-public go-github-com-jba-templatecheck
  (package
    (name "go-github-com-jba-templatecheck")
    (version "0.6.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jba/templatecheck")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "12iwkidz4p6wdl65jfddqxls80mv879k2rpb42dj7y4dja5advlc"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/jba/templatecheck"))
    (propagated-inputs `(("go-github-com-google-safehtml" ,go-github-com-google-safehtml)))
    (home-page "https://github.com/jba/templatecheck")
    (synopsis "templatecheck")
    (description
     "Package templatecheck checks Go templates for problems.  It can detect many
errors that are normally caught only during execution.  Use templatecheck in
tests to find template errors early, and along template execution paths that
might only rarely be reached.")
    ;; MIT
    (license license:expat)))

(define-public go-github-com-google-go-cmp-cmp
  (package
    (name "go-github-com-google-go-cmp")
    (version "0.5.9")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/google/go-cmp")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0a13m7l1jrysa7mrlmra8y7n83zcnb23yjyg3a609p8i9lxkh1wm"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/google/go-cmp/cmp"
       #:unpack-path "github.com/google/go-cmp"))
    (home-page "https://github.com/google/go-cmp")
    (synopsis "Package for equality of Go values")
    (description
     "This package is intended to be a more powerful and safer alternative to
@@code{reflect.DeepEqual} for comparing whether two values are semantically
equal.")
    (license license:bsd-3)))

;; TODO: This must be built with >= go@1.19. Work to bump version upstream first.
(define go-mvdan-cc-gofumpt
  (with-go-1.19
   (package
     (name "go-mvdan-cc-gofumpt")
     (version "0.4.0")
     (source (origin
               (method git-fetch)
               (uri (git-reference
                     (url "https://github.com/mvdan/gofumpt")
                     (commit (string-append "v" version))))
               (file-name (git-file-name name version))
               (sha256
                (base32
                 "13ahi8q1a9h4dj6a7xp95c79d5svz5p37b6z91aswbq043qd417k"))))
     (build-system go-build-system)
     (arguments
      '(#:import-path "mvdan.cc/gofumpt"
        #:tests? #f))
     (propagated-inputs
      (list go-github-com-pkg-diff
            go-github-com-kr-text
            go-github-com-kr-pretty
            go-golang-org-x-tools
            go-golang-org-x-sys
            go-golang-org-x-sync
            go-golang-org-x-mod
            go-github-com-rogpeppe-go-internal
            go-github-com-google-go-cmp-cmp
            go-github-com-frankban-quicktest))
     (home-page "https://mvdan.cc/gofumpt")
     (synopsis "gofumpt")
     (description
      "Enforce a stricter format than @@code{gofmt}, while being backwards compatible.
That is, @@code{gofumpt} is happy with a subset of the formats that
@@code{gofmt} is happy with.")
     (license license:bsd-3))))

(define-public go-mvdan-cc-xurls-v2
  (package
    (name "go-mvdan-cc-xurls-v2")
    (version "2.4.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mvdan/xurls")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0b040nbk1vwlk1qljavh8w8fn2r243q700n6gr8j2asmnz0xq84p"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "mvdan.cc/xurls/v2"
       #:unpack-path "mvdan.cc/xurls/v2"))
    (propagated-inputs `(("go-golang-org-x-sync" ,go-golang-org-x-sync)
                         ("go-github-com-rogpeppe-go-internal" ,go-github-com-rogpeppe-go-internal)))
    (home-page "https://mvdan.cc/xurls/v2")
    (synopsis "xurls")
    (description
     "Package xurls extracts urls from plain text using regular expressions.")
    (license license:bsd-3)))

(define-public go-github-com-client9-misspell
  (package
    (name "go-github-com-client9-misspell")
    (version "0.3.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/client9/misspell")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1vwf33wsc4la25zk9nylpbp9px3svlmldkm0bha4hp56jws4q9cs"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/client9/misspell"))
    (home-page "https://github.com/client9/misspell")
    (synopsis "Install")
    (description
     "Package misspell corrects commonly misspelled English words in source files.")
    (license license:expat)))

(define-public go-github-com-google-go-cmdtest
  (package
    (name "go-github-com-google-go-cmdtest")
    (version "0.4.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/google/go-cmdtest")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0zkghc60ymxmg19j90r6j7clq3xifh5m9kg1bgr4zpr5sv148x72"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/google/go-cmdtest"))
    (propagated-inputs
     (list
      go-github-com-google-renameio
      go-github-com-google-go-cmp-cmp))
    (home-page "https://github.com/google/go-cmdtest")
    (synopsis "Testing your CLI")
    (description
     "The cmdtest package simplifies testing of command-line interfaces.  It provides
a simple, cross-platform, shell-like language to express command execution.  It
can compare actual output with the expected output, and can also update a file
with new \"golden\" output that is deemed correct.")
    (license license:asl2.0)))

;; TODO: This must be built with >= go@1.19. Work to bump version upstream first.
(define-public go-mvdan-cc-unparam
  (with-go-1.19
   (package
     (name "go-mvdan-cc-unparam")
     (version "0.0.0-20221223090309-7455f1af531d")
     (source (origin
               (method git-fetch)
               (uri (git-reference
                     (url "https://github.com/mvdan/unparam")
                     (commit (go-version->git-ref version))))
               (file-name (git-file-name name version))
               (sha256
                (base32
                 "0wynf0b32azxljncw5fh9bwkxpdflvf9q1z16wyj432566yjh12c"))))
     (build-system go-build-system)
     (arguments
      '(#:import-path "mvdan.cc/unparam"
        #:tests? #f))
     (propagated-inputs `(("go-golang-org-x-sys" ,go-golang-org-x-sys)
                          ("go-golang-org-x-mod" ,go-golang-org-x-mod)
                          ("go-github-com-pkg-diff" ,go-github-com-pkg-diff)
                          ("go-golang-org-x-tools" ,go-golang-org-x-tools)
                          ("go-github-com-rogpeppe-go-internal" ,go-github-com-rogpeppe-go-internal)))
     (home-page "https://mvdan.cc/unparam")
     (synopsis "unparam")
     (description
      "Reports unused function parameters and results in your code.")
     (license license:bsd-3))))

(define-public go-golang-org-x-vuln
  (package
    (name "go-golang-org-x-vuln")
    (version "0.0.0-20221229164908-ebf31f7dc3ef")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://go.googlesource.com/vuln")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1w055g90k7anrrcvfrsqklxzl9pl0vqdiwpayj9f0brwys9xhj7d"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "golang.org/x/vuln"
       ;; Source-only package
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'build))))
    (propagated-inputs
     (list
      go-golang-org-x-sys
      go-github-com-google-renameio
      go-github-com-burntsushi-toml
      go-mvdan-cc-unparam
      go-honnef-co-go-tools
      go-golang-org-x-tools
      go-golang-org-x-sync
      go-golang-org-x-mod
      go-golang-org-x-exp
      go-github-com-google-go-cmp-cmp
      go-github-com-google-go-cmdtest
      go-github-com-client9-misspell))
    (home-page "https://golang.org/x/vuln")
    (synopsis "Go Vulnerability Management")
    (description
     "This repository contains packages for accessing and analyzing data from the
@@url{https://vuln.go.dev,Go Vulnerability Database}.  It contains the
following:")
    (license license:bsd-3)))

(define-public gopls
  (package
    (name "gopls")
    (version "0.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/golang/tools/archive/refs/tags/gopls/"
             "v" version ".tar.gz"))
       (sha256
        (base32
         "0c9r13515896wr1hixchbj1kq5isa2v2cf2lxdh4fny7pi001pdm"))))
    (build-system go-build-system)
    (arguments '(#:import-path "golang.org/x/tools/gopls"
                 #:unpack-path "github.com/golang/tools/gopls"))
    (propagated-inputs
     (list
      go-github-com-google-go-cmp-cmp
      go-github-com-jba-printsrc
      go-github-com-jba-templatecheck
      go-github-com-sergi-go-diff
      go-golang-org-x-mod
      go-golang-org-x-sync
      go-golang-org-x-sys
      go-golang-org-x-text
      go-golang-org-x-tools
      go-gopkg-in-yaml-v3
      go-honnef-co-go-tools
      go-mvdan-cc-gofumpt
      go-mvdan-cc-xurls-v2
      go-github-com-burntsushi-toml
      go-github-com-google-safehtml
      go-golang-org-x-exp
      go-golang-org-x-vuln))
    (home-page "https://github.com/golang/tools/blob/master/gopls")
    (synopsis "Official language server for the Go language.")
    (description "")
    (license license:bsd-3)))
