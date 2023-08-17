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
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system go)
  #:use-module (guix build utils)
  #:use-module (guix transformations)
  #:use-module (guix utils)

  #:use-module (gnu packages base)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages security-token)
  #:use-module (gnu packages tls))

(define with-go-1.19
  (package-input-rewriting/spec `(("go" . ,(const go-1.19)))))

(define-public go-1.21
  (package
    (inherit go-1.20)
    (name "go")
    (version "1.21.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/golang/go")
                    (commit (string-append "go" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "04cpahl10i1sncymdfm0vzcj3czv6lv0axwa1sisl9cz3rdrp7hj"))))
    (arguments
     (substitute-keyword-arguments (package-arguments go-1.20)
       ;; Source patching phases are broken up into discrete steps to allow
       ;; future versions to discard individual phases without having to
       ;; discard all source patching.
       ((#:phases phases)
        #~(modify-phases #$phases
            (delete 'skip-TestGoPathShlibGccgo-tests)
            (delete 'patch-source)
            (add-after 'unpack 'patch-os-tests
              (lambda _
                (substitute* "src/os/os_test.go"
                  (("/usr/bin") (getcwd))
                  (("/bin/sh") (which "sh")))))

            (add-after 'unpack 'apply-patches
              (lambda* (#:key inputs #:allow-other-keys)
                ;; Having the patch in the 'patches' field of <origin> breaks
                ;; the 'TestServeContent' test due to the fact that
                ;; timestamps are reset.  Thus, apply it from here.
                (invoke "patch" "-p1" "--force" "-i"
                        (assoc-ref inputs "go-fix-script-tests.patch"))))

            (add-after 'unpack 'patch-src/net
              (lambda* (#:key inputs #:allow-other-keys)
                (let ((net-base (assoc-ref inputs "net-base")))
                  (substitute* "src/net/lookup_unix.go"
                    (("/etc/protocols")
                     (string-append net-base "/etc/protocols")))
                  (substitute* "src/net/port_unix.go"
                    (("/etc/services")
                     (string-append net-base "/etc/services"))))))

            (add-after 'unpack 'patch-zoneinfo
              (lambda* (#:key inputs #:allow-other-keys)
                ;; Add the path to this specific version of tzdata's zoneinfo
                ;; file to the top of the list to search. We don't want to
                ;; replace any sources because it will affect how binaries
                ;; compiled with this Go toolchain behave on non-guix
                ;; platforms.
                (substitute* "src/time/zoneinfo_unix.go"
                  (("var platformZoneSources.+" all)
                   (format #f "~a~%\"~a/share/zoneinfo\",~%"
                           all
                           (assoc-ref inputs "tzdata"))))))

            (add-after 'unpack 'patch-cmd/go/testdata/script
              (lambda _
                (substitute* "src/cmd/go/testdata/script/cgo_path_space.txt"
                  (("/bin/sh") (which "sh")))))

            (add-after 'enable-external-linking 'enable-external-linking-1.21
              (lambda _
                ;; Invoke GCC to link any archives created with GCC (that is, any
                ;; packages built using 'cgo'), because Go doesn't know how to
                ;; handle the runpaths but GCC does.  Use substitute* rather than
                ;; a patch since these files are liable to change often.
                ;;
                ;; XXX: Replace with GO_EXTLINK_ENABLED=1 or similar when
                ;; <https://github.com/golang/go/issues/31544> and/or
                ;; <https://github.com/golang/go/issues/43525> are resolved.
                (substitute* "src/cmd/link/internal/ld/config.go"
                  (("\\(iscgo && \\(.+\\)") "iscgo"))
                (substitute* "src/internal/testenv/testenv.go"
                  (("!CanInternalLink.+") "true {\n"))
                (substitute* "src/syscall/exec_linux_test.go"
                  (("testenv.MustHaveExecPath\\(t, \"whoami\"\\)")
                   "t.Skipf(\"no passwd file present\")"))))

            (replace 'install
              (lambda* (#:key outputs #:allow-other-keys)
                ;; Notably, we do not install archives (180M), which Go will
                ;; happily recompile quickly (and cache) if needed, almost
                ;; surely faster than they could be substituted.
                ;;
                ;; The main motivation for pre-compiled archives is to use
                ;; libc-linked `net' or `os' packages without a C compiler,
                ;; but on Guix a C compiler is necessary to properly link the
                ;; final binaries anyway.  Many build flags also invalidate
                ;; these pre-compiled archives, so in practice Go often
                ;; recompiles them anyway.
                ;;
                ;; Upstream is also planning to no longer install these
                ;; archives: <https://github.com/golang/go/issues/47257>
                ;;
                ;; When necessary, a custom pre-compiled library package can
                ;; be created with `#:import-path "std"' and used with
                ;; `-pkgdir'.
                ;;
                ;; When moving files into place, any files that come from
                ;; GOROOT should remain in GOROOT to continue functioning. If
                ;; they need to be referenced from some other directory, they
                ;; need to be symlinked from GOROOT. For more information,
                ;; please see https://github.com/golang/go/issues/61921
                (let* ((out (assoc-ref outputs "out"))
                       (tests (assoc-ref outputs "tests")))
                  (for-each
                   (lambda (file)
                     (copy-recursively file (string-append out "/lib/go/" file)))
                   '("bin" "go.env" "lib" "VERSION" "pkg/include" "pkg/tool"))

                  (symlink "lib/go/bin" (string-append out "/bin"))

                  (for-each
                   (match-lambda
                     ((file dest output)
                      ;; Copy to output/dest and symlink from output/lib/go/file.
                      (let ((file* (string-append output "/lib/go/" file))
                            (dest* (string-append output "/" dest)))
                        (copy-recursively file dest*)
                        (mkdir-p (dirname file*))
                        (symlink (string-append "../../" dest) file*))))
                   `(("src"          "share/go/src"        ,out)
                     ("misc"         "share/go/misc"       ,out)
                     ("doc"          "share/doc/go/doc"    ,out)
                     ("api"          "share/go/api"        ,tests)
                     ("test"         "share/go/test"       ,tests))))))))))))

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

(define-public openconnect-gp-okta
  (package
    (name "openconnect-gp-okta")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kat-co/openconnect-gp-okta")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0j9znd0l1mfc9gwah6zca35yhvi4avz02xnrps0zk6phafg3lw1f"))))
    (build-system go-build-system)
    (arguments
     `(#:go ,go-1.20
       #:import-path "openconnect-gp-okta"))
    (inputs (list libfido2 libressl))
    (synopsis "@code{openconnect} wrapper which performs a webauthn flow")
    (description
     "@code{openconnect-gp-okta} first performs a webauthn flow against a
GlobalProtect VPN endpoint which uses Okta and an authentication device. It then
launches @code{openconnect} and passes the pre-login cookie obtained to it.")
    (home-page "http://github.com/kat-co/openconnect-gp-okta")
    (license license:gpl3+)))

(define-public govulncheck
  (package
    (name "govulncheck")
    (version "v0.0.0-20230110180137-6ad3e3d07815")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://go.googlesource.com/vuln")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1fhz27ni8bs872rgvqq700qacak9v45zy0fh2hilq21sk6dks72r"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "golang.org/x/vuln"
           #:go go-1.20
           #:install-source? #f
           #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'remove-go-mod-tidy
                          (lambda _
                            (substitute* "src/golang.org/x/vuln/checks.bash"
                              (("go mod tidy")
                               (string-append
                                #$(this-package-native-input "coreutils-minimal")
                                "/bin/true")))))
                        (replace 'build
                          (lambda arguments
                            (apply (assoc-ref %standard-phases
                                              'build)
                                   `(,@arguments #:import-path
                                                 "golang.org/x/vuln/cmd/govulncheck")))))))
    (native-inputs (list coreutils-minimal))
    (inputs (list go-golang-org-x-sys
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
     "This repository contains packages for accessing and analyzing data from
the @url{https://vuln.go.dev,Go Vulnerability Database}.")
    (license license:bsd-3)))

(define-public go-golang-org-x-vuln
  (package
    (inherit govulncheck)
    (name "go-golang-org-x-vuln")
    (arguments
     `(#:import-path "golang.org/x/vuln"
       #:tests? #f
       #:install-source? #t
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (propagated-inputs (package-inputs govulncheck))
    (native-inputs '())
    (inputs '())))

(define-public gopls
  (package
   (name "gopls")
   (version "0.12.0")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://go.googlesource.com/tools")
                  (commit (string-append "gopls/v" version))))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "04bq7rh6d6mgxm0lsi8y9v1x7cgx4nvjlsyvxl89r6rcqh3n1lfb"))))
   (build-system go-build-system)
   (arguments
    `(#:import-path "golang.org/x/tools/gopls"
      #:unpack-path "golang.org/x/tools"
      #:go ,go-1.20
      #:install-source? #f
      #:phases (modify-phases %standard-phases
                              (add-before 'unpack 'override-tools
                                          (lambda _
                                            (delete-file-recursively "src/golang.org/x/tools"))))))
   (propagated-inputs (list go-github-com-google-go-cmp-cmp
                            go-github-com-jba-printsrc
                            go-github-com-jba-templatecheck
                            go-github-com-sergi-go-diff
                            go-golang-org-x-mod
                            go-golang-org-x-sync
                            go-golang-org-x-sys
                            go-golang-org-x-text
                            go-gopkg-in-yaml-v3
                            go-honnef-co-go-tools
                            go-github-com-burntsushi-toml
                            go-github-com-google-safehtml
                            go-golang-org-x-exp
                            go-mvdan-cc-gofumpt
                            go-golang-org-x-vuln
                            go-mvdan-cc-xurls))
   (home-page "https://golang.org/x/tools/gopls")
   (synopsis "Official language server for the Go language")
   (description
    "Pronounced ``Go please'', this is the official Go language server
developed by the Go team.  It provides IDE features to any LSP-compatible
editor.")
   (license license:bsd-3)))
