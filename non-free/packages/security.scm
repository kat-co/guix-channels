(define-module (non-free packages security)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system copy)

  #:use-module (gnu packages compression))

(define-public kolide-launcher
  (package
    (name "kolide-launcher")
    (version "0.11.22")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/kolide/launcher/releases/download/"
             "v" version "/linux.amd64_v" version ".zip"))
       (sha256
        (base32
         "1hlh5miblyxgc8n9b3jrl05cv8j66ch9a3qjrxylgazzw1s88jf6"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       '(("." "opt/kolide/bin" #:exclude ("package-builder")))))
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "https://github.com/kolide")
    (synopsis "Osquery management system with extended tables")
    (description
     "The Kolide Osquery Launcher is a lightweight launcher/manager
which offers a few extra capabilities on top of osquery:
@itemize
@item secure automatic updates of osquery
@item many additional tables
@item tooling to generate deployment packages for a variety of platforms
@end itemize")
    ;; MIT
    (license license:expat)))
