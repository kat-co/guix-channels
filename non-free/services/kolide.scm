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

(define-module (non-free services kolide)
  #:use-module (srfi srfi-1)

  #:use-module (gnu build shepherd)
  #:use-module (gnu home services utils)
  #:use-module ((gnu packages admin) #:select (shadow))
  #:use-module (gnu services)
  #:use-module (gnu services admin)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system shadow)

  #:use-module (guix gexp)
  #:use-module (guix packages)

  #:use-module (non-free packages security)

  #:export (kolide-configuration
            kolide-service-type))

(define %kolide-root-path "/var/run/kolide/")
(define %kolide-log-path "/var/log/kolide.log")
(define %kolide-etc-path "kolide-k2/")
(define %kolide-metadata-path "installer-info.json")
(define %kolide-enroll-secret-path "secret")

(define %kolide-log-rotations
  (list (log-rotation
         (files (list %kolide-log-path))
         (frequency 'weekly))))

(define list-of-paths? (list-of string?))
(define (serialize-list-of-paths name paths)
  (specification->file-system-mapping paths #f))

(define (serialize-string name value)
  (format #f "--~a=~a" (object->snake-case-string name) value))

(define (serialize-boolean name value)
  (serialize-string name (if value "true" "false")))

(define-configuration kolide-configuration
  ;;; Shepherd configuration
  (kolide-launcher
   (package kolide-launcher)
   "Kolide launcher package to use")
  (osquery
   (package osquery)
   "osquery package to use")
  (containerize?
   (boolean #t)
   "Whether or not to run within a container with exposed paths")
  (exposed-paths
   (list-of-paths '())
   "A list of paths to be exposed when run within a container.")
  (log-path
   (string %kolide-log-path)
   "The path to send logs to")
  ;;; Standard configuration
  (installer-file-name
   (string "")
   "The ID generated for you when you request enrollment. This is either the
prefix of the file downloaded, or contained in
/etc/kolide-k2/installer-info.json.")
  (hostname
   (string "k2device.kolide.com")
   "The hostname to connect to")
  (transport
   (string "")
   "The transport protocol that should be used to communicate with remote.")
  (enroll-secret
   (string "")
   "The enroll secret that is used in your environment. If specified, a file
will be created at /etc/kolide-k2/secret.")
  (enroll-secret-path
   (string (string-append "/etc/" %kolide-etc-path %kolide-enroll-secret-path))
   "Optionally, the path to the enrollment secret")
  (autoupdate
   (boolean #f)
   "Whether or not the osquery autoupdater is enabled")
  (control
   (boolean #f)
   "Whether or not the control server is enabled")
  (control-hostname
   (string "k2control.kolide.com")
   "The hostname of the control server.")
  (with-initial-runner
   (boolean #f)
   "Run differential queries from config ahead of scheduled interval")
  ;; "Dev" configuration
  (debug
   (boolean #f)
   "Whether or not debug logging is enabled")
  (osquery-verbose
   (boolean #f)
   "Enable verbose osqueryd")
  (debug-log-file
   (string "")
   "File to mirror debug logs to (optional)")
  (insecure
   (boolean #f)
   "Do not verify TLS certs for outgoing connections")
  (insecure-transport
   (boolean #f)
   "Do not use TLS for transport layer")
  (notary-url
   (string "")
   "The Notary update server")
  (mirror-url
   (string "")
   "The mirror server for autoupdates")
  (update-channel
   (string "")
   "The channel to pull updates from")
  (notary-prefix
   (string "")
   "The prefix for Notary path that contains the collections")
  (disable-control-tls
   (boolean #f)
   "Disable TLS encryption fro the control features")
  (osquery-flag
   (string "")
   "Flags to pass to osquery (possibly overriding Launcher defaults"))

(define (kolide-container-mappings config)
  (let ((paths (kolide-configuration-exposed-paths config))
        (osquery (kolide-configuration-osquery config)))
    #~(cons*
       ;; Persist state between runs
       (specification->file-system-mapping #$%kolide-root-path #t)
       (map
        (lambda (spec) (specification->file-system-mapping spec #f))
        (list
         ;; Ensure it can read configuration
         (string-append "/etc/" #$%kolide-etc-path)
         ;; Include osqueryd in a place Kolide can find it
         (string-append #$osquery "/bin=/usr/local/bin")
         #$@paths)))))

(define (kolide-cli-args config fields)
  (let* ((cli-arg? (lambda (f) (not (member (configuration-field-name f)
                                            '(kolide-launcher
                                              osquery
                                              installer-file-name
                                              containerize?
                                              exposed-paths
                                              log-path
                                              enroll-secret)))))
         (value? (lambda (f) (not (equal? ((configuration-field-getter f) config)
                                          ""))))
         (or? (lambda (f) (every (lambda (p) (p f))
                                 (list cli-arg? value?))))
         (cli-args (filter or? fields))
         (osquery (kolide-configuration-osquery config)))
    (map (lambda (field)
           ((configuration-field-serializer field)
            (configuration-field-name field)
            ((configuration-field-getter field) config)))
         cli-args)))

(define (kolide-shepherd-service config)
  (let ((exposed-paths (kolide-container-mappings config))
        (containerize? (kolide-configuration-containerize? config))
        (log-path (kolide-configuration-log-path config))
        (cli-args (kolide-cli-args config kolide-configuration-fields))
        (kolide-launcher (kolide-configuration-kolide-launcher config))
        (osquery (kolide-configuration-osquery config)))

    (list (shepherd-service
           (documentation "Kolide")
           (provision '(kolide))
           (requirement '(user-processes loopback syslogd))

           (start (if containerize?
                      #~(make-forkexec-constructor/container
                         (list #$(file-append kolide-launcher "/bin/launcher")
                               (string-append "--root_directory=" #$%kolide-root-path)
                               (string-append
                                "--osqueryd_path="
                                #$(file-append osquery "/bin/osqueryd"))
                               #$@cli-args)
                         #:pid-file #$(string-append %kolide-root-path
                                                     "/launcher.pid")
                         ;; #:user "kolide"
                         ;; #:group "kolide"
                         #:mappings #$exposed-paths
                         #:log-file #$log-path)
                      #~(make-forkexec-constructor
                         (list #$(file-append kolide-launcher "/bin/launcher")
                               (string-append "--root_directory=" #$%kolide-root-path)
                               (string-append
                                "--osqueryd_path="
                                #$(file-append osquery "/bin/osqueryd"))
                               #$@cli-args)
                         #:pid-file #$(string-append %kolide-root-path
                                                     "/launcher.pid")
                         ;; #:user "kolide"
                         ;; #:group "kolide"
                         #:log-file #$log-path)))
           (stop #~(make-kill-destructor))
           (respawn? #t)))))

(define %kolide-accounts
  (list (user-group (name "kolide") (system? #t))
        (user-account
         (name "kolide")
         (group "kolide")
         (supplementary-groups
          '("root"))
         (system? #t)
         (comment "Kolide server user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define %kolide-activation
  #~(begin
      (use-modules (guix build utils))
      (let ((user (getpwnam "root")))
        (for-each
         (lambda (f)
           (mkdir-p f)
           (chown f (passwd:uid user) (passwd:gid user)))
         (list #$%kolide-root-path)))))

(define (kolide-etc config)
  (let ((enrollment-secret (kolide-configuration-enroll-secret config)))
    (list
     ;; Kolide inspects this file for the enrollment request id, so ensure it
     ;; exists.
     (list
      (string-append %kolide-etc-path %kolide-metadata-path)
      (plain-file
       %kolide-metadata-path
       (format #f "{\"download_file\":\"~a\"}"
               (kolide-configuration-installer-file-name config))))
     ;; If we've specified the secret, then place it in /etc
     (unless (equal? enrollment-secret "")
       (list (string-append %kolide-etc-path %kolide-enroll-secret-path)
             (plain-file %kolide-enroll-secret-path enrollment-secret))))))

(define kolide-service-type
  (service-type
   (name 'kolide)
   (description "Configures and runs Kolide launcher")
   (extensions
    (list (service-extension shepherd-root-service-type kolide-shepherd-service)
          (service-extension account-service-type (const %kolide-accounts))
          (service-extension activation-service-type
                             (const %kolide-activation))
          (service-extension etc-service-type kolide-etc)
          (service-extension rottlog-service-type
                             (const %kolide-log-rotations))))
   (default-value (kolide-configuration))))
