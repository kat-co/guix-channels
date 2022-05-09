;;; Copyright © 2021, 2022 Katherine Cox-Buday <cox.katherine.e@gmail.com>
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

(define-module (non-free services sentinelone)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 iconv)

  #:use-module (gnu build shepherd)
  #:use-module (gnu home services utils)
  #:use-module (gnu packages base)
  #:use-module ((gnu packages admin) #:select (shadow))
  #:use-module (gnu services)
  #:use-module (gnu services admin)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system shadow)

  #:use-module (guix base64)
  #:use-module (guix gexp)
  #:use-module (guix packages)

  #:use-module (non-free packages security)

  #:export (s1-configuration
            sentinelone-service-type))

(define %s1-root-path "/opt/sentinelone/")
(define %s1-log-path "/var/log/sentinelone/")
(define %s1-etc-path "sentinelone/")
(define %s1-installation-params-path "installation_params.json")

(define list-of-paths? (list-of string?))
(define (serialize-list-of-paths name paths)
  (specification->file-system-mapping paths #f))

(define (serialize-string name value)
  (format #f "--~a=~a" (object->snake-case-string name) value))

(define (serialize-boolean name value)
  (serialize-string name (if value "true" "false")))

(define-configuration s1-configuration
  ;;; Shepherd configuration
  (sentinelone
   (package hello)
   "SentinelOne package to use. This is set to a dummy value and must be
overriden in the config file for the machine since this is usually distributed
to user's machines.")
  (containerize?
   (boolean #t)
   "Whether or not to run within a container with exposed paths")
  (exposed-paths
   (list-of-paths '())
   "A list of paths to be exposed when run within a container.")
  (log-path
   (string %s1-log-path)
   "The path to send logs to")
  ;;; Standard configuration
  (management-token
   (string "")
   "The management token given to you by your organization."))

(define (s1-container-mappings config)
  (let ((paths (s1-configuration-exposed-paths config))
        (sentinelone (s1-configuration-sentinelone config)))
    #~(append
       ;; Map write directories
       (map
        (lambda (spec) (specification->file-system-mapping spec #t))
        (list
         #$(string-append %s1-root-path "=/opt/sentinelone")
         #$(string-append %s1-log-path "=/opt/sentinelone/log")))
       ;; Map read-only directories
       (map
        (lambda (spec) (specification->file-system-mapping spec #f))
        (list
         ;; Ensure it can read configuration
         #$(string-append "/etc/" %s1-etc-path "=/opt/sentinelone/configuration")
         ;; Include binaries
         #$(file-append sentinelone "/opt/sentinelone/bin=/opt/sentinelone/bin")
         #$@paths)))))

(define (s1-shepherd-service config)
  (let ((exposed-paths (s1-container-mappings config))
        (containerize? (s1-configuration-containerize? config))
        (sentinelone (s1-configuration-sentinelone config)))

    (list (shepherd-service
           (documentation "Runs SentinelOne's agent.")
           (provision '(sentinelone))
           (requirement '(user-processes loopback))

           ;; The sentinelone accounts must exist, but the agent must start as
           ;; root. It later drops to sentinelone.
           (start
            (if containerize?
                #~(make-forkexec-constructor/container
                   (list #$(file-append sentinelone
                                        "/opt/sentinelone/bin/sentinelone-agent"))
                   #:mappings #$exposed-paths)
                #~(make-forkexec-constructor
                   (list #$(string-append %s1-root-path "bin/sentinelone-agent")))))
           (stop #~(make-kill-destructor))
           (respawn? #f)))))

(define %s1-accounts
  (list (user-group (name "sentinelone") (system? #t))
        (user-account
         (name "sentinelone")
         (group "sentinelone")
         ;; (supplementary-groups
         ;;  '("root"))
         (system? #t)
         (comment "SentinelOne agent user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define (s1-activation config)
  (let ((containerize? (s1-configuration-containerize? config))
        (sentinelone (s1-configuration-sentinelone config))
        (management-token (s1-configuration-management-token config)))
    #~(begin
        (use-modules (guix build utils))
        (let ((user (getpwnam "sentinelone"))
              ;; This list came from the `dirs` file of the .deb installer.
              (writeable-paths (map (lambda (f) (string-append #$%s1-root-path f))
                                    (list "." ; main s1 folder
                                          "agent_modules"
                                          "assets"
                                          "auto_uploads"
                                          "cgroups"
                                          "comm_sdk"
                                          "configuration"
                                          "crash_drumps"
                                          "model"
                                          "mount"
                                          "rso"
                                          ".storage"
                                          "tmp"
                                          "uploads"))))
          ;; Create all the paths the agent expects and the root directory for
          ;; logs.
          (for-each
           (lambda (f)
             (mkdir-p f)
             (chown f (passwd:uid user) (passwd:gid user)))
           (cons #$%s1-log-path writeable-paths)))
        ;; Copy binaries over so that they're running in an expected place. Yes,
        ;; this is terrible.
        (let ((bin #$(string-append %s1-root-path "bin")))
          (unless (stat bin #f)
            (mkdir #$(string-append %s1-root-path "bin"))))
        (for-each
         (lambda (f)
           (copy-file (car f) (cdr f)))
         (list
          (cons #$(file-append sentinelone "/opt/sentinelone/bin/sentinelone-agent")
                #$(string-append %s1-root-path "bin/sentinelone-agent"))
          (cons #$(file-append sentinelone "/opt/sentinelone/bin/sentinelctl")
                #$(string-append %s1-root-path "bin/sentinelctl"))
          (cons #$(file-append sentinelone "/opt/sentinelone/bin/sentinelone-watchdog")
                #$(string-append %s1-root-path "bin/sentinelone-watchdog"))))
        ;; Even if a namespace is handling the filesystem layout, symlink so we
        ;; can run activation commands.
        (for-each
         (lambda (f)
           (let ((dest (cdr f)))
             (unless (and (stat dest #f) (symbolic-link? dest))
               ;; When redeploying, don't try and link these again.
               (symlink (car f) (cdr f)))))
         (list
          (cons #$%s1-log-path #$(string-append %s1-root-path "/log"))
          (cons #$(string-append "/etc/" %s1-etc-path %s1-installation-params-path)
                #$(string-append %s1-root-path "configuration/" %s1-installation-params-path))
          (cons #$(file-append sentinelone "/opt/sentinelone/detectors.pkg")
                #$(string-append %s1-root-path "detectors.pkg"))
          (cons #$(file-append sentinelone "/opt/sentinelone/benchmarks")
                #$(string-append %s1-root-path "benchmarks"))
          (cons #$(file-append sentinelone "/opt/sentinelone/ebpfs")
                #$(string-append %s1-root-path "ebpfs"))
          (cons #$(file-append sentinelone "/opt/sentinelone/filetypes_artifacts")
                #$(string-append %s1-root-path "filetypes_artifacts"))
          (cons #$(file-append sentinelone "/opt/sentinelone/home")
                #$(string-append %s1-root-path "home"))
          (cons #$(file-append sentinelone "/opt/sentinelone/lib")
                #$(string-append %s1-root-path "lib"))))
        ;; Generate the `basic.conf` file
        (unless (stat #$(string-append %s1-root-path "configuration/basic.conf")
                      #f)
          (let ((sentinelctl
                 #$(file-append sentinelone "/opt/sentinelone/bin/sentinelctl")))
            (invoke sentinelctl "management" "token" "set" #$management-token))))))

(define (s1-etc config)
  (list
   (list
    (string-append %s1-etc-path %s1-installation-params-path)
    (plain-file
     %s1-installation-params-path
     "{\"PACKAGE_TYPE\": \"deb\", \"SERVICE_TYPE\": \"sysvinit\"}"))))

(define sentinelone-service-type
  (service-type
   (name 'sentinelone)
   (description "Configures and runs the SentinelOne agent")
   (extensions
    (list (service-extension shepherd-root-service-type s1-shepherd-service)
          (service-extension account-service-type (const %s1-accounts))
          (service-extension activation-service-type s1-activation)
          (service-extension etc-service-type s1-etc)))))
