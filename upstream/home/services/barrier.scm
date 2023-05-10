;;; Copyright © 2023 Katherine Cox-Buday <cox.katherine.e@gmail.com>
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

(define-module (upstream home services barrier)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services utils)
  #:use-module (upstream packages barrier)
  #:use-module (gnu services configuration)

  #:use-module (guix gexp)
  #:use-module (guix packages)

  #:export (barriers-configuration
            home-barriers-service-type
            barrierc-configuration
            home-barrierc-service-type))

;;; Common functions

(define-maybe string)
(define-maybe boolean)

(define (serialize-string name value)
  value)

(define (serialize-boolean name value)
  (serialize-string name (if value "true" "false")))

(define (cli-field? exclusions field)
  "Returns true when FIELD is not a member of EXCLUSIONS."
  (not (member (configuration-field-name field)
               exclusions)))

(define (cli-arguments fields cli-filter config)
  "Returns a list of CLI arguments"
  (let ((args (list)))
    (for-each
     (lambda (field)
       (let ((val ((configuration-field-serializer field)
                   (configuration-field-name field)
                   ((configuration-field-getter field) config))))
         (unless (string-null? val)
           (set! args
                 (cons* (format #f "--~a" (configuration-field-name field))
                        val
                        args)))))
     (filter cli-filter fields))
    args))

;;; Barrier server service

(define-configuration barriers-configuration
  (barrier
   (package barrier)
   "The barrier package to use.")
  (name
   maybe-string
   "The name of the screen clients will reference.")
  (port-number
   (integer 24800)
   "The address to listen on"
   empty-serializer)
  (disable-client-cert-checking
   maybe-boolean
   "Whether or not client certificate checking should be disabled."
   empty-serializer)
  (disable-crypto
   maybe-boolean
   "Whether SSL should be used to encrypt traffic between hosts.")
  (profile-dir
   maybe-string
   "The profile path to use.")
  (drop-dir
   maybe-string
   "The path where files should be dropped.")
  (no-xinitthreads
   maybe-boolean
   "Whether or not to call xinitthreads.")
  (screen-change-script
   maybe-string
   "The path of a script to call when the screen changes.")
  (debug
   (string "WARNING")
   "The level below which log messages should be filtered. Possible values are:
FATAL, ERROR, WARNING, NOTE, INFO, DEBUG, DEBUG1, DEBUG2.")
  (barrier.conf
   (string "")
   "The text of the barrier.conf file for the server to load."
   empty-serializer))

(define (barriers-cli-field? field)
  "Returns whether or not a configuration field is meant to be a CLI argument."
  (cli-field? '(barrier.conf) field))

(define (barriers-cli-arguments config)
  "Returns a list of CLI arguments to pass to barriers."
  (cli-arguments barriers-configuration-fields barriers-cli-field? config))

(define (barriers-configuration-file config)
  "Create the barrier.conf file based on CONFIG."
  (mixed-text-file
   "barrier.conf"
   (barriers-configuration-barrier.conf config)))

(define (home-barriers-shepherd-service config)
  "Returns a <shepherd-service> running the barriers daemon."
  (let ((cli-args (barriers-cli-arguments config))
        (port-number (barriers-configuration-port-number config))
        (disable-client-cert-checking
         (barriers-configuration-disable-client-cert-checking config))
        (barrier (barriers-configuration-barrier config)))
    (list (shepherd-service
           (documentation "Runs barrier server.")
           (provision '(barriers))
           ;; (requirement '(user-processes networking))
           (start #~(make-forkexec-constructor
                     (list #$(file-append barrier "/bin/barriers")
                           "--no-tray"
                           "--no-daemon"
                           "--disable-client-cert-checking"
                           "--address" #$(format #f ":~a" port-number)
                           "--config" #$(barriers-configuration-file config)
                           #$@cli-args
                           #$(when disable-client-cert-checking
                               "--disable-client-cert-checking"))
                     #:log-file (string-append
                                 (or (getenv "XDG_LOG_HOME")
                                     (format #f "~a/.local/var/log"
                                             (getenv "HOME")))
                                 "/barrierc.log")))
           (stop #~(make-kill-destructor))))))

(define-public home-barriers-service-type
  (service-type
   (name 'home-barriers)
   (extensions
    (list
     (service-extension
      home-shepherd-service-type
      home-barriers-shepherd-service)
     (service-extension
      home-profile-service-type
      (const (list barrier)))))
   (default-value #f)
   (description "Configures and runs the barriers daemon.")))

;;; Barrier client service


(define-configuration barrierc-configuration
  (barrier
   (package barrier)
   "The barrier package to use.")
  (name
   maybe-string
   "The name of the screen clients will reference.")
  (server-address
   maybe-string
   "The server to connect to."
   ;; So that it doesn't get lumped in with CLI flags
   empty-serializer)
  (disable-crypto
   maybe-boolean
   "Whether SSL should be used to encrypt traffic between hosts.")
  (profile-dir
   maybe-string
   "The profile path to use.")
  (drop-dir
   maybe-string
   "The path where files should be dropped.")
  (no-xinitthreads
   maybe-boolean
   "Whether or not to call xinitthreads.")
  (debug
   (string "WARNING")
   "The level below which log messages should be filtered. Possible values are:
FATAL, ERROR, WARNING, NOTE, INFO, DEBUG, DEBUG1, DEBUG2.")
  (yscroll
   maybe-string
   "Defines the vertical scrolling delta."))

(define (barrierc-cli-field? field)
  (cli-field? '(barrier) field))

(define (barrierc-cli-arguments config)
  "Generates a list of CLI arguments to pass to barrierc."
  (cli-arguments barrierc-configuration-fields identity config))

(define (home-barrierc-shepherd-service config)
  (let ((cli-args (barrierc-cli-arguments config))
        (server-address (barrierc-configuration-server-address config))
        (barrier (barrierc-configuration-barrier config)))
    (list (shepherd-service
           (documentation "Runs the barrier client.")
           (provision '(barrierc))
           ;;(requirement '(user-processes networking))
           (start #~(make-forkexec-constructor
                     (list #$(file-append barrier "/bin/barrierc")
                           "--no-daemon"
                           "--no-tray"
                           ;; Allow shepherd to handle restarts
                           "--no-restart"
                           #$@cli-args
                           #$(unless (string-null? server-address)
                               server-address))
                     #:log-file (string-append
                                 (or (getenv "XDG_LOG_HOME")
                                     (format #f "~a/.local/var/log"
                                             (getenv "HOME")))
                                 "/barrierc.log")))
           (stop #~(make-kill-destructor))))))

(define-public home-barrierc-service-type
  (service-type
   (name 'home-barrierc)
   (extensions
    (list (service-extension
           home-shepherd-service-type
           home-barrierc-shepherd-service)
          (service-extension
           home-profile-service-type
           (const (list barrier)))))
   (default-value #f)
   (description "Configures and runs the barrier client daemon.")))
