;;; Copyright © 2022 Katherine Cox-Buday <cox.katherine.e@gmail.com>
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

(define-module (upstream services barrier)
  #:use-module (gnu home services utils)
  #:use-module (gnu packages barrier)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)

  #:use-module (guix gexp)
  #:use-module (guix packages)

  #:export (barriers-configuration
            barriers-service-type
            barrierc-configuration
            barrierc-service-type))

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
  (user
   (string "root")
   "The user to run the service as"
   empty-serializer)
  (xauthority
   (string "~/.Xauthority")
   "The location of the Xauthority file."
   empty-serializer)
  (name
   maybe-string
   "The name of the screen clients will reference.")
  (port-number
   (integer 24800)
   "The address to listen on"
   empty-serializer)
  (enable-drag-drop
   maybe-boolean
   "Whether drag and drop is enabled between hosts.")
  (disable-crypto
   maybe-boolean
   "Whether SSL should be used to encrypt traffic between hosts.")
  (profile-dir
   maybe-string
   "The profile path to use.")
  (drop-dir
   maybe-string
   "The path where files should be dropped.")
  (display
   (string "")
   "The X11 server to connect to.")
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

(define (barriers-shepherd-service config)
  "Returns a <shepherd-service> running the barriers daemon."
  (let ((cli-args (barriers-cli-arguments config))
        (xauthority (barriers-configuration-xauthority config))
        (user (barriers-configuration-user config))
        (port-number (barriers-configuration-port-number config)))
    (list (shepherd-service
           (documentation "Runs barrier server.")
           (provision '(barriers))
           (requirement '(user-processes networking))
           (start #~(make-inetd-constructor
                     (list #$(file-append barrier "/bin/barriers")
                           "--no-tray"
                           "--no-daemon"
                           "--disable-client-cert-checking"
                           "--address" #$(format #f ":~a" port-number)
                           "--config" #$(barriers-configuration-file config)
                           #$@cli-args)
                     #$(make-socket-address AF_INET INADDR_ANY port-number)
                     #:max-connections 1
                     #:user #$user
                     #:environment-variables (list #$(format #f "XAUTHORITY=~a" xauthority))
                     #:log-file "/var/log/barrier"))
           (stop #~(make-inetd-destructor))))))

(define barriers-service-type
  (service-type
   (name 'barriers)
   (extensions
    (list (service-extension shepherd-root-service-type barriers-shepherd-service)))
   (description "Configures and runs the barriers daemon.")))

;;; Barrier client service


(define-configuration barrierc-configuration
  (barrier
   (package barrier)
   "The barrier package to use.")
  (name
   maybe-string
   "The name of the screen clients will reference.")
  (enable-drag-drop
   maybe-boolean
   "Whether drag and drop is enabled between hosts.")
  (disable-crypto
   maybe-boolean
   "Whether SSL should be used to encrypt traffic between hosts.")
  (profile-dir
   maybe-string
   "The profile path to use.")
  (drop-dir
   maybe-string
   "The path where files should be dropped.")
  (display
   (string "")
   "The X11 server to connect to.")
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

(define (barrierc-shepherd-service config)
  (let ((cli-args (barrierc-cli-arguments config)))
    (list (shepherd-service
           (documentation "Runs the barrier client.")
           (provision '(barrierc))
           (requirement '(user-processes networking))
           (start #~(make-forkexec-constructor
                     (list #$(file-append barrier "/bin/barrierc")
                           "--no-tray"
                           #$@cli-args)
                     #:log-file "/var/log/barrier"))))))

(define barrierc-service-type
  (service-type
   (name 'barrierc)
   (extensions
    (list (service-extension shepherd-root-service-type barrierc-shepherd-service)))
   (description "Configures and runs the barrier client daemon.")))
