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

(define-module (upstream home services gopls)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services utils)
  #:use-module (gnu packages)
  #:use-module (gnu services configuration)

  #:use-module (upstream packages golang)

  #:use-module (guix gexp)
  #:use-module (guix packages)

  #:export (gopls-configuration))

(define %socket-path-default
  (string-append
   "unix;"
   (or (getenv "XDG_RUNTIME_DIR")
       (format #f "/run/user/~a" (getuid)))
   "/gopls"))

(define-configuration/no-serialization gopls-configuration
  (gopls
   (package gopls)
   "The gopls package to use.")
  (address
   (string %socket-path-default)
   "The address gopls should listen to. The default is
\"unix;$XDG_RUNTIME_DIR/gopls\"")
  (address-debug
   (string "")
   "The address gopls should listen to to serve debug information. The default is
off.")
  (resource-limits
   (list '())
   "The alist of resource limits to pass to Shepherd's make-forkexec functions."))

(define (home-gopls-shepherd-service config)
  (let* ((address (string-trim-both (gopls-configuration-address config)))
         (limits (gopls-configuration-resource-limits config))
         (address-debug (string-trim-both (gopls-configuration-address-debug config)))
         (debug? (not (zero? (string-length address-debug))))
         (gopls (gopls-configuration-gopls config)))
    (list
     (shepherd-service
      (documentation "Run the gopls LSP server.")
      (provision '(gopls))
      (start #~(make-forkexec-constructor
                `(#$(file-append gopls "/bin/gopls")
                    "-listen" #$address
                    ,@(if #$debug?
                          (list "-debug" #$address-debug)
                          '()))
                #:resource-limits '#$limits))
      (stop #~(make-kill-destructor))
      (respawn? #t)))))

(define-public home-gopls-service-type
  (service-type
   (name 'home-gopls)
   (extensions
    (list
     (service-extension home-shepherd-service-type home-gopls-shepherd-service)
     (service-extension home-profile-service-type (const (list gopls)))))
   (default-value (gopls-configuration))
   (description "Run gopls as a Shepherd service.")))
