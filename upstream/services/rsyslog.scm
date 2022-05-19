(define-module (upstream services rsyslog)
  #:use-module (ice-9 match)
  #:use-module (ice-9 ports) ;; with-output-to-string
  #:use-module (srfi srfi-9)

  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix records)

  #:use-module (upstream packages logging)

  #:export (rsyslog-configuration
            make-rsyslog-module
            make-rsyslog-input
            make-rsyslog-ruleset
            make-rsyslog-action
            rsyslog-service-type
            home-rsyslog-service-type))

(define (serialize-arg-alist args)
  (string-join
   (map
    (lambda (c) (format #f "~a=~s" (car c) (cdr c)))
    args)
   " "))

(define-record-type* <rsyslog-block>
  rsyslog-block make-rsyslog-block rsyslog-block?
  (name rsyslog-block-name)
  ;; alist of: '((argument name . value))
  (input-args rsyslog-block-input-args
              (default '()))
  ;; A list of rsyslog-block
  (sub-blocks rsyslog-block-sub-blocks
              (default '())))

(define (block-with-name name)
  (lambda (b)
    (and (rsyslog-block? b)
         (equal? (rsyslog-block-name b) name))))

(define* (serialize-rsyslog-block _ block #:optional (indent-level 0))
  "Serializes a rsyslog-block."
  (let ((args (rsyslog-block-input-args block))
        (sub-blocks (rsyslog-block-sub-blocks block)))
    (let loop ((i indent-level))
      (unless (zero? i)
        (format #t "\t")
        (loop (- i 1))))
    (format #t "~a(" (rsyslog-block-name block))
    (unless (null? args)
      (format #t "~a" (serialize-arg-alist args)))
    (format #t ")")
    (unless (null? sub-blocks)
      (format #t " {~%")
      (serialize-list-of-rsyslog-block _ sub-blocks (+ indent-level 1))
      (format #t "}"))
    (format #t "~%")))

(define* (serialize-list-of-rsyslog-block _ blocks #:optional (indent-level 0))
  (for-each
   (lambda (b) (serialize-rsyslog-block #f b indent-level))
   blocks))

(define-public (make-rsyslog-module module-name)
  (rsyslog-block (name "module") (input-args `((load . ,module-name)))))

(define module-list? (list-of (block-with-name "module")))
(define serialize-module-list serialize-list-of-rsyslog-block)

(define-public (make-rsyslog-input input-args)
  (rsyslog-block (name "input") (input-args input-args)))

(define input-list? (list-of (block-with-name "input")))
(define serialize-input-list serialize-list-of-rsyslog-block)

(define* (make-rsyslog-ruleset ruleset-name #:key (args '()) #:rest sub-blocks)
  (rsyslog-block (name "ruleset")
                  (input-args (cons `(name . ,ruleset-name) args))
                  (sub-blocks sub-blocks)))

(define ruleset-list? (list-of (block-with-name "ruleset")))
(define serialize-ruleset-list serialize-list-of-rsyslog-block)

(define* (make-rsyslog-action type #:rest args)
  (rsyslog-block (name "action") (input-args (cons `(type . ,type) args))))

(define extra-lines? list?)
(define (serialize-extra-lines _ lines)
  (format #t "~a" (string-join lines "\n")))



;; Defining the configuration of the rsyslogd shepherd service.
(define-configuration rsyslog-configuration
  (package
    (package rsyslog)
    "rsyslog package to use")
  (modules
   (module-list '())
   "The rsyslog modules to load.")
  (inputs
   (input-list '())
   "The rsyslog inputs to use.")
  (rulesets
   (ruleset-list '())
   "The rulesets to define.")
  (extra
   (extra-lines '())
   "Extra free-form lines of configuration."))

(define (rsyslog-configuration-file config)
  "Create a rsyslog configuration file based on CONFIG."
  (mixed-text-file
   "rsyslog.conf"
   (with-output-to-string
     (lambda ()
       (serialize-configuration config rsyslog-configuration-fields)))))

(define (add-rsyslog-configuration config)
  `(("config/rsyslogd/config" ,(rsyslog-configuration-file config))))

(define add-rsyslog-package
  (compose list rsyslog-configuration-package))

(define (rsyslog-shepherd-service config)
  (match config
    (($ <rsyslog-configuration> _ package modules inputs rulesets extra)

     (list (shepherd-service
            (documentation "rsyslog")
            (provision '(rsyslog))
            (start #~(make-forkexec-constructor
                      (list #$(file-append package "/sbin/rsyslogd")
                            "-n" "-iNONE"
                            "-f" #$(rsyslog-configuration-file config))
                      #:log-file "/var/log/rsylogd.log"))
            (stop #~(make-kill-destructor)))))))

(define rsyslog-service-type
  (service-type
   (name 'rsyslog)
   (extensions
    (list (service-extension shepherd-root-service-type rsyslog-shepherd-service)))
   (description "Configures and runs a rsyslog daemon.")))

(define home-rsyslog-service-type
  (service-type
   (name 'home-rsyslog)
   (extensions
    (list (service-extension home-files-service-type add-rsyslog-configuration)
          (service-extension home-profile-service-type add-rsyslog-package)
          (service-extension home-shepherd-service-type
                             rsyslog-shepherd-service)))
   (description "Configures and runs a rsyslog daemon.")))

;; ;; ;; XXX: Test
;; (define (kt-serialize-configuration config fields)
;;   (with-output-to-string
;;     (lambda ()
;;       (for-each
;;        (lambda (field)
;;          ((configuration-field-serializer field)
;;           (configuration-field-name field)
;;           ((configuration-field-getter field) config)))
;;        fields))))

;; (format
;;  #t
;;  "Configuration:~%~a~%"
;;  (kt-serialize-configuration
;;   (rsyslog-configuration
;;    (package rsyslog)
;;    (modules (list (make-rsyslog-module "imtcp") (make-rsyslog-module "omelasticsearch")))
;;    (inputs (list (make-rsyslog-input "imtcp" '((port . "5140")
;;                                        (address . "127.0.0.1")
;;                                        (ruleset . "default")))))
;;    (rulesets (list (make-ruleset
;;                     "default"
;;                     (make-rsyslog-action "omelasticsearch")
;;                     (make-rsyslog-action "omfile" `(file . "./kt.log")))))
;;    (extra '("foo" "bar")))
;;   rsyslog-configuration-fields))
