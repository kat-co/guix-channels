(define-module (upstream services dendrite)
  #:use-module (ice-9 format)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)

  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix least-authority)

  #:use-module (nonguix build-system binary)

  #:autoload   (gnu build linux-container) (%namespaces)
  #:use-module ((gnu packages admin) #:select (shadow))
  #:use-module (gnu packages gcc)

  #:use-module (gnu services)
  #:use-module (gnu services admin)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module ((gnu system file-systems) #:select (file-system-mapping))
  #:use-module (gnu system shadow)

  #:export (dendrite-configuration
            dendrite-global-configuration
            dendrite-database-configuration
            dendrite-cache-configuration
            dendrite-presence-configuration
            dendrite-report-stats-configuration
            dendrite-server-notices-configuration
            dendrite-jetstream-configuration
            dendrite-metrics-auth
            dendrite-metrics-configuration
            dendrite-dns-cache-configuration
            dendrite-app-service-api-configuration
            dendrite-client-api-configuration
            dendrite-keyserver-key
            dendrite-keyserver
            dendrite-federation-api-configuration
            dendrite-thumbnail-size
            dendrite-media-api-configuration
            dendrite-search-api-configuration
            dendrite-sync-api-configuration
            dendrite-user-api-configuration))

;;;; YAML Configuration Serialization

;; TODO: better docs on yaml-indentation-level

;; Used to determine the indentation level of the beginning of a block.
;; Anonymous blocks can be embedded in different context and should only ensure
;; the second key is aligned with the first.
(define %yaml-indentation-level (make-parameter 0))

(define (case-chain->snake x)
  (string-replace-substring (format #f "~a" x) "-" "_"))

(define (serialize-yaml-string value)
  (format #f "\"~a\"" value))

(define (serialize-yaml-boolean value)
  (format #f "~a" (if value "true" "false")))

(define (serialize-yaml-kv serialize-value name value)
  (format #f "~a: ~a"
          ;;(%yaml-indentation-level)
          (case-chain->snake name)
          (serialize-value value)))

(define (serialize-yaml-list serialize-value name value)
  #~(begin
      (use-modules (ice-9 format))
      (string-append
       #$(format #f "~a:" name)
       #$@(map (lambda (v)
                 #~(format #f "~%~v_- ~a"
                           #$(+ 2 (%yaml-indentation-level))
                           #$(serialize-value v)))
               value)
       (format #f "~%"))))

(define* (serialize-yaml-block-anonymous fields _ value
                                         #:key (first-level (%yaml-indentation-level))
                                         (rest-level 0))
  "Serializes a YAML block that is not nested under any key. REST-LEVEL is the
level of indentation key-values after the first key-value should be indented
at. This is useful when using anonymous blocks in lists."
  (define (field-value field)
    ((configuration-field-serializer field)
     (configuration-field-name field)
     ((configuration-field-getter field) value)))

  (let ((first-field-value (parameterize ((%yaml-indentation-level first-level))
                             (format #f "~v_~a"
                                     first-level
                                     (field-value (car fields))))))
    (parameterize ((%yaml-indentation-level (+ rest-level (%yaml-indentation-level))))
      (let ((field-values (map field-value (cdr fields))))
        #~(let* ((values (list
                          #$first-field-value
                          #$@field-values))
                 (set-values
                  (filter (lambda (s) (< 0 (string-length s))) values)))
            (string-join set-values (format #f "~%~v_" #$(%yaml-indentation-level))))))))

(define (serialize-yaml-block fields name value)
  (let ((name-yaml (case-chain->snake name))
        (level (%yaml-indentation-level)))
    #~(begin
        (use-modules (ice-9 format))
        (format #f "~%~v_~a:~%~a" #$level #$name-yaml
                #$(parameterize ((%yaml-indentation-level (+ 2 level)))
                    (serialize-yaml-block-anonymous fields #f value))))))

;;;; Dendrite

(define %dendrite-private-key-path "/var/lib/dendrite/keys/matrix_key.pem")
(define %dendrite-jetstream-storage-path "/var/run/dendrite/jetstream/")
(define %dendrite-media-storage-path "/var/run/dendrite/media/")
(define %dendrite-search-index-path "/var/run/dendrite/search-index/")
(define %dendrite-log-path "/var/log/dendrite.log")

(define %dendrite-user
  (user-account
   (name "dendrite")
   (group "dendrite")
   (system? #t)
   (comment "Dendrite server user")
   (home-directory "/var/empty")
   (shell (file-append shadow "/sbin/nologin"))))

(define %dendrite-group
  (user-group
   (name "dendrite")
   (system? #t)))

(define %dendrite-log-rotations
  (list (log-rotation
         (files (list %dendrite-log-path))
         (frequency 'weekly))))

(define-public %dendrite
  (package
    (name "dendrite")
    (version "?")
    (source ".")
    (build-system binary-build-system)
    (arguments
     `(#:install-plan '(("bin/" "bin" #:include ("dendrite" "create-account"
                                               "generate-keys")))
       #:patchelf-plan
       '(("bin/dendrite" ("libc" "gcc"))
         ("bin/create-account" ("libc" "gcc"))
         ("bin/generate-keys" ("libc" "gcc")))))
    (inputs `((,gcc "lib")))
    (home-page "https://github.com/matrix-org/dendrite")
    (synopsis "Dendrite Matrix server")
    (description "Dendrite is an officially supported Matrix server written in Go. Since Dendrite
has not yet been packaged for Guix, and it appears to be non-trivial, this is a
temporary package which utilizes a binary pre-compiled outside of Guix.
Eventually, it will be replaced with a properly packaged Dendrite. To use it,
inherit from the package and override the source field with a gexp that points
to the pre-compiled binary.")
    (license license:asl2.0)))

;;; Configuration

(define-maybe string (prefix dendrite-))
(define-maybe list-of-strings (prefix dendrite-))

(define dendrite-serialize-string
  (cut serialize-yaml-kv serialize-yaml-string <> <>))

(define dendrite-serialize-number
  (cut serialize-yaml-kv identity <> <>))

(define dendrite-serialize-boolean
  (cut serialize-yaml-kv serialize-yaml-boolean <> <>))

(define dendrite-serialize-list-of-strings
  (cut serialize-yaml-list serialize-yaml-string <> <>))

;; TODO: go back through and look for places to use maybe- functions

;; Dendrite database configuration

(define-configuration dendrite-database-configuration
  (connection-string
   (string "postgresql:///dendrite?host=/var/run/postgresql/&user=dendrite")
   "The connection string to connect to a PostgreSQL instance.")

  (max-open-conns
   (number 90)
   "The maximum number of connections allowed to PostgreSQL.")

  (max-idle-conns
   (number 6)
   "The max number of idle connections allowed to PostgreSQL.")

  (conn-max-lifetime
   (number -1)
   "The maximum lifetime of a connection (-1 means infinite).")
  (prefix dendrite-))

(define dendrite-serialize-dendrite-database-configuration
  (cut serialize-yaml-block dendrite-database-configuration-fields <> <>))

;; Dendrite cache configuration

(define-configuration dendrite-cache-configuration
  (max-size-estimated
   (string "1gb")
   "The estimated maximum size for the global cache in bytes, or in
terabytes,gigabytes, megabytes or kilobytes when the appropriate 'tb', 'gb',
'mb' or 'kb' suffix is specified. Note that this is not a hard limit, nor is it
a memory limit for the entire process. A cache that is too small may ultimately
provide little or no benefit.")

  (max-age
   (string "1h")
   "The maximum amount of time that a cache entry can live for in memory before it
will be evicted and/or refreshed from the database. Lower values result in
easier admission of new cache entries but may also increase database load in
comparison to higher values, so adjust conservatively. Higher values may make it
harder for new items to make it into the cache, e.g. if new rooms suddenly
become popular.")
  (prefix dendrite-))

(define dendrite-serialize-dendrite-cache-configuration
  (cut serialize-yaml-block dendrite-cache-configuration-fields <> <>))

;; Dendrite presence configuration

(define-configuration dendrite-presence-configuration
  (enable-inbound
   (boolean #f)
   "Whether Dendrite will receive presence events from other servers.")

  (enable-outbound
   (boolean #f)
   "Whether Dendrite will send presence events to other servers.")
  (prefix dendrite-))

(define dendrite-serialize-dendrite-presence-configuration
  (cut serialize-yaml-block dendrite-presence-configuration-fields <> <>))

;; Dendrite phone-home statistics reporting configuration

(define-configuration dendrite-report-stats-configuration
  (enabled
   (boolean #f)
   "Whether to report to enable this feature")

  (endpoint
   (string "https://panopticon.matrix.org/push")
   "The endpoint to push server statistics to.")
  (prefix dendrite-))

(define dendrite-serialize-dendrite-report-stats-configuration
  (cut serialize-yaml-block dendrite-report-stats-configuration-fields <> <>))

;; Dendrite server notices configuration

(define-configuration dendrite-server-notices-configuration
  (enabled
   (boolean #f)
   "Allows the server administrators to send messages to all users on the server.")

  (local-part
   (string "_server")
   "Part of the mxc:// URL for the user that will send the server notices.")

  (display-name
   (string "Server Alerts")
   "The name that will displayed when a message is sent.")

  (avatar-url
   (string "")
   "The URL for the avatar that will be displayed when a message is sent.")

  (room-name
   (string "Server Alerts")
   "The room name to be used when sending server notices. This room name will
appear in user clients.")
  (prefix dendrite-))

(define dendrite-serialize-dendrite-server-notices-configuration
  (cut serialize-yaml-block dendrite-server-notices-configuration-fields <> <>))

;; Dendrite server Jetstream configuration

(define-configuration dendrite-jetstream-configuration
  (addresses
   maybe-list-of-strings
   "A list of NATS server addresses to connect to. If none are specified, an
internal NATS server will be started automatically.")
  (disable-tls-validation
   (boolean #f)
   "Disable the validation of TLS certificates of NATS. This is not recommended in
production since it may allow NATS traffic to be sent to an insecure endpoint.")

  (storage-path
   (string %dendrite-jetstream-storage-path)
   "Persistent directory to store Jetstream streams in. This directory should be
preserved across Dendrite restarts.")

  (topic-prefix
   (string "Dendrite")
   "The prefix to use for stream names for this homeserver. This is really only
useful if you're running more than one Dendrite server ont he same NATS
deployment.")
  (prefix dendrite-))

(define dendrite-serialize-dendrite-jetstream-configuration
  (cut serialize-yaml-block dendrite-jetstream-configuration-fields <> <>))

;; Dendrite metrics configuration

(define-configuration dendrite-metrics-auth
  (username
   (string "metrics")
   "The username for the Prometheus scraper.")

  (password
   (string "metrics")
   "The password for the Prometheus scraper.")
  (prefix dendrite-))

(define dendrite-serialize-dendrite-metrics-auth
  (cut serialize-yaml-block dendrite-metrics-auth-fields <> <>))

(define-configuration dendrite-metrics-configuration
  (enabled
   (boolean #f)
   "Whether Prometheus metrics are enabled.")

  (basic-auth
   (dendrite-metrics-auth (dendrite-metrics-auth))
   "The authorization for the Prometheus scraper.")
  (prefix dendrite-))

(define dendrite-serialize-dendrite-metrics-configuration
  (cut serialize-yaml-block dendrite-metrics-configuration-fields <> <>))

;; Dendrite DNS cache configuration

(define-configuration dendrite-dns-cache-configuration
  (enabled
   (boolean #f)
   "An optional DNS cache for when no local caching resolver is available.")

  (cache-size
   (number 256)
   "The size of the cache.")

  (cache-lifetime
   (string "5m")
   "The lifetime of the cache. This accepts values that work with Go's
time.ParseDuration function (https://pkg.go.dev/time@master#ParseDuration).")
  (prefix dendrite-))

(define dendrite-serialize-dendrite-dns-cache-configuration
  (cut serialize-yaml-block dendrite-dns-cache-configuration-fields <> <>))

;; Dendrite Appservice API configuration

(define-configuration dendrite-app-service-api-configuration
  (disable-tls-validation
   (boolean #f)
   "Disable the validation of TLS certificates of appservices. This is not
recommended in production since it may allow appservice traffic to be sent to an
insecure endpoint.")

  (config-files
   ;; TODO: list of file-like-or-string
   (list-of-strings '())
   "Appservice configuration files to load into this homeserver.")
  (prefix dendrite-))

(define dendrite-serialize-dendrite-app-service-api-configuration
  (cut serialize-yaml-block dendrite-app-service-api-configuration-fields <> <>))

;; Dendrite Client API configuration

(define-configuration dendrite-client-api-configuration
  (registration-disabled
   (boolean #t)
   "Prevents new users from being able to register on this homeserver, except when
using the registration shared secret.")

  (guests-disabled
   (boolean #t)
   "Prevents new guest accounts from being created. Guest registration is also
disabled implicitly by setting 'registration_disabled' above.")

  (registration-shared-secret
   maybe-string
   "If set, allows registration by anyone who knows the shared secret, regardless
of whether registration is otherwise disabled.")

  (enable-registration-captcha
   (boolean #f)
   "Whether to require reCAPTCHA for registration. If you have enabled registration
then this is HIGHLY RECOMMENDED to reduce the risk of your homeserver being used
for coordinated spam attacks.")

  (recaptcha-public-key
   maybe-string
   "Public key to use with ReCAPTCHA.")

  (recaptcha-private-key
   maybe-string
   "Private key to use with ReCAPTCHA.")

  (recaptcha-bypass-secret
   maybe-string
   "A secret that can be used to bypass ReCAPTCHA.")

  (recaptcha-siteverify-api
   maybe-string
   "The API endpoint for the recaptcha service (e.g.
https://hcaptcha.com/siteverify. Dendrite uses ReCAPTCHA by default.")

  (recaptcha-api-js-url
   maybe-string
   "The JavaScript API endpoint for the recaptcha service (e.g.
https://js.hcaptcha.com/1/api.js). Dendrite uses the ReCAPTCHA value by
default.")

  (recaptcha-form-field
   maybe-string
   "The name of the form field the response will be populated into (e.g.
h-captcha-response). Dendrite uses the ReCAPTCHA value by default.")

  (recaptcha-sitekey-class
   maybe-string
   "The ReCAPTCHA sitekey class to use (e.g. h-captcha). Dendrite uses the
ReCAPTCHA value by default.")
  (prefix dendrite-))

(define dendrite-serialize-dendrite-client-api-configuration
  (cut serialize-yaml-block dendrite-client-api-configuration-fields <> <>))

;; Dendrite Turn server client configuration

;; TODO

;; Dendrite Federation API configuration

(define-configuration dendrite-keyserver-key
  (key-id string "The ID of the key.")

  (public-key string "The contents of the key.")
  (prefix dendrite-))

(define dendrite-serialize-dendrite-keyserver-key
  (cut serialize-yaml-block-anonymous dendrite-keyserver-key-fields <> <>
       #:first-level 0 #:rest-level 4))

(define list-of-dendrite-keyserver-key? (list-of dendrite-keyserver-key?))

(define dendrite-serialize-list-of-dendrite-keyserver-key
  (cut serialize-yaml-list
       (cut dendrite-serialize-dendrite-keyserver-key #f <>)
       <> <>))

(define-configuration dendrite-keyserver
  (server-name string "The URL of the key server (e.g. matrix.org).")

  (keys
   list-of-dendrite-keyserver-key
   "The keys published for the server.")
  (prefix dendrite-))

(define dendrite-serialize-dendrite-keyserver
  (cut serialize-yaml-block-anonymous dendrite-keyserver-fields <> <>
       #:first-level 0 #:rest-level 4))

(define list-of-dendrite-keyserver? (list-of dendrite-keyserver?))

(define dendrite-serialize-list-of-dendrite-keyserver
  (cut serialize-yaml-list
       (cut dendrite-serialize-dendrite-keyserver #f <>)
       <> <>))

(define-configuration dendrite-federation-api-configuration
  (send-max-retries
   (number 16)
   "How many times we will try to resend a failed transaction to a specific server.
The backoff is 2**x seconds, so 1 = 2 seconds, 2 = 4 seconds, 3 = 8 seconds etc.
Once the max retries are exceeded, Dendrite will no longer try to send
transactions to that server until it comes back to life and connects to us
again.")

  (disable-tls-validation
   (boolean #f)
   "Disable the validation of TLS certificates of remote federated homeservers. Do
not enable this option in production as it presents a security risk!")

  (disable-http-keepalives
   (boolean #f)
   "Disable HTTP keepalives, which also prevents connection reuse. Dendrite will
typically keep HTTP connections open to remote hosts for 5 minutes as they can
be reused much more quickly than opening new connections each time. Disabling
keepalives will close HTTP connections immediately after a successful request
but may result in more CPU and memory being used on TLS handshakes for each new
connection instead.")

  (key-perspectives
   (list-of-dendrite-keyserver
    (list (dendrite-keyserver
           (server-name "matrix.org")
           (keys
            (list (dendrite-keyserver-key
                   (key-id "ed25519:auto")
                   (public-key "Noi6WqcDj0QmPxCNQqgezwTlBKrfqehY1u2FyWP9uYw"))
                  (dendrite-keyserver-key
                   (key-id "ed25519:a_RXGa")
                   (public-key "l8Hft5qXKn1vfHrg3p4+W8gELQVo8N13JkluMfmn2sQ")))))))
   "Perspective keyservers to use as a backup when direct key fetches fail. This
  may be required to satisfy key requests for servers that are no longer online
  when joining some rooms.")

  (prefer-direct-fetch
   (boolean #f)
   "This option will control whether Dendrite will prefer to look up keys directly
or whether it should try perspective servers first, using direct fetches as a
last resort.")
  (prefix dendrite-))

(define dendrite-serialize-dendrite-federation-api-configuration
  (cut serialize-yaml-block dendrite-federation-api-configuration-fields <> <>))

;; ;; Dendrite Media API configuration
;; 
(define (dendrite-thumbnail-method? x)
  (member x '(crop scale)))

(define-configuration dendrite-thumbnail-size
  (width number "The width the thumbnail should be.")

  (height number "The height the thumbnail should be.")

  (method dendrite-thumbnail-method "The method used to create the thumbnail."
          dendrite-serialize-string)
  (prefix dendrite-))

(define dendrite-serialize-dendrite-thumbnail-size
  (cut serialize-yaml-block-anonymous dendrite-thumbnail-size-fields <> <>
       #:first-level 0 #:rest-level 4))

(define list-of-dendrite-thumbnail-size?
  (list-of dendrite-thumbnail-size?))

(define dendrite-serialize-list-of-dendrite-thumbnail-size
  (cut serialize-yaml-list
       (cut dendrite-serialize-dendrite-thumbnail-size #f <>)
       <> <>))

(define-configuration dendrite-media-api-configuration
  (base-path
   (string %dendrite-media-storage-path)
   "Storage path for uploaded media. May be relative or absolute.")

  (max-file-size-bytes
   (number 10485760)
   "The maximum allowed file size (in bytes) for media uploads to this
homeserver (0 = unlimited). If using a reverse proxy, ensure it allows requests
at least this large (e.g. the client_max_body_size setting in nginx).")

  (dynamic-thumbnails
   (boolean #f)
   "Whether to dynamically generate thumbnails if needed.")

  (max-thumbnail-generators
   (number 10)
   "The maximum number of simultaneous thumbnail generators to run.")

  (thumbnail-sizes
   (list-of-dendrite-thumbnail-size
    (list (dendrite-thumbnail-size
           (width 32)
           (height 32)
           (method 'crop))
          (dendrite-thumbnail-size
           (width 96)
           (height 96)
           (method 'crop))
          (dendrite-thumbnail-size
           (width 640)
           (height 480)
           (method 'scale))))
   "A list of thumbnail sizes to be generated for media content.")
  (prefix dendrite-))

(define dendrite-serialize-dendrite-media-api-configuration
  (cut serialize-yaml-block dendrite-media-api-configuration-fields <> <>))

;; Dendrite MSC configuration
(define-configuration dendrite-msc-configuration
  ;; XXX: Why does a list-of-strings not resolve unless something comes before it?
  (_ maybe-string "Test")
  (mscs
   (list-of-strings '())
   "A list of experimental MSCs to enable on this server.")
  (prefix dendrite-))

(define dendrite-serialize-dendrite-msc-configuration
  (cut serialize-yaml-block dendrite-msc-configuration-fields <> <>))

;; Dendrite Sync API configuration

(define-configuration dendrite-search-api-configuration
  (enabled
   (boolean #f)
   "Whether or not search is enabled.")

  (index-path
   (string %dendrite-search-index-path)
   "The directory where the search index will be created.")

  (language
   (string "en")
   "The language most likely to be used on the server. This is used when indexing
to ensure the returned results match expectations. A full list of possible
languages can be found at
https://github.com/blevesearch/bleve/tree/master/analysis/lang")
  (prefix dendrite-))

(define dendrite-serialize-dendrite-search-api-configuration
  (cut serialize-yaml-block dendrite-search-api-configuration-fields <> <>))

(define-configuration dendrite-sync-api-configuration
  (real-ip-header
   maybe-string
   "This option controls which HTTP header to inspect to find the real remote IP
address of the client. This is likely required if Dendrite is running behind a
reverse proxy server.")

  (search
   (dendrite-search-api-configuration (dendrite-search-api-configuration))
   "Configuration for the full-text search engine.")
  (prefix dendrite-))

(define dendrite-serialize-dendrite-sync-api-configuration
  (cut serialize-yaml-block dendrite-sync-api-configuration-fields <> <>))

;; Dendrite User API configuration

(define-configuration dendrite-user-api-configuration
  (bcrypt-cost
   (number 10)
   "The cost when hashing passwords on registration/login. Default: 10. Min: 4,
Max: 31 See https://pkg.go.dev/golang.org/x/crypto/bcrypt for more information.
Setting this lower makes registration/login consume less CPU resources at the
cost of security should the database be compromised. Setting this higher makes
registration/login consume more CPU resources but makes it harder to brute force
password hashes. This value can be lowered if performing tests or on embedded
Dendrite instances (e.g WASM builds).")

  (openid-token-lifetime-ms
   maybe-string
   "The length of time that a token issued for a relying party from
/_matrix/client/r0/user/{userId}/openid/request_token endpoint is considered to
be valid in milliseconds. The default lifetime is 3600000ms (60 minutes).")

  (auto-join-rooms
   (list-of-strings '())
   "Users who register on this homeserver will automatically be joined to the rooms
listed under `auto_join_rooms` option. By default, any room aliases included in
this list will be created as a publicly joinable room when the first user
registers for the homeserver. If the room already exists,make certain it is a
publicly joinable room, i.e. the join rule of the room must be set to 'public'.
As Spaces are just rooms under the hood, Space aliases may also be used.")
  (prefix dendrite-))

(define dendrite-serialize-dendrite-user-api-configuration
  (cut serialize-yaml-block dendrite-user-api-configuration-fields <> <>))

;; Opentracing configuration

;; TODO

;; Logging configuration

(define (dendrite-log-sink-type? x)
  (member x '(std file)))

(define (dendrite-log-sink-level? x)
  (member x '(debug info warn error)))

(define-configuration dendrite-log-sink
  (type
   dendrite-log-sink-type
   "The type of log sink. @code{std} controls the logs being sent to stdout.
@code{file} controls logs being written to a log folder on the disk."
   dendrite-serialize-string)

  (level
   dendrite-log-sink-level
   "The level the log sink should receive. The supported log levels are
@code{debug}, @code{info}, @code{warn}, @code{error}."
   dendrite-serialize-string)
  (prefix dendrite-))

(define dendrite-serialize-dendrite-log-sink
  (cut serialize-yaml-block-anonymous dendrite-log-sink-fields <> <>
       #:first-level 0 #:rest-level 4))

(define list-of-dendrite-log-sink?
  (list-of dendrite-log-sink?))

(define dendrite-serialize-list-of-dendrite-log-sink
  (cut serialize-yaml-list
       (cut dendrite-serialize-dendrite-log-sink #f <>)
       <> <>))

;;; Global configuration

(define-configuration dendrite-global-configuration
  (server-name
   (string "localhost")
   "The domain name of this homeserver.")

  (well-known-server-name
   maybe-string
   "The server name to delegate server-server communications to, with optional
port, e.g. localhost:443.")

  (well-known-client-name
   maybe-string
   "The server name to delegate client-server communications to, with optional
port, e.g. localhost:443.")

  (private-key
   (string %dendrite-private-key-path)
   "The path to the signing private key file, used to sign requests and events.
Note that this is NOT the same private key as used for TLS! To generate a
signing key, use @code{./bin/generate-keys --private-key matrix_key.pem}.")

  (key-validity-period
   (string "168h0m0s")
   "How long a remote server can cache our server signing key before requesting it
again. Increasing this number will reduce the number of requests made by other
servers for our key but increases the period that a compromised key will be
considered valid by other homeservers.")

  (trusted-third-party-id-servers
   (list-of-strings '("matrix.org" "vector.im"))
   "Lists of domains that the server will trust as identity servers to verify third
party identifiers such as phone numbers and email addresses.")

  (disable-federation
   (boolean #f)
   "Whether federation with other Matrix servers is enabled. If false, Dendrite
will not be able to communicate with other servers in the Matrix federation and
the federation API will not be exposed.")

  (database
   (dendrite-database-configuration (dendrite-database-configuration))
   "Global database connection pool, for PostgreSQL monolith deployments only. If
this section is populated then you can omit the @code{database} blocks in all
other sections. For monolith deployments using SQLite databases, you must
configure the @code{atabase} block for each component instead.")

  (cache
   (dendrite-cache-configuration (dendrite-cache-configuration))
   "Configuration for in-memory caches. Caches can often improve performance by
keeping frequently accessed items (like events, identifiers etc.) in memory
rather than having to read them from the database.")

  (presence
   (dendrite-presence-configuration
    (dendrite-presence-configuration))
   "Configures the handling of presence events. Inbound controls whether we receive
presence events from other servers, outbound controls whether we send presence
events for our local users to other servers.")

  (report-stats
   (dendrite-report-stats-configuration (dendrite-report-stats-configuration))
   "Configures phone-home statistics reporting. These statistics contain the server
name, number of active users and some information on your deployment config. We
use this information to understand how Dendrite is being used in the wild.")

  (server-notices
   (dendrite-server-notices-configuration (dendrite-server-notices-configuration))
   "Server notices allows server admins to send messages to all users on the
server.")

  (jetstream
   (dendrite-jetstream-configuration (dendrite-jetstream-configuration))
   "Configuration for the NATS JetStream.")

  (metrics
   (dendrite-metrics-configuration (dendrite-metrics-configuration))
   "Configuration for Prometheus metric collection.")

  (dns-cache
   (dendrite-dns-cache-configuration (dendrite-dns-cache-configuration))
   "Optional DNS cache. The DNS cache may reduce the load on DNS servers if there
is no local caching resolver available for use.")
  (prefix dendrite-))

(define dendrite-serialize-dendrite-global-configuration
  (cut serialize-yaml-block dendrite-global-configuration-fields <> <>))

;;; General configuration

(define-configuration dendrite-configuration
  ;; Configuration not persisted to YAML
  (dendrite
   (package %dendrite)
   "Dendrite package to use."
   empty-serializer)

  (http-bind-address
   (string "localhost:8008")
   "The address to listen for HTTP request on (:8008 by default)."
   empty-serializer)

  (https-bind-address
   maybe-string
   "The address to listen for HTTPS request on (off by default)."
   empty-serializer)

  (resource-limits
   (list
    '((nofile 65535 65535)))
   "The resource limits to apply to the server. The manual suggests setting the
nofile limit to 65535."
   empty-serializer)

  ;; Configuration persisted to the YAML file for Dendrite to consume.

  (version
   (number 2)
   "The version of the Dendrite configuration schema.")

  (global
   (dendrite-global-configuration (dendrite-global-configuration))
   "Global Matrix configuration. This configuration applies to all components.")

  (app-service-api
   (dendrite-app-service-api-configuration
    (dendrite-app-service-api-configuration))
   "Configuration for the Appservice API.")

  (client-api
   (dendrite-client-api-configuration (dendrite-client-api-configuration))
   "Configuration for the Client API.")

  (federation-api
   (dendrite-federation-api-configuration (dendrite-federation-api-configuration))
   "Configuration for the Federation API.")

  (media-api
   (dendrite-media-api-configuration (dendrite-media-api-configuration))
   "Configuration for the Media API.")

  (mscs
   (dendrite-msc-configuration (dendrite-msc-configuration))
   "Configuration for enabling experimental MSCs on this homeserver.")

  (sync-api
   (dendrite-sync-api-configuration (dendrite-sync-api-configuration))
   "Configuration for the Sync API.")

  (user-api
   (dendrite-user-api-configuration (dendrite-user-api-configuration))
   "Configuration for the User API.")

  (logging
   (list-of-dendrite-log-sink
    (list
     (dendrite-log-sink
      (type 'std)
      (level 'info))))
   "Logging configuration.")

  (prefix dendrite-))

(define dendrite-serialize-dendrite-configuration
  (cut serialize-yaml-block-anonymous dendrite-configuration-fields <> <>))

(define (dendrite-paths-on-disk config)
  "Returns all the paths Dendrite relies on to operate. The first return value is
a list of paths Dendrite owns, and the second is a list of paths it doesn't."
  (match-record
   config <dendrite-configuration>
   (global media-api sync-api)
   (match-record
    global <dendrite-global-configuration>
    (private-key jetstream)
    (let ((search (dendrite-sync-api-configuration-search sync-api)))
      (values
       (list (dirname private-key)
             (dendrite-jetstream-configuration-storage-path
              jetstream)
             (dendrite-media-api-configuration-base-path
              media-api)
             (dendrite-search-api-configuration-index-path
              search))
       (list (dirname %dendrite-log-path)
             ;; For binding to the unix socket
             ;;"/var/run/postgresql/"
             ))))))

(define (dendrite-file-system-mappings config)
  "Returns a list of file-system-mapping of paths Dendrite needs access to when
running for use in containerizing the service."
  (receive (owned used)
      (dendrite-paths-on-disk config)
    (map
     (lambda (path)
       (file-system-mapping
        (source path)
        (target source)
        (writable? #t)))
     (append owned used))))

(define (dendrite-generate-private-key config)
  (match-record config <dendrite-configuration>
    (dendrite global)
    (let ((private-key (dendrite-global-configuration-private-key global)))
      #~(if (stat #$private-key #f)
            (format #t "dendrite: Private key already present.~%")
            (let* ((user (getpwnam #$(user-account-name %dendrite-user)))
                   (uid (passwd:uid user))
                   (gid (passwd:gid user)))
              (format #t "Generating key at ~a~%" #$private-key)
              (system* #$(file-append dendrite "/bin/generate-keys")
                       "--private-key" #$private-key)
              (chown #$private-key uid gid)
              (chmod #$private-key #o400))))))

(define (dendrite-activation config)
  "Runs when the dendrite shepherd service is activated."
  #~(begin
      (use-modules (guix build utils))
      (let ((user (getpwnam #$(user-account-name %dendrite-user))))
        (for-each
         (lambda (f)
           (format #t "Ensuring directory exists: ~a~%" f)
           (mkdir-p f)
           (chown f (passwd:uid user) (passwd:gid user)))
         (list #$@(dendrite-paths-on-disk config))))
      #$(dendrite-generate-private-key config)
      (format #t "dendrite: Activated.~%")))

(define (dendrite-configuration-file config)
  (mixed-text-file
   "dendrite.yml"
   (serialize-configuration config dendrite-configuration-fields)))

(define (dendrite-actions config config-path)
  "Returns a list of shepherd-action for the Dendrite service."
  (match-record config <dendrite-configuration>
    (dendrite global)
    (let ((private-key (dendrite-global-configuration-private-key global)))
      (list
       (shepherd-action
        (name 'print-paths)
        (documentation "Prints the path to the Dendrite package in use.")
        (procedure #~(lambda _
                       (format #t "Dendrite: ~a~%" #$dendrite)
                       (format #t "Config: ~a~%" #$config-path)
                       (format #t "Log: ~a~%" #$%dendrite-log-path))))

       (shepherd-action
        (name 'generate-private-key)
        (documentation
         "Generate a private key in the location the configuration specifies, and ensures
the owner and permissions are correct.")
        (procedure
         #~(lambda _
             #$(dendrite-generate-private-key config))))

       (shepherd-action
        (name 'create-account)
        (documentation
         "Creates a new Matrix account on this server. Arguments are <username> and an
optional boolean which should be set to true if the user should be an admin.")
        (procedure
         #~(lambda* (_ username #:optional admin)
             (apply system*
                    #$(file-append dendrite "/bin/create-account")
                    `("-config" #$config-path
                      "-username" ,username
                      ,@(when admin '("-admin")))))))))))

(define (dendrite-shepherd-service config)
  (match-record config <dendrite-configuration>
    (dendrite http-bind-address https-bind-address resource-limits)
    (let* ((config-path (dendrite-configuration-file config))
           (dendrite* (least-authority-wrapper
                       (file-append dendrite "/bin/dendrite")
                       #:name "dendrite"
                       #:mappings (cons (file-system-mapping
                                         (source config-path)
                                         (target source))
                                        (dendrite-file-system-mappings config))
                       #:namespaces (fold delq %namespaces '(net)))))
      (list (shepherd-service
             (documentation "Runs a dendrite Matrix server.")
             (provision '(dendrite))
             ;; Matrix relies heavily on TLS which requires the system time to be
             ;; correct. If the clock drifts then you may find that federation no
             ;; works reliably (or at all) and clients may struggle to connect to
             ;; your Dendrite server. Therefore, we require ntpd to be running.
             (requirement '(networking syslogd ntpd))
             (start #~(make-forkexec-constructor
                       (list #$dendrite*
                             "-config" #$config-path
                             #$@(if (maybe-value-set? http-bind-address)
                                    (list "-http-bind-address" http-bind-address)
                                    '())
                             #$@(if (maybe-value-set? https-bind-address)
                                    (list "-https-bind-address" https-bind-address)
                                    '()))
                       #:resource-limits '#$resource-limits
                       #:user #$(user-account-name %dendrite-user)
                       #:group #$(user-group-name %dendrite-group)
                       #:log-file #$%dendrite-log-path))
             (stop #~(make-kill-destructor))
             (actions (dendrite-actions config config-path)))))))

(define-public dendrite-service-type
  (service-type
   (name 'dendrite)
   (description "Run a dendrite Matrix server.")
   (extensions
    (list (service-extension shepherd-root-service-type dendrite-shepherd-service)
          (service-extension activation-service-type dendrite-activation)
          (service-extension account-service-type (const (list
                                                          %dendrite-group
                                                          %dendrite-user)))
          (service-extension rottlog-service-type (const %dendrite-log-rotations))))
   (default-value (dendrite-configuration))))
