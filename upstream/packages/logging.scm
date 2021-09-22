;;; Copyright © 2020 Katherine Cox-Buday <cox.katherine.e@gmail.com>
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

(define-module (upstream packages logging)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages c)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages geo)
  #:use-module (gnu packages tcl)
  #:use-module (upstream packages c)
  #:use-module ((upstream packages networking) #:prefix upstream:))

(define-public rsyslog
  (let ((modules (list "fmhash" "fmhttp" "imbatchreport"
                       "imczmq" "imdiag" "imdocker"
                       "imfile" "imgssapi" "imkafka"
                       "imklog" "imkmsg" "immark"
                       "improg" "impstats" "imptcp" "imtcp"
                       "imtuxedoulog" "imudp" "imuxsock"
                       "mmanon" "mmaudit" "mmcount"
                       "mmdblookup" "mmexternal"
                       "mmfields" "mmjsonparse"
                       "mmkubernetes" "mmnormalize"
                       "mmpstrucdata" "mmrfc5424addhmac"
                       "mmrm1stspace" "mmsequence"
                       "mmsnmptrapd" "mmtaghostname"
                       "mmutf8fix" "omclickhouse"
                       "omczmq" "omelasticsearch"
                       "omfile-hardened" "omgssapi"
                       "omhttpfs" "omhttp" "omkafka"
                       "omlibdbi" "ommail" "ommysql" "ompgsql"
                       "omprog" "omruleset" "omsnmp"
                       "omstdout" "omtcl" "omtesting"
                       "omudpspoof" "omuxsock"
                       "pmaixforwardedfrom" "pmciscoios"
                       "pmcisconames" "pmdb2diag"
                       "pmlastmsg" "pmnormalize"
                       "pmnull" "pmpanngfw" "pmsnare")))
    (package
      (name "rsyslog")
      (version "8.1907.0")
      (source
       (origin
         (method url-fetch)
         (uri (string-append "https://github.com/"
                             "rsyslog/rsyslog/archive/"
                             "v" version ".tar.gz"))
         (sha256
          (base32
           "14piy5s7nm753ls1aijj221x5ip18phpp89wx62s7bizxp8h548f"))))
      (build-system gnu-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           ;; autogen.sh calls configure at the end of the script.
           (replace 'bootstrap
             (lambda _ (invoke "autoreconf" "-vfi")))
           (add-after 'build 'transfer-modules
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (mod (string-append out "/lib/rsyslog"))
                      (move-module
                       (lambda (name)
                         (let* ((new-dir (string-append
                                          (assoc-ref outputs name)
                                          "/lib/rsyslog")))
                           (mkdir-p new-dir)
                           (for-each
                            (lambda (old-file)
                              (let ((new-file
                                     (string-append
                                      new-dir
                                      "/"
                                      (basename old-file))))
                                (write (string-append old-file " -> "
                                                      new-file))
                                (newline)
                                (rename-file old-file new-file)))
                            (find-files mod (string-append "^" name)))))))
                 (for-each move-module (list ,@modules)))
               #t)))
         #:configure-flags
         (list
          "--enable-largefile"
          "--enable-inet"
          "--enable-regexp"
          "--enable-rsyslogrt"
          "--enable-rsyslogd"
          "--enable-unlimited_select"
          "--enable-uuid"
          "--enable-libgcrypt"
          "--enable-mmanon"
          "--enable-mmcount"
          "--enable-liblogging_stdlog"
          "--enable-kafka_static"
          "--enable-atomic_operations"
          ;; Input plugins
          "--enable-kmsg"
          "--enable-imptcp"
          "--enable-imdiag"
          "--enable-imfile"
          "--enable-imdocker"
          "--enable-impstats"
          "--enable-imczmq"
          "--enable-imbatchreport"
          "--enable-imkafka"
          "--enable-imtuxedoulog"
          "--enable-improg"
          ;; Output plugins
          "--enable-mail"
          "--enable-omfile_hardened"
          "--enable-omprog"
          "--enable-omstdout"
          "--enable-elasticsearch"
          "--enable-clickhouse"
          "--enable-omhttp"
          "--enable-omruleset"
          "--enable-omudpspoof"
          "--enable-omuxsock"
          "--enable-omczmq"
          "--enable-omhttpfs"
          "--enable-omtcl"
          "--enable-omkafka"
          ;; Parser Modules
          "--enable-pmlastmsg"
          "--enable-pmcisconames"
          "--enable-pmciscoios"
          "--enable-pmnull"
          "--enable-pmnormalize"
          "--enable-pmaixforwardedfrom"
          "--enable-pmsnare"
          "--enable-pmdb2diag"
          "--enable-pmpanngfw"
          ;; Message Modification Modules
          "--enable-mmnormalize"
          "--enable-mmjsonparse"
          "--enable-mmaudit"
          "--enable-mmsnmptrapd"
          "--enable-mmutf8fix"
          "--enable-mmrfc5424addhmac"
          "--enable-mmpstrucdata"
          "--enable-mmsequence"
          "--enable-mmdblookup"
          "--enable-mmdarwin"
          "--enable-mmfields"
          "--enable-mmrm1stspace"
          "--enable-mmkubernetes"
          "--enable-mmtaghostname"
          ;; Database Support
          "--enable-mysql"
          "--enable-libdbi"
          "--enable-pgsql"
          ;; Protocol Support
          "--enable-openssl"
          "--enable-gnutls"
          "--enable-gssapi_krb5"
          "--enable-snmp"
          ;; Function modules
          "--enable-fmhttp"
          "--enable-fmhash"
          "--enable-fmhash_xxhash")))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("libtool" ,libtool)
         ("pkg-config" ,pkg-config)
         ("bison" ,bison)
         ("flex" ,flex)))
      (inputs
       `(("libestr" ,libestr)
         ("libfastjson" ,libfastjson)
         ("liblogging" ,liblogging)
         ("libz" ,zlib)
         ("libuuid" ,util-linux "lib")
         ("libgcrypt" ,libgcrypt)
         ("libcurl" ,curl)
         ("liblognorm" ,liblognorm "lib")
         ("liblognorm" ,liblognorm "dev")
         ("mariadb" ,mariadb "lib")
         ("mariadb" ,mariadb "dev")
         ("postgresql" ,postgresql)
         ("libdbi" ,libdbi)
         ("net-smnp" ,net-snmp)
         ("openssl" ,openssl)
         ("gnutls" ,gnutls)
         ("libnet" ,libnet)
         ("librdkafka" ,librdkafka)
         ("lz4" ,lz4)
         ("czmq" ,czmq)
         ("zeromq" ,zeromq)
         ("mit-krb5" ,mit-krb5)
         ("cyrus-sasl" ,cyrus-sasl)
         ("libmaxminddb" ,libmaxminddb)
         ("tcl" ,tcl)))
      (outputs `("out" ,@modules))
      (home-page "https://www.rsyslog.com/")
      (synopsis "RSYSLOG is the rocket-fast system for log processing.")
      (description
       "It offers high-performance, great security features and a
modular design. While it started as a regular syslogd, rsyslog has
evolved into a kind of swiss army knife of logging, being able to
accept inputs from a wide variety of sources, transform them, and
output to the results to diverse destinations.

Rsyslog can deliver over one million messages per second to local
destinations when limited processing is applied (based on v7, December
2013). Even with remote destinations and more elaborate processing the
performance is usually considered \"stunning\".")
      (license (list license:gpl3
                     license:asl2.0
                     license:lgpl3)))))
