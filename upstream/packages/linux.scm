;;; Copyright since 2021 Katherine Cox-Buday <cox.katherine.e@gmail.com>
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

(define-module (upstream packages linux)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system linux-module)
  #:use-module (gnu packages)
  #:use-module (nongnu packages linux))

(define-public ath10k-openwrt-linux-module
  (let ((commit "e9603f4bdcc04417f1c7b3585e63654819dc11f6"))
    (package
      (name "ath10k-openwrt-linux-module")
      (version (git-version "0" "0" commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://git.kernel.org/pub/scm/linux/kernel/git/kvalo/ath.git")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1jq87cq1afx3v4mha339f3v3fjdb6la8fhvm1d49fv6r371bhbfj"))
         (patches
          (parameterize
              ((%patch-path
                (map (lambda (directory)
                       (string-append directory "/upstream/packages/patches"))
                     %load-path)))
            (search-patches "402-ath_regd_optional.patch")))))
      (build-system linux-module-build-system)
      (arguments
       `(#:tests? #f                    ; No tests are present
         #:linux ,linux
         #:make-flags (list "CONFIG_ATH_USER_REGD=y")
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'enter-module-directory
             (lambda _
               (chdir "drivers/net/wireless/ath/ath10k"))))))
      (home-page
       "https://wireless.wiki.kernel.org/en/users/drivers/ath10k")
      (synopsis
       "Kernel module for the Qualcom Atheros QCA988x family of chips
with OpenWRT patches applied")
      (description
       "ath10k is the mac80211 wireless driver for Qualcom Atheros
QCA988x family of chips which support IEEE 802.11ac. OpenWRT patches
have been applied to allow for more permissable transmission on bands
despite the region EEPROM reports.")
      (license license:gpl2+))))
