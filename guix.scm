;;; Guile PS/Tk.
;;; TODO: Copyright © 2024 Yuval Langer <yuval.langer@gmail.com>
;;;
;;; This file is part of guile-pstk.

(define-module (guix))

(import
 (ice-9 popen)
 (ice-9 textual-ports)

 (gnu packages base)
 (gnu packages bash)
 (gnu packages commencement)
 (gnu packages guile)
 (gnu packages guile-xyz)
 (gnu packages less)
 (gnu packages maths)
 (gnu packages rsync)
 (gnu packages tcl)
 (gnu packages web)

 (guix build-system guile)
 (guix modules)
 (guix build utils)
 (guix gexp)
 (guix git-download)
 (prefix (guix licenses) license:)
 (guix packages)
 (guix utils)
 )

(define vcs-file?
  ;; Return true if the given file is under version control.
  (or (git-predicate (current-source-directory))
      (const #t)))

(define (sh-runner code)
  (with-input-from-port (open-pipe code OPEN_READ)
    (lambda ()
      (get-string-all (current-input-port)))))

(define guile-pstk
  (let ((version "0.0.0")
        (revision "1"))
    (package
      (name "guile-pstk")
      (version (git-version version revision (sh-runner "git rev-parse HEAD")))
      (source
       (local-file "." "guile-pstk-checkout"
                   #:recursive? #t
                   #:select? vcs-file?))
      (build-system guile-build-system)
      (inputs
       (list
        tcl
        tk
        ))
      (propagated-inputs
       (list
        less
        grep
        findutils
        coreutils
        ))
      (native-inputs
       (list
        guile-3.0
        ))
      (arguments
       (list
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'build
              (lambda _
                (let* ((tcl-lib-directory (string-append #$output "/lib/pstk"))
                       (tk-init-file-target (string-append tcl-lib-directory "/tk-init.tcl")))
                  (import (guix build utils))
                  (mkdir-p tcl-lib-directory)
                  (copy-file "tk-init.tcl" tk-init-file-target)
                  (substitute* "pstk.scm"
                    (("\"tk-init.tcl\"") (string-append "\"" tk-init-file-target "\"")))
                  )
                #t)))))
      (home-page "https://codeberg.org/kakafarm/guile-pstk/")
      (synopsis
       "guile-pstk")
      (description
       "guile-pstk.")
      (license license:expat)
      )))

guile-pstk
