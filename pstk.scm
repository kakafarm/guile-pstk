;;; PS/Tk -- A Portable Scheme Interface to the Tk GUI Toolkit
;;; Copyright (C) 2021-2022 Daniil Archangelsky aka Kiky Tokamuro
;;; Copyright (C) 2008 Kenneth A Dickey
;;; Copyright (C) 2006-2008 Nils M Holm
;;; Copyright (C) 2004 Wolf-Dieter Busch
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
;;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
;;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.
;;;
;;; PS/Tk is based on Chicken/Tk by Wolf-Dieter Busch (2004):
;;; http://wolf-dieter-busch.de/html/Software/Tools/ChickenTk.htm
;;; which is in turn based on Scheme_wish by Sven Hartrumpf (1997, 1998):
;;; http://pi7.fernuni-hagen.de/hartrumpf/scheme_wish.scm
;;;
;;; These are the changes that I (Nils) made to turn Chicken/Tk into PS/Tk:
;;;
;;; - Removed all Chicken-isms except for PROCESS.
;;; - All PS/Tk function names begin with TK/ or TK-:
;;;     EVAL-WISH   --> TK-EVAL-WISH
;;;     GET-TK-VAR  --> TK-GET-VAR
;;;     SET-TK-VAR! --> TK-SET-VAR!
;;;     START-TK    --> TK-START
;;;     END-TK      --> TK-END
;;;     EVENT-LOOP  --> TK-EVENT-LOOP
;;; - Added TK-DISPATCH-EVENT.
;;; - Added TK-WAIT-FOR-WINDOW because TK/WAIT returned too early.
;;; - Removed some unused functions and variables.
;;; - Replaced keyword lists with property lists.
;;; - Removed ScrolledText compound widget.
;;; - Removed :WIDGET-NAME option.
;;; - Added a PLT Scheme version of RUN-PROGRAM.
;;;
;;; Contributions (in order of appearance):
;;; - Jens Axel Soegaard: PLT Scheme/Windows RUN-PROGRAM.
;;; - Taylor R Campbell: Scheme48 RUN-PROGRAM, portable GENSYM, and some R5RS
;;;   portability fixes.
;;; - Jeffrey T. Read: Gambit hacks (RUN-PROGRAM, keyword hack).
;;; - Marc Feeley: Various versions of RUN-PROGRAM (Bigloo, Gauche, Guile,
;;;   Kawa, Scsh, Stklos), SRFI-88 keyword auto-detection, some bug fixes.
;;; - David St-Hilaire: suggested catching unspecific value in form->string.
;;; - Ken Dickey: added Ikarus Scheme
;;; - Ken Dickey: added Larceny Scheme
;;; Thank you!
;;;
;;; Change Log:
;;; 2022-03-07 Added tk-throw for turn Tk error to Scheme errors
;;;            Added application termination in case of error in pipe to scheme process
;;; 2022-03-05 Rewrited on Guile module
;;;            Changed "_IONBF" to 'none
;;; 2021-12-01 Deleted all non Guile sections.
;;;            Changed "set-batch-mode?!" to "ensure-batch-mode!".
;;;            Deleted the "bottom" function call.
;;; 2008-06-22 Added Larceny Scheme support.
;;; 2008-02-29 Added R6RS (Ikarus Scheme) support, added TTK/STYLE.
;;; 2007-06-27 Renamed source file to pstk.scm.
;;; 2007-06-27 Re-factored some large procedures, applied some cosmetics.
;;; 2007-06-26 FORM->STRING catches unspecific values now, so event handlers
;;;            no longer have to return specific values.
;;; 2007-06-26 Re-imported the following ports from the processio/v1 snowball:
;;;            Bigloo, Gauche, Guile, Kawa, Scsh, Stklos.
;;; 2007-06-26 Added auto-detection of SRFI-88 keywords.
;;; 2007-03-03 Removed callback mutex, because it blocked some redraw
;;;            operations. Use TK-WITH-LOCK to protect critical sections.
;;; 2007-02-03 Added Tile support: TTK-MAP-WIDGETS, TTK/AVAILABLE-THEMES,
;;;            TTK/SET-THEME.
;;; 2007-01-20 Added (Petite) Chez Scheme port.
;;; 2007-01-06 Fix: TK-WAIT-FOR-WINDOW requires nested callbacks.
;;; 2007-01-05 Added code to patch through fatal TCL messages.
;;; 2007-01-05 Protected call-backs by a mutex, so accidental double
;;;            clicks, etc cannot mess up program state.
;;; 2006-12-21 Made FORM->STRING accept '().
;;; 2006-12-18 Installing WM_DELETE_WINDOW handler in TK-START now, so it does
;;;            not get reset in TK-EVENT-LOOP.
;;; 2006-12-18 Made TK-START and TK-END return () instead of #<unspecific>
;;;            (which crashes FORM->STRING).
;;; 2006-12-12 Fixed some wrong Tcl quotation (introduced by myself).
;;; 2006-12-09 Added TK/BELL procedure.
;;; 2006-12-08 Replaced ATOM->STRING by FORM->STRING.
;;; 2006-12-06 Added TK-WAIT-UNTIL-VISIBLE.
;;; 2006-12-03 Made more variables local to outer LETREC.
;;; 2006-12-03 Added Gambit port and keywords hack.
;;; 2006-12-02 Added Scheme 48 port, portable GENSYM, R5RS fixes.
;;; 2006-12-02 Added PLT/Windows port.

(define-module (pstk)
  #:export (*wish-program*
	    *wish-debug-input*
	    *wish-debug-output*
	    tk
	    tk-throw
	    tk-dispatch-event
	    tk-end
	    tk-eval
	    tk-event-loop
	    tk-get-var
	    tk-id->widget
	    tk-set-var!
	    tk-start
	    tk-var
	    tk-wait-for-window
	    tk-wait-until-visible
	    tk-with-lock
	    tk/after
	    tk/appname
	    tk/bell
	    tk/bgerror
	    tk/bind
	    tk/bindtags
	    tk/caret
	    tk/choose-color
	    tk/choose-directory
	    tk/clipboard
	    tk/destroy
	    tk/dialog
	    tk/event
	    tk/focus
	    tk/focus-follows-mouse
	    tk/focus-next
	    tk/focus-prev
	    tk/get-open-file
	    tk/get-save-file
	    tk/grab
	    tk/grid
	    tk/image
	    tk/lower
	    tk/message-box
	    tk/option
	    tk/pack
	    tk/place
	    tk/popup
	    tk/raise
	    tk/scaling
	    tk/selection
	    tk/update
	    tk/useinputmethods
	    tk/wait
	    tk/windowingsystem
	    tk/winfo
	    tk/wm
	    ttk-map-widgets
	    ttk/available-themes
	    ttk/set-theme
	    ttk/style)
  #:use-modules ((srfi srfi-88)

                 (ice-9 match)))

(define *wish-program* "tclsh")
(define *wish-debug-input* #f)
(define *wish-debug-output* #f)

(define *use-keywords?*
  (or (not (symbol? 'text:))
      (not (symbol? ':text))
      (string=? "text" (symbol->string 'text:))
      (string=? "text" (symbol->string ':text))))

;; XXX: Commenting out because we are going to define them at the end.
;; (define tk #f)
;; (define tk-dispatch-event #f)
;; (define tk-end #f)
;; (define tk-eval #f)
;; (define tk-event-loop #f)
;; (define tk-get-var #f)
;; (define tk-id->widget #f)
;; (define tk-set-var! #f)
;; (define tk-start #f)
;; (define tk-var #f)
;; (define tk-wait-for-window #f)
;; (define tk-wait-until-visible #f)
;; (define tk-with-lock #f)
;; (define tk/after #f)
;; (define tk/appname #f)
;; (define tk/bell #f)
;; (define tk/bgerror #f)
;; (define tk/bind #f)
;; (define tk/bindtags #f)
;; (define tk/caret #f)
;; (define tk/choose-color #f)
;; (define tk/choose-directory #f)
;; (define tk/clipboard #f)
;; (define tk/destroy #f)
;; (define tk/dialog #f)
;; (define tk/event #f)
;; (define tk/focus #f)
;; (define tk/focus-follows-mouse #f)
;; (define tk/focus-next #f)
;; (define tk/focus-prev #f)
;; (define tk/get-open-file #f)
;; (define tk/get-save-file #f)
;; (define tk/grab #f)
;; (define tk/grid #f)
;; (define tk/image #f)
;; (define tk/lower #f)
;; (define tk/message-box #f)
;; (define tk/option #f)
;; (define tk/pack #f)
;; (define tk/place #f)
;; (define tk/popup #f)
;; (define tk/raise #f)
;; (define tk/scaling #f)
;; (define tk/selection #f)
;; (define tk/update #f)
;; (define tk/useinputmethods #f)
;; (define tk/wait #f)
;; (define tk/windowingsystem #f)
;; (define tk/winfo #f)
;; (define tk/wm #f)
;; (define ttk-map-widgets #f)
;; (define ttk/available-themes #f)
;; (define ttk/set-theme #f)
;; (define ttk/style #f)

(define tk-throw
  (let ((enabled #f))
    (lambda (args)
      (if (null? args)
	  enabled
	  (set! enabled (car args))))))

(define (->string x)
  ;; XXX: Commenting out for the simpler implementation below.
  ;; (cond ((string? x) x)
  ;;       ((symbol? x) (symbol->string x))
  ;;       ((char?   x) (string x))
  ;;       ((number? x) (number->string x))
  ;;       (else
  ;;        (let ((out (open-output-string)))
  ;;          (display x out)
  ;;          (get-output-string out))))

  (with-output-to-string
    (lambda () (display x))))

;;; Start weird letrec definitions:

(define nl (string #\newline))
(define wish-input #f)
(define wish-output #f)
(define tk-is-running #f)
(define tk-ids+widgets '())
(define tk-widgets '())
(define commands-invoked-by-tk '())
(define inverse-commands-invoked-by-tk '())
(define in-callback #f)
(define callback-mutex #t)
(define ttk-widget-map '())
(define tk-init-string "package require Tk
if {[package version tile] != \"\"} {
    package require tile
}

namespace eval AutoName {
    variable c 0
    proc autoName {{result \\#\\#}} {
        variable c
        append result [incr c]
    }
    namespace export *
}

namespace import AutoName::*

proc callToScm {callKey args} {
    global scmVar
    if { [catch {
            set resultKey [autoName]
            puts \"(call $callKey \\\"$resultKey\\\" $args)\"
            flush stdout
            vwait scmVar($resultKey)
            set result $scmVar($resultKey)
            unset scmVar($resultKey)
            set result
        } ]
    } { exit 1 }
}

proc tclListToScmList {l} {
    switch [llength $l] {
        0 {
            return ()
        }
        1 {
            if {[string range $l 0 0] eq \"\\#\"} {
                return $l
            }
            if {[regexp {^[0-9]+$} $l]} {
                return $l
            }
            if {[regexp {^[.[:alpha:]][^ ,\\\"\\'\\[\\]\\\\;]*$} $l]} {
                return $l
            }
            set result \\\"
            append result\\
                [string map [list \\\" \\\\\\\" \\\\ \\\\\\\\] $l]
            append result \\\"

        }
        default {
            set result {}
            foreach el $l {
                append result \" \" [tclListToScmList $el]
            }
            set result [string range $result 1 end]
            return \"($result)\"
        }
    }
}

proc evalCmdFromScm {cmd {properly 0}} {
    if {[catch {
        set result [uplevel \\#0 $cmd]
    } err]} {
        puts \"(error \\\"[string map [list \\\\ \\\\\\\\ \\\" \\\\\\\"] $err]\\\")\"
    } elseif $properly {
        puts \"(return [tclListToScmList $result])\"
    } else {
        puts \"(return \\\"[string map [list \\\\ \\\\\\\\ \\\" \\\\\\\"] $result]\\\")\"
    }
    flush stdout
}
")

(define (report-error x)
  (newline)
  (display x)
  (newline)
  (when (tk-throw)
    (error 'tk (->string x))))

(define (run-program program)
  (define (open-i/o-process prog . args)
    (let ((c2p (pipe))
          (p2c (pipe)))
      (setvbuf (cdr c2p) 'none)
      (setvbuf (cdr p2c) 'none)
      (let ((pid (primitive-fork)))
        (cond ((= pid 0)
               (ensure-batch-mode!)
               (let ((input-fdes (fileno (car p2c)))
                     (output-fdes (fileno (cdr c2p))))
                 (port-for-each
                  (lambda (pt-entry)
                    (false-if-exception
                     (let ((pt-fileno (fileno pt-entry)))
                       (when (not (or (= pt-fileno input-fdes)
                                      (= pt-fileno output-fdes)))
                         (close-fdes pt-fileno))))))
                 (cond ((not (= input-fdes 0))
                        (when (= output-fdes 0)
                          (set! output-fdes (dup->fdes 0)))
                        (dup2 input-fdes 0)))
                 (when (not (= output-fdes 1))
                   (dup2 output-fdes 1))
                 (apply execlp prog prog args)))
              (else
               (close-port (cdr c2p))
               (close-port (car p2c))
               (values (car c2p)
                       (cdr p2c)))))))

  (open-i/o-process "/bin/sh" "-c"
                    (string-append "exec " program)))

(define flush-output-port force-output)

(define (flush-wish)
  (flush-output-port wish-input))

(define (option? x)
  (or (and *use-keywords?*
           (keyword? x))
      (and (symbol? x)
           (let* ((s (symbol->string x))
                  (n (string-length s)))
             (char=? #\: (string-ref s (- n 1)))))))

(define (make-option-string x)
  (if (and *use-keywords?*
           (keyword? x))
      (string-append " -" (keyword->string x))
      (let ((s (symbol->string x)))
        (string-append " -"
                       (substring s 0 (- (string-length s) 1))))))

(define (improper-list->string possibly-improper-list)
  ;; XXX: Commenting out and replacing with the implementation at the bottom.
  ;; (cond ((pair? a)
  ;;        (cons (string-append (if first "" " ")
  ;;                             (form->string (car a)))
  ;;              (improper-list->string (cdr a) #f)))
  ;;       ((null? a) '())
  ;;       (else (list (string-append " . " (form->string a)))))

  ;; We know we only ever get a pair as POSSIBLY-IMPROPER-LIST, so we:
  (let loop ((result (form->string (car possibly-improper-list)))
             ;; Take the first of argument POSSIBLY-IMPROPER-LIST
             ;; without any space separator.
             (a (cdr possibly-improper-list))
             ;; and the rest of A.
             )
    (match a
      ((first-of-a . rest-of-a)
       ;; Now that we are sure we are in the middle of the list we can
       ;; start adding space separators.
       (loop (cons (string-append (form->string first-of-a)
                                  " ")
                   result)
             rest-of-a))
      (() (reverse result))
      (improper-list-terminator
       (reverse (cons (form->string improper-list-terminator)
                      " . "
                      result))))))

(define (form->string x)
  (match x
    (#t "#t")
    (#f "#f")
    ((? number? x) (number->string x))
    ((? symbol? x) (symbol->string x))
    ((? string? x) x)
    (() "()")
    ((_ . _)
     (string-append "("
                    (apply string-append
                           (improper-list->string x))
                    ")"))
    ((eof-object? x) "#<eof>")
    (_ "#<other>")))

(define (string-translate s map)
  (define (s-prepend s1 s2)
    (cond ((null? s1) s2)
          (else (s-prepend (cdr s1) (cons (car s1) s2)))))
  (define (s-xlate s r)
    (cond ((null? s) (reverse r))
          (else (let ((n (assv (car s) map)))
                  (cond (n (s-xlate (cdr s)
                                    (s-prepend (string->list (cdr n)) r)))
                        (else (s-xlate (cdr s)
                                       (cons (car s) r))))))))
  (list->string
   (s-xlate (string->list s) '())))

(define (string-trim-left str)
  (string-trim str #\space))

(define* (get-property key args #:optional (thunk #f))
  ;; XXX: Originally the function signature was (get-property key args . thunk).
  (cond ((null? args)
         (cond ((null? thunk) #f)
               (else ((car thunk)))))
        ((eq? key (car args))
         (cond ((pair? (cdr args)) (cadr args))
               (else (report-error (list 'get-property key args)))))
        ((or (not (pair? (cdr args)))
             (not (pair? (cddr args))))
         (report-error (list 'get-property key args)))
        (else (apply get-property key (cddr args) thunk)))

  ;; XXX: Not sure if it's clearer than the above.
  (match args
    (()
     (match
      ((thunk-value)
       (thunk-value))
      (#f
       #f)
      (_
       (report-error (list 'get-property key args thunk)))))
    ((and (args-key . args-rest)
          (? eq? key args-key))
     (match args-rest
       ((args-value . args-rest)
        args-value)
       (_
        (report-error (list 'get-property key args)))))
    ((args-key args-value-0 more-value-1 . args-rest)
     (apply get-property key (cddr args) thunk))
    (_
     (report-error (list 'get-property key args)))))

(define tcl-true?
  (let ((false-values
         `(0 "0" 'false "false" ,(string->symbol "0"))))
    (lambda (obj) (not (memv obj false-values)))))

(define (widget? x)
  (and (memq x tk-widgets) #t))

(define (call-by-key key resultvar . args)
  (cond ((and in-callback (pair? callback-mutex)) #f)
        (else (set! in-callback (cons #t in-callback))
              (let* ((cmd (get-property key commands-invoked-by-tk))
                     (result (apply cmd args))
                     (str (string-trim-left
                           (scheme-arglist->tk-argstring
                            (list result)))))
                (set-var! resultvar str)
                (set! in-callback (cdr in-callback))
                result))))

(define gen-symbol
  (let ((counter 0))
    (lambda ()
      (let ((sym (string-append "g" (number->string counter))))
        (set! counter (+ counter 1))
        (string->symbol sym)))))

(define (widget-name x)
  (let ((name (form->string x)))
    (cond ((member name ttk-widget-map)
           (string-append "ttk::" name))
          (else name))))

(define (make-widget-by-id type id . options)
  (let
      ((result
        (lambda (command . args)
          (case command
            ((get-id) id)
            ((create-widget)
             (let* ((widget-type (widget-name (car args)))
                    (id-prefix (if (string=? id ".") "" id))
                    (id-suffix (form->string (gen-symbol)))
                    (new-id (string-append id-prefix "." id-suffix))
                    (options (cdr args)))
               (eval-wish
                (string-append
                 widget-type
                 " "
                 new-id
                 (scheme-arglist->tk-argstring options)))
               (apply make-widget-by-id
                      (append (list widget-type new-id)
                              options))))
            ((configure)
             (cond ((null? args)
                    (eval-wish
                     (string-append id " " (form->string command))))
                   ((null? (cdr args))
                    (eval-wish
                     (string-append
                      id
                      " "
                      (form->string command)
                      (scheme-arglist->tk-argstring args))))
                   (else
                    (eval-wish
                     (string-append
                      id
                      " "
                      (form->string command)
                      (scheme-arglist->tk-argstring args)))
                    (do ((args args (cddr args)))
                        ((null? args) '())
                      (let ((key (car args)) (val (cadr args)))
                        (cond ((null? options)
                               (set! options (list key val)))
                              ((not (memq key options))
                               (set! options
                                     (cons key (cons val options))))
                              (else (set-car! (cdr (memq key options))
                                              val))))))))
            ((cget)
             (let ((key (car args)))
               (get-property
                key
                options
                (lambda ()
                  (eval-wish
                   (string-append
                    id
                    " cget"
                    (scheme-arglist->tk-argstring args)))))))
            ((call exec)
             (eval-wish
              (string-trim-left
               (scheme-arglist->tk-argstring args))))
            (else
             (eval-wish
              (string-append
               id
               " "
               (form->string command)
               (scheme-arglist->tk-argstring args))))))))
    (set! tk-widgets (cons result tk-widgets))
    (set! tk-ids+widgets
          (cons (string->symbol id)
                (cons result tk-ids+widgets)))
    result))

(define (scheme-arg->tk-arg x)
  (cond ((eq? x #f) " 0")
        ((eq? x #t) " 1")
        ((eq? x '()) " {}")
        ((option? x) (make-option-string x))
        ((widget? x) (string-append " " (x 'get-id)))
        ((and (pair? x) (procedure? (car x)))
         (let* ((lambda-term (car x))
                (rest (cdr x))
                (l (memq lambda-term
                         inverse-commands-invoked-by-tk))
                (keystr (if l (form->string (cadr l))
                            (symbol->string (gen-symbol)))))
           (if (not l)
               (let ((key (string->symbol keystr)))
                 (set! inverse-commands-invoked-by-tk
                       (cons lambda-term
                             (cons key
                                   inverse-commands-invoked-by-tk)))
                 (set! commands-invoked-by-tk
                       (cons key
                             (cons lambda-term
                                   commands-invoked-by-tk)))))
           (string-append " {callToScm "
                          keystr
                          (scheme-arglist->tk-argstring rest)
                          "}")))
        ((procedure? x)
         (scheme-arglist->tk-argstring `((,x))))
        ((list? x)
         (cond ((eq? (car x) '+)
                (let ((result (string-trim-left
                               (scheme-arglist->tk-argstring
                                (cdr x)))))
                  (cond ((string=? result "") " +")
                        ((string=? "{" (substring result 0 1))
                         (string-append
                          " {+ "
                          (substring result 1
                                     (string-length result))))
                        (else (string-append " +" result)))))
               ((and (= (length x) 3)
                     (equal? (car x) (string->symbol "@"))
                     (number? (cadr x))
                     (number? (caddr x)))
                (string-append
                 "@"
                 (number->string (cadr x))
                 ","
                 (number->string (caddr x))))
               (else
                (string-append
                 " {"
                 (string-trim-left
                  (scheme-arglist->tk-argstring x))
                 "}"))))
        ((pair? x)
         (string-append
          " "
          (form->string (car x))
          "."
          (form->string (cdr x))))
        ((string? x)
         (if (string->number x)
             (string-append " " x)
             (string-append
              " \""
              (string-translate x
                                '((#\" . "\\\"")
                                  (#\$ . "\\u0024")
                                  (#\[ . "\\u005b")
                                  (#\\ . "\\\\")
                                  (#\] . "\\]")
                                  (#\{ . "\\{")
                                  (#\} . "\\}")))
              "\"")))
        (else (string-append " " (form->string x)))))

(define (scheme-arglist->tk-argstring args)
  (apply string-append
         (map scheme-arg->tk-arg
              args)))

(define (make-wish-func tkname)
  (let ((name (form->string tkname)))
    (lambda args
      (eval-wish
       (string-append
        name
        (scheme-arglist->tk-argstring args))))))

(define (read-wish)
  (let ((term (read wish-output)))
    (cond (*wish-debug-output*
           (display "wish->scheme: ")
           (write term)
           (newline)))
    term))

(define (wish arguments)
  (for-each
   (lambda (argument)
     (cond (*wish-debug-input*
            (display "scheme->wish: ")
            (display argument)
            (newline)))
     (display argument wish-input)
     (newline wish-input)
     (flush-wish))
   arguments))

(define (start-wish)
  (let-values (((wish-output-pipe wish-input-pipe)
                (run-program *wish-program*)))
    (set! wish-input wish-input-pipe)
    (set! wish-output wish-output-pipe)))

(define (read-line in)
  (define (collect-chars c s)
     (lambda (c s)
       (cond ((or (eof-object? c) (char=? c #\newline))
              (apply string (reverse s)))
             (else (collect-chars (read-char in) (cons c s))))))
  (define first-char (read-char in))
  (cond ((eof-object? first-char) first-char)
        (else (collect-chars first-char '()))))

(define (eval-wish cmd)
  (wish (string-append
         "evalCmdFromScm \""
         (string-translate cmd
                           '((#\" . "\\\"")
                             (#\\ . "\\\\")))
         "\""))
  (let again ((result (read-wish)))
    (cond ((not (pair? result))
           (report-error (string-append
                          "An error occurred inside Tcl/Tk" nl
                          " --> " (form->string result)
                          " " (read-line wish-output))))
          ((eq? (car result) 'return)
           (cadr result))
          ((eq? (car result) 'call)
           (apply call-by-key (cdr result))
           (again (read-wish)))
          ((eq? (car result) 'error)
           (report-error (string-append
                          "An error occurred inside Tcl/Tk" nl
                          " " cmd nl
                          " --> " (cadr result))))
          (else (report-error result)))))

(define (id->widget id)
  (get-property
   (string->symbol (form->string id))
   tk-ids+widgets
   (lambda ()
     (if (tcl-true? (tk/winfo 'exists id))
         (make-widget-by-id
          (tk/winfo 'class id)
          (form->string id))
         #f))))

(define (var varname)
  (set-var! varname "")
  (string-append
   "::scmVar("
   (form->string varname)
   ")"))

(define (get-var varname)
  (eval-wish
   (string-append
    "set ::scmVar("
    (form->string varname)
    ")")))

(define (set-var! varname value)
  (eval-wish
   (string-append
    "set ::scmVar("
    (form->string varname)
    ") {"
    (form->string value)
    "}")))

(define (start)
  (start-wish)
  (wish tk-init-string)
  (set! tk-ids+widgets '())
  (set! tk-widgets '())
  (set! in-callback #f)
  (set! tk (make-widget-by-id 'toplevel "." 'class: 'Wish))
  (set! commands-invoked-by-tk '())
  (set! inverse-commands-invoked-by-tk '())
  (tk/wm 'protocol tk 'WM_DELETE_WINDOW end-tk))

(define (end-tk)
  (set! tk-is-running #f)
  (wish "after 200 exit"))

(define (ispatch-event)
  (let ((tk-statement (read-wish)))
    (if (and (list? tk-statement)
             (eq? (car tk-statement) 'call))
        (apply call-by-key (cdr tk-statement)))))

(define (loop)
  (cond ((and (not tk-is-running)
              wish-output)
         (tk/wm 'protocol tk 'WM_DELETE_WINDOW '()))
        (else (dispatch-event)
              (loop))))

(define (event-loop)
  (set! tk-is-running #t)
  (loop))

(define (map-ttk-widgets x)
  (cond ((eq? x 'all)
         (set! ttk-widget-map '("button" "checkbutton" "radiobutton"
                                "menubutton" "label" "entry" "frame"
                                "labelframe" "scrollbar" "notebook"
                                "progressbar" "combobox" "separator"
                                "scale" "sizegrip" "treeview")))
        ((eq? x 'none)
         (set! ttk-widget-map '()))
        ((pair? x) (set! ttk-widget-map
                         (map form->string x)))
        (else (report-error
               (string-append
                "Argument to TTK-MAP-WIDGETS must be "
                "ALL, NONE or a list of widget types.")))))

;;; XXX: Commented out because string-split is already part of Guile.
;; (define (string-split c s)
;;   (define (split i k tmp res)
;;     (cond ((= i k)
;;            (if (null? tmp) res (cons tmp res)))
;;           ((char=? (string-ref s i) c)
;;            (split (+ i 1) k "" (cons tmp res)))
;;           (else (split (+ i 1) k
;;                        (string-append tmp
;;                                       (string (string-ref s i)))
;;                        res))))
;;   (reverse (split 0 (string-length s) "" '())))

(define (ttk-available-themes)
  ;; XXX: Using the Guile string-split instead of the above commented
  ;; out one.
  (string-split (eval-wish "ttk::style theme names")
                g#\space))

(define (do-wait-for-window w)
  (dispatch-event)
  (cond ((equal? (tk/winfo 'exists w) "0") '())
        (else (do-wait-for-window w))))

(define (wait-for-window w)
  (let ((outer-allow callback-mutex))
    (set! callback-mutex #t)
    (do-wait-for-window w)
    (set! callback-mutex outer-allow)))

(define (wait-until-visible w)
    (tk/wait 'visibility w))

(define (lock!)
  (set! callback-mutex
        (cons callback-mutex #t)))

(define (unlock!)
  (if (pair? callback-mutex)
      (set! callback-mutex
            (cdr callback-mutex))))

(define (with-lock thunk)
  (lock!)
  (thunk)
  (unlock!))

;;; End weird letrec definitions.

;;; Start weird letrec body:

(define tk-eval eval-wish)
(define tk-id->widget id->widget)
(define tk-var var)
(define tk-get-var get-var)
(define tk-set-var! set-var!)
(define tk-start start)
(define tk-end end-tk)
(define tk-dispatch-event dispatch-event)
(define tk-event-loop event-loop)
(define tk-wait-for-window wait-for-window)
(define tk-wait-until-visible wait-until-visible)
(define tk-with-lock with-lock)
(define tk/after (make-wish-func 'after))
(define tk/bell (make-wish-func 'bell))
(define tk/update (make-wish-func 'update))
(define tk/clipboard (make-wish-func 'clipboard))
(define tk/bgerror (make-wish-func 'bgerror))
(define tk/bind (make-wish-func 'bind))
(define tk/bindtags (make-wish-func 'bindtags))
(define tk/destroy (make-wish-func 'destroy))
(define tk/event (make-wish-func 'event))
(define tk/focus (make-wish-func 'focus))
(define tk/grab (make-wish-func 'grab))
(define tk/grid (make-wish-func 'grid))
(define tk/image (make-wish-func 'image))
(define tk/lower (make-wish-func 'lower))
(define tk/option (make-wish-func 'option))
(define tk/pack (make-wish-func 'pack))
(define tk/place (make-wish-func 'place))
(define tk/raise (make-wish-func 'raise))
(define tk/selection (make-wish-func 'selection))
(define tk/winfo (make-wish-func 'winfo))
(define tk/wm (make-wish-func 'wm))
(define tk/choose-color (make-wish-func "tk_chooseColor"))
(define tk/choose-directory (make-wish-func "tk_chooseDirectory"))
(define tk/dialog (make-wish-func "tk_dialog"))
(define tk/get-open-file (make-wish-func "tk_getOpenFile"))
(define tk/get-save-file (make-wish-func "tk_getSaveFile"))
(define tk/message-box (make-wish-func "tk_messageBox"))
(define tk/focus-follows-mouse (make-wish-func "tk_focusFollowsMouse"))
(define tk/focus-next (make-wish-func "tk_focusNext"))
(define tk/focus-prev (make-wish-func "tk_focusPrev"))
(define tk/popup (make-wish-func "tk_popup"))
(define tk/wait (lambda args (make-wish-func 'tkwait)))
(define tk/appname (make-wish-func "tk appname"))
(define tk/caret (make-wish-func "tk caret"))
(define tk/scaling (make-wish-func "tk scaling"))
(define tk/useinputmethods (make-wish-func "tk useinputmethods"))
(define tk/windowingsystem (make-wish-func "tk windowingsystem"))
(define ttk/available-themes ttk-available-themes)
(define ttk/set-theme (make-wish-func "ttk::style theme use"))
(define ttk/style (make-wish-func "ttk::style"))
(define ttk-map-widgets map-ttk-widgets)

;;; End weird letrec body.
