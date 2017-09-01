;;;; Implementation of the untitled shell history application.
(cond-expand
  (compiling (define (compiled?) #t))
  (else (define (compiled?) #f)))

(include "flags.scm")
(include "db.scm")
(import db flags)

(use args)

(define opts
  (list
    (args:make-option
      (v verbose) #:none "Verbose mode."
      (set-verbose-mode #t))
    (args:make-option
      (n number) #:required "Number of results to display. [default: 5]")
    (args:make-option
      (s search) #:required "Search for substring.")
    (args:make-option
      (t time) #:none "Order by most recently entered.")
    (args:make-option
      (r recurse) #:none "Recurse into contained directories.")
    (args:make-option
      (h help) #:none "Display usage information.")
    (args:make-option
      (l lucky) #:none "I'm feeling lucky!")
    (args:make-option
      (c checksum) #:required "Checksum value to update history.")))

(use
  posix
  section-combinators fmt
  matchable sqlite3
  filepath)

;; Update
(define (ignore-path)
  (let* ([ignore-name ".ushaignore"]
         [elements (list (home-dir) ignore-name)])
    (filepath:join-path elements)))

(: ignore? (string string -> boolean))
(define (ignore? ignore-path cmd)
  (let* ([base-command (first (string-split cmd))]
         [stop? (lambda (l) (equal? l base-command))])
    (with-input-from-file
      ignore-path
      (lambda ()
        (do ([ignore-line (read-line) (read-line)])
          ((or (eof-object? ignore-line)
               (stop? ignore-line))
           (stop? ignore-line)))))))

(: history-update (string string -> undefined))
(define (history-update cmd checksum)
  (let ([cwd (current-directory)])
    (cond [(ignore? (ignore-path) cmd)
           (maybe-print "Ignoring ucha update: " cmd)]
          [(db-checksum? checksum)
           (maybe-print "Skipping update; checksum matches.")]
          [else (db-insert cwd cmd checksum)])))

;; Search
(define-type response (list-of string))

(: fmt-responses ((list-of response) -> undefined))
(define (fmt-responses responses)
  (define (join-map r)
    (string-join (map ->string r) "\n"))
  (let* ([zipped (apply zip responses)]
         [columns (map (compose dsp join-map) zipped)])
    (fmt #t (apply columnar columns))))

(define (history-search . args)
  (let ([results (apply db-search args)])
    (if (not (null? results))
      (fmt-responses results))))

;; Args and main

(: parse-dir (string -> string))
(define (parse-dir dir)
  (let ([cwd (current-directory)])
    (match dir
           ["." cwd]
           [".." (filepath:drop-trailing-path-separator (filepath:drop-file-name cwd))]
           [dir-name dir-name])))

(receive
  (options operands) (args:parse (command-line-arguments) opts)
  (let ([opt (lambda (x) (alist-ref x options))])
    (define (do-search dir)
      (let ([number (or (opt 'number) 5)]
            [search (opt 'search)]
            [order-by (if (opt 'time) entered_on: count:)]
            [recurse (opt 'recurse)]
            [lucky (opt 'lucky)])
        (history-search dir number search order-by recurse lucky)))
    (define (do-insert cmd)
      (let ([checksum (opt 'checksum)])
        (history-update (string-join cmd) checksum)))

    (if (compiled?)
      (if (opt 'help)
        (print (args:usage opts))
        (match operands
               [() (do-search #f)]
               [("insert" . cmd) (do-insert cmd)]
               [(dir . _) (do-search (parse-dir dir))])))))
