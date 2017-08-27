;;;; Implementation of the untitled shell history application.
(include "flags.scm")
(include "db.scm")
(import db flags)

(use args)

(define opts
  (list
    (args:make-option
      (d dir) #:required "Specify directory to search [default: .]")
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
      (h help) #:none "Display usage information.")))

(use
  posix
  section-combinators fmt
  matchable sqlite3
  filepath)


;; Response Handling

(define (to-line line-width response)
  ; Format a single db response into a single line.
  (let* ([cmd (third response)]
         [count (second response)]
         [date (first response)])
    (fmt #f cmd (space-to line-width) count "   " (if (null? date) "" date) fl)))

;; Search

(define (fmt-responses responses)
  (define (cmd-width response)
    (+ 2 (string-length (third response))))
  (let* ([max-cmd-width (apply max (map cmd-width responses))]
         [strings (map (left-section to-line max-cmd-width) responses)])
    (string-concatenate strings)))

(define (search-history . args)
  (let* ([results (apply search-db args)]
         [formatted (fmt-responses results)])
    (if (not (null-list? results))
      (print formatted))))

;; Args and main

(define (parse-dir dir)
  (let ([cur-dir (current-directory)])
    (match dir
           ["." cur-dir]
           [".." (filepath:drop-trailing-path-separator (filepath:drop-file-name cur-dir))]
           [dir-name dir-name])))

(receive
  (options operands)
  (args:parse (command-line-arguments) opts)

  (define (opt x) (alist-ref x options))
  (if (opt 'help)
    (print (args:usage opts))
    (let* ([dir (parse-dir (opt 'dir))]
           [number (or (opt 'number) 5)]
           [search (opt 'search)]
           [order-by (if (opt 'time) entered_on: count:)]
           [recurse (opt 'recurse)])
      (search-history dir number search order-by recurse))))
