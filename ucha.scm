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
      (t time) #:none "Order by most recently entered.")))

(use
  data-structures posix
  section-combinators fmt
  matchable loops sqlite3
  filepath)


;; Response Handling

(define (to-line line-width response)
  ; Format a single db response into a single line.
  (let* ([cmd (third response)]
         [count (second response)]
         [date (first response)]
         [cmd-length (string-length cmd)]
         [pad-length (- line-width cmd-length)])
    (fmt #f cmd (space-to pad-length) count " " (if (null? date) "" date) nl)))

(define (line-width response)
  ; Get the width of a command-count output, adding 2 for spacing
  (fold + 0 (cons 2 (map (compose string-length ->string) response))))

;; Search

(define (fmt-responses responses)
  (let* ([max-line-width (apply max (map line-width responses))]
         [strings (map (left-section to-line max-line-width) responses)])
    (string-concatenate strings)))

(define (search-history . args)
  (let* ([results (apply search-db args)]
         [formatted (fmt-responses results)])
    (if (not (null-list? results))
      (print formatted))))

;; Args and main

(define (parse-dir dir)
  (match dir
         ["." (current-directory)]
         [dir-name dir-name]))

(receive
  (options operands)
  (args:parse (command-line-arguments) opts)

  (define (opt x) (alist-ref x options))
  (let* ([dir (parse-dir (opt 'dir))]
         [number (or (opt 'number) 5)]
         [search (opt 'search)]
         [order-by (if (opt 'time) entered_on: count:)])
    (search-history dir number search order-by)))
