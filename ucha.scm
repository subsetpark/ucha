;;;; Implementation of the untitled shell history application.
(include "flags.scm")
(include "db.scm")

(use 
  args data-structures posix
  section-combinators fmt 
  matchable loops sqlite3
  filepath)

(import db flags)

;; Response Handling

(define (to-line line-width response)
  ; Format a single db response into a single line.
  (let* ([cmd (third response)]
         [count (second response)]
         [date (first response)]
         [cmd-length (string-length cmd)]
         [pad-length (- line-width cmd-length)])
    (fmt #f cmd (pad pad-length) count (if (null? date) "" date) nl)))

(define (line-width response) 
  ; Get the width of a command-count output, adding 2 for spacing
  (fold + 0 (cons 2 (map (compose string-length ->string) response))))

;; Search

(define (fmt-responses responses)
  (let* ([max-line-width (apply max (map line-width responses))]
         [strings (map (left-section to-line max-line-width) responses)])
    (string-join strings "")))

(define (search-history dir number)
  (let [(results (search-db dir number))]
    (if (not (null-list? results))
      (print (fmt-responses results)))))

;; Args and main

(define opts
  (list (args:make-option 
          (d dir) #:required "Specify directory to search [default: .]")
        (args:make-option 
          (v verbose) #:none "Verbose mode."
          (set-verbose-mode #t))
        (args:make-option
          (n number) #:required "Number of results to display. [default: 5]")))

(define (parse-dir dir)
  (match dir 
         ["." (current-directory)]
         [dir-name dir-name]))

(receive (options operands) (args:parse (command-line-arguments) opts)
         (let ([dir (alist-ref 'dir options)]
               [number (or (alist-ref 'number options) 5)])
           (search-history (parse-dir dir) number)))
