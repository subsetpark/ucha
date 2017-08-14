;;;; Implementation of the untitled shell history application.

(use 
  args data-structures filepath posix sqlite3 section-combinators fmt matchable loops)

(: verbose-mode boolean)
(define verbose-mode #f)

;; Globals

(define-type DbResponse (list (or string null) string fixnum))

;; DB Operations

(define (db-path)
  (let* ([db-name ".esdb"]
         [user-info (user-information (current-user-id) #t)]
         [elements (list (vector-ref user-info 5) db-name)])
    (filepath:join-path elements)))

(define (db-open) (open-database (db-path)))

(define (process-row . columns)
  ; Ensure row length of >=3.
  (let ([response columns])
    (begin 
      (do-until (>= (length response) 3)
                (set! response (cons '() response))))
    response))

(define (get-rows stmt arg) 
  (begin
    (if verbose-mode 
      (print "Executing SQL: " stmt " with args " arg))
    (map-row process-row (db-open) stmt arg)))

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

(define (search-db cwd)
  (let ([stmt (conc "SELECT count, cmd FROM history "
                    "WHERE cwd = ?")])
    (get-rows stmt cwd)))

(define (fmt-responses responses)
  (let* ([max-line-width (apply max (map line-width responses))]
         [strings (map (left-section to-line max-line-width) responses)])
    (string-join strings "")))

(define (search-history dir)
  (let [(results (search-db dir))]
    (if (not (null-list? results))
      (print (fmt-responses results)))))

;; Args and main

(define opts
  (list (args:make-option 
          (d dir) #:required "Specify directory to search [default: .]")
        (args:make-option 
          (v verbose) #:none "Verbose mode."
          (set! verbose-mode #t))))

(define (parse-dir dir)
  (match dir 
         ["." (current-directory)]
         [dir-name dir-name]))

(receive (options operands) (args:parse (command-line-arguments) opts)
         (let ((dir (alist-ref 'dir options)))
           (search-history (parse-dir dir))))
