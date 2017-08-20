(module 
  db 
  (get-rows)
  (import scheme chicken)
  (use sqlite3 filepath posix)

  (define (db-path)
    (let* ([db-name ".esdb"]
           [user-info (user-information (current-user-id) #t)]
           [elements (list (vector-ref user-info 5) db-name)])
      (filepath:join-path elements)))

  (define (db-open) (open-database (db-path)))

  (define (process-row . columns)
    ; Ensure row length of >=3.
    (do ([response columns (cons '() response)])
      ((>= (length response) 3) response)))

  (define (get-rows stmt arg verbose-mode) 
    (begin
      (if verbose-mode 
        (print "Executing SQL: " stmt " with args " arg))
      (map-row process-row (db-open) stmt arg)))

  )
