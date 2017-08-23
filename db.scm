(module
  db
  (search-db)
  (import scheme chicken)
  (use srfi-1 srfi-13
       sqlite3 filepath posix data-structures)
  (import flags)

  (define (db-path)
    (let* ([db-name ".esdb"]
           [user-info (user-information (current-user-id) #t)]
           [elements (list (vector-ref user-info 5) db-name)])
      (filepath:join-path elements)))

  (define (db-open) (open-database (db-path)))

  (define (process-row . columns)
    ; Ensure row length of >=3.
    (do ([response columns (cons '() response)])
      ((>= (length response) 3)
       response)))

  (define (get-rows stmt interpolation-values)

    (if (verbose-mode)
      (print "Executing SQL:\n" stmt "\nWith arguments:\n" interpolation-values))

    (apply map-row process-row (db-open) stmt interpolation-values))

  (define (search-db cwd number search)
    (let* ([stmt-slots
             (vector
               "SELECT count, cmd FROM history WHERE "
               ""
               "LIMIT ?")]
           [interp-values '()])
    (begin
      (vector-set! stmt-slots 1 "cwd = ?")
      (set! interp-values
        (append! interp-values (list cwd))))

      (if search
        ; Where cmd LIKE search
        )

      (set! interp-values (append! interp-values (list number)))

      (get-rows
        (string-join (vector->list stmt-slots)) interp-values)))
  )
