(module
  db
  (search-db)
  (import scheme chicken)
  (use sqlite3 filepath posix data-structures)
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
      ((>= (length response) 3) response)))

  (define (get-rows stmt interpolation-values)

    (if (verbose-mode)
      (print "Executing SQL:\n" stmt "\nWith arguments:\n" interpolation-values))

    (let* ([arglist (list process-row (db-open) stmt)]
           [cmd-args (append arglist interpolation-values)])
      (apply map-row cmd-args)))

  (define (search-db cwd number)
    (let ([stmt (conc "SELECT count, cmd FROM history "
                      "WHERE cwd = ? LIMIT ?")])
      (get-rows stmt (list cwd number))))

  )
