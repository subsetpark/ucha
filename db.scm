(module
  db
  (search-db)
  (import scheme chicken)
  (use srfi-1 srfi-13
       sqlite3 filepath posix data-structures
       ssql)
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

  (define (make-stmt search)
    (let ([search-elems (list "%" search "%")]
          [search-stmt
            (if search
              `(like cmd ,(string-join search-elems "")) 1)])
      `(select
         (columns count cmd)
         (from history)
         (where (and (= cwd ?) ,search-stmt)))))

  (define (search-db cwd number search)
    (let ([stmt (string-join
                  (list (ssql->sql #f (make-stmt search)) "LIMIT ?")
                  " ")])
      (get-rows stmt (list cwd number))))
  )
