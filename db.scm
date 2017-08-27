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

  (define (make-stmt cwd number search order-by recurse)
    (let* ([count-column (if (eq? order-by count:) 'count '(sum count))]
           [search-columns (if (eq? order-by entered_on:)
                             `(columns entered_on ,count-column cmd)
                             `(columns ,count-column cmd))]

           [cwd-=-where (if cwd `(= cwd ?) 1)]
           [cwd-elems `(string-append ? "/%")]
           [cwd-like-where (if cwd `(like cwd ,cwd-elems) 1)]
           [cwd-where (if recurse `(or ,cwd-=-where ,cwd-like-where) cwd-=-where)]

           [search-elems `(string-append "%" ? "%")]
           [cmd-where (if search
                        `(like cmd ,search-elems) 1)]

           [where-stmt `(where (and ,cwd-where ,cmd-where))]
           [group-stmt (if cwd '() '(group cmd))]
           [order-stmt `(desc (order ,order-by))])

      (values
        `(select ,search-columns
                 (from history)
                 ,where-stmt
                 ,group-stmt
                 ,order-stmt)
        (list cwd (if recurse cwd #f) search number))))

  (define (search-db . stmt-parameters)
    (receive (stmt-ssql arglist) (apply make-stmt stmt-parameters)
             (let* ([stmt-sql (ssql->sql #f stmt-ssql)]
                    [stmt-elems (list stmt-sql "LIMIT ?")]
                    [stmt (string-join stmt-elems " ")]
                    [args (filter identity arglist)])
               (get-rows stmt args)))))
