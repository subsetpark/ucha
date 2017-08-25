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

  (define (make-stmt cwd search order-by)
    (let* ([count-column (if (eq? order-by count:) 'count '(sum count))]
           [search-columns (if (eq? order-by count:)
                             `(columns ,count-column cmd)
                             `(columns entered_on ,count-column cmd))]
           [cwd-stmt (if cwd `(= cwd ,cwd) 1)]
           [search-elems `("%" ,search "%")]
           [cmd-stmt (if search
                       `(like cmd ,(string-concatenate search-elems)) 1)]
           [group-stmt (if cwd '() '(group cmd))])
    `(select ,search-columns
       (from history)
       (where (and ,cwd-stmt ,cmd-stmt))
       (desc (order ,order-by))
       ,group-stmt)))

(define (search-db cwd number search order-by)
  (let ([stmt (string-join
                (list (ssql->sql #f (make-stmt cwd search order-by)) "LIMIT ?")
                " ")])
    (get-rows stmt (list number))))
)
