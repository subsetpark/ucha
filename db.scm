(module
  db
  (db-search db-insert db-checksum?)
  (import scheme chicken)
  (use srfi-1 srfi-13
       sqlite3 filepath posix data-structures
       ssql)
  (import flags)

  (define (db-open)
    (let ((db-path
            (let* ([db-name ".esdb"]
                   [elements (list (home-dir) db-name)])
              (filepath:join-path elements))))
      (open-database db-path)))

  (define (process-row . columns)
    ; Ensure row length of >=3.
    (do ([response columns (cons #f response)])
      ((>= (length response) 3)
       response)))

  (define (get-rows stmt interpolation-values)
    (maybe-print
      "Executing SQL:\n" stmt "\nWith arguments:\n" interpolation-values)
    (apply map-row process-row (db-open) stmt interpolation-values))

  (define (make-stmt cwd number search order-by recurse lucky)
    (let* ([count-column (if cwd 'count '(sum count))]
           [datetime-column (string->symbol "datetime(entered_on, \"localtime\")")]
           [search-columns
             (cond
               [lucky 'cmd]
               [(eq? order-by entered_on:) `(columns ,datetime-column ,count-column cmd)]
               [else `(columns ,count-column cmd)])]

           [cwd-=-where (if cwd `(= cwd ?) 1)]
           [cwd-elems `(string-append ? "/%")]
           [cwd-like-where (if cwd `(like cwd ,cwd-elems) 1)]
           [cwd-where (if recurse `(or ,cwd-=-where ,cwd-like-where) cwd-=-where)]

           [search-elems `(string-append "%" ? "%")]
           [cmd-where (if search
                        `(like cmd ,search-elems) 1)]

           [where-stmt `(where (and ,cwd-where ,cmd-where))]
           [group-stmt (if cwd '() '(group cmd))]
           [order-stmt `(desc (order ,order-by))]
           [number-stmt (if lucky 1 number)])

      (values
        `(select ,search-columns
                 (from history)
                 ,where-stmt
                 ,group-stmt
                 ,order-stmt)
        (list cwd (if recurse cwd #f) search number-stmt))))

  (define (db-search . stmt-parameters)
    (receive (stmt-ssql arglist) (apply make-stmt stmt-parameters)
             (let* ([stmt-sql (ssql->sql #f stmt-ssql)]
                    [stmt-elems (list stmt-sql "LIMIT ?")]
                    [stmt (string-join stmt-elems " ")]
                    [args (filter identity arglist)])
               (get-rows stmt args))))

(define (db-checksum? checksum)
  (maybe-print "Checking checksum against value: " checksum)
  (let* ([checksum-stmt "SELECT hash FROM checksum LIMIT 1"]
         [db (db-open)]
         [current-value (first-result db checksum-stmt)])
    (maybe-print "Current checksum value: " current-value)
    (equal? checksum current-value)))

(define (db-insert cwd cmd checksum)
  (let ([insert-stmt (string-join
                '("INSERT OR REPLACE INTO history"
                  "(cwd, cmd, count) VALUES"
                  "(?, ?, COALESCE((SELECT count FROM history WHERE cwd = ? AND cmd = ?), 0) + 1)"))]
        [checksum-stmt "UPDATE checksum SET hash = ?"]
        [db (db-open)])
    (maybe-print "Inserting value into history, cwd: " cwd ", cmd: " cmd)
    (execute db insert-stmt cwd cmd cwd cmd)
    (maybe-print "Updating checksum: " checksum)
    (execute db checksum-stmt checksum)))
)
