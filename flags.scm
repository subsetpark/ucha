(module
  flags
  (maybe-print set-verbose-mode home-dir)

  (import chicken scheme)
  (use posix)

  (: verbose-mode-flag boolean)
  (define verbose-mode-flag #f)

  (define (set-verbose-mode value) (set! verbose-mode-flag value))
  (define (maybe-print . s) (if verbose-mode-flag (apply print s)))
  (define (home-dir)
    (vector-ref (user-information (current-user-id) #t) 5)))
