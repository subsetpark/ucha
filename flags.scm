(module
  flags
  (verbose-mode set-verbose-mode)

  (import chicken scheme)

  (: verbose-mode-flag boolean)
  (define verbose-mode-flag #f)

  (define (verbose-mode)  verbose-mode-flag)
  (define (set-verbose-mode value) (set! verbose-mode-flag value)))
