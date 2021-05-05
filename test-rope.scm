(use rope)
(use test-utils)

(define (main . args)
  (define print-count 0)
  (define (outfile!)
    (let ((path (format #f "~3,'0d.png" print-count)))
      (inc! print-count)
      path))
  (define (draw! tree)
    (print-tree tree (outfile!)))

  (define rope (make-rope "Hel" "lo!"))
  (define rope2 (make-rope " W" "o"))
  (define rope3 (make-rope "rld."))

  (define con (rope-concat (rope-concat rope rope2) rope3))

  (draw! con)

  #?=(rope-index con 6)

  (let-values (((left-half right-half) (rope-split! con 6)))
    (draw! left-half)
    (draw! right-half)
    (set! con (rope-concat left-half right-half))
    (draw! con)
    )

  (set! con (rope-insert! con 6 (make-rope " R" "ope")))
  (draw! con)

  (set! con (rope-delete! con 3 8))
  (draw! con)

  0

  )
