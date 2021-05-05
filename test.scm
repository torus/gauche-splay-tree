(use splay)
(use test-utils)

;;;;;

(define (main . args)
  (define tree
    '(() "h" (() "e" (() "l" (() "l" (() "o" (() " " (() "w" (() "o" ())))))))))

  (define (go tree . ops)
    (splay (fold (^[p tree] (p tree)) `((root . ,tree))
                             ops)))

  (print-tree
   (splay-insert-left (go (splay-insert-right
                           (go tree splay-go-right splay-go-right
                               splay-go-right splay-go-left) "x")
                          splay-go-right splay-go-right splay-go-right)
                "y")))
