(define-module rope
  (use splay)
  (export
   make-rope
   rope-calc-weight!
   rope-index
   rope-concat
   rope-split!
   rope-insert!
   rope-delete!))

(select-module rope)

(define (make-rope left-str :optional (right-str #f))
  (make-splay-tree
   (make-splay-tree () left-str ())
   #f
   (and right-str (make-splay-tree () right-str ()))))

(define (rope-calc-weight! node)
  (define (sum-weight)
    (let loop ((total 0)
               (node (splay-left node)))
      (if (pair? node)
          (loop (+ total (rope-calc-weight! node)) (splay-right node))
          total)))

  (if (or (pair? (splay-left node)) (pair? (splay-right node)))
      (or (splay-value node)
          (let ((weight (sum-weight)))
            (set! (splay-value node) weight)
            weight))
      (string-length (splay-value node))))

(define (rope-index node i)
  (define (iter node i path)
    (if (and (<= (rope-calc-weight! node) i)
             (pair? (splay-right node)))
        (iter (splay-right node) (- i (rope-calc-weight! node))
              (cons (cons 'right (splay-right node)) path))
        (if (pair? (splay-left node))
            (iter (splay-left node) i (cons (cons 'left (splay-left node)) path))
            (values (string-ref (splay-value node) i) (cdr path)))))
  (iter node i `((root . ,node))))

(define (rope-concat node1 node2)
  (make-splay-tree node1 #f node2))

;; The original node may get modified.
;; Returns the left and the right trees.
(define (rope-split! node i)
  (define (new-tree rights)
    (if (pair? (cdr rights))
        (make-splay-tree (car rights) #f (new-tree (cdr rights)))
        (car rights)))

  (define (iter node i rights parent new-root)
    (if (and (< (rope-calc-weight! node) i)
             (pair? (splay-right node)))
        (iter (splay-right node) (- i (rope-calc-weight! node))
              rights node new-root)
        (if (pair? (splay-left node))
            (begin
              (set! (splay-value node) #f)
              (if parent
                  (set! (splay-right parent) (splay-left node))
                  (set! new-root (splay-left node)))
              (iter (splay-left node) i
                    (if (pair? (splay-right node))
                        (let ((right (splay-right node)))
                          (set! (splay-value node) #f)
                          (set! (splay-right node) ())
                          (cons right rights))
                        rights)
                    parent new-root))
            (values new-root (new-tree rights)))))

  (let-values (((node path) (rope-index node i)))
    (let ((splayed (splay path (^n (set! (splay-value n) #f)))))
      (let-values (((left right) (iter splayed i () #f node)))
        (values left right)))))


(define (rope-insert! node i new-node)
  (let-values (((left right) (rope-split! node i)))
    (rope-concat left (rope-concat new-node right))))

(define (rope-delete! node i j)
  (let-values (((node right) (rope-split! node j)))
    (let-values (((node middle) (rope-split! node i)))
      (rope-concat node right))))
