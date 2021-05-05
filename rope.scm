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
  (if (and (<= (rope-calc-weight! node) i)
           (pair? (splay-right node)))
      (rope-index (splay-right node) (- i (rope-calc-weight! node)))
      (if (pair? (splay-left node))
          (rope-index (splay-left node) i)
          (string-ref (splay-value node) i))))

(define (rope-concat node1 node2)
  (make-splay-tree node1 #f node2))

;; The original node may get modified.
;; Returns the left and the right trees.
(define (rope-split! node i)
  (define (new-tree rights)
    (if (pair? (cdr rights))
        (make-splay-tree (car rights) #f (new-tree (cdr rights)))
        (car rights)))

  (define (iter node i rights)
    (if (and (< (rope-calc-weight! node) i)
             (pair? (splay-right node)))
        (iter (splay-right node) (- i (rope-calc-weight! node)) rights)
        (if (pair? (splay-left node))
            (iter (splay-left node) i
                  (if (pair? (splay-right node))
                      (let ((right (splay-right node)))
                        (set! (splay-value node) #f)
                        (set! (splay-right node) ())
                        (cons right rights))
                      rights))
            (new-tree rights))))

  (let ((right (iter node i ())))
    (values node right)))


(define (rope-insert! node i new-node)
  (let-values (((left right) (rope-split! node i)))
    (rope-concat left (rope-concat new-node right))))

(define (rope-delete! node i j)
  (let-values (((node right) (rope-split! node j)))
    (let-values (((node middle) (rope-split! node i)))
      (rope-concat node right))))
