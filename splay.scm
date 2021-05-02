; Splay tree
(use gauche.process)

(define left car)
(define value cadr)
(define right caddr)

(define (make-tree left value right)
  (list left value right))

(define (zig-right x p)
  (make-tree (left x) (value x) (make-tree (right x) (value p) (right p))))

(define (zig-left x p)
  (make-tree (make-tree (left p) (value p) (left x)) (value x) (right x)))

(define (zig-zig-right x p g)
  (make-tree (left x) (value x)
             (make-tree (right x) (value p)
                        (make-tree (right p) (value g)
                                   (right g)))))

(define (zig-zig-left x p g)
  (make-tree (make-tree (make-tree (left p) (value g) (left g))
                        (value p)
                        (left x))
             (value x) (right x)))

(define (zig-zag-right x p g)
  (let ((a (left p))
        (b (left x))
        (c (right x))
        (d (right g)))
    (make-tree (make-tree a (value p) b)
               (value x)
               (make-tree c (value g) d))))

(define (zig-zag-left x p g)
  (let ((a (right p))
        (b (right x))
        (c (left x))
        (d (left g)))
    (make-tree (make-tree d (value g) c)
               (value x)
               (make-tree b (value p) a))))

;; path: ( (direction . node) ... )
;; directoin: 'left or 'right or 'root

;; search only works on numerical data
;; returns path
(define (search tree x)
  (let loop ((path ())
             (root tree)
             (dir 'root))
    (if (eq? (value root) x)
        (splay (cons (cons dir root) path))
        (if (< x (value root))
            (loop (cons (cons dir root) path) (left  root) 'left)
            (loop (cons (cons dir root) path) (right root) 'right)))))

(define (splay-1 path x p)
  (let ((d1 (car (car   path)))
        (d2 (car (cadr  path))))
    (let ((new-path
           (if (eq? d1 'left)
               (cons (cons d2 (zig-right x p)) (cddr path))
               (cons (cons d2 (zig-left  x p)) (cddr path)))))
      (splay new-path))))

(define (splay-2 path x p g)
  (let ((d1 (car (car   path)))
        (d2 (car (cadr  path)))
        (d3 (car (caddr path))))
    (let ((new-path
           (if (eq? d1 'left)
               (if (eq? d2 'left)
                   (cons (cons d3 (zig-zig-right x p g)) (cdddr path))
                   (cons (cons d3 (zig-zag-left  x p g)) (cdddr path)))
               (if (eq? d2 'right)
                   (cons (cons d3 (zig-zig-left  x p g)) (cdddr path))
                   (cons (cons d3 (zig-zag-right x p g)) (cdddr path))))))
      (splay new-path))))

;; path -> tree
(define (splay path)
  (let ((x (cdr (car path))))
        (if (pair? (cdr path))
            (let ((p (cdr (cadr path))))
              (if (pair? (cddr path))
                  (let ((g (cdr (caddr path))))
                    (splay-2 path x p g))
                  (splay-1 path x p)))
            x)))

;; path -> path
(define (go-left path)
  (let ((current (cdar path)))
    (if (pair? (left current))
        (let loop ((path (cons (cons 'left (left current)) path))
                   (current (left current)))
          (if (pair? (right current))
              (loop (cons (cons 'right (right current)) path)
                    (right current))
              path))
        (if (eq? (caar path) 'right)
            (cdr path)
            (cddr path)))))

(define (go-right path)
  (let ((current (cdar path)))
    (if (pair? (right current))
        (let loop ((path (cons (cons 'right (right current)) path))
                   (current (right current)))
          (if (pair? (left current))
              (loop (cons (cons 'left (left current)) path)
                    (left current))
              path))
        (if (eq? (caar path) 'left)
            (cdr path)
            (cddr path)))))

;; tree -> tree
(define (insert-left tree val)
  (make-tree (left tree) val (make-tree () (value tree) (right tree))))

(define (insert-right tree val)
  (make-tree (make-tree (left tree) (value tree) ()) val (right tree)))

;;;;;;

(define (print-tree tree)
  (define id 0)
  (define (iter tree parent)
    (if (pair? tree)
        (let* ((val (value tree))
               (node #"n~id"))
          (inc! id)
          (iter (left tree) node)
          (print #"~node [label = \"~|val|\"];")
          (when parent
            (print #"~parent -- ~|node|;"))
          (iter (right tree) node))
        (let ((dum #"dum~id"))
          (print #"~dum [style=\"\" label=\"\" height=0.1 width=0.1];")
          (print #"~parent -- ~dum [style=\"\"];")
          (inc! id))))
  (with-output-to-process "dot -Tpng -o out.png"
    (^[]
      (print "graph \"\" {")
      (iter tree #f)
      (print "}"))))


;;;;;

(define (test)
  (define tree
    '(() "h" (() "e" (() "l" (() "l" (() "o" (() " " (() "w" (() "o" ())))))))))

  (define (go tree . ops)
    (splay (fold (^[p tree] (p tree)) `((root . ,tree))
                             ops)))

  (print-tree
   (insert-left (go (insert-right
                     (go tree go-right go-right go-right go-left) "x")
                    go-right go-right go-right)
                "y")))
