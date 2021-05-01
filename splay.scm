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

(define (search tree x)
  (let loop ((path ())
             (root tree)
             (dir 'root))
    (if (eq? (value root) x)
        (splay (cons (cons dir root) path))
        (if (< x (value root))
            (loop (cons (cons dir root) path) (left  root) 'left)
            (loop (cons (cons dir root) path) (right root) 'right)))))
;; <- bottom   top->
;; (p1 p2 p3 p4 ...)

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

(define (splay path)
  (let ((x (cdr (car path))))
        (if (pair? (cdr path))
            (let ((p (cdr (cadr path))))
              (if (pair? (cddr path))
                  (let ((g (cdr (caddr path))))
                    (splay-2 path x p g))
                  (splay-1 path x p)))
            x)))

(define tree
  '(() 0 (() 1 (() 2 (() 3 (() 4 (() 5 (() 6 (() 7 ())))))))))

(define (print-tree tree)
  (define dummy 0)
  (define (iter tree parent)
    (if (pair? tree)
        (let* ((val (value tree))
               (node #"n~val"))
          (iter (left tree) node)
          (print #"~node [label = \"~|val|\"];")
          (when parent
            (print #"~parent -- ~|node|;"))
          (iter (right tree) node))
        (let ((dum #"dum~dummy"))
          (print #"~dum [style=\"\" label=\"\" height=0.1 width=0.1];")
          (print #"~parent -- ~dum [style=\"\"];")
          (inc! dummy))))
  (with-output-to-process "dot -Tpng -o out.png"
    (^[]
      (print "graph \"\" {")
      (iter tree #f)
      (print "}"))))
