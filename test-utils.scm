(define-module test-utils
  (use gauche.process)
  (use splay)
  (export
   print-tree))

(select-module test-utils)

(define (print-tree tree :optional (outpath "out.png"))
  (define id 0)
  (define (iter tree parent)
    (if (pair? tree)
        (let* ((val (splay-value tree))
               (node #"n~id"))
          (inc! id)
          (iter (splay-left tree) node)
          (print #"~node [label = \"'~|val|'\"];")
          (when parent
            (print #"~parent -- ~|node|;"))
          (iter (splay-right tree) node))
        (let ((dum #"dum~id"))
          (print #"~dum [style=\"\" label=\"\" height=0.1 width=0.1];")
          (print #"~parent -- ~dum [style=\"\"];")
          (inc! id))))

  (with-output-to-process #"dot -Tpng -o ~outpath"
    (^[]
      (print "graph \"\" {")
      (iter tree #f)
      (print "}"))))
