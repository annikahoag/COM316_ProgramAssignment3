; grid-search.ss

(define none -1)
(define n 0)
(define s 1)
(define e 2)
(define w 3)

(define search
  (lambda (grid stop-count)
    (search2 grid 1 stop-count)))

(define search2
  (lambda (grid count stop-count)
    (pause pause-num)
    (let ((x (robot-x))
          (y (robot-y)))
      (display count)
      (newline)
      (draw-visited x y)
      (set-node! grid x y visited)
      (move-robot grid x y 0)
      (draw-moved-robot (robot-x) (robot-y))
      (if (or
            (and (= x (robot-x)) (= y (robot-y)))
            (equal? robot goal)
            (>= count stop-count))
         #f
       ;else
         (search2 grid (+ count 1) stop-count)))))
           
          
(define move-robot
  (lambda (grid x y count)
    (let ((dir (random 4)))
      (cond
        ((and (= dir n) (> x 0) (<= (get-node grid (- x 1) y) free))
           (set! robot (list (- x 1) y)))
        ((and (= dir s) (< x (- num-col-row 1)) (<= (get-node grid (+ x 1) y) free))
           (set! robot (list (+ x 1) y)))
        ((and (= dir w) (> y 0) (<= (get-node grid x (- y 1)) free))
           (set! robot (list x (- y 1))))
        ((and (= dir e) (< y (- num-col-row 1)) (<= (get-node grid x (+ y 1)) free))
           (set! robot (list x (+ y 1))))
        ((> count 100)
           (move-robot-help grid x y 0))
        (else
          (move-robot grid x y (+ count 1)))))))

(define move-robot-help
  (lambda (grid x y count)
    (let ((dir (random 4)))
      (cond
        ((and (= dir n) (> x 0) (< (get-node grid (- x 1) y) obstacle))
           (set! robot (list (- x 1) y)))
        ((and (= dir s) (< x (- num-col-row 1)) (< (get-node grid (+ x 1) y) obstacle))
           (set! robot (list (+ x 1) y)))
        ((and (= dir w) (> y 0) (< (get-node grid x (- y 1)) obstacle))
           (set! robot (list x (- y 1))))
        ((and (= dir e) (< y (- num-col-row 1)) (< (get-node grid x (+ y 1)) obstacle))
           (set! robot (list x (+ y 1))))
        ((> count 100)
           (move-any-dir grid x y))
        (else
          (move-robot grid x y (+ count 1)))))))

(define move-any-dir
  (lambda (grid x y)
    (cond
      ((and (> x 0) (< (get-node grid (- x 1) y) obstacle))
         (set! robot (list (- x 1) y)))  
      ((and (< x (- num-col-row 1)) (< (get-node grid (+ x 1) y) obstacle))
         (set! robot (list (+ x 1) y)))
      ((and (> y 0) (< (get-node grid x (- y 1)) obstacle))
         (set! robot (list x (- y 1))))
      ((and (< y (- num-col-row 1)) (< (get-node grid x (+ y 1)) obstacle))
         (set! robot (list x (+ y 1))))
      (else
        (display "no move")))))

(define pause
  (lambda (count)
    (if (<= count 0)
       0
     ;else
       (pause (- count 1)))))