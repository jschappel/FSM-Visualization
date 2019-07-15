#lang racket

(require 2htdp/image 2htdp/universe)

(define BOXW 50)

(define BOXH 50)

(define NUMCOLS 10)

(define NUMROWS 10)

(define WIDTH (* NUMCOLS BOXW))

(define HEIGHT (* NUMROWS BOXH))

(define CENTERDOT-IMG (circle 5 "solid" "green"))

(struct posn (x y) #:transparent)
(struct state (box name) #:transparent)

(define (boxnum->posn n)
  (posn (+ (/ BOXW 2) (* (remainder n NUMCOLS) BOXW))
        (+ (/ BOXH 2) (* (quotient n NUMCOLS) BOXH))))

(define (make-escene n img)
  
  (if (= n 0)
      img
      (let* ((ROWNUM (quotient (sub1 n) NUMCOLS))
             (COLNUM (remainder (sub1 n) NUMCOLS))
             (dummy (displayln (format "n is ~s rownumber is ~s colnumber is ~s" (sub1 n) ROWNUM COLNUM))))
        (make-escene (sub1 n)
                     (place-image CENTERDOT-IMG
                                  (+ (/ BOXW 2) (* COLNUM BOXW))
                                  (+ (/ BOXH 2) (* ROWNUM BOXH))
                               
                                  img)))))

(define E-SCENE (make-escene (* NUMCOLS NUMROWS) (empty-scene WIDTH HEIGHT "red")))

(define TOTAL-BOXES (* NUMCOLS NUMROWS))


(define (quantize x y)
  (+ (quotient x BOXW) (* NUMCOLS (quotient y BOXH))))

(struct world (box) #:transparent)


(define INIT-WORLD (world '( )))
(define (draw-state box scn)
  
  (place-image (circle (/ BOXW 2) "solid" "black")
               (posn-x box)
               (posn-y box)
               scn))
(define (draw-states boxes scene)
  (cond[(empty? boxes)
        scene]
       [else (draw-state (car boxes) (draw-states (cdr boxes) E-SCENE))]))

(define (draw-world w)
  (let ((p (map (lambda(x) (boxnum->posn x)) (world-box w))))
  (if (empty? p) E-SCENE
      (draw-states p E-SCENE))))

(define (process-mouse w x y e)
  (if (string=? e "button-down")
      (world (cons (quantize x y) (world-box w)))
      w))

(big-bang
    INIT-WORLD
  (on-draw draw-world)
  (on-mouse process-mouse))





