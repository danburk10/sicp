;;sicp project 1

(define (sq x)
  (* x x))

;;sqrt, newtons method
(define (sqrt radicand)
  (define (sqrt-iter guess last-guess radicand)
    (define (improve-guess) (average guess (/ radicand guess)))
    (define (good-enough?) (< (abs (- (sq guess) (sq last-guess))) 0.01))
    (define (average x y) (/ (+ x y) 2))
    (if (good-enough?)
        last-guess
        (sqrt-iter (improve-guess)
                   guess
                   radicand)))
      (sqrt-iter 1 0 (abs radicand)))


;;position of object at time t
(define (position a v u t)
  (+
    (* .5 a t t)
    (* v t)
    u))

;;quadratic formula, 1st root
; (-b - sqrt(b^2 - 4ac))/2a
(define (root1 a b c)
  (if (< (sq b) (* 4 a c))
      #f
  (/ (+ (- b)
      (sqrt 
            (- (* b b) 
               (* 4 a c))))
    (* 2 a))))

;;quadratic formula, 2nd root
; (-b - sqrt(b^2 - 4ac))/2a
(define (root2 a b c)
  (if (< (sq b) (* 4 a c))
    #f
  (/ (- (- b)
      (sqrt 
            (- (sq b) 
               (* 4 a c))))
    (* 2 a))))



(define time-to-impact
  (lambda (vertical-velocity elevation)
    (root2 (/ -9.8 2) vertical-velocity elevation)))


(define time-to-height
  (lambda (vertical-velocity elevation target-elevation)
    (time-to-impact vertical-velocity (- target-elevation elevation))))

(define travel-distance-simple
  (lambda (elevation velocity angle)
    (* (v_x velocity angle)
       (time-to-impact (v_y velocity angle) elevation))))

(define v_x
  (lambda (velocity angle)
    (* velocity (cos (deg2rad angle)))))

(define v_y
  (lambda (velocity angle)
    (* velocity (sin (deg2rad angle)))))

(define meters-to-feet
  (lambda (m)
    (/ (* m 39.6) 12)))

(define feet-to-meters
  (lambda (f)
    (/ (* f 12) 39.6)))

(define hours-to-seconds
  (lambda (h)
    (* h 3600)))

(define seconds-to-hours
  (lambda (s)
    (/ s 3600)))

(define pi
  (* 4 (atan 1.0)))

(define deg2rad
  (lambda (deg)
    (/ (* deg pi) 180.)))

