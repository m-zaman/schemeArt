(speed 0)
(width 1.5)
(pencolor "#FDB515")
(fillcolor "#004080")
(ht)
(define (cadr s) (car (cdr s)))

(define (setp x)
	(setposition (car x) (cadr x))
)

(define (coolsquare a b c d p x bool)
	(if (not (= x 60)) 
	(begin
	(begin_fill)
	(draw a b c)
	(setp d)
	(setp a)
	(end_fill)
	(coolsquare 
		(changeco a b p) 
		(changeco b c p) 
		(changeco c d p) 
		(changeco d a p) 
		p (+ x 1) (- 1 bool)) 
	))
)

(define (draw a b c)
	(penup)	
	(setp a)
	(pendown)
	(setp b)
	(setp c)
)

(define (changeco a b p)
(cons 
	(+ (* (car a) p) (* (car b) (- 1 p))) 
	(cons (+ (* (cadr a) p) (* (cadr b) (- 1 p))) 
			nil))
)


(define (cooltri a b c p x bool)
	(if (not (= x 27)) 
	(begin 
	(begin_fill)
	(draw a b c)
	(setp a)
	(end_fill)
	(cooltri 
		(changeco a b p) 
		(changeco b c p) 
		(changeco c a p) 
		p (+ x 1) (- 1 bool))
	))

)


(define (fun1 l a b c)
	(define h (calcshift a b))
	(define g (calcshift a c))
	(define (fun2 z m)
		(if (< m 4)
		(begin (define w (shift z (list (* m 300) (* l -300)))) (cooltri w (shift w h) (shift w g) .9 1 1)
		 (fun2 z (+ m 1))))
	)
	(fun2 a 0)

	(if (< l 2) (fun1 (+ l 1) a b c)) 
)

(define (fun3 l a b c d)
	(define h (calcshift a b))
	(define g (calcshift a c))
	(define f (calcshift a d))
	(define (fun4 z m)
		(if (< m 4)
		(begin (define w (shift z (list (* m 300) (* l -300)))) (coolsquare w (shift w h) (shift w g) (shift w f) .93 1 1)
		 (fun4 z (+ m 1))))
	)
	(fun4 a 0)

	(if (< l 2) (fun3 (+ l 1) a b c d)) 
)


(define (shift a b)
	(list (+ (car a) (car b)) (+ (cadr a) (cadr b))))

(define (calcshift a b)
	(list (- (car b) (car a)) (- (cadr b) (cadr a))))

(fun3 0 '(-450 200) '(-300 350) '(-150 200) '(-300 50))
(fun1 0 '(-450 200) '(-450 350) '(-300 350))
(fun1 0 '(-450 200) '(-450 50) '(-300 50))
(fun1 0 '(-150 200) '(-150 350) '(-300 350))
(fun1 0 '(-300 50) '(-150 200) '(-150 50)))










