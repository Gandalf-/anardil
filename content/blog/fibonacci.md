Title: Fibonacci
Date: 2016-10-03
Category: Programming
Tags: programming, mathematics
Status: draft
Summary: More than 1,1,2,3,5,8,13

You've probably heard of the Fibonacci sequence. Afterall, it's all over nature
in flowers, pinecones, shells, and leaves. Most of us can name off the first
dozen or so numbers in the sequence too

  1,1,2,3,5,8,13,21,34,...

But have you ever wondered what's beyond those tiny numbers? How about WAY
beyond? Hold on to your seat, this is going to get crazy. 

Behold, the thousandth Fibonacci number

  113796925398360272257523782552224175572745930353730513145086634176691092536145985470146129334641866902783673042322088625863396052888690096969577173696370562180400527049497109023054114771394568040040412172632376

or approximately

  1.13796 x 10^209

Jeez! Physicists think there are only about 4 x 10^81 atoms in the observable
universe. We over shot that by quite a bit.

```Scheme
(define (fibonacci-log-print n name)
  
  (define (fibo-log-print a b p q count)
    (cond ((= count 0) (print-this b name))
          ((even? count)
           (fibo-log-print a b
                           (+ (* p p) (* q q))
                           (+ (* 2 p q) (* q q))
                           (/ count 2)))
          (else (fibo-log-print (+ (* b q) (* a q) (* a p))
                                (+ (* b p) (* a q))
                                p q
                                (- count 1)))))
  
  (fibo-log-print 1 0 0 1 n))

(fibonacci-log-print 100000000 "Data/log-hundred-million.txt")
```
