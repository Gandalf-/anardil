Title: Fibonacci
Date: 2016-10-03
Category: Programming
Tags: programming, racket, mathematics
Status: published
Summary: More than 1,1,2,3,5,8,13

You've probably heard of the Fibonacci sequence. After all, it's all over nature
in flowers, pinecones, shells, and leaves. Most of us can name off the first
dozen or so numbers in the sequence too

> 1, 1, 2, 3, 5, 8, 13, 21, 34, ...

But have you ever wondered what's beyond those tiny numbers? How about WAY
beyond? Hold on to your seat, this is going to get crazy.

Behold, the thousandth Fibonacci number

> 1137969253983602722575237825522241755727459303537305131450866341766910925361
> 45985470146129334641866902783673042322088625863396052888690096969577173696
> 370562180400527049497109023054114771394568040040412172632376

or approximately

> 1.13796 x 10^209

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

But that's peanuts compared to these enormous numbers:

| N'th | Digits | Link |
| ----------- | --- | --- |
|   1 Million | &nbsp;&nbsp;    208,988 | &nbsp;&nbsp; [Link 0.2 MB](/extra/fib-001-million.txt)
|   3 Million | &nbsp;&nbsp;    626,963 | &nbsp;&nbsp; [Link 0.6 MB](/extra/fib-003-million.txt)
|   4 Million | &nbsp;&nbsp;    835,951 | &nbsp;&nbsp; [Link 0.8 MB](/extra/fib-004-million.txt)
|   5 Million | &nbsp;&nbsp;  1,044,938 | &nbsp;&nbsp; [Link   1 MB](/extra/fib-005-million.txt)
|  10 Million | &nbsp;&nbsp;  2,089,877 | &nbsp;&nbsp; [Link   2 MB](/extra/fib-010-million.txt)
| 100 Million | &nbsp;&nbsp; 20,898,764 | &nbsp;&nbsp; [Link  20 MB](/extra/fib-100-million.txt)
