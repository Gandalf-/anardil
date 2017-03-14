Title: Racket
Date: 2015-11-08 18:25
Category: Programming
Tags: programming, racket, article
Summary: Thoughts on the Racket language

Racket is a dialect of Scheme, which itself is a derivate of Lisp. It's a functional
language, which I find very appealing. A lot can be expressed very concisely and
elegantly. It's not my go-to language for everything, but whenever I have a question about
some mathematical concept it's very easy to frame my question, construct an algorithm to
get me the solution, and then get an answer. Whether it's some sort of algebra, logic
puzzle, or number theory question.

### A simple example
Here's the Sieve of Eratosthenes in Racket:
```Scheme
#lang racket

(provide primes)

; Returns all a list of all prime numbers less than 'limit'
(define (primes limit)
  (let loop ((in (cddr
                   (build-list limit values)))
             (out '(1) ))
    (if (empty? in)
      (reverse out)
      (loop (filter 
              (lambda (x)
                (not
                  (= 0
                     (modulo x (car in)))))
              (cdr in))
            (cons (car in)
                  out))) ))

(primes 10000000)
```

It may seem a little dense, but if you start breaking it down into component parts you'll
quickly see that everything makes sense. We start by creating a list of integers, 3
through the limit as our base list. There's no need to include 1 or 2. For simplicity, 1
is already in our result list, and 2 is not prime. 

From here, we grab the first element, add it to our output list (since it must be a
prime), and then compare it to every other element in the master list. If the `current
prime % current number == 0`, then the current number isn't prime and we remove it from
the list. Rinse and repeat until the master list is empty. The result is an output list
containing all prime numbers less than the argument.

### A step further
Another simple and elegant example is arbitrary precision division using strings as
placeholders. Since a 32 bit floating point only has so many bits of precision, we use
characters and strings to keep track of the whole number and previous digits. We only need
to keep binary representations for the current digit of precision.
```Scheme
#lang racket

(provide divide-precise)

; Divide (/ x y) with 'precision` decimal places of accuracy
(define (divide-precise x y precision)
  
  (define (list-of-string->string x)
    (foldl string-append "" x))
  
  (define (floor-integer x)
    (cdr (member 
          #\. (reverse 
               (string->list 
                (number->string x))))))
  
  (define (quot x y)
    (list-of-string->string
     (map string
          (floor-integer
           (exact->inexact (/ x y))))))
  
  (define (remain x y)
    (- x (* (string->number (quot x y)) y)))
  
  (let loop ((out '() ) (i 0) (x x))
    (if (= i precision)
        (list-of-string->string out)
        (loop (if (= i 0)
                  (cons "." (cons (quot x y) out))
                  (cons (quot x y) out))
              (+ i 1)
              (* (remain x y) 10)))))

(divide-precise 4938492348925 8 250)
```

This might look complicated, but again if we break it down you'll see that there's nothing
too crazy going on here. The basic premise is the same of elementary division, like you
would do with paper and pencil. `(floor-integer)` accomplishes the same function as the
regular `floor()`, but handles string representations of the numbers. The same goes for
`(quot)` and `modulo()`, and `(remain)` and `remainder()`.
