#lang racket

(define (piece str str2)
  (substring str 0 3))

(piece "hello" "hithere")


(if (< 2 3)
    "math works"
    "math broke")

(define (reply-non-string s)
    (if (and (string? s) (string-prefix? s "hello "))
    "Hi!"
    "Heyyy!"))

(reply-non-string "hello there")

(define (reply-more s)
  (cond
    [(string-prefix? s "hello: ")
     "hi!"]
    [(string-prefix? s "goodbye ")
     "bye!"]
    [(string-suffix? s "?")
     "I don't know"]
    [else "huh?"]))

(reply-more "goodbye ")