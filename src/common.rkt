#lang racket

;; common utilities that are specific for this project

(provide (struct-out Point)
         (struct-out NamedPoint)
         point->named-point)

;; TODO use Point for all locations
(struct Point
  (x y)
  #:prefab)

(struct NamedPoint
  (name x y)
  #:prefab)


(define (point->named-point p name)
  (match p
    [(Point x y) (NamedPoint name x y)]))
