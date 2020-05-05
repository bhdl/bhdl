#lang racket

;; common utilities that are specific for this project

(provide (struct-out Point))

;; TODO use Point for all locations
(struct Point
  (x y)
  #:prefab)


