#lang racket

(provide (struct-out IC))

(struct IC
  (datasheet alts orients fps))
