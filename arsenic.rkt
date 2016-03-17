; arsenic
; Next gen phopshorus. Compiler poisonous if ingested.

; Copyright (C) Alyssa Rosenzweig 2016
; ALL RIGHTS RESERVED

#lang racket

(require racket)
(require json)

(define source-path
  (vector-ref
    (current-command-line-arguments)
    0))

(define (source-project path)
  (let ([handle (open-input-file path)])
    (let ([json (read-json handle)])
      (close-input-port handle)
      json)))

(source-project source-path)
