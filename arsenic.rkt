; arsenic
; Next gen phopshorus. Compiler poisonous if ingested.

; Copyright (C) Alyssa Rosenzweig 2016
; ALL RIGHTS RESERVED

#lang racket

(require racket)
(require json)

; Unzipped sb2 directory, from command-line or working directory

(define source-directory
  (let ([arguments (current-command-line-arguments)])
    (if (= (vector-length arguments) 0)
      (current-directory)
      (vector-ref arguments 0))))

; Parses project.json from a given directory

(define (source-project path)
  (let* ([handle (open-input-file (build-path path "project.json"))]
         [json (read-json handle)])
    (close-input-port handle)
     json))

; Project hashmap
(define project (source-project source-directory))

; Generate SSA-form IR for a script

(define (ir-script script)
  (reverse (cadr (ir-codeblock (last script) 0 '()))))

(define (ir-codeblock reporter base source)
  (foldl
    (lambda (command ir)
      (match-let*
          ([(list identifiers source base) ir]
           [(list emission newbase) (ir-head-block command base)]
           [identifier (+ newbase 1)])
          (list 
            (cons identifier identifiers)
            (append (cons (list "label" identifier) emission) source)
            identifier)))
      (list base source base)
      reporter))

(define (ir-head-block block base)
  (case (first block)
    [("doForever")  (ir-cblock block base)]
    [else           (ir-command block base)]))

(define (ir-cblock cblock base)
  (match-let ([(list source args base) (ir-parameters cblock base ir-codeblock #f)])
    (list source base)))

(define (ir-command command cbase)
  (match-let ([(list source args base) (ir-parameters command cbase ir-reporter #t)])
    (list 
      (cons 
        (list "call" (string-append "scratch_" (car command)))
        source)
      base)))

(define (ir-parameters command cbase backend marker)
  (foldl 
    (lambda (reporter ir)
      (match-let*
        ([(list source args base) ir]
         [(list identifier emission consumption) (backend reporter base source)])
        (list
          (if marker (cons (list "param" identifier) emission) emission)
          (cons identifier args)
          (+ base consumption))))
    (list '() '() cbase)
    (cdr command)))
  
(define (ir-reporter reporter base source)
  (cond [(number? reporter) (list (list "constint" reporter) source 0)]
        [(string? reporter) (list (list "conststr" reporter) source 0)]
        [else               (list (list "und")               source 0)]))

(display (map
  (lambda (child)
    (map ir-script (hash-ref child 'scripts)))
  (filter
    (lambda (child) (hash-has-key? child 'spriteInfo))
    (hash-ref project 'children))))
