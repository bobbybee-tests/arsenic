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
  (reverse (car (foldl
    (lambda (command ir)
      (match-let*
        ([(list source base) ir]
         [(list emission newbase) (ir-command command base)])
        (list (cons emission source) newbase)))
    (list '() 0)
    (last script)))))

(define (ir-command command cbase)
  (match-let ([(list source args base) (ir-parameters command cbase)])
    (list 
      (cons 
        (list "call" (string-append "scratch_" (car command)))
        source)
      base)))

(define (ir-parameters command cbase)
  (foldl 
    (lambda (reporter ir)
      (match-let*
        ([(list source args base) ir]
         [(list identifier emission consumption) (ir-reporter reporter base)])
        (list 
          (cons emission source)
          (cons identifier args)
          (+ base consumption))))
    (list '() '() cbase)
    (cdr command)))
  
(define (ir-reporter reporter base)
  (if (number? reporter)
    (list reporter '() 0) 
    (list base (list (list "=" base "42")) 1)))

(display (map
  (lambda (child)
    (map ir-script (hash-ref child 'scripts)))
  (filter
    (lambda (child) (hash-has-key? child 'spriteInfo))
    (hash-ref project 'children))))
