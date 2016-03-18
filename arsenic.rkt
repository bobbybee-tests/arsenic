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

; Generate IR for a script

(define (ir-script script)
  ;(apply append (map ir-command (last script))))
  ;(ir-command (car (last script))))
  ;(map ir-command (last script)))
  (foldl
    (lambda (command ir)
      (match-let*
        ([(list source base) ir]
         [(list emission newbase) (ir-command command base)])
        (list (append source emission) newbase)))
    (list '() 0)
    (last script)))

(define (ir-command command cbase)
  (foldl 
    (lambda (reporter ir)
      (match-let*
        ([(list source base) ir]
         [(list identfier emission consumption) (ir-reporter reporter base)])
        (list (append source emission) (+ base consumption))))
    (list '() cbase)
    (cdr command)))
  
; stub

(define (ir-reporter reporter base)
  (let ([identifier (string-append "temp" (number->string base))])
    (list identifier (list "=" identifier "42") 1)))

(map
  (lambda (child)
    (map ir-script (hash-ref child 'scripts)))
  (filter
    (lambda (child) (hash-has-key? child 'spriteInfo))
    (hash-ref project 'children)))
