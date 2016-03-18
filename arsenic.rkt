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
  (apply append (map ir-block (last script))))

(define (ir-block block)
  (cond
    [(list? block) (ir-command block)]
    [(

(define (ir-command block)
  (case (first block)
    [("gotoX:y:")         (list (list "param" (ir-reporter (list-ref block 1)))
                                (list "param" (ir-reporter (list-ref block 2)))
                                (list "call"  "gotoX:y:"))]
    [else                 (list (list "unknown"))]))
           

(map
  (lambda (child)
    (map ir-script (hash-ref child 'scripts)))
  (filter
    (lambda (child) (hash-has-key? child 'spriteInfo))
    (hash-ref project 'children)))
