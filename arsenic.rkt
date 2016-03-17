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

; takes a full script and compiles it, using recursion as necessary

(define (compile-script script)
  (string-join (map compile-block (last script)) "\n"))

; stub
(define (compile-block block)
  (~a block))

(display (map
  (lambda (child)
    (map compile-script (hash-ref child 'scripts)))
  (filter
    (lambda (child) (hash-has-key? child 'spriteInfo))
    (hash-ref project 'children))))
