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

; Pretty print Scratch objects for introspection

(define (pretty-print-sprite sprite)
  (list "Sprite "
        (hash-ref sprite 'objName)))

(define (pretty-print-watcher watcher)
  (list "Watcher "
        (hash-ref watcher 'cmd)
        ", target "
        (hash-ref watcher 'target)))

; Pretty print Scratch project

(display (string-join
 (map 
   (lambda (child)
     (string-join
      (cond
        ((hash-has-key? child 'spriteInfo) (pretty-print-sprite child))
        ((hash-has-key? child 'target)     (pretty-print-watcher child))
        (else                              "Unknown Scratch object found"))
      ""))
   (hash-ref project 'children))
  "\n"))
