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
  (apply append (map ir-command (last script))))

(define (ir-command command)
  (cdr command))

; stub

(define (ir-reporter reporter base)
  (values (string-append "temp" base) (list "=" "tempN" "42") (+ base 1)))

(map
  (lambda (child)
    (map ir-script (hash-ref child 'scripts)))
  (filter
    (lambda (child) (hash-has-key? child 'spriteInfo))
    (hash-ref project 'children)))
