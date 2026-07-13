#lang racket/base

(require racket/match
         racket/port
         rope/define-rope-type
         rope/rope)

(provide (all-defined-out))

(define STRING-LEAF-LIMIT 512)

(define string-rope-ops
  (rope-ops STRING-LEAF-LIMIT
            (λ () "")
            string-length
            string-length
            substring
            (λ (raws) (apply string-append raws))
            string-ref))

(define-rope-type string string-rope-ops)

;; Per-read complexity: O(k), where k is the number of bytes transferred in that call.
;; 
;; Total complexity for reading all data: O(n * m), where m is the average number of bytes per
;; character.
;;
;; Given that m is typically a small constant (e.g., UTF-8 characters are 1 - 4 bytes), the per-call
;; complexity can be considered O(1), and the total compexity can be considered O(n).
(define (open-input-string-rope rope)
  (define active-cursor (box (string-rope->cursor rope)))
  (define pending-bytes (box #""))

  (define (read-in bstr)
    (when (and (zero? (bytes-length (unbox pending-bytes)))
               (not (string-cursor-at-end? (unbox active-cursor))))
      (define cur (unbox active-cursor))
      (define char (string-cursor-peek cur))
      (set-box! pending-bytes (string->bytes/utf-8 (string char)))
      (set-box! active-cursor (string-cursor-advance cur)))

    (match (unbox pending-bytes)
      [#"" eof]
      [current-buffer
       (let* ([req-len (bytes-length bstr)]
              [buf-len (bytes-length current-buffer)]
              [transfer-len (min req-len buf-len)]
              [consumed (subbytes current-buffer 0 transfer-len)]
              [retained (subbytes current-buffer transfer-len)])
         (bytes-copy! bstr 0 consumed)
         (set-box! pending-bytes retained)
         transfer-len)]))

  (define (close)
    (set-box! active-cursor #f)
    (set-box! pending-bytes #""))

  (make-input-port/read-to-peek
   'string-rope-input-port
   read-in
   #f
   close))
