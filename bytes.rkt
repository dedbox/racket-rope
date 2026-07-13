#lang racket/base

(require racket/match
         racket/port
         rope/define-rope-type
         rope/rope)

(provide (all-defined-out))

(define BYTES-LEAF-LIMIT 512)

(define-rope-type bytes
  (rope-ops BYTES-LEAF-LIMIT
            (λ () #"")
            bytes-length
            bytes-length
            subbytes
            (λ (raws) (apply bytes-append raws))
            bytes-ref))

;; Per-read complexity: O(1).
;; 
;; Total complexity for reading all data: O(n).
(define (open-input-bytes-rope rope)
  (define active-cursor (box (bytes-rope->cursor rope)))
  (define pending-bytes (box #""))

  (define (read-in bstr)
    (when (and (zero? (bytes-length (unbox pending-bytes)))
               (not (bytes-cursor-at-end? (unbox active-cursor))))
      (define cur (unbox active-cursor))
      (define byte (bytes-cursor-peek cur))
      (set-box! pending-bytes (bytes byte))
      (set-box! active-cursor (bytes-cursor-advance cur)))

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
   'bytes-rope-input-port
   read-in
   #f
   close))
