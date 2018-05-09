#lang racket
(provide add-active-token def-active-token process-string)
(require srfi/13)

;;Hashtable with tokens and associated functions
(define associations (make-hash))

;;Adds a token with respective function to a hashmap for later use in process-string
(define (add-active-token str function)
  (hash-set! associations str function))

;;Recursively applies first token found in a string appending the results together
(define (process-string str)
  (let ([token (token-to-execute str)])
    (cond
      [(empty? token) str]
      [else (string-append (substring str 0 (string-contains str token))
                           (process-string ((hash-ref associations token)
                                            (substring str (string-contains str token)))))])))

;;token-to-execute decides which token (if any) comes first in the string and returns its position in str
(define (token-to-execute str)
  (let ([token null]
        [position (string-length str)])
    (for ([(key value) associations])
      (let ([key-position (regexp-match-positions (string-append key "[^a-zA-Z0-9]") str)])
        (cond
          [(and key-position (equal? (caar key-position) 0))  (set! position 0) (set! token key)]
          [else (set! key-position (regexp-match-positions (string-append "[^a-zA-Z0-9]" key "[^a-zA-Z0-9]") str))
                (cond
                  [(and key-position (< (caar key-position) position)
                        (set! position (caar key-position))
                        (set! token key))])])));)
    token))

;;def-active-token macro
(define-syntax-rule (def-active-token token str body)
  (hash-set! associations token 
             (lambda str body)))


;;Tokens implementation

;;Local Type Inference
(def-active-token "var" (str)
  (string-append (substring str (+ (string-contains str "new ") 4) (string-contains str "(")) (substring str 3)))

;;String Interpolation
(def-active-token "#" (str)
  (if (not (equal? (second (string->list str)) #\"))
      (string-append "\" + ("
                     (substring str (+ (string-contains str "{") 1) (string-contains str "}"))
                     ") + \""
                     (substring str (+ (string-contains str "}") 1)))
      (substring str 1)))

;;Type Aliases
(def-active-token "alias" (str)
  (let ([alias (string-trim-all (substring str (+ (string-contains str "alias ") 6) (string-contains str "=")))]
        [type (string-trim-all (substring str (+ (string-contains str "=") 1) (string-contains str ";")))])
    (set! str (substring str (+ (string-contains str ";") 1)))
    (string-replace-substring str alias type)))

(define (string-replace-substring str substr newsubstring)
  (let ([regexp-substr (string-append "[^a-zA-Z0-9]" substr "[^a-zA-Z0-9]")])
    (if (regexp-match-positions regexp-substr str)
        (let ([fromindex (caar (regexp-match-positions regexp-substr str))]
              [toindex (cdar (regexp-match-positions regexp-substr str))])
          (string-append (string-append (substring str 0 (+ fromindex 1)) newsubstring)
                         (string-replace-substring (substring str (- toindex 1)) substr newsubstring)))
        str)))

;;Helpers
(define (string-trim-all-left str)
  (if (or (equal? (string-contains str " ") 0) (equal? (string-contains str "\n") 0) (equal? (string-contains str "\t") 0)) ;se tem espaço no inicio
      (string-append (string-trim-all-left (substring str 1)))
      str))

(define (string-trim-all str)
  (list->string (reverse
                 (string->list (string-trim-all-left (list->string (reverse
                                                                    (string->list (string-trim-all-left str)))))))))