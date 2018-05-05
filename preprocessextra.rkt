#lang racket
(provide add-active-token def-active-token process-string)
(require srfi/13)

;;Hashtable with tokens and functions
(define associations (make-hash))

;;Adds a token with respective function to a hashmap for later use in process-string
(define (add-active-token str function)
  (hash-set! associations str function))

;;process-string
(define (process-string str)
  (if (equal? str "")
      ""
      (if (not (empty? (token-to-execute str)))
            (string-append (substring str 0 (string-contains str (token-to-execute str)))
                                     (process-string ((hash-ref associations (token-to-execute str))
                                                      (substring str (string-contains str (token-to-execute str))))))
            str))
  )

;;token-to-execute decides which token (if any) comes first in the string 
(define (token-to-execute str)
  (let ([token null] [position (string-length str)])
    (for ([(key value) associations]) ;;For each token and function do
      (when (string-contains str key) ;;If token exists in string
        (when (< (string-contains str key) position) ;;If token's position is lower than our lowest position
          (set! position (string-contains str key))  ;;Define our lowest position as this token's position
          (set! token key))))                        ;;Also, define token to execute as this token
    token))

;;def-active-token macro
(define-syntax-rule (def-active-token token str body)
  (hash-set! associations token 
             (lambda str body)))

;;Extra Tokens implementation

;;Getters and Setters Token
;;Exemplo: $int n = 2; -> int n = 2;
;;                        public void setN(int n) {
;;                            this.n = n;
;;                        }
;;                        public int getN() {
;;                            return n;
;;                        }
(def-active-token "$" (str)
  (let ([type (substring str 1 (string-contains str " "))]
        [var-name (string-trim-all (substring str (+ (string-contains str " ") 1)
                             (if (string-contains str "=")
                                 (min (string-contains str "=") (string-contains str ";"))
                                 (string-contains str ";"))))])
    (string-append (substring str 1) (implement-setter type var-name) (implement-getter type var-name)))
   )

(define (string-trim-all-left str)
  (if (or (equal? (string-contains str " ") 0) (equal? (string-contains str "\n") 0) (equal? (string-contains str "\t") 0)) ;se tem espaço no inicio
      (string-append (string-trim-all-left (substring str 1)))
      str)
  )

(define (string-trim-all str)
  (list->string (reverse
                 (string->list (string-trim-all-left (list->string (reverse
                                                                    (string->list (string-trim-all-left str))))))))
  )

(define (implement-setter type var-name)
  (string-append "\npublic void set" (string-titlecase var-name) "(" type " " var-name ") {\n\tthis." var-name " = " var-name ";\n}")
  )

(define (implement-getter type var-name)
  (string-append "\npublic " type " get" (string-titlecase var-name) "() {\n\treturn " var-name ";\n}");
  )


;;#include Token
;;Given #include <file-name> writes the contenct of file-name and removes #include <file-name> line
;;If #include "file-name", the file will be the one in source's directory
;;If #include <file-name>, the file will be the one in #include directory
;;For easier implementation, assume that "file-name" is the same as <file-name>
;;So we search for file-name in source's directory
(def-active-token "#include" (str)
  (let ([file-name (substring str (+ (string-contains str "<") 1) (string-contains str ">"))])
    (if (file-exists? (string-append (path->string (current-directory)) file-name))
        (string-append (file->string (string-append (path->string (current-directory)) file-name))
                       (substring str (+ (string-contains str ">") 1)))
        "No such file in directory.")))