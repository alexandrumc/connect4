#lang racket

(require racket/include)
(include "connect4-test.rkt")

(define RED 1)
(define YELLOW 2)
(define EMPTY 0)
(define INFINITY 99999)

;; Task pregătitor
(define init-state
  (λ (height width player)
    (list player (init-board height width))
    ))

(define init-board
  (λ (height width)
    (make-list width (build-list height (λ (x) 0))
     )))

(define filtru
  (λ (x)
    (if (null? x)
        #f
        #t)))

(define modified-member
  (λ (x)
    (if (equal? (or (member 1 x) (member 2 x)) #f)
        #t
        #f)))


(define is-empty-board?
  (λ (board)
    (andmap (λ (lst) (modified-member lst)) board)))

(define get-height
  (λ (board)
    (length (car board))))

(define get-width
  (λ (board)
     (length board)))

(define (get-position board index)
    (if (equal? index 0)
        (car board)
        (get-position (cdr board) (- index 1))))
        
(define get-disc
  (λ (board position)
    (get-position (get-position board (car position)) (cdr position))
    )) 

(define get-player
  (λ (state)
    (car state)))

(define get-board
  (λ (state)
    (second state)))

(define state-test
    '(2 ((0 0 0 0 0 0 )
      (0 0 0 0 0 0)
      (1 2 1 1 2 1)
      (2 1 1 2 1 0)
      (1 0 0 0 0 0)
      (2 2 1 0 0 0)
      (2 2 0 0 0 0))))

;; Task 1 a) - Determinarea acțiunilor posibile
(define get-available-actions
  (λ (board)
    (get-actions board '(0) '())))

(define (has-empty-place lst)
  (if (equal? (member 0 lst) #f)
      #f
      #t))

(define (increment-list list)
  (map (λ (x) (+ x 1)) list))

(define (get-actions list count result)
  (if (null? list)
      result
      (if (has-empty-place (car list))
          (get-actions (cdr list) (increment-list count) (append result count))
          (get-actions (cdr list) (increment-list count) result))))
          
  
;; Task 1 b) - Aplicarea unei acțiuni
(define (get-left lst index)
  (if (equal? index 0)
      '()
      (if (equal? index 1)
          (list (car lst))
          (cons (car lst) (get-left (cdr lst) (- index 1))))))
      
(define (get-right lst count index)
  (if (equal? count index)
      (cdr lst)
      (get-right (cdr lst) (+ count 1) index)))      

(define (get-column board index)
  (list-ref board index))

(define (put-disc color column result index)
  (if (equal? index 0)
      (append result (cons color (cdr column)))
      (put-disc color (cdr column) (append result (list (car column))) (- index 1))))


(define (get-index column index)
  (if (equal? (car column) 0)
      index
      (get-index (cdr column) (+ index 1)))) 

(define apply-action
  (λ (state action)
    (if (equal? action 0)
        (if (equal? (get-player state) 1)
            (list 2 (append (list (put-disc 1 (list-ref (get-board state) action) '()
                                            (get-index (list-ref (get-board state) action) 0)))
                    (get-right (get-board state) 0 0)))
             (list 1 (append (list (put-disc 2 (list-ref (get-board state) action) '()
                                            (get-index (list-ref (get-board state) action) 0)))
                    (get-right (get-board state) 0 0))))
        
        (if (equal? action (- (get-width (get-board state)) 1))
            (if (equal? (get-player state) 1)
                (list 2 (append (get-left (get-board state) action)
                                (list (put-disc 1 (list-ref (get-board state) action) '()
                                            (get-index (list-ref (get-board state) action) 0)))))
                (list 1 (append (get-left (get-board state) action)
                                (list (put-disc 2 (list-ref (get-board state) action) '()
                                            (get-index (list-ref (get-board state) action) 0))))))
            (if (equal? (get-player state) 1)
                (list 2 (append (get-left (get-board state) action)
                        (list (put-disc 1 (list-ref (get-board state) action) '()
                                            (get-index (list-ref (get-board state) action) 0)))
                        (get-right (get-board state) 0 action)))
                (list 1 (append (get-left (get-board state) action)
                        (list (put-disc 2 (list-ref (get-board state) action) '()
                                            (get-index (list-ref (get-board state) action) 0)))
                        (get-right (get-board state) 0 action))))))))


  

(define apply-actions
  (λ (state actions)
    (foldl (λ (x v)
             (apply-action v x)) state actions)))

;; Task 1 c) - Verificarea stării finale
(define (is-full-column? column)
  (if (equal? (member 0 column) #f)
      #t
      #f))

(define (is-full-board? board)
  (andmap is-full-column? board))

(define (win lst count player)
  (if (equal? count 4)
      player
      (if (null? lst)
          #f
          (if (equal? (car lst) player)
              (win (cdr lst) (+ count 1) player)
              (win (cdr lst) 0 player)))))

(define (win-board board player)
  (ormap (λ (x) (win x 0 player)) board))

(define (win-on-column lst count player)
  (if (equal? count 4)
      #t
      (if (null? lst)
          #f
          (if (equal? count 3)
              (if (equal? (car lst) 0)
                  (win-on-column (cdr lst) (+ count 1) player)
                  (win-on-column (cdr lst) 0 player))
              (if (equal? (car lst) player)
                  (win-on-column (cdr lst) (+ count 1) player)
                  (win-on-column (cdr lst) 0 player))))))

(define (find-win-on-columns board player index)
  (if (null? board)
      #f
      (if (equal? (win-on-column (car board) 0 player) #t)
          index
          (find-win-on-columns (cdr board) player (+ index 1)))))


      
(define (win-on-line lst count index-col player)
  (if (equal? count 4)
      (- index-col 1)
      (if (null? lst)
          #f
          (if (equal? count 3)
              (if (equal? (car lst) 0)
                  (win-on-line (cdr lst) (+ count 1) (+ index-col 1) player)
                  (win-on-line (cdr lst) 0 (+ index-col 1) player))
              (if (equal? (car lst) player)
                  (win-on-line (cdr lst) (+ count 1) (+ index-col 1) player)
                  (win-on-line (cdr lst) 0 (+ index-col 1) player))))))

(define (find-win-on-line board player)
  (ormap (λ (x)
           (if (equal? (win-on-line x 0 0 player) #f)
               #f
               (if (equal? (index-of (lined-board board (get-height board) '() 0) x) 0)
                   #t
                   (if (equal? (list-ref (list-ref (lined-board board (get-height board) '() 0)
                       (- (index-of (lined-board board (get-height board) '() 0) x) 1)) (win-on-line x 0 0 player)) 0)
                       #f
                       (win-on-line x 0 0 player)))))
               (lined-board board (get-height board) '() 0)))

  

(define (lined-board board height result count)
  (if (equal? count height )
      result
      (lined-board board height (append result (list (map (λ (x)
             (list-ref x count)) board))) (+ count 1))))

(define (form-diag2 board pos column res)
  (if (equal? column -1)
      res
      (if (equal? pos -1)
          res
          (form-diag2 board (- pos 1) (- column 1) (append res (list (list-ref (list-ref board column) pos)))))))

(define (get-diag2 board pos column width res)
  (if (equal? column width)
      res
      (get-diag2 board pos (+ column 1) width (append res (list (form-diag2 board pos column '()))))))

(define (form-diag1 board pos column height res)
  (if (equal? column -1)
      res
      (if (equal? pos height)
          res
          (form-diag1 board (+ pos 1) (- column 1) height (append res (list (list-ref (list-ref board column) pos)))))))

(define (get-diag1 board pos column width res)
  (if (equal? column width)
      res
      (get-diag1 board pos (+ column 1) width (append res (list (form-diag1 board pos column (get-height board) '()))))))

(define (get-diagonals board)
  (append (get-diag1 board 0 3 (get-width board) '()) (get-diag1 (reverse board) 0 3 (get-width board) '())
          (get-diag2 board (- (get-height board) 1) 3 (get-width board) '())
          (get-diag2 (reverse board) (- (get-height board) 1) 3 (get-width board) '())))


(define is-game-over?
  (λ (state)
    (if (equal? (or (win-board (get-board state) 1) (win-board (get-board state) 2)
                    (win-board (lined-board (get-board state) (get-height (get-board state)) '() 0 ) 1)
                    (win-board (lined-board (get-board state) (get-height (get-board state)) '() 0 ) 2)
                    (win-board (get-diagonals (get-board state)) 1)
                    (win-board (get-diagonals (get-board state)) 2)) #f)
        (if (equal? (is-full-board? (get-board state)) #t)
                3
                #f)
        (or (win-board (get-board state) 1) (win-board (get-board state) 2)
            (win-board (lined-board (get-board state) (get-height (get-board state)) '() 0 ) 1)
            (win-board (lined-board (get-board state) (get-height (get-board state)) '() 0 ) 2)
            (win-board (get-diagonals (get-board state)) 1)
            (win-board (get-diagonals (get-board state)) 2)))))
    

;; Task 2 - Euristică simplă
(define select-random-action
  (λ (state rand-gen)
    (list-ref (get-available-actions (get-board state))
    (random (length (get-available-actions
                 (get-board state))) rand-gen))))


(define (test-action state action player)
  (if (equal? (is-game-over? (apply-action state action)) player)
      action
      #f))

(define (verify-actions state actions player)
  (ormap (λ (x) (test-action state x player)) actions))

(define get-action
  (λ (state)
    (if (equal? (verify-actions state (get-available-actions (get-board state)) (get-player state)) #f)
        (if (equal? (verify-actions (cons (- 3 (get-player state)) (cdr state))
                                    (get-available-actions (get-board state)) (- 3  (get-player state))) #f)
            (select-random-action state random)
            (verify-actions (cons (- 3 (get-player state)) (cdr state)) (get-available-actions (get-board state))
                                  (- 3 (get-player state))))
        (verify-actions state (get-available-actions (get-board state)) (get-player state)))))

(define AI #f) ;Trebuie modificat în #t după ce se implementează play-game

(define play-game
  (λ (state strategy1 strategy2)
    (play-game-bux state strategy1 strategy2 '())))


(define (play-game-bux state strategy1 strategy2 result) 

  (let* ((act1 (apply-action state ((car strategy1) state (cdr strategy1))))
      (act2 (apply-action act1 ((car strategy2) act1 (cdr strategy2))))
      (mov1 (list ((car strategy1) state (cdr strategy1))))
      (mov2 (list ((car strategy2) act1 (cdr strategy2)))))
    
          (if (equal? (is-game-over? act1) #f)
              (if (equal? (is-game-over? act2) #f)
                  (play-game-bux act2 strategy1 strategy2 (append result mov1 mov2))
                  (if (equal? (is-game-over? act2) 3)
                      (cons result 3)
                      (cons result (is-game-over? act2))))
              (if (equal? (is-game-over? act1) 3)
                  (cons result 3)
                  (cons result (is-game-over? act1))))))
              
                
                                                                                                 

;; Bonus 
(define evaluate
  (λ (state)
    ;TODO
    'your-code-here))

(define negamax
  (λ (state maxDepth)
    ;TODO
    'your-code-here))
     
;; SECȚIUNE DE TESTARE - NU modificați această linie!
;; ATENȚIE! Pentru a primi punctaj pe temă, NU modificați această secțiune!
;;
;; CHECK - TASK 0 - NU modificați această linie!
;; ATENȚIE! NU modificați această secțiune
(Task 0 : 20 puncte) ;;check-exp
(check-exp-part 'is-empty-board?1 .02 (is-empty-board? (init-board 7 7)) #t)
(check-exp-part 'is-empty-board?2 .02 (is-empty-board? (get-board state-test)) #f)
(check-exp-part 'is-empty-board?3 .02 (is-empty-board? (get-board (init-state 7 8 RED))) #t)
(check-exp-part 'get-height1 .02 (get-height (get-board (init-state 7 8 YELLOW))) 7)
(check-exp-part 'get-height2 .02 (get-height (get-board state-test)) 6)
(check-exp-part 'get-height3 .02 (get-height (init-board 10 14)) 10)
(check-exp-part 'get-width1 .02 (get-width (get-board (init-state 7 8 YELLOW))) 8)
(check-exp-part 'get-width2 .02 (get-width (get-board state-test)) 7)
(check-exp-part 'get-width3 .02 (get-width (init-board 10 14)) 14)
(check-exp-part 'get-width4 .01 (get-width (init-board 20 20)) 20)
(check-exp-part 'get-player1 .02 (get-player state-test) YELLOW)
(check-exp-part 'get-player2 .02 (get-player (init-state 15 7 RED)) RED)
(check-exp-part 'get-player3 .02 (get-player (init-state 10 8 YELLOW)) YELLOW)
(check-exp-part 'get-disc1 .05 (get-disc (get-board (init-state 10 8 YELLOW)) (cons 1 2)) EMPTY)
(check-exp-part 'get-disc2 .05 (get-disc (get-board state-test) (cons 1 2)) EMPTY)
(check-exp-part 'get-disc3 .05 (get-disc (get-board state-test) (cons 5 0)) YELLOW)
(check-exp-part 'get-disc4 .05 (get-disc (get-board state-test) (cons 5 1)) YELLOW)
(check-exp-part 'get-disc5 .05 (get-disc (get-board state-test) (cons 5 2)) RED)
(check-exp-part 'get-disc6 .05 (get-disc (get-board state-test) (cons 5 3)) EMPTY)
(check-exp-part 'get-disc7 .05 (get-disc (get-board state-test) (cons 2 3)) RED)
(check-exp-part 'get-disc8 .05 (get-disc (get-board state-test) (cons 2 5)) RED)
(check-exp-part 'get-disc9 .05 (get-disc (get-board state-test) (cons 3 0)) YELLOW)
(check-exp-part 'get-disc10 .05 (get-disc (get-board state-test) (cons 3 3)) YELLOW)
(check-exp-part 'get-disc11 .05 (get-disc (get-board state-test) (cons 6 0)) YELLOW)
(check-exp-part 'get-disc12 .05 (get-disc (get-board state-test) (cons 6 1)) YELLOW)
(check-exp-part 'get-disc13 .05 (get-disc (get-board state-test) (cons 6 5)) EMPTY)
(check-exp-part 'get-disc14 .05 (get-disc (get-board state-test) (cons 0 0)) EMPTY)
(check-exp-part 'get-disc15 .05 (get-disc (get-board state-test) (cons 6 5)) EMPTY)
;; SFÂRȘIT CHECK - TASK 0 - NU modificați această linie!
;;
;; CHECK - Task1 - NU modificați această linie!
;; ATENȚIE! NU modificați această secțiune!
(Task 1 : 50 puncte) ;;check-exp
(check-exp-part 'get-available-actions1 .04 (get-available-actions (get-board state-test)) '(0 1 3 4 5 6))
(check-exp-part 'get-available-actions2 .04 (get-available-actions (init-board 7 9)) '(0 1 2 3 4 5 6 7 8))
(check-exp-part 'get-available-actions3 .04 (get-available-actions (get-board (init-state 10 8 YELLOW))) '(0 1 2 3 4 5 6 7))
(check-exp-part 'get-available-actions4 .04 (get-available-actions (get-board (apply-action state-test 3))) '(0 1 4 5 6))
(check-exp-part 'get-available-actions5 .04 (get-available-actions (get-board (apply-actions state-test '(3 5 5 5)))) '(0 1 4 6))
(check-exp-part 'apply-action1 .02 (get-disc (get-board (apply-action state-test 3)) (cons 3 5)) YELLOW)
(check-exp-part 'apply-action2 .02 (get-player (apply-action state-test 3)) RED)
(check-exp-part 'apply-action3 .02 (get-player (apply-action (init-state 7 7 YELLOW) 1)) RED)
(check-exp-part 'apply-action4 .02 (get-disc (get-board (apply-action (init-state 6 6 RED) 1)) (cons 1 0)) RED)
(check-exp-part 'apply-action5 .02 (get-disc (get-board (apply-action (init-state 4 6 YELLOW) 2)) (cons 2 0)) YELLOW)
(check-exp-part 'apply-actions1 .02 (get-player (apply-actions (init-state 7 6 YELLOW) '(1 0 2 1 1 3 4 1 2 3))) YELLOW)
(check-exp-part 'apply-actions2 .02 (get-disc (get-board (apply-actions (init-state 6 6 RED) '(0 1 1 0 1 1 0 2 3))) (cons 0 2)) RED)
(check-exp-part 'apply-actions3 .02 (get-available-actions (get-board (apply-actions (init-state 6 6 RED) '(0 1 1 0 1 1 0 2 3 1 1)))) '(0 2 3 4 5))
(check-exp-part 'apply-actions4 .02 (get-available-actions (get-board (apply-actions (init-state 4 4 RED) '(1 1 1 1 0 0 0 0 2 2 2 2 3 3)))) '(3))
(check-exp-part 'apply-actions5 .02 (get-available-actions (get-board (apply-actions state-test '(1 1 1 1 1 1 0 0 0 0 0 0)))) '(3 4 5 6))
(check-exp-part 'apply-actions6 .02 (get-available-actions (get-board (apply-actions (init-state 4 4 RED) '(2 0 0 0 1 1 3 0 2 2 1 1 3 3 3 2)))) '())
(check-exp-part 'apply-actions7 .02 (get-available-actions (get-board (apply-actions (init-state 7 9 YELLOW) '(5 5 8 5 5 8 8 0 0 4 1 5 5 6 1 1 7 8 2 3 1 5 3 6 1 0 3 1 1 0 4 3 2)))) '(0 2 3 4 6 7 8))
(check-exp-part 'apply-actions8 .02 (get-available-actions (get-board (apply-actions (init-state 12 12 RED) '(9 6 8 10 3 1 1 3 7 5 11 11 7 3 11 0 5 6 7 9 5 3 0 10 5 10 10 6 1 7 0 3)))) '(0 1 2 3 4 5 6 7 8 9 10 11))
(check-exp-part 'apply-actions9 .02 (get-available-actions (get-board (apply-actions (init-state 15 15 YELLOW) '(8 10 0 13 9 2 9 6 1 5 14 6 3 3 11 5 13 7 13 13 3 13 10 8 9 11 1 12 12 6 4 5 2 12)))) '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14))
(check-exp-part 'apply-actions10 .02 (get-available-actions (get-board (apply-actions (init-state 8 15 RED) '(6 1 3 3 10 12 14 1 8 3 9 10 3 11 3 9 7)))) '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14))
(check-exp-part 'is-game-over?1 .02 (is-game-over? (apply-actions (init-state 4 4 RED) '(1 1 1 1 0 0 0 0 2 2 2 2 3))) RED)  
(check-exp-part 'is-game-over?2 .02 (is-game-over? (apply-actions (init-state 4 4 RED) '(1 1 1 1 0 0 0 0 2 2 2 2))) #f)
(check-exp-part 'is-game-over?3 .02 (is-game-over? (apply-actions (init-state 6 6 YELLOW) '(0 5 0 5 0 3 0))) YELLOW)
(check-exp-part 'is-game-over?4 .02 (is-game-over? state-test) #f)
(check-exp-part 'is-game-over?5 .02 (is-game-over? (apply-actions (init-state 4 4 RED) '(2 0 0 0 1 1 3 0 2 2 1 1 3 3 3 2))) 3)
(check-exp-part 'is-game-over?6 .04 (is-game-over? (apply-actions (init-state 8 15 RED) '(6 1 3 3 10 12 14 1 8 3 9 10 3 11 3 9 7))) RED)
(check-exp-part 'is-game-over?7 .04 (is-game-over? (apply-actions (init-state 8 15 YELLOW) '(6 8 5 2 1 4 9 7 12 9 12 9 8 3 3 10 6 7 11 6 12 13 9 6 0 6 10 7 1 10 7 12 13 14 8 11 7 7 7 5 7 9 2 11 1 3 11 12 3 12 11 9 11 1 8 9 12 14 5 3 2))) YELLOW)
(check-exp-part 'is-game-over?8 .04 (is-game-over? (apply-actions (init-state 8 8 RED) '(2 0 0 1 7 0 1 4 7 2 1 3 6 7 7 6 3 3 3 7 7 2 0 4 4 3 7 4))) YELLOW)
(check-exp-part 'is-game-over?9 .04 (is-game-over? (apply-actions (init-state 4 20 RED) '(16 13 8 13 17 14 5 0 15 13 18))) RED)
(check-exp-part 'is-game-over?10 .04 (is-game-over? (apply-actions (init-state 7 7 RED) '(4 4 0 1 1 4 1 1 1 5 3 1 4 6 4 2 0 5 6 3 6 5 0 6 3 1 5 3))) YELLOW)
(check-exp-part 'is-game-over?11 .04 (is-game-over? (apply-actions (init-state 5 8 RED) '(5 1 0 0 1 6 7 4 7 2 4 2 3 3 4 4 6 2 2 0 1 6 4 3))) YELLOW)
(check-exp-part 'is-game-over?12 .04 (is-game-over? (apply-actions (init-state 4 8 RED) '(0 0 0 0 1 1 1 1 2 3 2 2 2 3 4 3 3 4 4 4 5 5 5 5 7 6 6 6 6 7 7 7))) 3)
(check-exp-part 'is-game-over?13 .04 (is-game-over? (apply-actions (init-state 4 8 RED) '(1 0 5 3 7 0 0 1 6 0 7 1 7 6 5 2))) #f)
(check-exp-part 'is-game-over?14 .04 (is-game-over? (apply-actions (init-state 9 4 YELLOW) '(0 2 1 1 3 3 1 0 2 1 2 2 3 3 1 0 2 3 2 3 0))) YELLOW)
(check-exp-part 'is-game-over?15 .04 (is-game-over? (apply-actions (init-state 9 4 RED) '(1 1 1 1 3 0 0 2 1 1 2 2 0 2 1 1 3 2))) #f)
;; SFÂRȘIT CHECK - TASK 1 - NU modificați această linie!
;;
;; CHECK - Task2 - NU modificați această linie!
;; ATENȚIE! NU modificați această secțiune!
(Task 2 : 30 puncte) ;;check-exp
(define FUNCTIONS (list is-game-over? play-game get-available-actions apply-actions)) ;;check-exp
(check-in-part 'get-action1 .1 (get-action (apply-actions (init-state 9 4 YELLOW) '(0 0 3 3 3 2 2 1 2 2 3 1 0 1 0))) '(1 3))
(check-in-part 'get-action2 .1 (get-action (apply-actions (init-state 7 7 RED) '(1 4 2 4 6 2 1 4 1 0 5 3 6 0 5 1 5 0 4 2 1 5))) '(0 3))
(check-exp-part 'get-action3 .1 (get-action (apply-actions (init-state 10 6 YELLOW) '(3 4 3 3 4 5 4 2 3 2 3 3 2 4 2 3 3 2 4 4 0 0 3 4 1 0 3 1 0 1 5 2))) 1)
(check-exp-part 'get-action4 .05 (get-action (apply-actions (init-state 4 4 YELLOW) '(1 0 1 1 0 3 0 0 3 2 3 3))) 2)
(check-exp-part 'get-action5 .05 (get-action (apply-actions (init-state 4 4 RED) '(0 3 2 3 3 1 1 2 2))) 3)
(check-exp-part 'get-action6 .05 (get-action (apply-actions (init-state 8 8 RED) '(3 0 2 1 6 6 5 1))) 4)
(check-exp-part 'get-action7 .1 (get-action (apply-actions (init-state 10 5 YELLOW) '(1 2 4 1 3 0 3 2 2 1 2 1 2))) 1)
(check-exp-part 'get-action8 .1 (get-action (apply-actions (init-state 12 12 YELLOW) '(6 3 0 9 4 2 10 7 1 7 0 0 9 2 0 8 2 8 8 10 10 10 5 2 3 11 4 4 4 8 3 2 2 11 11 8 8))) 9)
(check-in-part 'get-action9 .1 (get-action (apply-actions (init-state 20 8 RED) '(3 2 2 1 2 7 2 5 0 6 1 5 0 5))) '(2 3))
(check-in-part 'get-action10 .1 (get-action (apply-actions (init-state 10 10 YELLOW) '(0 4 0 5 0 6 1 7 1 8 1 9 2 9 2 8 2))) '(0 1 2 3 4 5 6 7 8 9))
(check-exp-part 'play-game1 .05 (check-game AI (init-state 8 9 RED) (cons select-random-action (current-pseudo-random-generator)) (cons  select-random-action (current-pseudo-random-generator)) FUNCTIONS 1) #t) 
(check-exp-part 'play-game2 .05 (check-game AI (init-state 7 7 YELLOW) (cons select-random-action (current-pseudo-random-generator)) (cons  select-random-action (current-pseudo-random-generator)) FUNCTIONS 1) #t) 
(check-exp-part 'play-game3 .05 (check-game AI state-test (cons select-random-action (current-pseudo-random-generator)) (cons  select-random-action (current-pseudo-random-generator)) FUNCTIONS 1) #t) 
;; SFÂRȘIT CHECK - TASK 2 - NU modificați această linie!
;;
;; CHECK - BONUS - NU modificați această linie!
;; ATENȚIE! NU modificați această secțiune
(Bonus 3 : 20 puncte BONUS) ;;check-exp
(check-exp-part 'bonus1 .1 (check-play-game AI state-test (cons negamax 1) (cons negamax 3) FUNCTIONS 4 RED) #t)
(check-exp-part 'bonus2 .1 (check-play-game AI state-test (cons select-random-action (current-pseudo-random-generator)) (cons negamax 4) FUNCTIONS 4 RED) #t)
(check-exp-part 'bonus3 .1 (check-play-game AI (apply-actions (init-state 4 6 RED) '(1 0 1 1 0 3)) (cons select-random-action (current-pseudo-random-generator)) (cons negamax 5) FUNCTIONS 6 YELLOW) #t)
(check-exp-part 'bonus4 .1 (check-play-game AI (init-state 7 7 YELLOW) (cons select-random-action (current-pseudo-random-generator)) (cons negamax 3) FUNCTIONS 6 RED) #t)
(check-exp-part 'bonus5 .1 (check-play-game AI (apply-actions (init-state 9 6 YELLOW) '(1 2 0 3 1 0 2 4 5 0)) (cons select-random-action (current-pseudo-random-generator)) (cons negamax 3) FUNCTIONS 6 RED) #t)
(check-exp-part 'bonus6 .1 (check-play-game AI (init-state 8 8 RED) (cons negamax 1) (cons negamax 3) FUNCTIONS 6 YELLOW) #t)
(check-exp-part 'bonus7 .1 (check-play-game AI (init-state 4 6 RED) (cons negamax 1) (cons negamax 5) FUNCTIONS 4 YELLOW) #t)
(check-exp-part 'bonus8 .1 (check-play-game AI (init-state 5 7 RED) (cons negamax 2) (cons negamax 5) FUNCTIONS 2 YELLOW) #t)
(check-exp-part 'bonus9 .1 (check-play-game AI (apply-actions (init-state 10 8 RED) '(1 5 2 6 7 1 0 2 6 4 0 7 7)) (cons negamax 2) (cons negamax 4) FUNCTIONS 4 YELLOW) #t)
(check-exp-part 'bonus10 .1 (check-play-game AI (apply-actions (init-state 10 6 RED) '(1 0 1 1 0 3)) (cons select-random-action (current-pseudo-random-generator)) (cons negamax 3) FUNCTIONS 4 YELLOW) #t)
;; SFÂRȘIT CHECK - BONUS - NU modificați această linie!
;; SFÂRȘIT SECȚIUNE DE TESTARE
(sumar)
