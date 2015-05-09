#lang racket

;Enigma

;define hash-tables
(define commands (make-hash))
(define directions (make-hash))
(define descriptions (make-hash))
(define riddles (make-hash))
(define answers (make-hash))

;initialize hash-tables
(define (start)
  (hash-set! commands 'move move)
  (hash-set! commands 'go move)
  (hash-set! commands 'walk move)
  (hash-set! commands 'quit quit)
  (hash-set! commands 'exit quit)
  (hash-set! commands 'help help)
  
  (hash-set! directions 1  '((north 0) (south 0) (east 2) (west 0)))
  (hash-set! directions 2  '((north 0) (south 0) (east 3) (west 1)))
  (hash-set! directions 3  '((north 4) (south 5) (east 6) (west 2)))
  (hash-set! directions 4  '((north 0) (south 3) (east 0) (west 7)))
  (hash-set! directions 5  '((north 3) (south 0) (east 0) (west 0)))
  (hash-set! directions 6  '((north 0) (south 0) (east 0) (west 3)))
  (hash-set! directions 7  '((north 0) (south 0) (east 0) (west 0)))
  
  (hash-set! descriptions 1 (string-append
                             "Well, you are in the first room.\n"))
  (hash-set! descriptions 2 (string-append
                             "Very clever, but you haven't get very far yet.\n"))
  (hash-set! descriptions 3 (string-append
                             "Well well, it seems like we have a Mr Smarty here."
                             "Keep trying and you might leave.\n"))
  (hash-set! descriptions 4 (string-append
                             "It's hard to say, but you got more far than I thought you would.\n"))
  (hash-set! descriptions 5 (string-append
                             "I am almost impressed. Good performance, "
                             "keep trying and you probably will find the exit.\n"))
  (hash-set! descriptions 6 (string-append 
                             "Nice to see you got so far. "
                             "However you still are in my trap, oops my house.\n"))
  (hash-set! descriptions 7 (string-append 
                             "Congratulations, you made me very happy, "
                             "now you shall get your freedom back.\n"))
  
  (hash-set! riddles 1 (string-append
                        "I will disappear every time you say my name.\n"
                        "What am I?\n"))
  (hash-set! riddles 2 (string-append
                        "I am always around you but often forgotten.\n"
                        "I am pure and clean most time, but occasionally rotten.\n"
                        "What am I?\n"))
  (hash-set! riddles 3 (string-append
                        "When you have me, you feel like sharing me.\n"
                        "But, if you do share me, you don't have me.\n"
                        "What am I?\n"))
  (hash-set! riddles 4 (string-append
                        "I have a face but no eyes, hands but no arms.\n"
                        "What am I?\n"))
  (hash-set! riddles 5 (string-append
                        "What does man love more than life?\n"
                        "Fear more than death or mortal strife?\n"
                        "What do the poor have, what the rich require,\n"
                        "And what contented men desire?\n"
                        "What does the miser spend, the spendthrift save,\n"
                        "And all men carry to their graves?\n"))
  (hash-set! riddles 6 (string-append
                        "I go up and never come down no matter how hard you wish.\n"
                        "As I get higher, more wrinkles crawl on to the face.\n"
                        "What am I?\n"))
  (hash-set! riddles 7 (string-append
                        "Until I am measured,\n"
                        "I am not known.\n"
                        "Yet how you miss me,\n"
                        "When I have flown!\n"
                        "What am I?\n"))
  
  (hash-set! answers 1 'silence)
  (hash-set! answers 2 'air)
  (hash-set! answers 3 'secret)
  (hash-set! answers 4 'clock)
  (hash-set! answers 5 'nothing)
  (hash-set! answers 6 'age)
  (hash-set! answers 7 'time))

;append string to show the direction(s) available
(define (append-directions-message directions)
  (if (null? (cdr directions))
      (format " and ~a.\n" (car directions))
      (format ", ~a~a" (car directions) (append-directions-message (cdr directions)))))

;returns the number of the next room
(define (lookup room-id direction)
  (car (assq-ref (get-room-directions room-id) direction)))

;works with associative lists, where it returns a list without the element searched (cdr)
(define (assq-ref assqlist id)
  (cdr (assq id assqlist)))

;inserts ">" symbol before the text field and gets what the user types
(define (read-command)
  (map string->symbol (string-split (read-line))))

;return room description or if the result is #f print an error
(define (get-room-description rid)
  (let ((description (hash-ref descriptions rid #f)))
    (if (string? description)
        description
        (print-error "ROOM DESCRIPTION" (format "It was not possible to get information of the room ~a!\n" rid)))))

;return directions for a room in the hash table
(define (get-room-directions rid)
  (hash-ref directions rid #f))

;return available directions
;map (function list) returns a new list with the results of the function applyed in the list elements (arguments)
;filter (function list) returns a new list with the results of when the list elements are true for the function (arguments)
(define (get-available-directions rid)
  (let ((room-directions (get-room-directions rid)))
    (if (pair? room-directions)
        (map car (filter
                  (lambda (x)
                    (> (car (cdr x)) 0))
                  room-directions))
        room-directions)))

;show to the users directions to other rooms
(define (show-available-directions rid)
  (let ((directions (get-available-directions rid)))
    (if (list? directions)
        (case (length directions)
          ((1) (printf "You see a door to the ~a.\n" (car directions)))
          (else (printf "You see doors to the ~a~a" (car directions) (append-directions-message (cdr directions)))))
        (print-error "AVAILABLE DIRECTIONS" (format "It was impossible to retrieve directions for the room ~a." rid)))))

;command help, to print in the user screen what he can do
(define (help args rid)
  (printf (string-append
           "\nTo move between the rooms type move followed by the direction you want to move (north, south, west or east).\n"
           "It is possible to use de commands walk or go instead of move.\n"
           "All riddles' answers are a single word answer, and you do not have to put a article (a, an, the) before it.\n"
           "To quit the game type the command quit or exit, it will end the game immediately.\n"
           "And remember, always use lower case letter typing commands or answers!\n\n"))
  (cycle rid))

;function print-error to print error messages
(define (print-error error-title error-message)
  (printf (format "<~a>\nOoops, something is wrong!\n~a\n" error-title error-message)))

;first time that the riddle is asked (differs of ask-riddle in the message presented.
(define (ask-first-riddle next rid this)
  (printf "Well well, not so easy my friend. To go to another room you have to solve this riddle first.\n\n")
  (printf (hash-ref riddles rid))
  (verify-first-answer (read-command) next rid this))

;function ask-riddle, it gets a riffle from hash table and print it.
(define (ask-riddle next rid this)
  (printf (hash-ref riddles rid))
  (verify-answer (read-command) next rid this))

;verify if te first answer tiped is the same in the hash table.
(define (verify-first-answer args next rid this)
  (if (eq? (hash-ref answers rid) (car args))
      ((move-next next rid)
       (printf "Well done. I'm going to let you continue.\n"))
      ((printf (string-append
                "Nice try, but you did not gave a correct answer.\n"
                "I am going to let you try again.\n\n"))
       (ask-riddle next rid this))))

;verify if answer tiped is the same in the hash table.
(define (verify-answer args next rid this)
  (if (eq? (hash-ref answers rid) (car args))
      ((move-next next rid)
       (printf "Well done. I'm going to let you continue.\n"))
      ((printf (string-append
                "Nice try, but you did not gave a correct answer.\n"
                "Take a time to think and come back again when you have an idea of the answer.\n\n"))
       (cycle this))))

;change the room initializing a new game cycle
(define (move-next next rid)
  (printf "Moving to the next room in the ~a...\n" (car next))
  (cycle rid))

;function for the commands move, go and walk
;verifies the direction and the directions available
(define (move args rid)
  (cond
    ((or (null? args) (> (length args) 1))
     (printf "Sorry, please say one valid direction to move (north, south, east or west).\n")
     (cycle rid))
    ((member (car args) '(north south east west))
     (let ((new-room (lookup rid (car args))))
       (if (zero? new-room)
           ((printf "Sorry, you can't go ~a from your current room.\n" (car args))
            (cycle rid))
           (ask-first-riddle args new-room rid))))
    (else 
     (printf "Sorry, '~a' isn't a valid direction to move.\n" (car args))
     (cycle rid))))

;function for quit and exit commands
(define (quit args rid)
  (if (or (null? args) (and (eq? (car args) 'game) (null? (cdr args))))
      (exit)
      ((print-error "INTERPRETER FAILED" "I could not understand what you want. Try again with similar words or use the command help for assitance.")
       (cycle rid))))

;command interpreter for the commands typed, dividing in action and args
(define (command-interpreter command rid)
  (if (not (null? command))
      (let ((action (car command))
            (args (cdr command)))
        (let ((procedure (hash-ref commands action #f)))
          (cond
            ((procedure? procedure) (procedure args rid))
            ((eq? procedure #f) 
             ((print-error "INTERPRETER FAILED" "I could not understand what you want. Try again with similar words or use the command help for assitance.")
              (cycle rid)))
            (else (print-error "UNKNOWN ERROR" "This error is unknown.")))))
      ((print-error "INTERPRETER FAILED" "I could not understand what you want. Try again with similar words or use the command help for assitance.")
       (cycle rid))))


;start a new cycle of the game. in other words, keeps the game on.
(define (cycle rid)
  (printf "~a\n" (get-room-description rid))
  (if (eq? rid 7) (exit) 'continue)
  (show-available-directions rid)
  (printf "> ")
  (command-interpreter (read-command) rid))

; function to start the game
(define (startgame room-id)
  (start)
  (printf (string-append 
           "You wake a little dizzy, on the floor. Why are you on the floor?\n\n"
           "The room is dark, when you try to get up, a lamp in the ceiling is switched on, "
           "and suddenly you see, strange letters, saying Welcome to Enigma.\n\n"
           "And you start to hear, a hoarse voice, over a old sound system.\n\n"
           "Welcome to my house, or as I prefer, Enigma. Your freedom is now mine.\n"
           "In order to get it back, you have to entertain me, what is not a easy task.\n"
           "To do so, you will find in the answer of this riddle: \n\"I can be "
           "simple or I can be complex, I can present myself as a poem or even a single phrase. What am I?\"\n\n"
           "No need to answer, save your brain to find your way out.\n"))  
  (cycle room-id))

;starts the game
(startgame 1)