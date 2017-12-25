;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |MY FORUM|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require racket/string)
(require 2htdp/image)
(require 2htdp/universe)

;Notes on Functionality:

;   When an Error occurs, press backspace on it to type something new.
;   When a reply ID isn't found in Viewall-mode, replace it with a good ID
;   When a Command isn't given, the text is replaced with an empty text-box
;   When you try to send an empty-message, you'll get an error saying a post must
;   be something. Press enter to get rid of it and type a message.



;BIG-BANG
;simple-net-forum: String Number -> World
;  -Where user is a  String is your myNEU username and number is the last 4 digits of your husky id

#; (define (simple-net-forum user id)
     ...)
(define (simple-net-forum user id) 
  (big-bang (make-viewall "" '())
            [port PORT]
            [name (string-append user ":" id)]
            [register REGISTER]
            [to-draw draw-world]
            [on-receive received-message.v2]
            [on-key key-pressed]))


; CONTSANTS ;

(define FONTSIZE 16)
(define FONTCOLOR "black")
(define REPLYCOLOR "blue")
(define ERRORCOLOR "red")



(define REGISTER "dictionary.ccs.neu.edu")
(define PORT 10006)
(define BACKGROUND (rectangle 500 600 "solid" "white"))


(define SEARCH-BOX
  (place-image (text "Seek and you shall find..." 12 "black")
               70 20
               (rectangle 400 100 "solid" "red")))  
(define VIEWALL-BOX
  (place-image (text "VIEW-ALL: PLEASE TYPE A COMMAND" 12 "black")
               50 20
               (rectangle 400 100 "solid" "gray")))
(define NEWITEM-BOX
  (place-image (text "NEWITEM: be nice..." 12 "black")
               60 20
               (rectangle 400 100 "solid" "lightblue")))
(define THREADVIEW-BOX
  (place-image (text "Please report Hamilton memes." 16 "black")
               150 30
               (rectangle 400 50 "solid" "green")))
; CONSTANTS ;  



; A World is one of
; - Viewall
; - Threadview
; - Newitem
; - Search
; INTERPRETATION: Represents four different "views" in your program.


(define (world-temp w)
  (cond
    [(viewall? w)...]
    [(threadview? w)...]
    [(newitem? w)...]
    [(search? w)...]))
     
;;;; Examples within sections


;;;;;;;;;;;;;;;;;;
; A ServerMsg is one of:
; - (list "POST" Natural String String)
; - (list "REPLY" Natural String String)
; - (list "ERROR" String)
; INTERPRETATION:
; – Receiving a "POST" message means there is a new post with the given ID#,
;    author, and contents.  This is the same
;    information as you've been receiving via "id:author:contents", except
;    that the data is properly broken apart for you, instead of mashed into
;    one string.
; – Receiving a "REPLY" message there is a new reply containing the ID# of
;    the parent post (that this is a reply to), the author of the reply as 
;    the next string, and whose content is the final string.
; – Receiving an "ERROR" message means the client made a mistake, with the
;    error message given as the string.

(define (servermsg-temp msg)
  (cond
    [(string=? (first msg) "POST")....]
    [(string=? (first msg) "REPLY")....]
    [(string=? (first msg) "ERROR")....]))


;EXAMPLES; 

(define servermsg-ex1 
  (list "POST" 1498 "casambre.v" "Sir, I heard your name at Princeton"))
(define servermsg-ex2
  (list "REPLY" 1498 "That depends, who's askin?"))
(define servermsg-ex3
  (list "ERROR" "Yah don' messed up A-Aron"))
(define servermsg-ex4
  (list "POST" 1998 "hamilton.a" "I was seeking an accelerated course of study"))
(define servermsg-ex5   
  (list "REPLY" 1998 "burr.a" "You punched the Bursar?"))
(define servermsg-ex6
  (list "REPLY" 2000 "king.h" "Next to Washington they all look small"))
(define servermsg-ex7
  (list "POST" 2000 "king.g" "Oceans rise, empires fall..."))






; A History is a [List-of Post]
;  INTERPRETATION: a list of the prior posts received from the server
; because we can only store the posts in a the history, we need to nest all
; of the replies within the post struct

(define (hist-temp lop)
  (cond
    [(empty? lop)...]
    [(cons? lop)...]))

; A Reply is a (make-reply number string string)
(define-struct reply [id author content])
; where number is the ID of the Post you're replying to, String is the author
; and String is the content of the reply

(define (reply-temp rep)
  (...(reply-id rep)...(reply-author rep)...(reply-content rep)))

;Examples

(define reply1 (make-reply 1232 "hamilton.a" "Bend over, I'll show you where my shoe fits"))
(define reply2 (make-reply 1498 "hamilton.a" "You're a better lawyer than me"))
(define reply3 (make-reply 1776 "hamilton.a" "We know who's really doin' the planting"))
(define reply4 (make-reply
                1800 "hamilton.a" "Sir, do you want me to run the Treasury or State Department?"))
(define reply5 (make-reply 1232 "hamilton.a" "Jefferson always reticent with the President"))
(define reply6 (make-reply 1232 "washington.g" "Sure, if we know who's gonna lead 'em..."))
(define reply7 (make-reply 1498 "jefferson.t" "Let's show him what we know..."))





; A Post is a (list Number (make-post string string [List-of-Reply])
; where number is the ID, string is the author, and string is the content))
(define-struct post [author content replies])
(define (post-temp p)
  (...(post-author p)...(post-content p)...(post-replies p)))

 

;Examples
(define post1 (make-post "jefferson.t" "Every action has an equal opposite reaction"
                         (list reply1 reply5 reply6)))
(define post2 (make-post "burr.a" "I'm willin to wait for it"
                         (list reply2 reply7 )))
(define post3 (make-post "jefferson.t" "Thanks to Hamilton our cabinet's fractured into factions"
                         (list reply3)))
(define post4 (make-post "washington.g" "I'm gonna need a right-hand man."
                         (list reply4)))



;  Template for Post
#;(define (Post-temp post)
    (first post...)
    (post-author (second post)...)(post-content (second post)...)(post-replies (second post)))
  


; Examples for Post
(define Post-ex1
  (list 1232 post1))
(define Post-ex2
  (list 1498 post2))
(define Post-ex3
  (list 1776 post3))
(define Post-ex4      
  (list 1800 (make-post "washington.g" "I'm gonna need a right-hand man." (list reply4))))
 

; A ClientMsg is one of
; - "CATCHUP"
; - (list "POST" String)
; - (list "REPLY" Natural String)
; INTERPRETATION:
; – Sending the message "CATCHUP" tells the server you would like it
;    to send you all the prior posts the server has received.  You are only
;    allowed to ask the server for catch-up messages once; a second request
;    will result in an error.
; – Sending the message (list "POST" String) – i.e., a two-item
;    list whose first item is the string "POST", and whose second item is a
;    String – indicates you are writing a new post, where the string provides
;    the text of the post
; – Sending the message (list "REPLY" Natural String) indicates that
;    you are replying to an existing post (Note: you cannot reply to a reply).
;    The number indicates the post's id, and the string is the text of the post.

; TEMPLATE ;
(define (clientmsg-temp cm)
  (cond
    [(string? cm)...]
    [(string=? (first cm) "POST")...]
    [(string=? (first cm) "REPLY")...]))

; EXAMPLES ;
(define client-msg1 "CATCHUP")
(define client-msg2 (list "POST" "My first post <3"))
(define client-msg3 (list "REPLY" "Lol stop being so weird..."))
(define client-msg4 (list "POST" "Relax again, Alexander"))
(define client-msg5 (list "REPLY" 1498 "Smile more..."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;; A Viewall is a (make-viewall String History) make-view-all [Command  History]
; INTERPRETATION: The user is viewing all posts (but not replies), 
;   and possibly typing a Command.

(define-struct viewall [command history])
(define (view-all-temp vw)
  (...(viewall-command vw)...(viewall-history vw)))

 
;Examples:

(define viewall-ex1 (make-viewall "CATCHUP" (list Post-ex1 Post-ex2)))
(define viewall-ex2 (make-viewall "new" (list '())))
(define viewall-ex3 (make-viewall "reply 1213" (list Post-ex3 Post-ex4)))
;;The number in the reply is the ID, but
; but it should be turned into a string
(define viewall-ex4 (make-viewall "view 9919" (list Post-ex3 Post-ex1)))

(define viewall-ex5 (make-viewall "" (list Post-ex1)))
  

;;;;;;;;;;;  A Command is a string of the form
; – "catchup"
; – "new"
; – "reply ", followed by a number
; – "view ", followed by a number

(define (command-temp com)
  (cond
    [(string=? "CATCHUP" com)...]
    [(string=? "new" com)...]
    [(string=? (substring com 0 5) "reply")...]
    [(string=? (substring com 0 4) "view")...]))

(define com-ex1 "catchup")
(define com-ex2 "new")
(define com-ex4 "reply 1441")
(define com-ex5 "view")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;; A Threadview is a (make-threadview Post History)
; INTERPRETATION: The user is viewing a specific Post and its replies.

(define-struct threadview [post history])

#;(define (threadview-temp tv)
    (threadview-post tv)(threadview-history tv))

(define threadview-ex1 (make-threadview Post-ex1 (list Post-ex2)))
(define theadview-ex2 (make-threadview Post-ex2 (list Post-ex3 Post-ex4)))
(define threadview-ex3 (make-threadview Post-ex1 '())) ;note this will never occur


 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;






;;;;;;; A Newitem is a (make-newitem [Maybe Natural] String History)
; INTERPRETATION: The user is entering a new post (if the maybe is #false),
;   or a new reply (if the maybe is a number).
(define-struct newitem [maybenat message history])

(define (maybenat-temp mn)
  (cond
    [(boolean? mn) ...]
    [(number? mn) ...]))

(define (newitem-temp ni)
  (maybenat-temp (... (newitem-string ni) ...
                      (newitem-history ni))))

(define newitem1 (make-newitem #false "POST1" '()))
(define newitem2 (make-newitem #false "POST2" (list Post-ex1)))
(define newitem3 (make-newitem #false "POST3" (list Post-ex1 Post-ex2)))
(define newitem4 (make-newitem 6666 "REPLY1" '()))
(define newitem5 (make-newitem 6942 "REPLY2" (list Post-ex1))) 
(define newitem6 (make-newitem 5467 "REPLY3" (list Post-ex1 Post-ex2)))
                  


; A Search is a (make-search History String)
; INTERPRETATION: The user is trying to view only a subset
;   of the existing messages.

(define-struct search [history searchstring])

(define (search-temp s)
  (... (search-history s) ...
       (search-searchstring s)))

(define search1 (make-search '() "1"))
(define search2 (make-search (list Post-ex1) "What"))
(define search3 (make-search (list Post-ex1 Post-ex2) "Who"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   ON-KEY  HANDLERS        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;
 

; backspace String -> String
; removes a 1string from end of string
(define (backspace string)
  (if (= 0 (string-length string))
      string
      (substring string 0 (- (string-length string) 1))))
      
;Examples 
(define bs-ex1 (backspace "computer"))
(define bs-ex2 (backspace "Hello"))
(define bs-ex3 (backspace ""))

;Check-expect
(check-expect (backspace "computer") "compute")
(check-expect (backspace "ho") "h")
(check-expect (backspace "") "")


;;;;;;;;;; 

;real-view?    viewall -> boolean
; checks if the "view" command is legit in order to get to a threadview

(define (real-view? vw)
  (and (>= (string-length (viewall-command vw)) 6)
       (number? (string->number (substring (viewall-command vw) 5)))))

(check-expect (real-view? (make-viewall "view" '()))
              #false)
(check-expect (real-view? (make-viewall "asdf" '()))
              #false)
(check-expect (real-view? (make-viewall "view it" '()))
              #false)
(check-expect (real-view? (make-viewall "view 9" '()))
              #true)




;add-key     String 1String -> String
; adds a letter that was typed onto a given string

(define (add-key string key)
  (string-append string key))

;Examples
(define add-key-ex1 (add-key "Hell" "o"))
(define add-key-ex2 (add-key "CS250" "0"))
(define add-key-ex3 (add-key "" "H"))

;Check-expect
(check-expect (add-key "he" "y") "hey")
(check-expect (add-key "fundi" "e") "fundie")
(check-expect (add-key "" "y") "y")



;; switch-to-threadview   World -> Threadview
;; to make a new threadview given the View-Command's ID in the viewall world, with the
;; threadview showing a single Post, while keeping the present history the same

(define (switch-to-threadview w)
  (cond
    [(viewall? w)
     (cond
       [(real-view? w)
        (if
         (boolean? (assoc (string->number (second (string-split (viewall-command w))))
                          (viewall-history w)))
         (make-viewall "Yah don' messed up A-Aron..." (viewall-history w))
         (make-threadview
          (assoc (string->number (second (string-split (viewall-command w))))
                 (viewall-history w))
          (viewall-history w)))]
       [else w])]
    [(threadview? w) w]
    [(newitem? w) w]
    [(search? w) w]))


(check-expect (switch-to-threadview (make-viewall "view" (list Post-ex1)))
              (make-viewall "view" (list Post-ex1)))

(check-expect (switch-to-threadview
               (make-viewall
                "view 3421"
                (list (list 3421 (make-post "USER" "CONTENT" (list reply1))))))
              (make-threadview (list 3421 (make-post "USER" "CONTENT" (list reply1))) ;post 
                               (list (list 3421 (make-post "USER" "CONTENT" (list reply1))))))
 

(check-expect (switch-to-threadview
               (make-viewall
                "view 1111"
                (list
                 (list 1111 (make-post "casambre.v" "CONTENT" (list reply1 reply2))) 
                 (list 3313 (make-post "OTHER" "MESSAGE" '())))))
              (make-threadview
               (list 1111 (make-post "casambre.v" "CONTENT" (list reply1 reply2))) ; found post
               (list                                            ;same history
                (list 1111 (make-post "casambre.v" "CONTENT" (list reply1 reply2)))
                (list 3313 (make-post "OTHER" "MESSAGE" '())))))
                                                                             
(check-expect (switch-to-threadview
               (make-viewall
                "view 12"
                (list (list 1222 (make-post "casambre.v" "hard ID" (list reply1))))))
              (make-viewall "Yah don' messed up A-Aron..."
                            (list (list 1222 (make-post "casambre.v" "hard ID" (list reply1))))))
                
(check-expect (switch-to-threadview (make-threadview Post-ex1 (list Post-ex2)))
              (make-threadview Post-ex1 (list Post-ex2)))
(check-expect (switch-to-threadview (make-newitem #false "Hiyaaa!!" (list)))
              (make-newitem #false "Hiyaaa!!" '()))
(check-expect (switch-to-threadview (make-newitem 2142 "OllyOllyoxenfree" (list Post-ex1)))
              (make-newitem 2142 "OllyOllyoxenfree" (list Post-ex1)))
(check-expect (switch-to-threadview (make-search (list Post-ex1) "What"))
              (make-search (list Post-ex1) "What"))



;; Sig:    World   ->   ViewAll
;;  Purpose: to change worldstates given a world, used as the helper function for on key F1
(define (switch-to-viewall w)
  (cond
    [(viewall? w) w]
    [(threadview? w) (make-viewall "" (threadview-history w))]
    [(newitem? w) (make-viewall "" (newitem-history w))]
    [(search? w) (make-viewall "" (search-history w))]))

(check-expect (switch-to-viewall (make-viewall "blaah" (list Post-ex1)))
              (make-viewall "blaah" (list Post-ex1)))
(check-expect (switch-to-viewall (make-threadview Post-ex1 (list Post-ex1 Post-ex2)))
              (make-viewall "" (list Post-ex1 Post-ex2)))
(check-expect (switch-to-viewall (make-newitem #false "New Post!" (list Post-ex3)))
              (make-viewall "" (list Post-ex3)))
(check-expect (switch-to-viewall (make-search (list Post-ex1 Post-ex2) "find my word"))
              (make-viewall "" (list Post-ex1 Post-ex2)))



;   switch-to-search: EditviewPosts -> SearchPosts
; Purpose : to switch to search mode given the current world state
(define (switch-to-search w)
  (cond
    [(viewall? w) (make-search (viewall-history w) "")]
    [(threadview? w)(make-search (threadview-history w) "")]
    [(newitem? w)(make-search (newitem-history w) "")]
    [(search? w) w]))


(check-expect (switch-to-search (make-viewall "blaah" (list Post-ex1)))
              (make-search (list Post-ex1) ""))
(check-expect (switch-to-search (make-threadview Post-ex1 (list Post-ex1 Post-ex2)))
              (make-search (list Post-ex1 Post-ex2) ""))
(check-expect (switch-to-search (make-newitem #false "New Post!" (list Post-ex3)))
              (make-search (list Post-ex3) ""))
(check-expect (switch-to-search (make-search (list Post-ex1) "Swagg"))
              (make-search (list Post-ex1) "Swagg"))
                                
; switch-to-new item    Viewall -> Newitem ; the world is definitely a viewall
; Switches from a view all world to a new item world given a certain command

(define (switch-to-newitem w)
  (cond
    [(string=? "new" (viewall-command w))
     (make-newitem
      #false
      ""
      (viewall-history w))]
    [(and (>= (string-length (viewall-command w)) 7)
          (string=? (substring (viewall-command w) 0 5) "reply")
          (string-numeric? (second (string-split (viewall-command w))))
          (not (false? (assoc (string->number (second (string-split (viewall-command w))))
                              (viewall-history w)))))
     (make-newitem
      (string->number (second (string-split (viewall-command w))))
      ""
      (viewall-history w))]
    [else w]))
    
    

(check-expect (switch-to-newitem (make-viewall "new" (list empty)))
              (make-newitem #false "" (list empty)))
(check-expect (switch-to-newitem (make-viewall "new" (list Post-ex1)))
              (make-newitem #false "" (list Post-ex1)))
(check-expect (switch-to-newitem (make-viewall "reply 1234"  
                                               (list (list 1234 (make-post "asd" "asdf" '()))))) 
              (make-newitem 1234 "" (list (list 1234 (make-post "asd" "asdf" '())))))  
(check-expect (switch-to-newitem (make-viewall "reply 4566" (list Post-ex2 Post-ex3)))
              (make-viewall "reply 4566" (list Post-ex2 Post-ex3)))
(check-expect (switch-to-newitem (make-viewall "reply" (list Post-ex1)))
              (make-viewall "reply" (list Post-ex1)))
                                 
 




; process-command  Viewall -> World   
;takes the current Viewall world and responds to the command appropriately.
; This is going to be the on-key handler When the user presses Enter and is in the view all world
; "new" means make a new post in new-item mode
; "catchup" sends catchup to the server
;reply means make a reply in new item mode 


(define (process-command vw)
  (cond
    [(string=? "CATCHUP" (viewall-command vw)) (send-message.v3 vw)]
    [(string=? "new" (viewall-command vw)) (switch-to-newitem vw)]
    [(and (>= (string-length (viewall-command vw)) 5)
          (string=? (substring (viewall-command vw) 0 5) "reply"))
     (switch-to-newitem vw)]
    [(and (>= (string-length (viewall-command vw)) 4)
          (string=? (substring (viewall-command vw) 0 4) "view"))
     (switch-to-threadview vw)]
    [else (make-viewall "" (viewall-history vw))]))

(check-expect (process-command (make-viewall "CATCHUP" (list empty)))
              (send-message.v3 (make-viewall "CATCHUP" (list empty)))) 
(check-expect (process-command (make-viewall "new" (list Post-ex1)))
              (switch-to-newitem (make-viewall "new" (list Post-ex1))))
(check-expect (process-command (make-viewall "reply 2331" (list Post-ex1 Post-ex2 Post-ex3)))
              (switch-to-newitem (make-viewall "reply 2331" (list Post-ex1 Post-ex2 Post-ex3))))
(check-expect (process-command (make-viewall "view 456" (list Post-ex1 Post-ex2 Post-ex3)))
              (switch-to-threadview (make-viewall "view 456" (list Post-ex1 Post-ex2 Post-ex3))))
              
                               
 
;;;;;;;;;;;
;send-message.v3: World -> MakePackage of [World ClientMsg] 
;; When on key sends a make package, its sending a ClientMsg. Making a package of the resulting world
; (in this case, its an empty string for both the edit and search. The world that


(define (send-message.v3 w)
  (cond
    [(viewall? w)
     (if (string=? (viewall-command w) "CATCHUP")
         (make-package (make-viewall "" (viewall-history w))
                       "CATCHUP")
         w)]
    [(threadview? w) w ]
    [(newitem? w)
     (cond
       [(boolean? (newitem-maybenat w))
        (make-package (switch-to-viewall w)  ;resulting worldstate
                      (list "POST" (newitem-message w)))]
       [(number? (newitem-maybenat w))
        (make-package (switch-to-viewall w)  ;resulting worldstate
                      (list "REPLY" (newitem-maybenat w) (newitem-message w)))]
       [else w])]
    [(search? w) w]))

 

(check-expect (send-message.v3 (make-viewall "CATCHUP" '()))
              (make-package (make-viewall "" '()) "CATCHUP"))
(check-expect (send-message.v3 (make-viewall "view" '()))
              (make-viewall "view" '()))
(check-expect (send-message.v3 (make-threadview Post-ex1 (list Post-ex2)))
              (make-threadview Post-ex1 (list Post-ex2)))
(check-expect (send-message.v3 (make-newitem #false "Hey!" '()))
              (make-package (switch-to-viewall (make-newitem #false "Hey!" '()))
                            (list "POST" "Hey!")))
(check-expect (send-message.v3 (make-newitem 1232 "Hey there!" '()))
              (make-package (switch-to-viewall (make-newitem 1232 "Hey there!" '()))
                            (list "REPLY" 1232 "Hey there!")))
(check-expect (send-message.v3 (make-newitem "Uh-oh" "Hey there!" '()))
              (make-newitem "Uh-oh" "Hey there!" '()))
(check-expect (send-message.v3 (make-search '() "Stringggg"))
              (make-search '() "Stringggg"))



; World -> Handler Result 
; To handle what the world does when Enter is pressed


(define (enter-world w)
  (cond
    [(viewall? w) (if (string=? "CATCHUP" (viewall-command w))
                      (send-message.v3 w)
                      (process-command w))]
    [(newitem? w) (send-message.v3 w)]
    [else w]))




(check-expect (enter-world (make-viewall "CATCHUP" '()))
              (enter-world (send-message.v3 (make-viewall "CATCHUP" '()) )))
(check-expect (enter-world (make-viewall "reply 2123" '()))
              (process-command(make-viewall "reply 2123" '())))
(check-expect (enter-world (make-newitem #false "Hey" '()))
              (send-message.v3 (make-newitem #false "Hey" '())))
(check-expect (enter-world (make-search '() "po"))
              (make-search '() "po"))  

;backspace-world   World -> Handler Result
; To handle the World given its functionality when backspace is pressed
(define (backspace-world w)
  (cond
    [(viewall? w) (make-viewall (backspace (viewall-command w))(viewall-history w))]
    [(threadview? w) w]
    [(newitem? w)
     (make-newitem
      (newitem-maybenat w)
      (backspace (newitem-message w))
      (newitem-history w))] 
    [(search? w) (make-search (search-history w)(backspace (search-searchstring w)))]))



(check-expect (backspace-world viewall-ex1)
              (make-viewall (backspace (viewall-command viewall-ex1))
                            (viewall-history viewall-ex1)))
(check-expect (backspace-world threadview-ex1)
              threadview-ex1)
(check-expect (backspace-world (make-newitem #false "hello" '()))
              (make-newitem #false "hell" '()))
(check-expect (backspace-world (make-search '() "po"))
              (make-search '()  "p"))


; add-1key-world Sig:  World -> Handler Result
; To add a given key based on the world state

(define (add-1key-world w key)
  (cond
    [(viewall? w) (make-viewall (add-key (viewall-command w) key)(viewall-history w))]
    [(threadview? w) w]
    [(newitem? w) (make-newitem
                   (newitem-maybenat w)
                   (add-key (newitem-message w) key)
                   (newitem-history w))]
    [(search? w)(make-search (search-history w)(add-key (search-searchstring w) key))]))



(check-expect (add-1key-world viewall-ex1 "h")
              (make-viewall (add-key (viewall-command viewall-ex1) "h")
                            (viewall-history viewall-ex1)))

(check-expect (add-1key-world threadview-ex1 "h")
              threadview-ex1)

(check-expect (add-1key-world newitem1 "h")
              (make-newitem #false (add-key "POST1" "h")'()))

(check-expect (add-1key-world search1 "h")
              (make-search '() (add-key "1" "h")))


;key-pressed: world key -> World (or a MakePackage)
;Runs either a send-message, add-key, backspace, or switches
; worldstates


(define (key-pressed w key)
  (cond
    [(string=? "f1" key) (switch-to-viewall w)]

    [(string=? "f2" key) (switch-to-search w)]
                                           
    [(string=? "\r" key) (enter-world w)]
     

    [(string=? "\b" key) (backspace-world w)]
      
                           
    [(= (string-length key) 1) (add-1key-world w key)]

    [else w]))

                                          


     

(define kp-ex1 (key-pressed viewall-ex3 "\b"))
(define kp-ex2 (key-pressed viewall-ex3 "e"))
(define kp-ex3 (key-pressed viewall-ex3 "f1"))
(define kp-ex4 (key-pressed viewall-ex3 "f2"))
(define kp-ex5 (key-pressed viewall-ex3 "\r"))

(define kp-ex6 (key-pressed threadview-ex1 "\b"))
(define kp-ex7 (key-pressed threadview-ex1 "e"))
(define kp-ex8 (key-pressed threadview-ex1 "f1"))
(define kp-ex9 (key-pressed threadview-ex1 "f2"))
(define kp-ex10 (key-pressed threadview-ex1 "\r"))

(define kp-ex11 (key-pressed newitem1 "\b"))
(define kp-ex12 (key-pressed newitem1 "e"))
(define kp-ex13 (key-pressed newitem1 "f1"))
(define kp-ex14 (key-pressed newitem1 "f2"))
(define kp-ex15 (key-pressed newitem1 "\r"))

(define kp-ex21 (key-pressed search1 "\b"))
(define kp-ex22 (key-pressed search1 "e"))
(define kp-ex23 (key-pressed search1 "f1"))
(define kp-ex24 (key-pressed search1 "f2"))
(define kp-ex25 (key-pressed search1 "\r"))

;viewall checks

(check-expect (key-pressed viewall-ex1 "h")
              (add-1key-world viewall-ex1 "h"))
(check-expect (key-pressed viewall-ex1 "\b")
              (backspace-world viewall-ex1))
(check-expect (key-pressed viewall-ex1 "f1")
              (switch-to-viewall viewall-ex1))
(check-expect (key-pressed viewall-ex1 "f2")
              (switch-to-search viewall-ex1))
(check-expect (key-pressed (make-viewall "blah" '()) "\r")
              (enter-world (make-viewall "blah" '())))
(check-expect (key-pressed (make-viewall "CATCHUP" '()) "\r")
              (enter-world  (make-viewall "CATCHUP" '())))
(check-expect (key-pressed viewall-ex1 "pa")
              viewall-ex1)
;threadview checks 

(check-expect (key-pressed threadview-ex1 "h")
              (add-1key-world threadview-ex1 "h"))
(check-expect (key-pressed threadview-ex1 "\b")
              (backspace-world threadview-ex1))
(check-expect (key-pressed threadview-ex1 "f1")
              (switch-to-viewall threadview-ex1))
(check-expect (key-pressed threadview-ex1 "f2")
              (switch-to-search threadview-ex1))
(check-expect (key-pressed threadview-ex1 "\r")
              (enter-world threadview-ex1))
(check-expect (key-pressed threadview-ex1 "pa")
              threadview-ex1)

;newitem checks
;(define newitem1 (make-newitem #false "POST1" (list '()))) 
(check-expect (key-pressed newitem1 "h")
              (add-1key-world newitem1 "h"))
(check-expect (key-pressed newitem1 "\b")
              (backspace-world newitem1))
(check-expect (key-pressed newitem1 "f1")
              (switch-to-viewall newitem1))
(check-expect (key-pressed newitem1 "f2")
              (switch-to-search newitem1))
(check-expect (key-pressed newitem1 "\r") 
              (enter-world newitem1))
(check-expect (key-pressed newitem1 "pa")
              newitem1) 
;search checks 
;(define search1 (make-search '() "1"))
(check-expect (key-pressed search1 "h")
              (add-1key-world search1 "h"))
(check-expect (key-pressed search1 "\b")
              (backspace-world search1))
(check-expect (key-pressed search1 "f1")
              (switch-to-viewall search1))
(check-expect (key-pressed search1 "f2")
              (switch-to-search search1))
(check-expect (key-pressed search1 "\r")
              (enter-world search1))
(check-expect (key-pressed search1 "pa")
              search1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   ON-DRAW HANDLERS        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;Draw-World: World -> Image
;To draw the world state based on whether in edit or search mode

(define (draw-world world) 
  (cond
    [(viewall? world) (above (draw-viewall-box (viewall-command world))
                             (draw-history.v3 (viewall-history world)))]
    [(threadview? world) (above THREADVIEW-BOX 
                                (draw-post-and-replies (threadview-post world)))]
    [(newitem? world) (above (draw-newitem-box (newitem-message world))
                             (draw-history.v3 (newitem-history world)))]
    [(search? world)(above (make-search-box (search-searchstring world))
                           (draw-searched-history (search-history world)
                                                  (search-searchstring world)))
                    ]))
 
 
        
(check-expect (draw-world viewall-ex1)
              (above
               (draw-viewall-box (viewall-command viewall-ex1))
               (draw-history.v3 (viewall-history viewall-ex1))))
(check-expect (draw-world threadview-ex1)
              (above THREADVIEW-BOX
                     (draw-post-and-replies (threadview-post threadview-ex1))))
(check-expect (draw-world newitem1)
              (above (draw-newitem-box (newitem-message newitem1))
                     (draw-history.v3 (newitem-history newitem1))))
(check-expect (draw-world search1)
              (above (make-search-box (search-searchstring search1))
                     (draw-searched-history (search-history search1)(search-searchstring search1))))
;;;;;;;;; Helpers for On Draw


;message->image String -> Image
; To turn any message in any world into an image

(define (message->image string)
  (cond
    [(string=? (substring string 0 (min 6 (string-length string))) "ERROR:")
     (text string FONTSIZE ERRORCOLOR)]
    [else (text string FONTSIZE FONTCOLOR)]))

;Examples:
(define mi-ex1 (message->image "Hey!"))
(define mi-ex2 (message->image ""))
(define mi-ex3 (message->image "ERROR: No String"))

;Check-expect
(check-expect (message->image "Hey!")
              (text "Hey!" FONTSIZE FONTCOLOR))
(check-expect (message->image "")
              (text "" FONTSIZE FONTCOLOR))
(check-expect (message->image "ERROR: No String")
              (text "ERROR: No String" FONTSIZE ERRORCOLOR))




;Draw-viewall-box: String -> Image
; Draw a box that displays the command accessor from the make-viewall
(define (draw-viewall-box string)
  (overlay
   (message->image string)
   VIEWALL-BOX))


;Check-expect
(check-expect (draw-viewall-box "Hello World")
              (overlay (message->image "Hello World")
                       VIEWALL-BOX))
(check-expect (draw-viewall-box "")
              (overlay (message->image "")
                       VIEWALL-BOX))


;Draw-newitem-box: String -> Image
;To draw a box that contains the message accessor of a newitem struct
(define (draw-newitem-box str)
  (overlay
   (message->image str)
   NEWITEM-BOX))

(check-expect (draw-newitem-box "hello")
              (overlay (message->image "hello")
                       NEWITEM-BOX))
(check-expect (draw-newitem-box "")
              (overlay (message->image "")
                       NEWITEM-BOX))




;search-box: String -> Image
; Draw a box that displays the edit accessor from the make-search
(define (make-search-box string)
  (overlay
   (message->image string)
   SEARCH-BOX))

;Example:

(define sc-ex1 (make-search-box "Hello World"))
(define sc-ex2 (make-search-box ""))
(define sc-ex3 (make-search-box "Hello!!!"))

;Check-expect
(check-expect (make-search-box "Hello World")
              (overlay (message->image "Hello World")
                       SEARCH-BOX))
(check-expect (make-search-box "")
              (overlay (message->image "")
                       SEARCH-BOX))
;;;;;;;;;;;;;;;



;; List of Posts -> Image
;; Now intstead of Drawing A list of Strings, I have to draw a list of Posts
;;;foldr:[X Y] [X Y -> Y] Y [List-of X] -> Y
;; X = Post
;; Y = Image
;; This function uses the post->image to turn the posts into an image. Purpose: To Draw a [List of
;; Posts] onto the server history

;;;;;THIS FUNCTION ONLY DRAWS A LIST OF POSTS WITHOUT THE REPLIES, USE IN THE DRAW VIEW ALL STATE


(define (draw-just-posts lop)
  (local
    (
     ;;post->img->history: this function places an
     ;;image of a post onto the background to
     ;;represent the history     Sig:  post image -> Image
     ;;         Checks:
     ;(post->img->his (list 1122 (make-post "writes" "me") empty-image)
     ;    --> (above (post->image (list 1122 (make-post "writes" "me" reply1))
     ;      empty-image)
     ;;(post->img->history (list 1488 (make-post "what was" "the hw?" reply2))  
     ;   --> (above (post->img (text "what was the hw?" FONTSIZE FONTCOLOR)
     ;   (text "1344 Idk what the hw was." FONTSIZE FONTCOLOR)                                  
     (define (post->img->history post img)
       (above
        (post->img post)
        img)))                 
    (overlay
     (foldr post->img->history empty-image lop)
     BACKGROUND)))

                

(check-expect (draw-just-posts '()) BACKGROUND)
               
(check-expect (draw-just-posts (list Post-ex1))
              (overlay
               (above
                (text "1232 jefferson.t Every action has an equal opposite reaction"
                      FONTSIZE FONTCOLOR)
                empty-image)
               BACKGROUND))
(check-expect (draw-just-posts (list Post-ex1 Post-ex2))
              (overlay
               (above
                (text "1232 jefferson.t Every action has an equal opposite reaction"
                      FONTSIZE FONTCOLOR)
                (text "1498 burr.a I'm willin to wait for it"
                      FONTSIZE FONTCOLOR))
               BACKGROUND))
              
 


;; post->img :     Post -> Image
;; To turn a single Post into an image so that it can be drawn on the history
;; REMINDER:  A Post is a (list Number (make-post string string [List-of-Reply])


(define (post->img a-post)
  (text (string-append (number->string (first a-post))
                       " "
                       (post-author (second a-post))
                       " "
                       (post-content (second a-post)))
        FONTSIZE FONTCOLOR)) 



(check-expect (post->img (list 1122 (make-post "writes" "me" (list reply1))))
              (text "1122 writes me" FONTSIZE FONTCOLOR)) 
(check-expect (post->img (list 1442 (make-post "trying" "to douse" (list reply1 reply2))))
              (text "1442 trying to douse" FONTSIZE FONTCOLOR))
(check-expect (post->img (list 1233 (make-post "" ""(list reply3))))
              (text "1233  " FONTSIZE FONTCOLOR)) 



 


;;; NOTE: THE FOLLOWING FUNCTION ISN'T EVEN USED BECAUSE IT DRAWS ALL OF THE POSTS WITH ALL OF THE
;;;       REPLIES


#;(define draw-history.v3...)
;; Now intstead of Drawing A list of Strings, I have to draw a list of Posts
;;;foldr:[X Y] [X Y -> Y] Y [List-of X] -> Y
;; X = Post
;; Y = Image
;; This function uses the post->image to turn the posts into an image. Purpose: To Draw a [List of
;; Posts] onto the server history
;; Sign : List-of-Post -> Image
;; This function uses draw-posts-and-replies to create a full view of both the posts with replies 

(define (draw-history.v3 lop)         ;;; THIS FUNCTION ISN'T EVEN USED BECAUSE 
  (local
    (
     ;;post->img->history: this function places an
     ;;image of a post onto the background to
     ;;represent the history     Sig:  post image -> Image
     ;;         Checks:
     ;(post->img->his (list 1122 (make-post "writes" "me") empty-image)
     ;    --> (above (post->image (list 1122 (make-post "writes" "me" reply1))
     ;      empty-image)
     ;;(post->img->history (list 1488 (make-post "what was" "the hw?" reply2))  
     ;   --> (above (post->img (text "what was the hw?" FONTSIZE FONTCOLOR)
     ;   (text "1344 Idk what the hw was." FONTSIZE FONTCOLOR)                                  
     (define (post->img->history post img)
       (above/align "left"
                    (draw-post-and-replies post)
                    img)))                 
    (overlay
     (foldr post->img->history empty-image lop) 
     BACKGROUND)))

               

(check-expect (draw-history.v3 '()) BACKGROUND)
               
(check-expect (draw-history.v3 (list Post-ex1))
              (overlay
               (above/align "left"
                            (text "1232 jefferson.t Every action has an equal opposite reaction"
                                  FONTSIZE FONTCOLOR)
                            (text "1232 hamilton.a Bend over, I'll show you where my shoe fits"
                                  FONTSIZE REPLYCOLOR)
                            (text "1232 hamilton.a Jefferson always reticent with the President"
                                  FONTSIZE REPLYCOLOR)
                            (text "1232 washington.g Sure, if we know who's gonna lead 'em..."
                                  FONTSIZE REPLYCOLOR))
               BACKGROUND))
(check-expect (draw-history.v3 (list Post-ex1 Post-ex2))
              (overlay
               (above/align "left"
                            (text "1232 jefferson.t Every action has an equal opposite reaction"
                                  FONTSIZE FONTCOLOR)
                            (text "1232 hamilton.a Bend over, I'll show you where my shoe fits"
                                  FONTSIZE REPLYCOLOR)
                            (text "1232 hamilton.a Jefferson always reticent with the President"
                                  FONTSIZE REPLYCOLOR)
                            (text "1232 washington.g Sure, if we know who's gonna lead 'em..."
                                  FONTSIZE REPLYCOLOR)
                            (text "1498 burr.a I'm willin to wait for it"
                                  FONTSIZE FONTCOLOR)
                            (text "1498 hamilton.a You're a better lawyer than me"
                                  FONTSIZE REPLYCOLOR)
                            (text "1498 jefferson.t Let's show him what we know..."
                                  FONTSIZE REPLYCOLOR))
               BACKGROUND))
               


;Design a function draw-post-and-replies : Post -> Image, that takes a Post
;and renders both it and any of its Replys. You should distinguish the replies somehow
;(either by font, or indentation, or something) so that it is obvious which is the post and
;which are the replies. When you render an individual Post or Reply, you must show the author’s
;name as well as the content.


;(draw-post-and-replies: Post -> Image    ;;I'm renaming the given function to post->img;;
;Purpose: to render both the post and the replies, with the replies distinguished
; this is a helper function for drawhistory.v3, which is a helper for draw-world

(define (draw-post-and-replies a-post)
  (above/align "left"
               (text (string-append (number->string (first a-post))
                                    " "
                                    (post-author (second a-post))
                                    " "
                                    (post-content (second a-post)))
                     FONTSIZE FONTCOLOR)
               (draw-lor (post-replies (second a-post)))))
                      

(check-expect (draw-post-and-replies
               (list 1122(make-post "NAME"
                                    "CONTENT"
                                    (list
                                     (make-reply
                                      1122
                                      "hamilton.a"
                                      "Bend over, I'll show you where my shoe fits")))))
              (above/align "left"
                           (text "1122 NAME CONTENT" FONTSIZE FONTCOLOR)
                           (text "1122 hamilton.a Bend over, I'll show you where my shoe fits"
                                 FONTSIZE REPLYCOLOR))) 

             
(check-expect (draw-post-and-replies (list 1442 (make-post "king.g" "friends and family..."
                                                           (list
                                                            (make-reply 1442
                                                                        "washington.g"
                                                                        "...to remind you...")
                                                            (make-reply 1442
                                                                        "george.k"
                                                                        "of my love")))))
              (above/align "left"
                           (text "1442 king.g friends and family..." FONTSIZE FONTCOLOR)
                           (text "1442 washington.g ...to remind you..."FONTSIZE REPLYCOLOR)
                           (text "1442 george.k of my love" FONTSIZE REPLYCOLOR)))
                                               
             
(check-expect (draw-post-and-replies (list 1233 (make-post "" "" empty)))
              (text "1233  " FONTSIZE FONTCOLOR))
(check-expect (draw-post-and-replies (list 2222 (make-post "kayama.y" "Hiyaaa!!" empty)))
              (above/align "left"
                           (text "2222 kayama.y Hiyaaa!!" FONTSIZE FONTCOLOR)
                           empty-image))




;;;;;;;;;;;


; draw-searched-history: LoP String -> Image
;Draws the history of Posts but only the ones that contain the searched string

(define (draw-searched-history lop string)
  (draw-history.v3 (list-contains.v2 lop string)))

 
;Check-Expect
(check-expect (draw-searched-history (list Post-ex1) "act")
              (draw-history.v3 (list-contains.v2 (list Post-ex1) "act")))
(check-expect (draw-searched-history  (list Post-ex1 Post-ex2) "wait for")
              (draw-history.v3 (list-contains.v2 (list Post-ex2) "wait for")))
(check-expect (draw-searched-history (list Post-ex1 Post-ex2 Post-ex3) "action") 
              (draw-history.v3 (list-contains.v2 (list Post-ex1 Post-ex3) "action")))
(check-expect (draw-searched-history (list Post-ex1 Post-ex2 Post-ex3) "You don't have the votes")
              (draw-history.v3 (list-contains.v2 '() "You don't have the votes")))




;SIGNATURE :    [Post -> Boolean] LoP   ->   LoP
; To return a List of Posts of only the strings that contain a substring;
;this is a helper for draw-search
;local function that takes in one string and returns a boolean


(define (list-contains.v2 lop search-string)
  (local
    [(define (contains? a-post)
       (string-contains? (post-content (second a-post)) search-string))]
    (filter contains? lop)))

;; contains? [ Post -> Boolean ] 
;; contains? (list 2222 (make-post david.v "Whatsup")) "wha")) -> #true
;; contains? (list 2222 (make-post david.v "Hello")) "wha")) -> #false

;Check Expect
(check-expect (list-contains.v2 (list Post-ex1) "act")
              (list Post-ex1))
(check-expect (list-contains.v2 (list Post-ex1 Post-ex2)  "wait for")
              (list Post-ex2))
(check-expect (list-contains.v2 (list Post-ex1 Post-ex2 Post-ex3) "action")
              (list Post-ex1 Post-ex3))
(check-expect (list-contains.v2 (list Post-ex4) "aaaaaa") '())
  





;foldr:[X Y] [X Y -> Y] Y [List-of X] -> Y
;   [reply image -> image] empty-image [list of replies] -> image
;; draw-lor:   purpose: to turn the list of replies into a single image to be
;implemented in post->image
                      
(define (draw-lor lor)
  (foldr reply->img empty-image lor))

;; reply->img Sig:   Reply Image -> Image
;; Rendering a single reply    
(define (reply->img rep img)
  (above/align "left" 
               (text
                (string-append
                 (number->string (reply-id rep))" "(reply-author rep)" "(reply-content rep))
                FONTSIZE
                REPLYCOLOR)
               img))

(check-expect (reply->img (make-reply 1222 "user" "content") empty-image)
              (above/align "left"
                           (text "1222 user content" FONTSIZE REPLYCOLOR)
                           empty-image))
(check-expect (reply->img (make-reply 1222 "user" "content") (text "1221 The Rock Aghhh!"
                                                                   FONTSIZE
                                                                   REPLYCOLOR))
              (above/align "left"
                           (text "1222 user content" FONTSIZE REPLYCOLOR)
                           (text "1221 The Rock Aghhh!" FONTSIZE REPLYCOLOR)))







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    ON-RECEIVE   HANDLERS     ;;;;;;;;;;;;;;;;;;;;;;



;received-message:   World ServerMsg -> World
; Adds the received message to the list of Strings

#;(define (Post-temp post)
    (first post...)
    (post-author (second post)...)
    (post-content (second post)...)
    (post-replies (second post)))


(define (received-message.v2 w ServerMsg)
  (cond
    [(string=? (first ServerMsg) "POST") (add-post-world w (reformat-post ServerMsg))]
    [(string=? (first ServerMsg) "REPLY")
     (cond
       [(viewall? w)(make-viewall (viewall-command w)
                                  (add-reply-world (viewall-history w) ServerMsg))]
       [(threadview? w)(make-threadview
                        (threadview-post w)
                        (add-reply-world (threadview-history w) ServerMsg))]
       [(newitem? w)(make-newitem
                     (newitem-maybenat w)
                     (newitem-message w)
                     (add-reply-world (newitem-history w) ServerMsg))]
       [(search? w) (make-search
                     (add-reply-world (search-history w) ServerMsg)
                     (search-searchstring w))])]
    [(string=? (first ServerMsg) "ERROR") (add-error-world w (second ServerMsg))]))
        
;;; Check-expects

;;"POST" checks
;(define servermsg-ex1
;(list "POST" 1498 "casambre.v" "Sir, I heard your name at Princeton"))

(check-expect (received-message.v2 (make-newitem #false "never be satisfied" (list))
                                   (list "POST" 1234 "jefferson.t" "string"))
              (add-post-world
               (make-newitem
                #false
                "never be satisfied"
                (list))
               (reformat-post (list "POST" 1234 "jefferson.t" "string"))))

(check-expect (received-message.v2 (make-threadview Post-ex1 (list Post-ex2))
                                   (list "POST" 133 "go.a" "str"))
              (add-post-world (make-threadview Post-ex1 (list Post-ex2))
                              (reformat-post (list "POST" 133 "go.a" "str"))))
                               

(check-expect (received-message.v2 viewall-ex1 servermsg-ex1)
              (add-post-world viewall-ex1 (reformat-post servermsg-ex1)))


(check-expect (received-message.v2 search1 servermsg-ex1)
              (add-post-world search1 (reformat-post servermsg-ex1)))

;; "REPLY" checks
(check-expect (received-message.v2 (make-newitem #false
                                                 "Cuz if the tom-cat can get married"
                                                 (list Post-ex1))
                                   (list "REPLY" 1498 "That depends, who's askin?"))
              (make-newitem #false "Cuz if the tom-cat can get married"
                            (add-reply-world
                             (list Post-ex1)
                             (list "REPLY" 1498 "That depends, who's askin?"))))


(check-expect (received-message.v2 (make-viewall ""
                                                 (list
                                                  (list 1498 (make-post "author" "content" '()))))
                                   (list "REPLY" 1498 "auth." "That depends, who's askin?")) 
              (make-viewall "" (add-reply-world
                                (list (list 1498 (make-post "author" "content" '())))
                                (list "REPLY" 1498 "auth." "That depends, who's askin?"))))


;(define servermsg-ex6 (list "REPLY" 2000 "king.h" "Next to Washington they all look small"))
;(define threadview-ex1 (make-threadview Post-ex1 (list Post-ex2)))
(check-expect (received-message.v2 threadview-ex1 servermsg-ex6)
              (make-threadview Post-ex1 (add-reply-world (list Post-ex2) servermsg-ex6)))
              
;(define search2 (make-search (list Post-ex1) "What"))

(check-expect (received-message.v2 search2 servermsg-ex6)
              (make-search (add-reply-world (list Post-ex1) servermsg-ex6) "What"))

;;"ERROR" checks

#;(define servermsg-ex3
    (list "ERROR" "Yah don' messed up A-Aron"))
(check-expect (received-message.v2 viewall-ex1 servermsg-ex3)
              (add-error-world viewall-ex1 "Yah don' messed up A-Aron"))
(check-expect (received-message.v2 threadview-ex1 servermsg-ex3)
              (add-error-world threadview-ex1 "Yah don' messed up A-Aron"))
(check-expect (received-message.v2 newitem1 servermsg-ex3)
              (add-error-world newitem1 "Yah don' messed up A-Aron"))
(check-expect (received-message.v2 search1 servermsg-ex3)
              (add-error-world search1 "Yah don' messed up A-Aron"))             
                                  
                                 
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;              
  
;; Sig:   World Post -> World
;; Adds a POST to the worlds list of posts

 
(define (add-post-world w post)
  (cond
    [(viewall? w) (make-viewall (viewall-command w)(cons post (viewall-history w)))] 
    [(threadview? w) (make-threadview(threadview-post w)(cons post (threadview-history w)))]
    [(newitem? w) 
     (make-newitem (newitem-maybenat w) (newitem-message w)(cons post (newitem-history w)))]
    [(search? w) (make-search (cons post (search-history w)) (search-searchstring w))]))
     
 
;;;CHECK EXPECTS 
(check-expect (add-post-world (make-viewall "reply 1232" (list Post-ex1)) Post-ex2)
              (make-viewall "reply 1232" (list Post-ex2 Post-ex1)))
(check-expect (add-post-world (make-threadview Post-ex1 (list Post-ex2)) Post-ex3)
              (make-threadview Post-ex1 (list Post-ex3 Post-ex2))) 
(check-expect (add-post-world (make-newitem #false "Theodosia" (list)) Post-ex1) 
              (make-newitem #false "Theodosia" (list Post-ex1)))
(check-expect (add-post-world (make-search (list Post-ex1 Post-ex2) "find this string") Post-ex3)
              (make-search (list Post-ex3 Post-ex1 Post-ex2) "find this string"))
                                           
 


;;;;;;;;;;;;;;;;;;;;;



;; World String -> World
;; Adds an Error to the world

(define (add-error-world w str)
  (cond
    [(viewall? w) (make-viewall
                   (string-append "ERROR: " str " <--- " (viewall-command w))
                   (viewall-history w))]
    [(threadview? w) (make-viewall
                      (string-append "ERROR: " str " <--- " "")
                      (threadview-history w)) ] 
    [(newitem? w) (make-newitem (newitem-maybenat w)
                                (string-append "ERROR: " str " <--- "(newitem-message w))
                                (newitem-history w))]
    [(search? w) (make-search (search-history w)
                              (string-append "ERROR: " str " <--- " (search-searchstring w)))]))
     
     

(check-expect (add-error-world (make-viewall "reply 1232" (list Post-ex1)) "something wrong")
              (make-viewall "ERROR: something wrong <--- reply 1232" (list Post-ex1)))
(check-expect (add-error-world (make-viewall "" (list empty)) "You got an error.")
              (make-viewall "ERROR: You got an error. <--- " (list empty))) 
(check-expect (add-error-world (make-threadview Post-ex1 (list Post-ex2)) "Wrong!")
              (make-viewall "ERROR: Wrong! <--- " (list Post-ex2)))
(check-expect (add-error-world (make-newitem #false "Theodosia" (list '())) "add this error")
              (make-newitem #false "ERROR: add this error <--- Theodosia" (list empty)))
(check-expect (add-error-world (make-search (list Post-ex1 Post-ex2) "find this string") "Wrong!")
              (make-search (list Post-ex1 Post-ex2) "ERROR: Wrong! <--- find this string"))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;  add-reply-world
;History ServerMsg -> History 
;adds a Reply to the relevant Post in the History
;you need to find the Post in the History with the same ID# as the one present in the reply
;message, create a new post that adds the reply to the previous list of replies, and then uses
;replace-post to create a new History with the updated post.

(check-expect (add-reply-world (list (list 1111 (make-post "casambre.v" "hello!" empty)))
                               (list "REPLY" 1111 "horace.m" "hey!"))
              (list
               (list 1111 (make-post "casambre.v" "hello!" (list
                                                            (make-reply 1111 "horace.m" "hey!"))))))
 
(check-expect (add-reply-world 
               (list (list 1498 (make-post "AUTHOR"
                                           "CONTENT"
                                           (list (make-reply 1498 "AUTHOR-2" "REPLY-2")
                                                 (make-reply 1498 "AUTHOR-1" "REPLY-1"))))
                     (list 2211 (make-post "AUTHOR"
                                           "CONTENT"
                                           empty)))
               (list "REPLY" 1498 "AUTHOR-3" "REPLY-3"))
              (list (list 1498 (make-post "AUTHOR"
                                          "CONTENT"
                                          (list
                                           (make-reply 1498 "AUTHOR-3" "REPLY-3")
                                           (make-reply 1498 "AUTHOR-2" "REPLY-2")
                                           (make-reply 1498 "AUTHOR-1" "REPLY-1")))) 
                    (list 2211 (make-post "AUTHOR" "CONTENT" empty))))

(check-expect (add-reply-world (list (list 9999 (make-post "user.me" "stuff stuff" empty)))
                               (list "REPLY" 9998 "ohno" "wrong-id!"))
              (list (list 9999 (make-post "user.me" "stuff stuff" empty)))) 
;; find the post that matches id 
;; add the reply to post-replies
;; recreate the history with the updated post in the same place (replace)
(define (add-reply-world his svmsg)
  (local (
          (define (find-post n his)  ;; n is the id of the reply
            (assoc n his)) ;;; see below for details
          (define found-post
            (find-post (second svmsg) his)))
    (if (boolean? found-post)
        his
        (replace-post his (list (second svmsg)
                                (make-post
                                 (post-author (second found-post))
                                 (post-content (second found-post))

                                 (cons (make-reply
                                        (second svmsg)
                                        (third svmsg)
                                        (fourth svmsg))
                                       (post-replies (second found-post)))))))))
 
                                         
; assoc : Number History -> [Maybe Post]
;; returns the post of the if
;;Where number is the id of the REPLY and History is the List of Posts assoc is searching
;; to find the matching posts
;;   Check-expect (assoc 1234 (list 1234 (make-post "casambre.v" "ayooo"  empty))
;;                            (lisk 3456 (make-post "user" "yo" empty)))
;;                         --> (list 1234 (make-post "casambre.v" "ayoo" empty))
;;   Check-expect (assoc 9999 (list 3511 (make-post "casambre.v" "ayoo" empty))
;;                         --> #false

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;replace-post : History Post -> History

;searches for a Post in the given History that has the same ID# as the given Post,
;and replaces it with the given Post.


(define (replace-post his pos)
  (cond
    [(empty? his) empty]
    [(cons? his) (if (same-id?  pos (first his))
                     (cons pos (rest his))
                     (cons (first his) (replace-post (rest his) pos)))]))


(check-expect (replace-post (list Post-ex1)
                            (list 2211 (make-post "seabury.s" "Wrong. ID." empty)))
              (list Post-ex1)) 
(check-expect (replace-post (list Post-ex1 Post-ex2)
                            (list 1498 (make-post "casa.v" "Take me down to the.." empty))) 
              (list Post-ex1 (list 1498 (make-post "casa.v" "Take me down to the.." empty))))
(check-expect (replace-post (list Post-ex1 Post-ex2 Post-ex3)
                            (list 1776 (make-post "casa.v" "Replace!" empty)))
              (list Post-ex1 Post-ex2 (list 1776 (make-post "casa.v" "Replace!" empty))))
                                                                    
(check-expect (replace-post (list Post-ex1 Post-ex2 Post-ex3)
                            (list 1498 (make-post "casa.v" "e!!!!" empty)))
              (list Post-ex1 (list 1498 (make-post "casa.v" "e!!!!" empty)) Post-ex3))
                                                                   

 
; Purpose: to check whether two posts have the same id
; Post Post -> Boolean
(check-expect (same-id? (list 1111 "author" "content" empty)(list 1111 "different" "post")) #true)
(check-expect (same-id? (list 1111 "author" "content" empty)(list 9919 "different" "post")) #false)

(define (same-id? post1 post2)
  (= (first post1)(first post2)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       


               
;;;;; reformat-post: ServerMsg -> Post
;;;;; Turns the ServerMsg format of a post into the History format of a post       


(check-expect (reformat-post servermsg-ex1) 
              (list 1498 (make-post
                          "casambre.v"
                          "Sir, I heard your name at Princeton" (list))))
(check-expect (reformat-post servermsg-ex4)
              (list 1998
                    (make-post "hamilton.a" "I was seeking an accelerated course of study" '())))
(check-expect (reformat-post servermsg-ex7)
              (list 2000 (make-post "king.g" "Oceans rise, empires fall..." '())))

 

(define (reformat-post ServerMsg)
  (list (second ServerMsg)
        (make-post (third ServerMsg)(fourth ServerMsg) '())))
 
   
        
;; server-msg-kind? : ServerMsg -> Boolean
;; Purpose: Determines what kind of post the server msg is bringing in an performs the
;; NOTE: I understand this function is kind of useless but I just think it looks nicer

(define (server-msg-kind? ServerMsg)
  (cond
    [(string=? (first ServerMsg) "POST") "POST"]
                                                                 
    [(string=? (first ServerMsg) "REPLY") "REPLY"]
    [(string=? (first ServerMsg) "ERROR") "ERROR"]))

(check-expect (server-msg-kind? servermsg-ex1) "POST")
(check-expect (server-msg-kind? servermsg-ex2) "REPLY")
(check-expect (server-msg-kind? servermsg-ex3) "ERROR")
(check-expect (server-msg-kind? servermsg-ex4) "POST")
(check-expect (server-msg-kind? servermsg-ex7) "POST")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;   ALL OF THE OLD FUNCTIONS  ;;;;;;;;;;;;;



;  draw-set-amount: LoS number  -> Image
; To draw only a set amount of images, given by the number, so that the image doesn't overflow
;on screen
; this is a helper to our draw-history function.
;This allows us to make the font size of the strings in history
;whatever we want, since it can vary


#;(define (draw-set-amount los num)
    (cond
      [(empty? los) BACKGROUND]
      [(= num 1) (place-image
                  (message->image (first los))
                  (half-width BACKGROUND)
                  (* num 50)
                  BACKGROUND)]
      [else (place-image
             (message->image (first los))
             (half-width BACKGROUND)
             (* num 50)
             (draw-set-amount (rest los) (- num 1)))]))


;Examples:
#;(define dsa-ex1 (draw-set-amount '() 9))
#;(define dsa-ex2 (draw-set-amount los-ex3 2))  
#;(define dsa-ex3 (draw-set-amount los-ex4 9))
 
;Check-expect
#;(check-expect (draw-set-amount '() 9)
                BACKGROUND)
#;(check-expect (draw-set-amount los-ex2 2)
                (place-image (message->image (first los-ex2))
                             (half-width BACKGROUND)
                             (* 2 50)
                             (place-image (message->image (first (rest los-ex2)))
                                          (half-width BACKGROUND)
                                          (* 1 50)
                                          BACKGROUND)))

#;(check-expect (draw-set-amount los-ex2 1)
                (place-image (message->image (first los-ex2))
                             (half-width BACKGROUND)
                             (* 1 50)
                             BACKGROUND))



          
#;(define SPACE-BETWEEN-POSTS 50)
;draw-history: LoS -> Image
;To draw the previous messages onto the program
;Only draw the amount that fits if LoS is too large
#;(define (draw-history LoS)
    (draw-set-amount LoS ( - (/ (image-height BACKGROUND) SPACE-BETWEEN-POSTS) 1)))

;(define dh-ex1 (draw-history '()))
;(define dh-ex2 (draw-history los-ex3))

;Check-expects
#;(check-expect (draw-history '())
                (draw-set-amount '() (length '())))

; not > than amount that can fit
 
#; (check-expect (draw-history los-ex3)
                 (draw-set-amount los-ex3
                                  ( - ( / (image-height BACKGROUND) SPACE-BETWEEN-POSTS) 1)))

; > greater than amount that can fit
#; (check-expect (draw-history los-ex4) 
                 #;(draw-set-amount los-ex4
                                    (- ( / (image-height BACKGROUND) SPACE-BETWEEN-POSTS) 1)))



; half-width: Image -> Number
; gets the value of half of the image given    ;used in our draw-set amount function
; as the x value in our place-image 

#;(define (half-width image)
    (/ (image-width image) 2))

;Examples
;(define half-ex1 (half-width (square 200 "solid" "blue")))
;(define half-ex2 (half-width (rectangle 100 200 "solid" "blue")))

; Check-expects
 
;(check-expect (half-width (square 200 "solid" "blue")) 100)
;(check-expect (half-width (rectangle 100 200 "solid" "blue")) 50)
;(check-expect (half-width (circle 50 "solid" "blue")) 50)







#; (define (received-message world ServerMsg)
     (cond
       [(search? world)
        (make-search
         (search-edit world)
         (cons message (search-history world))
         (search-search world))]
       [(editview? world)
        (make-editview
         (editview-edit world)
         (cons message (editview-history world))
         (editview-search world))]))


;Examples:

#;(define (draw-set-amount.v2 los)
    (cond
      [(empty? los) BACKGROUND]
      [(cons? los) (overlay
                    (above
                     (message->image (first los))
                     (draw-history (rest los)))
                    BACKGROUND)]))       ;;Draw-set amount to be abastracted

;foldr:[X Y] [X Y -> Y] Y [List-of X] -> Y
;draw-history.v2  X= String  Y = Image
;Purpose: to render a single image of the message history
; by turning a list of messages into a list of images, and placing
; each image on top of each other. The newest messages will be at the top

#;(define (draw-history.v2 los)
    (local
      (
       ;;msg->img->history: this function places an
       ;;image of a message onto the background to
       ;;represent the history     Sig:  string image -> Image
       ;;         Checks:
       ;(msg->img->his "What was the hw?" empty-image)
       ;    --> (above (message->image (text "What was the hw?" FONTSIZE FONTCOLOR)
       ;      empty-image)
       ;;(msg->img->history "Idk what the hw was."
       ;   --> (above (message->image (text "what was the hw?" FONTSIZE FONTCOLOR)
       ;   (text "Idk what the hw was." FONTSIZE FONTCOLOR)                                  
       (define (msg->img->his msg img)
         (above
          (message->image msg)
          img)))                 
      (overlay
       (foldr msg->img->his empty-image los)
       BACKGROUND)))

 
;;; Check-expects:
#;(check-expect (draw-history.v2 los-ex1) BACKGROUND)
               
#;(check-expect (draw-history.v2 los-ex2)
                (overlay
                 (above
                  (text "Hello" FONTSIZE FONTCOLOR)
                  (text "World" FONTSIZE FONTCOLOR))
                 BACKGROUND))
#;(check-expect (draw-history.v2 los-ex3)
                (overlay
                 (above
                  (text "CS2500" FONTSIZE FONTCOLOR)
                  (text "Fundies" FONTSIZE FONTCOLOR)
                  (text "Hello" FONTSIZE FONTCOLOR)
                  (text "World" FONTSIZE FONTCOLOR))
                 BACKGROUND))

;Examples:
#;(define rm-ex1 (received-message sp-ex1 "Hey!"))
#;(define rm-ex2 (received-message sp-ex2 "Hey!"))
#;(define rm-ex3 (received-message ep-ex1 "Hey!"))
#;(define rm-ex4 (received-message ep-ex2 "Hey!"))

;Check-Expect
#;(check-expect (received-message sp-ex1 "Hey!")
                (make-search (search-edit sp-ex1)
                             (cons "Hey!" (search-history sp-ex1))
                             (search-search sp-ex1)))
#;(check-expect (received-message ep-ex1 "Hey!") 
                (make-editview (editview-edit ep-ex1)
                               (cons "Hey!" (editview-history ep-ex1))
                               (editview-search ep-ex1)))
#;(check-expect (received-message sp-ex2 "Hey!")
                (make-search (search-edit sp-ex2)
                             (cons "Hey!" (search-history sp-ex2))
                             (search-search sp-ex2)))
#; (check-expect (received-message ep-ex2 "Hey!")
                 (make-editview (editview-edit ep-ex2)
                                (cons "Hey!" (editview-history ep-ex2))
                                (editview-search ep-ex2)))



;A History is a [List of Strings]
;  INTERPRETATION: a list of the prior posts received from the server

; A list of strings is one of
; -empty
; -(cons String LoS)
#;(define (ListOfStrings-temp los)
    (cond
      [(empty? los)...]
      [(cons? los) (...(first los)... (ListOfStrings-temp (rest los)...))]))




;message-content: String-> String
;Checks the content of the message after the second :

#;(define (message-content string)
    (if (> (length (string-split string ":")) 2)
        (los->string (first (rest (rest (string-split string ":")))))
        string))
  
;;local function that takes in one string and returns a boolean
  
; list-contains: LoS String -> LoS
; To return a LoS of only the strings that contain a substring   ; this is a helper for draw-search
; given as a string

#;(define (list-contains los substr)
    (cond
      [(empty? los) '()]
      [(cons? los) (if (string-contains? (message-content (first los)) substr)
                       (cons (first los)(list-contains (rest los) substr))
                       (list-contains (rest los) substr))]))



;;los->string: Los - > String
;; turns a list of strings into a string
#;(define (los->string los)
    (cond
      [(empty? los) '()]
      [(cons? los) (string-append (first los) (los->string (rest los)))]
      [else los]))
 
(define-struct editview [edit history search])
; A EditviewPosts is a (make-editview edit history search)
; where edit is a string, history is a list of strings, and search is a string


;INTERPRETATION: Means the user is viewing all posts and potentially typing in a new post
(define (EditviewPosts-temp ep)
  (...(editview-edit ep)...
      (hitsory-temp (editview-history ep))...
      (editview-search ep)...))


;Examples
(define ep-ex1 (make-editview "Hey Guys!" (list Post-ex1) ""))
(define ep-ex2 (make-editview "" '() "")) ;;; This would represent a completely new World    
(define ep-ex3 (make-editview "Hey" (list Post-ex1) "Fundies"))



;A Edit is a String
; INTERPRETATION: the contenets of the post the user is currently editing
; No Template because this is atomic data

(define edit-ex1 "Hello")
(define edit-ex2 "")
(define edit-ex3 "Why must we switch partners?")
;;; You can have a non-empty string
;in your search-accessor because the data doesn't change, when switching from
; an edit to a search


