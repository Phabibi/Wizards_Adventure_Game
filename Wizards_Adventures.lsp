;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Wizards Adventures ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;;;;Author: Parsa Habibi 
;;;;;; Date:  September 22nd 2019 


(defparameter *places* '((living_room (you are in the living room.))
                        (garden (you are now in the garden.)) 
                        (attic  (you are in the attic))))     

;; making the key_values for places 

(defun describe_loc (location nodes)
  (cadr (assoc location nodes)))

;Paths (location direction obj)  

(defparameter *edges* '((living_room (garden west door)
                                     (attic upstairs ladder)) 
                       (garden (living_room east door))
                       (attic  (living_room downstairs ladder))))

;Object paths (obj_holding_obj  location  object description) 

(defparameter *obj_edge* '((living_room (couch south wizard sleeping) 
                                        (table south-west whiskey half-full)
                                        (floor north basket dusty))
                           (garden (rocks corner well dirty))
                           (attic  (floor corner box mystirous)
                                   (floor north-west doll creepy))))

;edge_describe
;;describes the paths availble from the room your in                        
(defun describe_edge (edge)  
  `(there is a ,(caddr edge) going ,(cadr edge) from here.)) 

;obj_describe 
;; describes the object in the room

(defun describe_obj (obj) 
  `(there is a ,(cadddr obj) ,(caddr obj) on the ,(car obj) ,(cadr obj) of the place.)) 


;;describing multiple paths
(defun describe-paths (location edges)
(apply #'append (mapcar (function describe_obj) 
                        (cdr (assoc location edges)))))

     
;; Describing it all

(defparameter *loc* 'living_room)
(defun look () 
  (append (describe_loc *loc* *places*)
          (describe-paths *loc* *obj_edge*)))
;; walking 

(defun walk (direction)
 (let ((next (find direction
                   (cdr (assoc *loc* *edges* ))
                   :key #'cadr)))
 (if next
     (progn (setf *loc* (car next))
            (look))
     '(you cannot go that way.))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;THE REPLE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun startgame()
  (let ((cmd (game_read)))
           (unless (eq (car cmd) 'bye)
             (print (game_eval cmd))
             (startgame))))

(defun game_read ()
  ;;this will read a command from command line in a "(command)" format 
  ;;the 'String is there since 'String command will make "command"
  (let ((cmd (read-from-string 
                (concatenate 'string "(" (read-line) ")"))))
  ;; this little guy is a local function that will just quotes the command 
  ;; walk east  -> (walk 'east) given the fact that cmd = "(walk east)" 
  ;; *side-note* : quote-it will call it self untill cdr is just NIL aka we've parsed everythng
       (flet ((quote-it (x)
                (list 'quote x)))
             (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))


(defparameter bin '(look walk pickup inventory))
;; This guy evalutes our command , in a way we want , ie doesnt allow what shouldnt be allowed 
(defun game_eval (expr)
  (if (member (car expr) bin)
      (eval expr)
     '(I dont know what youre saying dude!)))
 
