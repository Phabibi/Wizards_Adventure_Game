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


