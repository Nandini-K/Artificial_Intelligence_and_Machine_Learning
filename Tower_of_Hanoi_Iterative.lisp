;**************************************************************************************************

;	Copyright (c) 2016 Nandini Khanwalkar 
;	nandini2@pdx.edu

;**************************************************************************************************

; Tower of Hanoi : Iterative

;**************************************************************************************************

; Welcome Message

(write-line "Tower of Hanoi - Puzzle Solver")
(write-line "Enter the height for the tower :")

; I/O for no. of disks

(setq height (read))

;**************************************************************************************************

; Initialize the towers/stacks

(setq A (make-array (+ 1 height) :fill-pointer 0))
(setq B (make-array (+ 1 height) :fill-pointer 0))
(setq C (make-array (+ 1 height) :fill-pointer 0))
(setq value (+ 1 height))

(vector-push value A)	(vector-push value B)	(vector-push value C)
(decf value 1)
(loop for top from 1 to height 
	do	(vector-push value A)
		(decf value 1)
)

(setq A.top height) (setq B.top 0) (setq C.top 0)

;**************************************************************************************************

; Function to print stacks

	(defun show-stacks ()	(setq cell height)
				(terpri)
				(write-line "   ----------------------------")
				(terpri)
				(write-line "	A	B	C")
				(loop 
					(terpri)
					(write-string "	")	(setq x (aref A cell))	(write x) 
					(write-string "	")	(setq y (aref B cell))	(write y) 
					(write-string "	")	(setq z (aref C cell))	(write z) 
					(decf cell 1)
					(when (< cell 1) (return))		
				)
				(terpri)
	)

;**************************************************************************************************

; Functions for legal moves

	(defun legal-move-AB ()	(cond	((> (aref B B.top) (aref A A.top)) 	(vector-push (vector-pop A) B)
										(setf (aref A A.top) NIL)
										(decf A.top 1)
										(incf B.top 1)
					)
					((> (aref A A.top) (aref B B.top))	(vector-push (vector-pop B) A)
										(setf (aref B B.top) NIL)
										(decf B.top 1)
										(incf A.top 1)
					)
				)
				(show-stacks)
	)

	(defun legal-move-AC ()	(cond	((> (aref C C.top) (aref A A.top)) 	(vector-push (vector-pop A) C)
										(setf (aref A A.top) NIL)
										(decf A.top 1)
										(incf C.top 1)
					)
					((> (aref A A.top) (aref C C.top))	(vector-push (vector-pop C) A)
										(setf (aref C C.top) NIL)
										(decf C.top 1)
										(incf A.top 1)
					)
				)
				(show-stacks)
	)

	(defun legal-move-BC ()	(cond	((> (aref B B.top) (aref C C.top)) 	(vector-push (vector-pop C) B)
										(setf (aref C C.top) NIL)
										(decf C.top 1)
										(incf B.top 1)
					)
					((> (aref C C.top) (aref B B.top))	(vector-push (vector-pop B) C)
										(setf (aref B B.top) NIL)
										(decf B.top 1)
										(incf C.top 1)
					)
				)
				(show-stacks)
	)

;**************************************************************************************************

; Main Body
; While incomplete
; 	Make a legal move

(show-stacks)

(cond	((evenp height)	(loop
				(legal-move-AB)	
				(legal-move-AC)
				(when (eql (aref A A.top) (aref B B.top)) (return))
				(legal-move-BC)
				(when (eql (aref A A.top) (aref B B.top)) (return))
			)
	)
	((oddp height)	(loop
				(legal-move-AC)
				(when (eql (aref A A.top) (aref B B.top)) (return))
				(legal-move-AB)				
				(legal-move-BC)
				(when (eql (aref A A.top) (aref B B.top)) (return))
			)
	)
)

;**************************************************************************************************

; End 
