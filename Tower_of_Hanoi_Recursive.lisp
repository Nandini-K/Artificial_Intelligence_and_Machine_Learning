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
				(write-line	"	A	B	C")
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

; Recursive Functions

	(defun toh-A-to-C (n)	(cond((eql n 0) (return-from toh-A-to-C)))
				(toh-A-to-B (- n 1))
				(legal-move-AC)
				(toh-B-to-C (- n 1))
	)
	(defun toh-A-to-B (n)	(cond((eql n 0) (return-from toh-A-to-B)))
				(toh-A-to-C (- n 1))
				(legal-move-AB)
				(toh-C-to-B (- n 1))
	)
	(defun toh-B-to-C (n)	(cond((eql n 0) (return-from toh-B-to-C)))
				(toh-B-to-A (- n 1))
				(legal-move-BC)
				(toh-A-to-C (- n 1))
	)
	(defun toh-C-to-A (n)	(cond((eql n 0) (return-from toh-C-to-A)))
				(toh-C-to-B (- n 1))
				(legal-move-AC)
				(toh-B-to-A (- n 1))
	)
	(defun toh-C-to-B (n)	(cond((eql n 0) (return-from toh-C-to-B)))
				(toh-C-to-A (- n 1))
				(legal-move-BC)
				(toh-A-to-B (- n 1))
	)
	(defun toh-B-to-A (n)	(cond((eql n 0) (return-from toh-B-to-A)))
				(toh-B-to-C (- n 1))
				(legal-move-AB)
				(toh-C-to-A (- n 1))
	)

;**************************************************************************************************

; Main

(setq n height)
(show-stacks)
(toh-A-to-C n )

;**************************************************************************************************

; End