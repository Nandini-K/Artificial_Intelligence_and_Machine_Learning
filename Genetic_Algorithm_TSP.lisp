;**************************************************************************************************

;	Copyright (c) 2016 Nandini Khanwalkar 
;	nandini2@pdx.edu

;**************************************************************************************************

; Take the input file, read the input and store it in city structures

(defstruct city	id
		x
		y
)

(defvar line) 
(terpri)
(write-line 	"Travelling Salesman Problem - Optimal Solution Finder")
(write-line 	"Enter the name of input file: ")
(setf inputfile (read))

(setf in (open inputfile))

(setf line (read-line in))
(setq N (parse-integer line))

(cond ( (eql N 0)	(terpri)
		(write-line "Input Error:	The input must have atleast 1 city.")
		(write-line "		Please try again with a valid input.")
		(terpri)
		(close in)
		(exit)
	)
)

(setq citylist())
(loop for i from 1 to N do
	(setq val (read-line in))
	(setq cdata (with-input-from-string (in val)
	(loop for cdata = (read in nil nil) while cdata collect cdata))
	)
	(setq id (first cdata))
	(setq x	(second cdata))
	(setq y (third cdata))
	(setq city (make-city :id id :x x :y y))
	(setq citylist (cons city citylist))
)

(setf line (read-line in))
(setq startcity (parse-integer line))

(cond ( (eql N 1) (setf fitpath (make-array '(2) :fill-pointer 0))
		(vector-push startcity fitpath)
		(vector-push startcity fitpath)
		(terpri)
		(write-string "Fittest Solution: ")
		(terpri)
		(write (list 'Path- fitpath 'Distance- '0))
		(terpri)
		(exit)
	)
)

(close in)

;**************************************************************************************************

; Distance matrix

(defun dist-c1-c2 (x1 x2 y1 y2) 
(return-from dist-c1-c2 (sqrt(+ (expt (- x1 x2) 2) (expt (- y1 y2) 2))))				
)

(setf dist-mat (make-array (list (+ 1 N) (+ 1 N))))

(loop for city in citylist do 	(setq city1 city)
	(loop for city in citylist do	(setq city2 city)
		(cond ( (< (city-id city2) (city-id city1)) 
				(setq x1 (city-x city1))
				(setq x2 (city-x city2))
				(setq y1 (city-y city1))
				(setq y2 (city-y city2))
				(setf (aref dist-mat (city-id city1) (city-id city2)) (dist-c1-c2 x1 x2 y1 y2))
				(setf (aref dist-mat (city-id city2) (city-id city1)) (dist-c1-c2 x1 x2 y1 y2))
			)
		)
	)
)

;**************************************************************************************************

; Initialize population, evaluate fitness and sort w.r.t. fitness

(setq touchpoints (make-array (list N) :fill-pointer 0))
(loop for city in citylist do
	(vector-push (city-id city) touchpoints)
)

(defstruct tour path
	distance
	fitness
)

(setq genpop())

(loop for l from 0 to 99 do
	(setq tourpath (make-array (list N) :fill-pointer 0)) 
	(setq inserted())
	(setq fp 0)
	(loop	(setf r (random N))
		(cond (	(null (member r inserted))	
				(vector-push (aref touchpoints r) tourpath)
				(setq inserted (cons r inserted))
				(incf fp)
			)
		)
		(when (eql fp N) (return))
	)
	(setq tourdist 0)
	(loop for p from 1 below N do 
		(setq c1 (aref tourpath p))
		(setq c2 (aref tourpath (- p 1)))
		(setq tourdist (+ tourdist (aref dist-mat c1 c2)))	
	)
	(setq c1 (aref tourpath 0)) (setq c2 (aref tourpath (- N 1)))
	(setq tourdist (+ tourdist (aref dist-mat c1 c2)))
	(setq fitness (/ 500 tourdist))
	(setq tour (make-tour :path tourpath :distance tourdist :fitness fitness))
	(setq genpop (cons tour genpop))
)

(stable-sort genpop #'> :key #'tour-fitness)

; Output fittest solution for 1st generation	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(setq fittour (elt genpop 0))
(setq fittourpath (tour-path fittour))
(setf fitpath (make-array (list (+ N 1)) :fill-pointer 0))
(setq startpos (position startcity fittourpath))
(loop for pos from startpos below N do
	(vector-push (aref fittourpath pos) fitpath)
)
(loop for pos from 0 below startpos do
	(vector-push (aref fittourpath pos) fitpath)
)
(vector-push startcity fitpath)
(terpri)
(write-string "1st Generation Fittest Solution: ")
(terpri)
(write (list 'Path- fitpath 'Distance- (tour-distance fittour)))

;**************************************************************************************************

; GA : Elitism, mutation, crossover

(loop for generation from 2 to 100 do

; Survivors :		~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	(setq gennex())
	(loop for e from 0 to 19 do	
		(setq gennex (cons (elt genpop e) gennex))
	)

; Mutation :		~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	(loop for popfit from 0 to 19 do	
		(setf p (random N))
		(setf q (random N))
		(setf elite (elt genpop popfit))
		(setf elitetour (tour-path elite))
		(setf tempt1 (make-array (list N)))
		(setf tempt2 (make-array (list N)))
		(loop for cell from 0 below N do	
			(setf (aref tempt1 cell) (aref elitetour cell))
			(setf (aref tempt2 cell) (aref elitetour cell))
		)
		
; Mutation 1 		~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	
		(setf tempc1 (aref tempt1 p))
		(setf (aref tempt1 p) (aref tempt1 q))
		(setf (aref tempt1 q) tempc1)

		(setq tourdist 0)
		(loop for i from 1 below N do 
			(setq c1 (aref tempt1 i))
			(setq c2 (aref tempt1 (- i 1)))
			(setq tourdist (+ tourdist (aref dist-mat c1 c2)))	
		)
		(setq c1 (aref tempt1 0)) (setq c2 (aref tempt1 (- N 1)))
		(setq tourdist (+ tourdist (aref dist-mat c1 c2)))
		(setq fitness (/ 500 tourdist))
		(setq tour (make-tour :path tempt1 :distance tourdist :fitness fitness))
		(setq gennex (cons tour gennex))

; Mutation 2		~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

		(setf tempc2 (aref tempt2 (- (- N p) 1)))
		(setf (aref tempt2 (- (- N p) 1)) (aref tempt2 (- (- N q) 1)))
		(setf (aref tempt2 (- (- N q) 1)) tempc2)
		(setq tourdist 0)
		(loop for i from 1 below N do 
			(setq c1 (aref tempt2 i))
			(setq c2 (aref tempt2 (- i 1)))
			(setq tourdist (+ tourdist (aref dist-mat c1 c2)))	
		)
		(setq c1 (aref tempt2 0)) (setq c2 (aref tempt2 (- N 1)))
		(setq tourdist (+ tourdist (aref dist-mat c1 c2)))
		(setq fitness (/ 500 tourdist))
		(setq tour (make-tour :path tempt2 :distance tourdist :fitness fitness))
		(setq gennex (cons tour gennex))
	)
	
; Crossover :			~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	(loop for popfit from 0 to 19 do
		(setf elite1 (elt genpop popfit))
		(setf elitetour1 (tour-path elite1))
		(setf elite2 (elt gennex (+ popfit 20)))
		(setf elitetour2 (tour-path elite2))
		(setf elite3 (elt gennex (+ popfit 40)))
		(setf elitetour3 (tour-path elite3))
			
		(setf parent1 (make-array (list N)))
		(setf parent2 (make-array (list N)))
		(setf parent3 (make-array (list N)))
		(setf child1 (make-array (list N) :fill-pointer 0))
		(setf child2 (make-array (list N) :fill-pointer 0))
		(loop for cell from 0 below N do	
			(setf (aref parent1 cell) (aref elitetour1 cell))
			(setf (aref parent2 cell) (aref elitetour2 cell))
			(setf (aref parent3 cell) (aref elitetour3 cell))
		)
		
		(setf cp (random N))
		(loop for cell from 0 to cp do	
			(vector-push (aref parent2 cell) child1)
			(vector-push (aref parent3 cell) child2)
		)

; Crossover 1		~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

		(loop for cell from 0 below N do	
			(cond ( (null (find (aref parent1 cell) child1))	
					(vector-push (aref parent1 cell) child1)
				)
			)
		)
		(setq tourdist 0)
		(loop for i from 1 below N do 
			(setq c1 (aref child1 i))
			(setq c2 (aref child1 (- i 1)))
			(setq tourdist (+ tourdist (aref dist-mat c1 c2)))
		)	
		(setq c1 (aref child1 0)) (setq c2 (aref child1 (- N 1)))
		(setq tourdist (+ tourdist (aref dist-mat c1 c2)))
		(setq fitness (/ 500 tourdist))
		(setq tour (make-tour :path child1 :distance tourdist :fitness fitness))
		(setq gennex (cons tour gennex))

; Crossover 2		~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

		(loop for cell from 0 below N do	
			(cond ( (null (find (aref parent1 cell) child2))	
					(vector-push (aref parent1 cell) child2)
				)
			)
		)
		(setq tourdist 0)
		(loop for i from 1 below N do 	
			(setq c1 (aref child2 i))
			(setq c2 (aref child2 (- i 1)))
			(setq tourdist (+ tourdist (aref dist-mat c1 c2)))	
		)
		(setq c1 (aref child2 0)) (setq c2 (aref child2 (- N 1)))
		(setq tourdist (+ tourdist (aref dist-mat c1 c2)))
		(setq fitness (/ 500 tourdist))
		(setq tour (make-tour :path child2 :distance tourdist :fitness fitness))
		(setq gennex (cons tour gennex))
	)
	
	(stable-sort gennex #'> :key #'tour-fitness)

	(setq genpop gennex)
)

; Output fittest solution for 100th generation	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(setq fittour (elt gennex 0))
(setq fittourpath (tour-path fittour))
(setf fitpath (make-array (list (+ N 1)) :fill-pointer 0))
(setq startpos (position startcity fittourpath))
(loop for pos from startpos below N do
	(vector-push (aref fittourpath pos) fitpath)
)
(loop for pos from 0 below startpos do
	(vector-push (aref fittourpath pos) fitpath)
)
(vector-push startcity fitpath)
(terpri) (terpri)
(write-string "100th Generation Fittest Solution: ")
(terpri)
(write (list 'Path- fitpath 'Distance- (tour-distance fittour)))
(terpri) (terpri)

;**************************************************************************************************