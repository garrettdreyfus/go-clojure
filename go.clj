(defn abs "(abs n) is the absolute value of n" [n]
	(cond
	 (not (number? n)) (throw (IllegalArgumentException.
							 "abs requires a number"))
	 (neg? n) (- n)
	 :else n))	
(defn cart
	([xs]
	 xs)
	([xs ys]
	 (mapcat (fn [x] (map (fn [y] (list x y)) ys)) xs))
	([xs ys & more]
	 (mapcat (fn [x] (map (fn [z] (cons x z)) (apply cart (cons ys more)))) xs)))

(defn between
 [value low high]
	(and (>= value low) (< value high)))

(defn createBoard
 [width height]
	(repeat height (repeat width 0)))

(def not-nil? (complement nil?))

(defn atCoord
 [board x y]
	(nth (nth board y) x))
(defn removeNil
 [arr]
	(keep #(if(not-nil? %) %) arr))

(defn wrapToBoard
 [board points]
	(removeNil (map #(if (and (between (nth % 0) 0 (count (nth board 0))) (between (nth % 1) 0 (count board))) %) points)))
(defn parse-int [s]
			 (Integer. (re-find  #"\d+" s )))
(defn neighbors
 ([board x y]
	(wrapToBoard board (map #(list (+ (nth % 0) x) (+ (nth % 1) y))  (keep #(if (not (= (abs (nth % 0)) (abs (nth % 1)))) %) (cart (range -1 2) (range -1 2))))))
 ([board x y value]
	(keep #(if(= (atCoord board (nth % 0) (nth % 1)) value) %) (neighbors board x y))))
(defn subtractLists [a b]
  (remove (into #{} b) a)) 
(defn isSurrounded
 [board explored explorable value surroundedYet?]
	(if (> (count explorable) 0)
		(let [x (first (first explorable)) y (second (first explorable)) surrounded (if surroundedYet? (= (count (neighbors board x y 0)) 0) false ) ]
			(isSurrounded 
				board 
				(conj explored (first explorable)) 
				(subtractLists (neighbors board x y value) (concat explored explorable))
				value
				surrounded)) 
	(list explored surroundedYet?)))
(defn hasBoardChanged 
	[board x y]
	(into () (reduce concat 
		(map 
			(fn [[x y]] 
					(let [[group s] (isSurrounded board () (list (list x y)) (atCoord board x y) true)]
						(if s group (list))	
					))
			(concat (neighbors board x y) )))))
(defn printBoard
 [board]
		 (doseq [x board] (prn x)))
(defn modifyAtIndex1D
	[arr index value]
		(reduce concat (list (take index arr) (list value) (drop (+ 1 index) arr))))
(defn modifyAtIndex2D 
	[arr x y value] 
		(modifyAtIndex1D arr y (modifyAtIndex1D (nth arr y) x value)))
(defn removeGroup
	[group board]
	(if (> (count group) 0)
		(removeGroup (pop group) (modifyAtIndex2D board (nth (first group) 0) (nth (first group) 1) 0 ))
board))

(defn updateBoard
	[board x y]
		(removeGroup (hasBoardChanged board x y) board))
(defn flip
	[value]
		(if (= value 1) 2 1))
(defn countTeam
	[board team]
	(reduce + (for [row board cell row] (if (= cell team) 1 0)))
)
;(prn (countTeam '((0 0 0 0 0) (0 0 2 2 0) (0 2 1 1 2) (0 0 2 2 0) (0 0 0 0 0)) 2))
(defn deltaHeur
	[board team]
		(- (countTeam board team) (countTeam board (flip team)))		
)
(defn randomGen 
	[board team]
)
(defn sum
	[arr]
		(reduce + arr))
(defn iterativeGen 
	[board team]
	(let [coords (for [x (range (count board)) y (range (count (nth board 0)))] (list x y)) ]
		(removeNil (map (fn [[x y]] (if (= (atCoord board x y) 0) (updateBoard (modifyAtIndex2D board x y team) x y))) coords))))
(defn runner 
	([board heuristic reduction generateNewBoards currentTeam realTeam depth maxDepth]
		(if (< depth maxDepth) 
			(reduction (map #(runner % heuristic reduction generateNewBoards (flip currentTeam) realTeam (inc depth) maxDepth) (generateNewBoards board currentTeam)))
			(heuristic board realTeam))))
(defn maxIndex [arr] (first (apply max-key second (map-indexed vector arr))))
(defn decide ([board heuristic reduction generateNewBoards choice team maxDepth ]
	"runner traverses the decision tree.
		heuristic: a function which takes a board and a team and returns a value of value :)
		reduction: a function which takes a list of heuristics and reduces it to one value
		generateNewBoards: a function that generates possible next steps for a team 
		currentTeam: the currentTeam that is making the decisision
		realTeam: the team we are calculating the heuristic for
		depth: the current amount of times we have recurred/ how many decisions we've made
		maxDepth: the amount of decisions that should be made
		choice: a function which takes a list of heuristics and returns the index of the one it wants
		"
	(let [possibleBoards (generateNewBoards board team)]
		;(doseq [x possibleBoards] (printBoard x)(prn "---------")))))
		(nth possibleBoards (maxIndex (map #(runner % heuristic reduction generateNewBoards team team 0 maxDepth) possibleBoards)))
		)))

(loop [board (decide (createBoard 5 5) deltaHeur sum iterativeGen maxIndex 2 2 ) team 2 ]
	(printBoard board)
	(prn "-------")
	(recur (decide board deltaHeur sum iterativeGen maxIndex (flip team) 2) (flip team) ))
