; Sample world for project by Santiago, Nate and Scott.
; Agents will have intrinsic personality features that dictate both
; how they behave and what opinions they form of others

; Opinions should be based on the agent's opinion of another, depending
; on their actions, and is also dependant on their ability to "see" the
; action (adjacency in the gridworld). After each movement an agent should
; decide to perform an action (or not) and then do it. This could be an
; interaction with another agent, or just by themselves (e.g. eating food)
; (Therefore they should not have global knowledge, only regional/adjacent)

; Agents will likely only form one opinion "rating" of every other agent.
; This can change, but will likely be stored in a hashtable (*agent-opinions*)
; either way. Agent personality traits will also be stored in a table, and will
; vary according to the OCEAN system of traits, outlined below (ranked from 1 to 5).
; A low score (1-2) means that they will behave opposed to the description below,
; a median (3) means they are effectively ambivalent in this aspect, and high (4-5)
; means they exhibit the behavior described below.
; Openness: Likes variety & difference. Values different traits from its own
;           and actions that it has not performed before. 
; Conscientiousness: Plans actions strongly and dislikes spontaneity, or behavior
;           it deems disruptive. Neat, systematic and wary, strongly dislikes
;           opposing behavior.
; Extraversion: Enjoys and seeks out interaction with others. Enjoys any (nonhostile)
;           form of interaction with other agents, and the more the merrier. Become
;           upset when without interaction for prolonged periods of time.
; Agreeableness: Kind, empathetic, wants to help others, tries to cooperate and 
;           enjoys seeing others succeed. Exhibits selfless decisionmaking.
; Neuroticism: "Moodiness" or anxiety, will respond worse to stress or hostile
;           behavior, interprets threats with much greater severity.

(defparameter *n-agents* 4)
; This is just an example/placeholder 
(defparameter *agent-names* '(Happy Sleepy Dopey Bashful Grumpy Sneezy Doc))
(defparameter *agent-traits* (make-hash-table :test 'equal))

; Some example trait storage, with the values corresponding to (O C E A N)
(setf (gethash 'Happy *agent-traits*) '(4 3 5 5 1))
(setf (gethash 'Grumpy *agent-traits*) '(2 4 2 1 3))
(setf (gethash 'Doc *agent-traits*) '(3 5 3 4 2))
(setf (gethash 'Bashful *agent-traits*) '(3 3 1 3 2))

(defparameter *blocked-points*
   '(p12 p22 p24 p36 p42 p44))

; temporarily ripped from sample world - lets agents move around
; downscaled from 6x6 to 4x4 also
(def-roadmap
  '(p11 p12 p13 p14
    p21 p22 p23 p24
    p31 p32 p33 p34
    p41 p42 p43 p44)
  '((hor1 p11 1 p12 1 p13 1 p14)
    (hor2 p21 1 p22 1 p23 1 p24)
    (hor3 p31 1 p32 1 p33 1 p34)
    (hor4 p41 1 p42 1 p43 1 p44)

    (ver1 p11 1 p21 1 p31 1 p41)
    (ver2 p12 1 p22 1 p32 1 p42)
    (ver3 p13 1 p23 1 p33 1 p43)
    (ver4 p14 1 p24 1 p34 1 p44)))

(defparameter *agent-array* 
   (make-array *n-agents* :initial-contents 
        (butlast *agent-names* (- (length *agent-names*) *n-agents*))))
(defparameter *agent-positions*
   (make-array *n-agents* :initial-contents 
        '(P13 P35 P41 P51))); ** may be replaced by randomized positions
(defparameter *goals*
   (make-array *n-agents* :initial-contents        
        '(P61 P26 P51 P21))); ** may be replaced by randomized positions

(defparameter *agent-goal-indices*; allows hash access to indices, and 
   (make-hash-table :size 23))     ; hence from an agent to its goal or v.v.
(dotimes (j *n-agents*) 
   (setf (gethash (aref *agent-array* j) *agent-goal-indices*) j)
   (setf (gethash (aref *goals* j) *agent-goal-indices*) j))

; beginning set of operators, defined in gridworld-definitions
; will need to expand // think about what each of these does
; agents will likely not actually communicate (just "talk" as an action),
; but possibly could add modifiers to it, dependant on mood and
; opinion of others (e.g. talk-friendly, talk-hostile, etc.)
(setq *operators* '(walk work talk help sing play)) ;; TODO: add play

(setq walk 
; ?x goes from point ?y to adjacent point ?z. For ME, the road was
; specified as well, but it doesn't matter here -- only adjacency 
; matters, and whether the target square is occupied. The value
; of walking is positive if ?z is the ?x's goal square.
  (make-op :name 'walk :pars '(?x ?y ?z)
           :preconds '((is_at ?x ?y)
                       (is_adjacent_to ?y ?z)
                       (not (is_occupied ?z)))
           :effects '((is_at ?x ?z) (not (is_occupied ?y))
                      (not (is_at ?x ?y)) (is_occupied ?z))
           :time-required 1
           :value 0))

; ?x tries to eat ?y, if not full, in same space (?z) and ?y is edible
;(setq eat
;  (make-op :name 'eat :pars '(?x ?y ?z)
;	   :preconds '((is_at ?x ?z)
;		       (is_at ?y ?z)
;		       (is_edible ?y)
;		       (not (is_full ?x))
;		       (not (in_danger ?x)))
;	   :effects '((not (is_at ?y ?z))
;		      (is_full ?x))
;	   :time-required 1
;	   ; should probably figure out what the value of eating is
;	   :value 1))
; ?x does some miscellaneous work that it has to do. Need to find out how
; to properly evaluate stress/danger (as it is dependant on ?x's traits)
; will probably need to convert it to a numerical value (rather than is/isnt)
; e.g. a workaholic would be stressed without their work done and may do work to 
; calm themselves, but a lazy agent would be the inverse
; Should also implement sth that makes them get new `has_work' every few timesteps
; Work should be able to be sped up if they are being helped by someone else, somehow
(setq work
  (make-op :name 'work :pars '(?x)
	   :preconds '((not (is_tired ?x))
		       (not (in_danger ?x))
		       (not (is_stressed ?x))
		       (has_work ?x))
	   :effects '((not (has_work ?x))
		      (is_tired ?x)
		      (is_stressed ?x))
	   :time-required 6
	   :value '(reward-for-work? ?x)))
; make this dependant on their traits and predicted urgency
(defun reward-for-work? (x) 1)

; ?x attempts to sleep at location ?z
; cannot be interrupted by sense of danger or discomfort, only if 
; `is_noisy' is updated. 
;(setq sleep
;  (make-op :name 'sleep :pars '(?x ?z)
;	   :preconds '((is_tired ?x)
;		       (is_at ?x ?z)
;		       (not (in_danger ?x))
;		       (not (is_noisy ?z)))
;	   :effects '((not (is_tired ?x)))
;	   :time-required 4
;	   :value '(reward-for-sleep? ?x)))
; dependant on their level of tiredness and sense of obligation (i.e. work to do)
;(defun reward-for-sleep? (x) 1)

;; Not quite sure how to implement this one, but it should do the following:
;;     - Double the rate of time being used for an action (make it go faster)
;;     - Double the speed of movement if the action is moving (help escape danger)
;;     - Be dependant on ?x's traits, with ?y reacting according to theirs
;; It may be too complicated to implement, given time constraints
;(setq help
;  (make-op :name 'help :pars '(?x ?y)...

; ?x starts a conversation with ?y. Both interpret the conversation depending
; on their current moods and traits. Both must be at location ?z and be able
; to hear each other
(setq talk
  (make-op :name 'talk :pars '(?x ?y ?z)
	   :preconds '((is_at ?x ?z)
		       (is_at ?y ?z)
		       (is_lonely ?x)
		       (not (is_noisy ?z))
		       (not (in_danger ?x)))
	   :effects '(not (is_lonely ?x))
	   :time-required 1
	   :value '(reward-for-talk? ?x ?y)))
; reward-for-talk should be dependant on traits of ?x, opinon of ?y, current
; need for socialization of ?x and perceived willingness/business of ?y (which
; in turn depends on ?x's traits)
(defun reward-for-talk? (x) 1)

; ?x starts to sing at location ?z. Possibly could modify this so that it reduces
; ?x's stress levels? Mostly here to be a *spontaneous* (and sometimes disruptive)
; action.
(setq sing
  (make-op :name 'sing :pars '(?x ?z)
	   :preconds '((is_at ?x ?z)
		       (not (in_danger ?x)))
	   :effects '((is_noisy ?z))
	   :time-required 1
	   :value '(reward-for-sing? ?x)))
; you know the drill, dependant on their traits. Maybe semi-random (but modified
; by their "C" trait)
(defun reward-for-sing? (x) 1)
