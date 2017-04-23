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

(defparameter *n-agents* 7)
; This is just an example/placeholder 
(defparameter *agent-names* '(Happy Sleepy Dopey Bashful Grumpy Sneezy Doc))
(defparameter *agent-traits* (make-hash-table :test 'equal))

; Some example trait storage, with the values corresponding to (O C E A N)
(setf (gethash 'Happy *agent-traits*) '(4 3 5 5 1))
(setf (gethash 'Grumpy *agent-traits*) '(2 4 2 1 3))
(setf (gethash 'Doc *agent-traits*) '(3 5 3 4 2))
(setf (gethash 'Bashful *agent-traits*) '(3 3 1 3 2))

(defparameter *blocked-points*
   '(p12 p22 p24 p36 p42 p44 p52 p55 p64 p65))

; temporarily ripped from sample world - lets agents move around
(def-roadmap
  '(p11 p12 p13 p14 p15 p16
    p21 p22 p23 p24 p25 p26
    p31 p32 p33 p34 p35 p36
    p41 p42 p43 p44 p45 p46
    p51 p52 p53 p54 p55 p56
    p61 p62 p63 p64 p65 p66)
  '((hor1 p11 1 p12 1 p13 1 p14 1 p15 1 p16)
    (hor2 p21 1 p22 1 p23 1 p24 1 p25 1 p26)
    (hor3 p31 1 p32 1 p33 1 p34 1 p35 1 p36)
    (hor4 p41 1 p42 1 p43 1 p44 1 p45 1 p46)
    (hor5 p51 1 p52 1 p53 1 p54 1 p55 1 p56)
    (hor6 p61 1 p62 1 p63 1 p64 1 p65 1 p66)

    (ver1 p11 1 p21 1 p31 1 p41 1 p51 1 p61)
    (ver2 p12 1 p22 1 p32 1 p42 1 p52 1 p62)
    (ver3 p13 1 p23 1 p33 1 p43 1 p53 1 p63)
    (ver4 p14 1 p24 1 p34 1 p44 1 p54 1 p64)
    (ver5 p15 1 p25 1 p35 1 p45 1 p55 1 p65)
    (ver6 p16 1 p26 1 p36 1 p46 1 p56 1 p66)))
(defun reward-for-walk? (x z)
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Positive if the step to z is x's goal, 0 otherwise
  (if (eql (gethash x *agent-goal-indices* )
           (gethash z *agent-goal-indices* )) 10 0))
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
           :value '(reward-for-walk? ?x ?z))); +ve if ?z is ?x's goal, o/w 0
