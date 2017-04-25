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
; =========================================
  
; Rather than directly telling the agents about inaccessible locations,
; we'll create a set of world facts (not known in advance to the agents)
; about which squares are initially occupied. When an agent is initially
; placed at a location, the 'is_occupied' predication for the location 
; is added in the world. 
;
; To enable an agent to see whether it can walk to an adjacent location,
; 'facts-evident-to' (in "gridworld-planning.lisp") has been changed
; to include facts about adjacent locations, and thus about whether or
; not they are occupied. (But in the absence of direct knowledge,
; agents assume, via the CWA, that locations are not occupied.)
;
; The initial, protected 'is_occupied' facts are created from the 
; following, as part of the def-roadmap function in "gridworld-
; definitions.lisp"; so this has to precede the def-roadmap call:
(defparameter *blocked-points*
   '(p12 p22 p24 p36 p42 p44 p52 p55 p64 p65))

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

; Note: adjacency knowledge is created as part of *roadmap-knowledge*
; in gridworld-definitions.lisp (which is imparted to an agent when
; its knowledge is initialized); but 'is_occupied' facts for both 
; the roadmap and for agent placement are *protected-facts* and
; *world-facts*, not *roadmap-knowledge*, and thus initially unknown
; to the agents.

; From an agent's perspective, all locations are initially not occupied,
; via the CWA.

(def-object 'agent nil)

;   DECLARATIONS OF PROBLEM-SPECIFIC GLOBALS IN gridworld-definitions.lisp
;   (MANY OF WHICH ARE EXPECTED TO BE RESET HERE):
;   (defparameter *n-agents* 4)
;   ^^^^^^^^^^^^^^^^^^^^^^^^^^^
;   (defparameter *roadmap-knowledge* (make-hash-table ...))
;   (defparameter *general-knowledge* nil)
;   (defparameter *extra-initial-knowledge* (make-array *n-agents*))
;   (defparameter *left-comoving-preds* nil)
;   (defparameter *right-comoving-preds* nil)
;   (defparameter *occluded-preds* nil)
;   (defparameter *right-comoving-preds* nil)
;   (defvar *visited-places* (make-array *n-agents*))
;   (defvar *visited-objects* (make-array *n-agents*))
;   (defvar *world-facts* (make-hash-table ...))
;   (defvar *protected-facts* (make-hash-table ...))
;
;   DECLARATIONS OF PROBLEM-SPECIFIC GLOBAL IN gridworld-planning.lisp
;   (BOTH OF WHICH ARE EXPECTED TO BE RESET HERE):
;   (defparameter *operators* nil)
;   (defvar *search-beam* nil)
;


(defparameter *agent-names* '(Happy Grumpy Doc Bashful Sleepy Sneezy Dopey))
                    ; We use *some* of these;


(defparameter *agent-traits* (make-hash-table :test 'equal))

; Some example trait storage, with the values corresponding to (O C E A N)
(setf (gethash 'Happy *agent-traits*) '(4 3 5 5 1))
(setf (gethash 'Grumpy *agent-traits*) '(2 4 2 1 3))
(setf (gethash 'Doc *agent-traits*) '(3 5 3 4 2))
(setf (gethash 'Bashful *agent-traits*) '(3 3 1 3 2))

(defparameter *agent-array* 
   (make-array *n-agents* :initial-contents 
        (butlast *agent-names* (- 7 *n-agents*))))
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

; place-object has been altered so as to add 'is-occupied' facts 
; (for agents placed at a point) to *world-facts* and *protected-facts*.
(dotimes (j *n-agents*)
   (place-object (aref *agent-array* j) 'agent 
                 (aref *agent-positions* j) 0 nil 
                 ;(list `(has_goal ,(aref *agent-array* j) ,(aref *goals* j))) 
                 ;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~unnecessary
                 nil
                 nil )); no associated things or attitudes

; (setq *occluded-preds* ; we leave it as nil here
;    '(likes contains is_playable knows is_edible is_potable))
; (setq *right-comoving-preds* '(has)); we leave it as nil here
; (setq *left-comoving-preds* ; we leave it as nil here
;    '(is_in))

;(setq *general-knowledge*
;   '(
;     ((tells ?x (that ?y)) => ?y) ; not needed in this world
;     ((knows ?x (that ?y)) => ?y) ; not needed in this world
;   ))

(setq *operators* '(walk stay-put)); defvar is in gridworld-planning.lisp

; This solves the problem in about 17 seconds (using a 150-step bound
; and with implement-effects uncompiled):
(setq *search-beam* ; defvar is in gridworld-planning.lisp
    (list (cons 4 *operators*) (cons 4 *operators*)
          (cons 3 *operators*) (cons 2 *operators*)))

; This gets AG3 trapped at P11, with AG4 already at its goal P21:
; (setq *search-beam* ; defvar is in gridworld-planning.lisp
;     (list (cons 4 *operators*) (cons 4 *operators*)
;           (cons 4 *operators*) (cons 4 *operators*)))

; This gets AG3 trapped at P11, with AG4 already at its goal P21:
; (setq *search-beam* ; defvar is in gridworld-planning.lisp
;    (list (cons 4 *operators*) (cons 3 *operators*)
;          (cons 3 *operators*) (cons 2 *operators*)
;          (cons 2 *operators*) (cons 1 *operators*)))

; This solved the problem in 10 seconds, when run with at most 200 steps,
; with implement-effects uncompiled:
; (setq *search-beam* ; defvar is in gridworld-planning.lisp
;    (list (cons 4 *operators*) (cons 4 *operators*)))

; This succeeded in 13 sec (with much more meandering, but much
; higher speed), with "implements-effects.lisp" compiled.
; With the latter uncompiled, it finished in 8 seconds, but
; with AG3 trapped at P11 by AG4 at P21.
; (setq *search-beam* ; defvar is in gridworld-planning.lisp
;    (list (cons 4 *operators*)))

; The only action with a positive value is staying-put at the goal
; location, though an agent may also stay put when not at its goal
; and unable to move (all adjacent squares occupied). Note: The
; agent must be able to perceive adjacent square occupancy -- on the
; spot, not in advance (so something like (allows_access_to P33 P34) 
; needs to be updated in the actual world when agents move). One change
; from the previous code this requires is that the location itself
; must be added to the 'objects-colocated-with' output, and hence to
; the 'local-objects' variable in that function. This can be done by
; saying that each location is_at itself. Also 'notice-new-local-facts 
; is dependent on 'is_at' predications, so I again this indicates
; the need to say that each location is_at itself. This is now done 
; in "gridworld-definitions.lisp".
;
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

(defun reward-for-walk? (x z) 0)
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Positive if the step to z is x's goal, 0 otherwise
;  (if (eql (gethash x *agent-goal-indices* )
;           (gethash z *agent-goal-indices* )) 10 0))

(setq walk.actual
; ?x goes from point ?y to adjacent point ?z. For ME, the road was
; specified as well, but it doesn't matter here -- only adjacency 
; matters, and whether the target square is occupied. The value
; of walking is positive if ?z is the ?x's goal square.
  (make-op.actual :name 'walk.actual :pars '(?x ?y ?z)
           :startconds '((is_at ?x ?y) (is_adjacent_to ?y ?z)
                         (not (is_occupied ?z)))
           :stopconds '((is_at ?x ?z))
           :deletes '((is_at ?x ?y) (is_occupied ?y))
           :adds '((is_at ?x ?z) (is_occupied ?z))))

(setq stay-put
; Can stay put at any time, but this is dispreferred to walking, unless 
; the goal has been reached (at which point staying put reaps a big reward)
  (make-op :name 'stay-put :pars '(?x ?y)
           :preconds '((is_at ?x ?y))
           :effects nil
           :time-required 1
           :value '(reward-for-stay-put? ?x ?y))); +ve if ?z is ?x's goal, o/w -1

(setq stay-put.actual
; Can stay put at any time, but this is dispreferred to walking, unless 
; the goal has been reached (at which point staying put reaps a big reward)
  (make-op.actual :name 'stay-put.actual :pars '(?x ?y)
                  :startconds '((is_at ?x ?y))
                  :stopconds '((is_at ?x ?y))
                  :deletes nil
                  :adds nil ))

(defun reward-for-stay-put? (x y) -10)
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Positive if y is x's goal, -1 otherwise; (much like reward-for-walk?)
;  (if (eql (gethash x *agent-goal-indices*)
;           (gethash y *agent-goal-indices*)) 10 -1))
