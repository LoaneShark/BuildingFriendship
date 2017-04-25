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
   '(p12 p22 p24 p36 p42 p44))

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

; Note: adjacency knowledge is created as part of *roadmap-knowledge*
; in gridworld-definitions.lisp (which is imparted to an agent when
; its knowledge is initialized); but 'is_occupied' facts for both 
; the roadmap and for agent placement are *protected-facts* and
; *world-facts*, not *roadmap-knowledge*, and thus initially unknown
; to the agents.

; From an agent's perspective, all locations are initially not occupied,
; via the CWA.

;(def-object 'agent nil)

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


(defparameter *agent-traits* (make-hash-table :test #'equal))

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
        '(P13 P34 P41 P21))); ** may be replaced by randomized positions
(defparameter *goals*
   (make-array *n-agents* :initial-contents        
        '(P33 P24 P41 P21))); ** may be replaced by randomized positions

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

(setq *operators* '(walk stay-put answer_user_ynq answer_user_whq)); defvar is in gridworld-planning.lisp

; This solves the problem in about 17 seconds (using a 150-step bound
; and with implement-effects uncompiled):
(setq *search-beam* ; defvar is in gridworld-planning.lisp
    (list (cons 4 *operators*) (cons 4 *operators*)
          (cons 3 *operators*) (cons 2 *operators*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function answer_to_ynq? returns a well-formed formula indicating whether 
;; or not the arg wff is currently in AG's KB, under the closed world 
;; assumption. For example, if AG is currently hungry according to AG's KB,
;; then (is_hungry AG) is returned as the response to 
;; (answer_to_ynq? '(is_hungry AG)); else, (not (is_hungry AG)) is returned.
;; This is the `model' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun answer_to_ynq? (wff)
  (check-yn-fact-in-kb 'NIL wff (state-node-wff-htable *curr-state-node*))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function answer_to_ynq.actual? returns a well-formed formula indicating  
;; whether the arg wff is currently in AG's KB, under the closed world 
;; assumption. In addition, the answer is translated into a proper English 
;; sentence and printed on screen.  For example, if AG is currently hungry 
;; according to AG's KB, then (is_hungry AG) is returned as the response to 
;; (answer_to_ynq.actual? '(is_hungry AG)), and ``AG is hungry'' without the 
;; double quotes is printed.  Otherwise, (not (is_hungry AG)) is 
;; returned and ``it is not the case that AG is hungry'' is printed without 
;; the double quotes.
;; This is the `actual' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun answer_to_ynq.actual? (wff)
  (check-yn-fact-in-kb 'T wff (state-node-wff-htable *curr-state-node*))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function answer_to_whq? returns a collection of well-formed formula(s) 
;; as the answer to the arg wff reflecting what are currently in AG's KB, 
;; under the closed world assumption. Arg wff is a wh-question that has 
;; variables prefixed with ? appearing in slots filled by wh-words.  
;; For example, if AG likes only APPLE1 and BANANA2 according to AG's KB,
;; then ((likes AG APPLE1) (likes AG BANANA2)) is returned as response to 
;; (answer_to_whq? '(likes AG ?wh)). If no answer is found, 
;; then '(not (knows (AG the-answer))) is returned.
;; This is the `model' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun answer_to_whq? (wff)
  (check-whq-answer-in-kb 'NIL wff (state-node-wff-htable *curr-state-node*))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function answer_to_whq.actual? returns a collection of well-formed 
;; formula(s) as the answer to the arg wff reflecting what are currently in 
;; AG's KB, under the closed world assumption. Arg wff is a wh-question 
;; with variables prefixed with ? appearing in slots filled by wh-words.  
;; For example, if AG likes only APPLE1 and BANANA2 according to AG's KB,
;; ((likes AG APPLE1) (likes AG BANANA2)) is returned as the response to 
;; (answer_to_whq.actual? '(likes AG ?wh)), and ``AG likes APPLE1'' and ``AG likes 
;; BANANA2'' without double quotes are printed on two lines.  If no answer 
;; is found, '(not (knows (AG the-answer))) is returned and ``it is not the 
;; case that AG knows the answer'' without the double quotes is printed .
;; This is the `actual' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun answer_to_whq.actual? (wff)
  (check-whq-answer-in-kb 'T wff (state-node-wff-htable *curr-state-node*))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; With operator answer_user_ynq, AG answers the yes-no question ?q asked 
;; by USER.
;; This is the `model' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq answer_user_ynq 
      (make-op :name 'answer_user_ynq :pars '(?q)
        :preconds '( (wants USER (that (tells AG USER (whether ?q)))) )
        :effects '( (not (wants USER (that (tells AG USER (whether ?q)))))
                    (knows USER (that (answer_to_ynq? ?q)))
            )
        :time-required 1
        :value 10
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; With operator answer_user_ynq.actual, AG answers the yes-no question 
;; ?q asked by USER.
;; This is the `actual' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq answer_user_ynq.actual 
  (make-op.actual :name 'answer_user_ynq.actual :pars '(?q)
  :startconds '( (wants USER (that (tells AG USER (whether ?q)))) )
  :stopconds '( (not (wants USER (that (tells AG USER (whether ?q))))) )
  :deletes '( (wants USER (that (tells AG USER (whether ?q)))) )
  :adds '( ;(knows USER (that (answer_to_ynq?.actual ?q)))        
           (says+to+at_time AG (that (answer_to_ynq.actual? ?q)) USER (current_time?))
           (not (wants USER (that (tells AG USER (whether ?q)))))
         )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; With operator answer_user_whq, AG answers the wh-question ?q asked by 
;; USER.
;; This is the `model' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq answer_user_whq 
  (make-op :name 'answer_user_whq :pars '(?q)
  :preconds '( (wants USER (that (tells AG USER (answer_to_whq ?q)))) )
  :effects '( (not (wants USER (that (tells AG USER (answer_to_whq ?q)))))
        (knows USER (that (answer_to_whq? ?q)))
        )
  :time-required 1
  :value 10
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; With operator answer_user_whq.actual, AG answers the wh-question ?q 
;; asked by USER.
;; This is the `actual' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq answer_user_whq.actual 
  (make-op.actual :name 'answer_user_whq.actual :pars '(?q)
  :startconds '( (wants USER (that (tells AG USER (answer_to_whq ?q)))) )
  :stopconds '( (not (wants USER (that (tells AG USER (answer_to_whq ?q))))) )
  :deletes '( (wants USER (that (tells AG USER (answer_to_whq ?q)))) )
  :adds '( ;(knows USER (that (answer_to_whq.actual? ?q)))        
         (says+to+at_time AG (that (answer_to_whq.actual? ?q)) USER (current_time?))
         (not (wants USER (that (tells AG USER (answer_to_whq ?q)))))
       )
  ) 
)

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
