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
(defparameter *agent-names* '(Happy Sleepy Dopey Bashful Grumpy Sneezy Doc))
(defparameter *agent-traits* (make-hash-table :test 'equal))

; Some example trait storage, with the values corresponding to (O C E A N)
(setf (gethash 'Happy *agent-traits*) '(4 3 5 5 1))
(setf (gethash 'Grumpy *agent-traits*) '(2 4 2 1 3))
(setf (gethash 'Doc *agent-traits*) '(3 5 3 4 2))
(setf (gethash 'Bashful *agent-traits*) '(3 3 1 3 2))
