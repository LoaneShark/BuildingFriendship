; Run the *n-agents* in *agent-array* in a turn-taking loop, to 
; simulate concurrent actions by the agents in step-by-step manner.

(defun run-multiple-agents (n-steps-per-agent)
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; E.g., allow n-steps-per-agent = 100
; Result: list of agents that got to their goal location.
; [One thing taken care of elsewhere is the advancements of *real-clock*,
; which should be incremented by 1 on each cycle through the agents 
; (not for each agent). This is done in 'go!' where the incrementation
; is done only for agent index j = 0.]

 (let (agent loc result)
      ; Initialize the *curr-state-node* of each agent:
      (dotimes (j *n-agents*)
         (initialize-state-node j))
         ; this tacitly sets the value of *curr-state-node*[j]
      (dotimes (i n-steps-per-agent); i = step index 
         (dotimes (j *n-agents*) ; j = agent index
            (setq agent (aref *agent-array* j))
            (when (not (member agent result)); ignore finished agents
               ; first update the agent's knowledge, since there may
               ; have been local changes cuased by other agents; then go!
               (notice-new-local-facts (aref *curr-state-node* j) agent)
               (go! j) ; this resets the jth agent's current knowledge state;
                       ; naturally, the actual world is altered as well
                       ; and the next agent will act therein;
               ; if the agent is at its goal, put it on the result list
               (setq loc (find-location agent 
                           (state-node-wff-htable (aref *curr-state-node* j))))
               (when (eql loc (aref *goals* j)) 
                     (format t "~%~%==> AGENT ~s HAS REACHED ITS GOAL ~s :-)"
                               agent loc)
                     (push agent result)); put agent on success list
            ); end of 'when' for active agents
         ); end of agent loop
      ); end of step loop
      (reverse result); return successful agents in fastest-first order
 )); end of run-multiple-agents 

; good for tracing:
; (trace initialize-state-node chain-forward state-node-name all-instances-of-operator all-bindings-of-goals-to-fact-htable all-bindings-of-goals-to-fact-htable1 all-bindings-of-posgoal-to-fact-htable possible-positive-unifiers all-bindings-of-neggoal-to-fact-htable find-all-positive-bindings all-bindings-of-goal-to-fact-htable instantiate-op facts-evident-to objects-colocated-with points-visible-to 
;    unifier                              ; maybe
; add_list_of_tuples_to_hashtable) ; this one give much useless NIL stuff
;
; *general-knowledge*

(defun check-world (query)
; ~~~~~~~~~~~~~~~~~~~~~~~
; For debugging, for queries such as (is_occupied p12), (is_at AG1 P13),
; (is_occupied p14), (.s_adjacent_to P13 P23),
  (let ((key (convert_pred_to_hashkey query)))
       (gethash key *world-facts*)))

; To monitor functions, use monitoring code from
;     http://www-users.cs.umn.edu/~gini/lisp/metering.cl
;
(defun run-with-monitoring (n)
    (load "monitoring-package") 
    (mon:with-monitoring 
      (facts-evident-to generate_allkeys_from_hashkey copy_construct_hashtable
       add_htable_to_hashtable)
      () (run-multiple-agents n)))

; Timing results for n = 10:                                   Cons
;                                %     %                       Per      Total   Total
; Function                       Time  Cons  Calls  Sec/Call   Call     Time    Cons
; ------------------------------------------------------------------------------------
; FACTS-EVIDENT-TO:              0.93  0.00    144  0.013264      0  1.910000       0
; GENERATE_ALLKEYS_FROM_HASHKEY: 0.04  0.00  13341  0.000006      0  0.080000       0
; ADD_HTABLE_TO_HASHTABLE:       0.02  0.00    306  0.000163      0  0.050000       0
; ------------------------------------------------------------------------------------
; TOTAL:                         1.00  0.00  13791                   2.040000       0

(defun run-with-total-monitoring (n)
  (load "monitoring-package")    
  (mon:monitor-form (run-multiple-agents n)))

; Timing results for n = 10:
; NB: A function may be time-consuming because its *arguments*
;     are time-consuming to compute!                             Cons
;                                       %    %                   Per   Total   Total
; Function                              Time Cons Calls Sec/Call Call  Time    Cons
; ----------------------------------------------------------------------------------
; REMOVEF:                              0.49 0.00  3967 0.001051  0  4.170000   0
; ALL-BINDINGS-OF-GOALS-TO-FACT-HTABLE: 0.17 0.00  2095 0.000687  0  1.440000   0
; ADD_HTABLE_TO_HASHTABLE:              0.09 0.00  4033 0.000191  0  0.770000   0
; UNIONF:                               0.04 0.00 61755 0.000006  0  0.360000   0
; VAR:                                  0.02 0.00 183355 0.000001 0  0.170000   0
; CONTAINS-VAR:                         0.02 0.00 87345 0.000002  0  0.170000   0
; SIMPLIFY-VALUE:                       0.02 0.00 50182 0.000003  0  0.140000   0
; CONVERT_PRED_TO_HASHKEY:              0.02 0.00 25972 0.000005  0  0.130000   0
; ADD_TUPLE_TO_HASHTABLE:               0.01 0.00 13072 0.000009  0  0.120000   0
; GEN_HASHKEY_SYMBOL:                   0.01 0.00 67710 0.000002  0  0.120000   0
; GENERATE_ALLKEYS_FROM_HASHKEY:        0.01 0.00 25556 0.000004  0  0.090000   0
; FACTS-EVIDENT-TO:                     0.01 0.00   104 0.000865  0  0.090000   0
; --------------------------------------------------------------------------------
; TOTAL:                                0.91 0.00 525146             7.770000   0
; Estimated monitoring overhead: 0.00 seconds
; Estimated total monitoring overhead: 0.00 seconds


; (AG2 AG4)

