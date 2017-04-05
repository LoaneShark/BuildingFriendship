;; SEPARATED OUT BECAUSE IT CAN CAUSE TROUBLE WHEN COMPILED (THOUGH NOT
;; NECESSARILY IN THIS PARTICULAR DOMAIN) -LKS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This procedure chains forward from the *curr-state-node*[j], using some 
;; fixed (or user-set) search beam.  It reports the seemingly best plan 
;; (*plan*) and corresponding state sequence (*states*); then it executes 
;; the first step of that best plan, updating *curr-state-node*[j], *plan*
; and *states*.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defun go! (j) ; Revised Dec. 2009; Oct 2/12: allow agent index j
        (let ((agent (aref *agent-array* j))
              poss-actions step action-name old-wffs old-terms)

(format t "~% GO ~%")
                ;Handle the spontaneous fire and flood.
                ;(handleExtOps)
                ; Above line commented out only for opportunic runs


                (setq poss-actions
                 (chain-forward agent (aref *curr-state-node* j) *search-beam*))

                (when (null poss-actions)
                      (incf (aref *AG-clock* j) 1)
                      (if (eql j 0); first agent only -- pseudo-parallelism
                          (incf *real-clock* 1))
                      (return-from go! "NO MORE ACTIONS POSSIBLE!")
                )
;(format t "~%~%POSSIBLE ACTIONS & VALUES: ~a" poss-actions)
;(format t "~%SEEMINGLY BEST PLAN: ~a" (aref *plan* j))

                ; (Some of the following comments added/rearranged by LKS)
                ; Save wffs and terms of curr. state node for reuse in the new one
                (setq old-wffs (state-node-wff-htable (aref *curr-state-node* j)))
                (setq old-terms (state-node-terms (aref *curr-state-node* j)))
                ; Oct 6/12, LKS: to release storage, set children to be just the
                ; first (chosen) child:
                (setf (state-node-children (aref *curr-state-node* j))
                      (list (car (state-node-children (aref *curr-state-node* j)))))
                ; Reset *curr-state-node*[j] to the first (leftmost) successor.
                (setf (aref *curr-state-node* j)
                     (eval (cdar (state-node-children (aref *curr-state-node* j)))) )
                (setf (state-node-terms (aref *curr-state-node* j)) old-terms)
                (clrhash (state-node-wff-htable (aref *curr-state-node* j)))
                (add_htable_to_hashtable old-wffs
                        (state-node-wff-htable (aref *curr-state-node* j)) 'NIL)

                (setq step (pop (aref *plan* j)))
;(format t "~%~%STEP TO BE TAKEN: ~a" step)
;(format t "~%EXPECTED STATE; ~%  ~a" (second (aref *states* j)))
                (pop (aref *plan* j))
                ; Remove the previous state and bring to the fore the expected 
                ; current state.
                (pop (aref *states* j))

    ; Reset the "successor actions already explored" to nil,
    ; because otherwise we won't take account of possibilities.
    ; engendered by newly discovered local facts.                    
                (setf (state-node-operators (aref *curr-state-node* j)) nil)
                (setf (state-node-children (aref *curr-state-node* j)) nil)
    ; LKS, Sep 28/12: To free memory, reset grandparent pointer to nil:
                (if (state-node-parent (aref *curr-state-node* j))
                                       ; (action-name . state-node-name)
                    (if (state-node-parent 
                          (eval (cdr (state-node-parent 
                                        (aref *curr-state-node* j)))))
                        (setf (state-node-parent 
                               (eval (cdr (state-node-parent 
                                           (aref *curr-state-node* j))))) nil))
                )

                ; Actually implement the effects of the action in the world.
                (setq action-name 
                   (car (state-node-parent (aref *curr-state-node* j))))
                ; DELETE THE FOLLOWING, AS *NODE-NOW* IS NOT USED ANYWHERE
                ; (setq *node-now* (aref *curr-state-node* j))
                (implement-effects action-name agent)
        )
); end of go!

