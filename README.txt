Apr 6/14:

This directory has been created to help CSC 291 students whose
proposals seem to require multiple agents (or agents that act as
exogenous events, like starting local fires).

The main difference from single-agent worlds is that we have an
indexed array of agents, and various other agent-dependent
parameters have been changed into arrays. Detailed differences
from the single mobile agent environment are listed below.

The specific world exemplified here was first defined (for the "FF",
i.e., fast-forward, planner) by Brenner & Nebel (2009), "Continual
planning and acting in dynamic multiagent environments", Journal of
Autonomous Agents and Multiagent Systems 19 (3), pp. 297-331. (See
http://www.informatik.uni-freiburg.de/~ki/people/brenner/publications.html)
An example is shown on p.9 of the paper, for a 6 x 6 grid of squares,
with 4 agents, and various random squares occupied by obstacles. The
agents start at randomized initial positions and need to reach certain
randomized goal locations. In their attempts to reach their goals,
they can use look-ahead, but while they know the grid structure, they
can't see the obstacles -- or other agents -- till they are adjacent
to them (so for planning more than one step ahead, they assume that
unseen squares are unoccupied).

Note that sometimes some agents will be unable to reach their
goal. In Brenner & Nebel's example, if one of the agents numbered
0, 1, or 2 strays to the top left corner square, then the agent
numbered 3 might trap it there, when it gets to its goal just
below the top left corner. However, other than interference with
each other's motions, agents don't interact with one another. Brenner 
& Nebel also didn't allow for knowing, wanting, or asking. So there
are no operators for such actions in the sample world. (However,
you can define operators using these modal predicates.)

==================================================================
Operation:
; If changes have been made and there are *.fasl files: rm *fasl
acl
(load "init.lisp")
; If changes have been made or there are no *.fasl files: (load "compile-all")
(run-multiple-agents 100) ; 100 steps might be more than needed

To see printout of state-node terms, uncomment the lines

; (format t "~%~%state-node terms: ~%~s~%" new-terms); DEBUG
and 
; (format t "~%~%Terms in state-node, within 'notice-new-local-facts':~%~s~%"; DEBUG
;     new-terms); DEBUG

in "gridworld-planning.lisp"
==================================================================

Changes to gridworld code (LKS):

- Changed various global parameters/variables to arrays of length =
  *n-agents*, to allow for multiple agents;
- Designed operators so that they always include a first argument
  that specifies the agent; ensured that when one agent is planning,
  it doesn't consider instantiations of operators for *other* agents;
- Made various functions (including 'go!') agent-dependent, to allow
  for multiple agents; in some functions, the agent index (I always use
  "j") is used to identify the agent, in others the name is used.
  I created a hash table that allows finding "j", given the agent name.
- I changed 'graft-into-packet' to allow 'and'-ed antecedents in
  *world-knowledge* (even though this isn't needed here, this should
  definitely be allowed).
- I've included the CMU monitoring package, which can be useful for timing 
  of various functions and debugging. It is loaded in "init.lisp", but also 
  in the functions 'run-with-monitoring' and 'run-with-total-monitoring.
  Examples of results of monitoring are shown in "run-agents.lisp".
- JUST FOR THIS MULTIAGENT WORLD: revised def-roadmap so that it creates
  adjacency info, but omits 'is_on' info, which isn't needed, and also
  creates initial 'is_occupied' facts as part of *world-facts* (but *not*
  as part of *roadmap-knowledge* -- the agents should not know in advance
  which locations are occupied.
- JUST FOR THIS MULTIAGENT WORLD: revised place-object so that locations
  where agents are placed become occupied.
- JUST FOR THIS MULTIAGENT WORLD: facts-evident-to requires not only objects
  colocated with an agent, but also neighboring locations (points) visible
  to an agent (so that it can tell whether or not the location is occupied);
- NOT SURE IF FOR KEEPS: In 'go!', I reset the grandparent pointer to nil,
  to release storage; this sped things up in the colorballs world;
- NOT SURE IF FOR KEEPS: In 'go!', I set unused children to nil in order
  to release storage; however, this didn't speed things up or reduce global
  garbage collection;
- Deleted *is-actual*, which plays no role anywhere;
- *node-now*, also isn't used anywhere, but was kept as it may play
  a role in other worlds.;

======================================================================
