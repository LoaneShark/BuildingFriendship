; File for initial loading of gridworld code
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Note: Do not change the loading order, as some definitions in earlier
; files may be needed in later files.
(load "monitoring-package.lisp")
(load "gridworld-definitions")
(load "gridworld-planning")
(load "simulation-and-function-evaluation")
(load "go.lisp"); uncompiled -- to avoid compiler bugs
(load "implement-effects.lisp"); uncompiled -- to avoid compiler bugs
(load "gridworld-world")
(load "run-agents"); makes no difference if compiled or uncompiled
;(dotimes (j *n-agents*) (initialize-state-node j)); done in run-agents
