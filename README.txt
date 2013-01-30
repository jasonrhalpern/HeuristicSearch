Jason Halpern (jrh2170)
Artificial Intelligence
Homework #2
Spring 2012, Professor Pasik

The zip folder includes the following 3 files:
puzzle.lsp
search.lsp
queue.lsp

LOADING THE FILES:
In order to run the program, you have to first unzip the folder. Then you just have to
(load "puzzle.lsp") because that file includes the other files in it with the appropriate
load functions.

There can be stack overflow issues when running the code in CLISP if the number of node
expansions ends up being high.

FILES:
queue.lsp - This file includes a lot of the queue and heap code that Professor Pasik
presented in class. The only modifications I made were to use the appropriate enqueue
and key functions. In this case, I used enqueue-priority to create a priority queue and I
used my evaluation-function for the key instead of the identity function.

search.lsp - This file includes the node structure and search functions from class. I made 
the necessary modifications to the node structure and functions such that a heuristic cost
is also included within the node. I also changed the paramters to expand, general-search and
graph-search so the correct heuristic function is now passed around. In addition, at the bottom
of this file is my evaluation function, which is the actual cost from the root to this node
plus the heuristic cost from this node to the goal state. I also changed the information in the
parameters to the appropriate enqueue (enqueue-priority), key (evaluation-function) and 
samep (equal-states) functions.

puzzle.lsp - This file includes the bulk of the code that I wrote for this assignment. There
are the 3 heuristic functions that I wrote - manhattan, misplaced and extracredit. For this 
assignment, the extra credit heuristic is the manhattan distance heuristic plus a linear conflict
heuristic. Linear conflict is an admissible heuristic since it always underestimates the true cost
to the goal. Linear conflict adds a cost of 2 for each pair of conflict tiles. A pair of tiles have 
a linear conflict if: 
1) The two tiles are on the same line
2) The goal positions of both tiles are on that line
3) If tile x is to the left of tile y, but the goal of tile x is to the right of tile y

In this file, you can also see the state representation that I created for the nodes. Each state is 
a 3x3 array which represents the puzzle board. I wrote functions that allowed me to get the value of
a square at a specific dimension in the board and to set the value of a square in a given dimension. 
I wrote a function to test if a given state is the goal state and to test if two states are
equal states. I also wrote a function to swap the values of two squares and I wrote functions to 
find either the x or y coordinate of a number on a given board. I wrote functions to convert a 
list to the state represenation and vice versa. This file also has the functions I wrote to generate
random solvable states. My successor function is also in this file.

RUNNING THE PROGRAM:
I followed the instructions from your email in terms of input and output. You can run the program 
using the following -
8-puzzle Input:
(8-puzzle start-state heuristic)
 
start-state - is a list of the form '(1 2 3 4 5 6 7 8 0), where 0 is the blank square.
heuristic - is either #'manhattan, #'misplaced, or #'extracredit

In order to generate 5 random solvable states you can use the following:
(random-case)

SAMPLE:
Here are some examples of how the heuristics compare for random states:

1) '(2 3 1 0 4 5 6 7 8)

misplaced
(("up" "right" "right" "down" "left" "left" "up" "right" "down" "right "up"
 "left" "left")
 13 144)

manhattan
(("up" "right" "right" "down" "left" "left" "up" "right" "down" "right "up"
 "left" "left")
 13 79)

extracredit
(("up" "right" "right" "down" "left" "left" "up" "right" "down" "right "up"
 "left" "left")
 13 50)
 
2) '(0 3 5 4 2 1 6 7 8)
 
 misplaced
 (("right" "down" "right" "up" "left" "down" "left" "up") 8 18)
 
 manhattan
 (("right" "down" "right" "up" "left" "down" "left" "up") 8 10)
 
 extracredit
 (("right" "down" "right" "up" "left" "down" "left" "up") 8 10)
 
3) '(0 4 3 5 8 1 7 6 2)
 
 misplaced
 STACK OVERFLOW
 
 manhattan
 (("down" "down" "right" "up" "left" "up" "right" "right" "down" "down" "left" "up" "left"
  "up" "right" "right" "down" "left" "left" "up"
  20 188)
 
 extracredit
 (("down" "down" "right" "up" "left" "up" "right" "right" "down" "down" "left" "up" "left"
  "up" "right" "right" "down" "left" "left" "up"
  20 67)
  
4) '(5 4 2 6 1 3 7 0 8)
  
  misplaced
  (("left" "up" "right" "right" "up" "left" "left" "down" "right" "up" "right" "down" "left"
   "up" "left"
    15 302)
  
  manhattan
   (("left" "up" "right" "right" "up" "left" "left" "down" "right" "up" "right" "down" "left"
   "up" "left"
    15 85)
  
  extracredit
   (("left" "up" "right" "right" "up" "left" "left" "down" "right" "up" "right" "down" "left"
   "up" "left"
    15 75)
	
5) '(1 5 0 6 2 4 7 3 8)

misplaced
(("left" "down" "down" "left" "up" "right" "right" "up" "left" "left") 10 27)

manhattan
(("left" "down" "down" "left" "up" "right" "right" "up" "left" "left") 10 10)

extracredit
(("left" "down" "down" "left" "up" "right" "right" "up" "left" "left") 10 10)