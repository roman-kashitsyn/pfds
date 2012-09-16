Erratas in "Purely Functional Data Structures"
----------------------------------------------

- "Figure 6.9 : Skew binary random-access list" (p. 79)
  * Type error in  : updateTree has type of Tree a
    but used where the type of (int, Tree a) is required.

  * Error in `lookupTree` and `updateTree` : `if i < w div 2`
    but `if i <= w div 2` is required.
  
