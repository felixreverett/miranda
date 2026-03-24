t_graph * ::= Emptygraph | Node * [t_graph *]

glist :: [t_graph char]
glist = [ Node 'A' [glist!2, glist!1],
          Node 'B' [glist!3],
          Node 'C' [glist!0, glist!3],
          Node 'D' []
        ]

graph :: t_graph char
graph = hd glist

mymember :: [*] -> * -> bool
mymember [] x = False
mymember (x:xs) y = (x=y) \/ member xs y

printgraph :: t_graph char -> [char]
printgraph g
  = xprintgraph g []
    where
    xprintgraph (Node c []) set = "Node " ++ [c] ++ " empty"
    xprintgraph (Node c nodes) set
      = "Node " ++ [c] ++ " seen", if mymember set c
      = "Node " ++ [c] ++ " [" ++ (xaddsublist nodes (set ++ [c])) ++ "]", otherwise

    xaddsublist (x: []) set = xprintgraph x set
    xaddsublist (x: xs) set = (xprintgraph x set) ++ ", " ++ (xaddsublist xs set)

main = printgraph graph