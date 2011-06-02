ghc --make lamen-server.hs && strip lamen-server && runghc lamen.hs setup && runghc lamen.hs deploy incoming/sample.tar.bz2 
