all: mapConnections mapConnections.js

mapConnections.js: mapConnections.hs
	hastec mapConnections.hs

mapConnections:
	ghc --make mapConnections.hs -threaded

clean:
	-rm -r main
	-rm *~
	-rm mapConnections.hi
	-rm mapConnections.o

distclean: clean
	-rm mapConnections
	-rm mapConnections.js
