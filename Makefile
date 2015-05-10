HASTE_SOURCES = Packet.hs  LocateIP.hs  mapConnections.hs  
TYPESCRIPT_SOURCES = drop.ts

JAVASCRIPT_FROM_TYPESCRIPT = $(patsubst %.ts,%.js, $(TYPESCRIPT_SOURCES))

default:all

mapConnections.js: $(HASTE_SOURCES)
	hastec mapConnections.hs

mapConnections: $(HASTE_SOURCES)
	ghc --make mapConnections.hs -threaded

%.js: %.ts
	tsc $< 


all: mapConnections mapConnections.js $(JAVASCRIPT_FROM_TYPESCRIPT)

clean:
	-rm mapConnections
	-rm -r main
	-rm *~
	-rm *.hi
	-rm *.o
	-rm *.js

