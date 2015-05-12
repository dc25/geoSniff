COMMON_SOURCES = Packet.hs  LocateIP.hs  GeoSniff.hs  
GHC_SOURCES = $(COMMON_SOURCES) geoSniffService.hs
HASTE_SOURCES = $(COMMON_SOURCES) geoSniffClient.hs
TYPESCRIPT_SOURCES = drop.ts

JAVASCRIPT_FROM_TYPESCRIPT = $(patsubst %.ts,%.js, $(TYPESCRIPT_SOURCES))

default:all

geoSniffClient.js: $(HASTE_SOURCES)
	hastec geoSniffClient.hs -o $@

geoSniffService: $(GHC_SOURCES)
	ghc --make geoSniffService.hs -threaded

%.js: %.ts
	tsc $< 


all: geoSniffService geoSniffClient.js $(JAVASCRIPT_FROM_TYPESCRIPT)

clean:
	-rm geoSniffService
	-rm -r main
	-rm *~
	-rm *.hi
	-rm *.o
	-rm *.js

