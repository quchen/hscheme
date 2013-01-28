EXENAME = hscheme
INCLUDE = src
HS = ghc --make -i$(INCLUDE)

all :
	$(HS) -O ./src/Main.hs -o $(EXENAME)

debug :
	@echo -n "GHC: "
	$(HS) ./src/Main.hs -o $(EXENAME) -Wall
	@echo "Hlint:"
	@hlint ./src

clean :
	rm -f $(EXENAME)
	find -iname "*.prof" -delete
	find -iname "*.eventlog" -delete
	find -iname "*.hi" -delete
	find -iname "*.o" -delete