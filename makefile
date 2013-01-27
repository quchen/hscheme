EXENAME = hscheme
INCLUDE = src
HS = ghc --make -i$(INCLUDE)

all :
	$(HS) -O2 ./src/Main.hs -o $(EXENAME)

wall :
	$(HS) ./src/Main.hs -o $(EXENAME) -Wall

clean :
	rm -f $(EXENAME)
	find -iname "*.prof" -delete
	find -iname "*.eventlog" -delete
	find -iname "*.hi" -delete
	find -iname "*.o" -delete