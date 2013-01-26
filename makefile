EXENAME = hscheme

all :
	ghc -O2 --make Main.hs -o $(EXENAME)

wall :
	ghc -O2 --make Main.hs -o $(EXENAME) -Wall

clean :
	rm -f $(EXENAME)
	find -iname "*.prof" -delete
	find -iname "*.eventlog" -delete
	find -iname "*.hi" -delete
	find -iname "*.o" -delete