all :
	ghc -O2 --make Main.hs -o hscheme

clean :
	rm -f $(EXENAME)
	find -iname "*.prof" -delete
	find -iname "*.eventlog" -delete
	find -iname "*.hi" -delete
	find -iname "*.o" -delete