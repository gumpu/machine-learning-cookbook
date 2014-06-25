
test1.csv : rf.r
	R --vanilla < rf.r

density_plot.pdf : test1.csv
	R --vanilla  --args \
	    ../RawData/Owls/training.csv test1.csv kind \
	    < ../Tools/density_plot.r

clean :
	-rm -f *.pdf
	-rm -f test1.csv
	echo 'Done'

