
R = R --vanilla --slave

all : dataset.rdata

dataset.rdata : owl.r
	$(R) < owl.r

clean :
	-rm -f *.rdata
	-rm -f *.csv
	-rm -f *.pdf
	echo 'Done'

