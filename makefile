all :
	echo 'Done'

clean :
	make -C Book         clean
	make -C RawData      clean
	make -C RandomForest clean

