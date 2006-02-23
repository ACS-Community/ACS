#
# The following directory are FULLY generated under Linux:
#     acscourse
#
# Still the Exercices directory is not handle, not backed up and 
# not archived in CVS.
#

TAG=HEAD

.PHONY: acscourse
acscourse:
	echo ==== Updating acscourse module
	rm -rf acscourse
	cvs export -r $(TAG) -d acscourse ACS/LGPL/CommonSoftware/acscourse
	cd acscourse/ws/src; make man
	chmod -R ugo+r acscourse

.PHONY: Exercises
Exercises:
	echo ==== Updating Exercises
	cd Exercises; \
	  rm -rf carcontrol dttemp10 dttemp12 dttemp13; \
	  gtar -xzf carcontrol.tar.gz; \
	  gtar -xzf dttemp10.tar.gz; \
	  gtar -xzf dttemp12.tar.gz; \
	  gtar -xzf dttemp13.tar.gz;
	chmod -R ugo+r Exercises

all: acscourse Exercises
	echo ... done.

clean: 
	rm -rf acscourse Exercises/dttemp10 Exercises/dttemp12 Exercises/dttemp13

