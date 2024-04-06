############################################################################
#
#  Program:         SuperLU_DIST
#
#  Module:          Makefile
#
#  Purpose:         Top-level Makefile
#
#  Creation date:   September 1, 1999  version 1.0
#
#  Modified:        
#
############################################################################

include ../dir.inc
include ./make.inc

#all: install lib example
all: lib

lib: blaslib superlulib

#example:
#	( cd EXAMPLE; $(MAKE) )

clean: cleanlib

blaslib:
	( cd CBLAS; $(MAKE) )

superlulib:
	( cd SRC; $(MAKE) )

cleanlib:
	( cd SRC; $(MAKE) clean )
	( cd CBLAS; $(MAKE) clean )
