############################################################################
#
#  Program:         SuperLU_DIST
#
#  Module:          make.inc
#
#  Purpose:         Top-level Definitions
#
#  Creation date:   February 4, 1999   version alpha
#
#  Modified:	    September 1, 1999  version 1.0
#                   March 15, 2003     version 2.0
#
############################################################################
#
#  The machine (platform) identifier to append to the library names
#
PLAT		= _linux

#
#  The name of the libraries to be created/linked to
#
DSuperLUroot 	= ${SrcDir}/SuperLU_DIST_2.0
DSUPERLULIB   	= ${DSuperLUroot}/superlu$(PLAT).a
#
#BLASDEF	     	= -DUSE_VENDOR_BLAS
BLASLIB      	= ../blas${PLAT}.a
MPILIB		= -L${MpiDir}/lib -lpmpich -lmpich 
#		-lmpi -lfmpi -lpmpi \
#                  -lelan -lelan3

#PERFLIB     	= -L/vol1/VAMPIR/lib -lVT
LIBS	     	= ${DSUPERLULIB} ${BLASLIB} ${PERFLIB} ${MPILIB}

#
#  The archiver and the flag(s) to use when building archive (library)
#  If your system has no ranlib, set RANLIB = echo.
#
ARCH         	= ar
ARCHFLAGS    	= cr
RANLIB       	= ranlib

CC           	= ${MPICC}
CFLAGS          = -O3
NOOPTS		=
FORTRAN         = ${MPIFC}
FFLAGS          = -O3
LOADER	        = ${MPIFC}
#LOADER	        = 
#LOADOPTS	= -bmaxdata:0x80000000
#LOADOPTS	= -bmaxdata:0x70000000
#
#  C preprocessor defs for compilation (-DNoChange, -DAdd_, or -DUpCase)
#
CDEFS        = -DNoChange
