######################################
#       Makefile for libblas.a       #
###################################### 
include Makefile.in
include ../dir.inc

CC = ${MPICC}
F77 = ${MPIFC}
AR = ar rv
RANLIB := ranlib

OBJS := ${patsubst %.f,%.o,${SOURCE}}
#OBJS = ${SOURCE:.f=.o}

default : libblas.a

libblas.a : ${OBJS}
	${AR} $@ ${OBJS}
	${RANLIB} $@

%.o:%.f
	${F77} -c $<

clean : 
	rm -f *.o

realclean :
	rm -f *.o ; rm -f libblas.a
