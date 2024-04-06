include ./dir.inc
AR := ar rv
RANLIB := ranlib

default:
	(cd metis.src ; make )
	(cp metis.src/libmetis.a ${LibDir} )
	(cd blas.src ; make )
	(cp blas.src/libblas.a ${LibDir} )
	(cd fepgsolv.src ; make )
	(cp fepgsolv.src/libfepgsolv.a ${LibDir} )
	(cd fepg.src ; make )
	(cp fepg.src/libfepg.a ${LibDir} )
	(cd SuperLU_DIST_2.0 ; make lib )
	(cp SuperLU_DIST_2.0/blas_linux.a ${LibDir} ; cp SuperLU_DIST_2.0/superlu_linux.a ${LibDir} )

clean:
	(cd metis.src ; make clean )
	(cd blas.src ; make clean )
	(cd fepgsolv.src ; make clean )
	(cd fepg.src ; make clean )
	(cd SuperLU_DIST_2.0 ; make cleanlib )

realclean :
	(cd metis.src ; make realclean )
	(cd blas.src ; make realclean )
	(cd fepgsolv.src ; make realclean )
	(cd fepg.src ; make realclean )
	(cd SuperLU_DIST_2.0 ; make clean )
	rm *.a dir.inc -f
