/*
 * GKlib.h
 * 
 * George's library of most frequently used routines
 *
 * $Id: GKlib.h 14866 2013-08-03 16:40:04Z karypis $
 *
 */

#ifndef _GKLIB_H_
#define _GKLIB_H_ 1

#define GKMSPACE

#if defined(_MSC_VER)
#define __MSC__
#endif
#if defined(__ICC)
#define __ICC__
#endif


#include "gk_arch.h" /*!< This should be here, prior to the includes */


/*************************************************************************
* Header file inclusion section
**************************************************************************/
#include <stddef.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include <memory.h>
#include <errno.h>
#include <ctype.h>
#include <math.h>
#include <float.h>
#include <time.h>
#include <string.h>
#include <limits.h>
#include <signal.h>
#include <setjmp.h>
#include <assert.h>
#include <sys/stat.h>

#if defined(__WITHPCRE__)
  #include <pcreposix.h>
#else
  #if defined(USE_GKREGEX)
    #include "gkregex.h"
  #else
    #include <regex.h>
  #endif /* defined(USE_GKREGEX) */
#endif /* defined(__WITHPCRE__) */



#if defined(__OPENMP__) 
#include <omp.h>
#endif




#include "./gklib/gk_types.h"
#include "./gklib/gk_struct.h"
#include "./gklib/gk_externs.h"
#include "./gklib/gk_defs.h"
#include "./gklib/gk_macros.h"
#include "./gklib/gk_getopt.h"

#include "./gklib/gk_mksort.h"
#include "./gklib/gk_mkblas.h"
#include "./gklib/gk_mkmemory.h"
#include "./gklib/gk_mkpqueue.h"
#include "./gklib/gk_mkpqueue2.h"
#include "./gklib/gk_mkrandom.h"
#include "./gklib/gk_mkutils.h"

#include "./gklib/gk_proto.h"


#endif  /* GKlib.h */


