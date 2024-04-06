/*
 * -- Distributed SuperLU routine (version 2.0) --
 * Lawrence Berkeley National Lab, Univ. of California Berkeley.
 * March 15, 2003
 *
 */

#include <math.h>
#include "superlu_ddefs.h"


void superlumain_(int *nprow, int *npcol, int *m_GA, int *n_GA,
            int *nnz_loc, int *m_loc, int *fst_row, double *nzval,
            int *colind, int *rowptr, int *ldb, double *b, int *nrhs)
/*
 * Purpose
 * =======
 *
 * The driver program PDDRIVE.
 *
 * This example illustrates how to use PDGSSVX with the full
 * (default) options to solve a linear system.
 * 
 * Five basic steps are required:
 *   1. Initialize the MPI environment and the SuperLU process grid
 *   2. Set up the input matrix and the right-hand side
 *   3. Set the options argument
 *   4. Call pdgssvx
 *   5. Release the process grid and terminate the MPI environment
 *
 * On the Cray T3E, the program may be run by typing
 *    mpprun -n <procs> pddrive -r <proc rows> -c <proc columns> <input_file>
 * =========
 * Parameters
 * =========
 * nprow,npcol : the grid information
 * m,n         : Dimension of the global matrix, m is the row ,n is the col
 * nnz_loc     : number of nonzeros in the local submatrix
 * m_loc       : number of rows local to this processor
 * fst_row     : global index of the first row
 * nzval       : array of nonzero values,packed by row
 * rowptr      : array of beginning of rows in nzval[] and colind[]
 * colind      : array of column indices of the nonzeros
 * ldb         : leading dimension of the right-hand side matrix
 * b           : the right-hand side matrix
 * nrhs        : number of right-hand sides
 */
{
    superlu_options_t options;
    SuperLUStat_t stat;
    SuperMatrix A;
    ScalePermstruct_t ScalePermstruct;
    LUstruct_t LUstruct;
    SOLVEstruct_t SOLVEstruct;
    gridinfo_t grid;
    NRformat_loc *AStore;
    double   *berr;
    /* double   *b, *xtrue; */
    /* int_t    m, n, nnz; */
    /* int_t    nprow, npcol; */
    /* int      iam, info, ldb, ldx, nrhs; */
    int_t    m,n,i,j;
    int      iam,info;
    char     trans[1];
    /* char     **cpp, c; */
    /* FILE *fp, *fopen(); */

    /* ------------------------------------------------------------
       INITIALIZE THE SUPERLU PROCESS GRID. 
       ------------------------------------------------------------*/
    superlu_gridinit(MPI_COMM_WORLD, *nprow, *npcol, &grid);

    /* Bail out if I do not belong in the grid. */
    iam = grid.iam;
    if ( iam >= (*nprow) * (*npcol) )	goto out;
    if ( !iam ) printf("\tProcess grid\t%d X %d\n", grid.nprow, grid.npcol);

#if ( VAMPIR>=1 )
    VT_traceoff();
#endif

#if ( DEBUGlevel>=1 )
    CHECK_MALLOC(iam, "Enter main()");
#endif

    /* ------------------------------------------------------------
       GET THE MATRIX FROM FILE AND SETUP THE RIGHT HAND SIDE. 
       ------------------------------------------------------------*/
    /* dcreate_matrix(&A, nrhs, &b, &ldb, &xtrue, &ldx, fp, &grid);*/

    if ( !(berr = doubleMalloc_dist(*nrhs)) )
	ABORT("Malloc fails for berr[].");

    /* ------------------------------------------------------------
       NOW WE SOLVE THE LINEAR SYSTEM.
       ------------------------------------------------------------*/

     
    /* Set the default input options:*/
        options.Fact = DOFACT;
        options.Equil = YES;
        options.ColPerm = MMD_AT_PLUS_A;
        options.RowPerm = LargeDiag;
        options.ReplaceTinyPivot = YES;
        options.Trans = NOTRANS;
        options.IterRefine = DOUBLE;
        options.SolveInitialized = NO;
        options.RefineInitialized = NO;
        options.PrintStat = YES;
    
    set_default_options_dist(&options);

#if (0)
    options.RowPerm = NOROWPERM;
    options.IterRefine = NOREFINE;
    options.ColPerm = NATURAL;
    options.Equil = NO; 
    options.ReplaceTinyPivot = NO;
#endif


    m = *m_GA;
    n = *n_GA;

    dCreate_CompRowLoc_Matrix_dist(&A, m, n, *nnz_loc, *m_loc,
                                   *fst_row, nzval, colind, 
                                   rowptr, SLU_NR_loc, SLU_D,
                                   SLU_GE);

    printf(" Print some information of A ========\n");
    printf(" iam,m,n,nnz_loc,m_loc,fst_row == %d,%d,%d,%d,%d,%d\n",iam,m,n,*nnz_loc,*m_loc,*fst_row);
    AStore=(NRformat_loc *)A.Store;
    printf(" Some information of Matrix A ==========\n");
/*    if(iam==1) {*/
    printf(" iam === %d \n",iam);
    printf(" nnz_loc = %d \n",AStore->nnz_loc);
    printf(" m_loc = %d \n",AStore->m_loc);
    printf(" fst_row = %d \n",AStore->fst_row);
/*    printf(" rowptr === ");
    for(i=0;i<AStore->m_loc+1;i++)
       printf(" %d ",AStore->rowptr[i]);
    printf("\n");
    printf(" colind ========\n");
    for(i=0;i<AStore->m_loc;i++)
    {
       for(j=AStore->rowptr[i];j<AStore->rowptr[i+1];j++)
           printf(" %d ",AStore->colind[j]);
        printf("\n");
    }*/
   /* }*/

    /* Initialize ScalePermstruct and LUstruct. */
    ScalePermstructInit(m, n, &ScalePermstruct);
    LUstructInit(m, n, &LUstruct);

    /* Initialize the statistics variables. */
    PStatInit(&stat);

    /* Call the linear equation solver. */
    pdgssvx(&options, &A, &ScalePermstruct, b, *ldb, *nrhs, &grid,
	    &LUstruct, &SOLVEstruct, berr, &stat, &info);


    /* Check the accuracy of the solution. */
    /* pdinf_norm_error(iam, ((NRformat_loc *)A.Store)->m_loc,
		     nrhs, b, ldb, xtrue, ldx, &grid); */

    PStatPrint(&options, &stat, &grid);        /* Print the statistics. */

    /* ------------------------------------------------------------
       DEALLOCATE STORAGE.
       ------------------------------------------------------------*/

    PStatFree(&stat);
    /* Destroy_CompRowLoc_Matrix_dist(&A); */
    ScalePermstructFree(&ScalePermstruct);
    Destroy_LU(n, &grid, &LUstruct);
    LUstructFree(&LUstruct);
    if ( options.SolveInitialized ) {
        dSolveFinalize(&options, &SOLVEstruct);
    }
    /*SUPERLU_FREE(b);*/
    /* SUPERLU_FREE(xtrue); */
    SUPERLU_FREE(berr);

    /* ------------------------------------------------------------
       RELEASE THE SUPERLU PROCESS GRID.
       ------------------------------------------------------------*/
out:
    superlu_gridexit(&grid);

    /* ------------------------------------------------------------
       TERMINATES THE MPI EXECUTION ENVIRONMENT.
       ------------------------------------------------------------*/
    /* MPI_Finalize(); */

#if ( DEBUGlevel>=1 )
    CHECK_MALLOC(iam, "Exit main()");
#endif

}


int cpp_defs()
{
    printf(".. CPP definitions:\n");
#if ( PRNTlevel>=1 )
    printf("\tPRNTlevel = %d\n", PRNTlevel);
#endif
#if ( DEBUGlevel>=1 )
    printf("\tDEBUGlevel = %d\n", DEBUGlevel);
#endif
#if ( PROFlevel>=1 )
    printf("\tPROFlevel = %d\n", PROFlevel);
#endif
#if ( StaticPivot>=1 )
    printf("\tStaticPivot = %d\n", StaticPivot);
#endif
    printf("....\n");
}
