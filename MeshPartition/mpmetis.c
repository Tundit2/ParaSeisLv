/*
 * Copyright 1994-2011, Regents of the University of Minnesota
 *
 * mpmetis.c
 *
 * Drivers for the mesh partitioning routines
 *
 * Started 8/28/94
 * George
 *
 * $Id: mpmetis.c 14362 2013-05-21 21:35:23Z karypis $
 *
 */

#include "metis.h"
#include <stdio.h>
#include "GKlib.h"




/*************************************************************************/
/*! Let the game begin! */
/*************************************************************************/
//int main(int argc, char *argv[])
void partdmain_(int *ne, int *nn, idx_t *eptr, idx_t *eind, int *nparts, int *gtype, idx_t *epart, idx_t *npart)
{
  idx_t objval;
  int status=0;
  int ncommon = 2;

  printf("*******************\n");
  printf("Print out data to check\n");
  printf("ne, nn, nparts, gtype = \n");
  printf("%d %d %d %d\n", *ne, *nn, *nparts, *gtype);
  printf("*******************************************\n");
  printf("eptr:\n");
  printf("%d %d %d \n",eptr[0], eptr[1], eptr[2]);
  printf("%d %d %d \n",eptr[(*ne)-2], eptr[(*ne)-1], eptr[(*ne)]);
  printf("*******************************************\n");
  printf("%d %d %d \n",eind[0], eind[1], eind[2]);
  printf("%d %d %d \n",eind[(*ne)*4-3], eind[(*ne)*4-2], eind[(*ne)*4-1]);
  printf("*******************************************\n");

/*
  gk_startcputimer(iotimer);
  mesh = ReadMesh(params);

  if (mesh->ncon > 1) {
    printf("*** Meshes with more than one balancing constraint are not supported yet.\n");
    exit(0);
  }

  ReadTPwgts(params, mesh->ncon);
  gk_stopcputimer(params->iotimer);
*/

//  MPPrintInfo(params, mesh);

//  epart = imalloc(mesh->ne, "main: epart");
//  npart = imalloc(mesh->nn, "main: npart");


//  gk_malloc_init();
//  gk_startcputimer(parttimer);

  switch (*gtype) {
    case 1:
      status = METIS_PartMeshDual(ne, nn, eptr, eind, 
                   NULL, NULL, &ncommon, nparts, 
                   NULL, NULL, &objval, epart, npart);
      break;

    case 2:
      status = METIS_PartMeshNodal(ne, nn, eptr, eind, 
                   NULL, NULL, nparts, NULL, NULL, &objval, 
                   epart, npart);
      break;
  }

//  gk_stopcputimer(parttimer);
/*  if (gk_GetCurMemoryUsed() != 0)
        printf("***It seems that Metis did not free all of its memory! Report this.\n");
  params->maxmemory = gk_GetMaxMemoryUsed();
  gk_malloc_cleanup(0);
*/
/*  if (status != METIS_OK) {
    printf("\n***Metis returned with an error.\n");
  }
/*  else {
    if (!params->nooutput) {
       Write the solution 
      gk_startcputimer(params->iotimer);
      WriteMeshPartition(params->filename, params->nparts, mesh->ne, epart, mesh->nn, npart);
      gk_stopcputimer(params->iotimer);
    }
*/
//    MPReportResults(params, mesh, epart, npart, objval);
//  }

//  FreeMesh(&mesh);
//  gk_free((void **)&epart, &npart, LTERM);
/*  gk_free((void **)&params->filename, &params->tpwgtsfile, &params->tpwgts, 
      &params->ubvecstr, &params->ubvec, &params, LTERM);
*/
}


/*************************************************************************/
/*! This function prints run parameters */
/*************************************************************************/
/*void MPPrintInfo(params_t *params, mesh_t *mesh)
{ 
  if (params->ufactor == -1) {
    if (params->ptype == METIS_PTYPE_KWAY)
      params->ufactor = KMETIS_DEFAULT_UFACTOR;
    else 
      params->ufactor = PMETIS_DEFAULT_UFACTOR;
  }

  printf("******************************************************************************\n");
  printf("%s", METISTITLE);
  printf(" (HEAD: %s, Built on: %s, %s)\n", SVNINFO, __DATE__, __TIME__);
  printf(" size of idx_t: %zubits, real_t: %zubits, idx_t *: %zubits\n", 
      8*sizeof(idx_t), 8*sizeof(real_t), 8*sizeof(idx_t *));
  printf("\n");
  printf("Mesh Information ------------------------------------------------------------\n");
  printf(" Name: %s, #Elements: %"PRIDX", #Nodes: %"PRIDX", #Parts: %"PRIDX"\n", 
      params->filename, mesh->ne, mesh->nn, params->nparts);
  if (mesh->ncon > 1)
    printf("  Balancing Constraints: %"PRIDX"\n", mesh->ncon);

  printf("\n");
  printf("Options ---------------------------------------------------------------------\n");
  printf(" ptype=%s, objtype=%s, ctype=%s, rtype=%s, iptype=%s\n",
      ptypenames[params->ptype], objtypenames[params->objtype], ctypenames[params->ctype], 
      rtypenames[params->rtype], iptypenames[params->iptype]);

  printf(" dbglvl=%"PRIDX", ufactor=%.3f, minconn=%s, contig=%s, nooutput=%s\n",
      params->dbglvl,
      I2RUBFACTOR(params->ufactor),
      (params->minconn  ? "YES" : "NO"), 
      (params->contig   ? "YES" : "NO"),
      (params->nooutput ? "YES" : "NO")
      );

  printf(" seed=%"PRIDX", niter=%"PRIDX", ncuts=%"PRIDX"\n", 
      params->seed, params->niter, params->ncuts);

  printf(" gtype=%s, ncommon=%"PRIDX", niter=%"PRIDX", ncuts=%"PRIDX"\n", 
      gtypenames[params->gtype], params->ncommon, params->niter, params->ncuts);

  printf("\n");
  switch (params->ptype) {
    case METIS_PTYPE_RB:
      printf("Recursive Partitioning ------------------------------------------------------\n");
      break;
    case METIS_PTYPE_KWAY:
      printf("Direct k-way Partitioning ---------------------------------------------------\n");
      break;
  }
}


/*************************************************************************/
/*! This function does any post-partitioning reporting */
/*************************************************************************/
/*void MPReportResults(params_t *params, mesh_t *mesh, idx_t *epart, idx_t *npart,
         idx_t objval)
{ 

  gk_startcputimer(params->reporttimer);

  /* ComputePartitionInfo(params, graph, part); */

/*  printf(" - %s: %"PRIDX".\n\n", 
      (params->objtype == METIS_OBJTYPE_CUT ? "Edgecut" : "Volume"), objval);

  gk_stopcputimer(params->reporttimer);


  printf("\nTiming Information ----------------------------------------------------------\n");
  printf("  I/O:          \t\t %7.3"PRREAL" sec\n", gk_getcputimer(params->iotimer));
  printf("  Partitioning: \t\t %7.3"PRREAL" sec   (METIS time)\n", gk_getcputimer(params->parttimer));
  printf("  Reporting:    \t\t %7.3"PRREAL" sec\n", gk_getcputimer(params->reporttimer));
  printf("\nMemory Information ----------------------------------------------------------\n");
  printf("  Max memory used:\t\t %7.3"PRREAL" MB\n", (real_t)(params->maxmemory/(1024.0*1024.0)));

  {
    struct rusage usage;
    getrusage(RUSAGE_SELF, &usage);
    printf("  rusage.ru_maxrss:\t\t %7.3"PRREAL" MB\n", (real_t)(usage.ru_maxrss/(1024.0)));
  }

  printf("  proc/self/stat/VmPeak:\t %7.3"PRREAL" MB\n", (real_t)gk_GetProcVmPeak()/(1024.0*1024.0));

  printf("******************************************************************************\n");

}
*/
