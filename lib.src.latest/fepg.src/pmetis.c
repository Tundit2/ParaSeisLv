/*
 * Copyright 1997, Regents of the University of Minnesota
 *
 * pmetis.c
 *
 * This file contains the driving routine for multilevel method
 *
 * Started 8/28/94
 * George
 *
 * $Id: pmetis.c,v 1.1 1998/11/27 17:59:39 karypis Exp $
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include<sys/socket.h>
#include<sys/ioctl.h>
#include<net/if.h>

#define LICENSEFILE "pass.cfg"
#define TEMPFILE "/etc/fepg/hostid.cfg"
#define verify1 verify1_
#define BYTE unsigned char
void EncodePass(char *str,char *pass);
void DecodePass(char *str,char *pass);
//void verify1_(int *rtnvalue, char *licpathname)
void verify1_(int *rtnvalue, char *licfname, int strlenth)
{
	FILE *fp;
	char str[50],pass[50];
	char hostid[50];
        char licpathname[1000];
        char licensefile[1000];
	struct ifreq ifr;
	struct sockaddr *shard;
	int sockfd,i;
	char adds[6];
	char temp[100];
	int j;
	int deduce;
	int fillnum;
	int length;
	char c;

/*************      liushaopeng modifyed ********************/
        for(i=0;i<1000;i++)
        {
          if(licfname[i]=='\0') break;
          if(licfname[i]=='\\'&&licfname[i+1]=='0') break;
          licpathname[i]=licfname[i];
        }
        licpathname[i]='\0';
/************************************************************/

	*rtnvalue = 0;
	sockfd=socket(AF_INET,SOCK_DGRAM,0);
	strcpy(ifr.ifr_name, "eth0");
	if (ioctl(sockfd, SIOCGIFHWADDR, &ifr) < 0)
	{  
	   exit(-1);
	}
	shard=&ifr.ifr_hwaddr;
	adds[0]='d';adds[1]=',';
	adds[2]='1';adds[3]='7';
	adds[4]='y';adds[5]='k';
	for(i=0;i<100;i++)
	{	
           temp[i]='\0';
        }
 	sprintf(temp,"%02X%02X%02X%02X%02X%02X",
		shard->sa_data[0],
		shard->sa_data[1],
		shard->sa_data[2],
		shard->sa_data[3],
		shard->sa_data[4],
		shard->sa_data[5]
	);
	for(i=0;i<100;i++)
	{
		if(temp[i]=='F' ||temp[i]=='f')
		{
			for(j=i+1;j<100;j++)
			{
				temp[j-1]=temp[j];
			}
			i--;	
		}		 
	}
	if(strlen(temp) > 10){
		deduce = strlen(temp)-10;
		for(i=0;i<deduce;i++)
		{
			for(j=i+1;j<100;j++)
			{
				temp[j-1]=temp[j];
			}
		}
	}
	for(i=0;i<100;i++)
	{
		if(temp[i]>=48 && temp[i]<=57)
			temp[i]+=20;
	}		
//////////////////
	//读license
//	printf("%s, %d, %d",licpathname,sizeof(licpathname),strlen(licpathname));

//	i = strlenth;
        i = strlen(licpathname);
        for(j=0;j<i;j++)
        {
           licensefile[j]=licpathname[j];
        }
	for(j=i-1;j>=0;j--)
	{
//		printf("%d,%d\n",j,licpathname[j]);
			
		if(licpathname[j]==32)
		{
//			licpathname[j]='\0';
			licensefile[j]='\0';
			continue;
		}
                else
                {
			break;
		}
	}
//	fp=fopen(licpathname,"r");
//        licpathname=&licensefile[0];
//	fp=fopen("/usr/local/pfepglib/license.cfg","r");
	fp=fopen(licpathname,"r");
//        printf("%s",licensefile);
//        return;
        printf("%s%d","fp=",fp);
	if(!fp)
	{
		*rtnvalue = -2;
		return;
	}
	memset(str,0,sizeof(str));
	fillnum = 0;
	while(feof(fp)==0){
		if(fillnum == 20)
		{
//			printf("license=(%s)\n",str);
			//解开明文
			DecodePass(str,pass);
			length=strlen(pass);
			if((length == strlen(temp)) && (strncmp(pass,temp,length)==0))
			{	
				*rtnvalue = 1;
				break;
			}
		}
		c = fgetc(fp);
		if((c != '\n') && (c != '\r')) 
		{
			str[fillnum]=c;
			fillnum++;
		}
		else{
			memset(str,0,sizeof(str));
			fillnum = 0;
		}
		//fread(str,sizeof(char),20,fp);
		
	}
	fclose(fp);
	return;
}

/*将str加密为pass*/
void EncodePass(char *str,char *pass)
{
    char s1[20];
    int i, j, k, npass;
    int ch[257],nch; /*可逆查找，ASCII-序号*/
    int chi[257];
	char buf[2],str1[20];
    int i2;

	time_t aclock;
	time( &aclock );                 /* Get time in seconds */

	memset(pass,0,sizeof(pass));
    
    npass = 10;
    nch = 0;
    for( i = 48;i<= 57;i++)
	{
        ch[nch] = i;
        chi[i] = nch;
        nch = nch + 1;
    }
    for( i = 65;i<=90;i++)
	{
        ch[nch] = i;
        chi[i] = nch;
        nch = nch + 1;
    }
    for( i = 97;i<=122;i++)
	{
        ch[nch] = i;
        chi[i] = nch;
        nch = nch + 1;
    }
    ch[nch] = 32;
    chi[32] = nch;
    nch = nch + 1;
    
    
    i=strlen(str);
	if(i>10) str[10]='\0';
    i=strlen(str);
    for( k = 0;k< npass - i;k++)
	{
		strcpy(str1," ");
        strcat(str1,str);
        strcpy(str,str1);
	}
    
	srand(aclock);

    s1[0]=0;
	buf[1]='\0';
    for( k = 0;k< npass;k++)
	{
        i = (int)(((nch-1) * (double)rand()/RAND_MAX));
		buf[0]=(BYTE)ch[i];
        strcat(s1,buf);
	}
    
    strcpy(pass, s1);
    for( j = 0;j< npass;j++)
	{
        i2 = chi[(BYTE)str[j]] + chi[(BYTE)s1[j]];
        if( i2 >= nch)
		{
			i2 = i2 - nch;
		}
		buf[0]=(BYTE)ch[i2];
        strcat(pass,buf);
    }
}

void DecodePass(char *str,char *pass)
{
    char s1[50], s2[50];
    int i, j, npass, i2;
    int ch[257];
	int nch;
    int chi[257];
	char buf[2];
	char pass1[50];
    
    npass = 10;
    nch = 0;
    
    for( i = 48;i<=57;i++)
	{
        ch[nch] = i;
        chi[i] = nch;
        nch = nch + 1;
    }
    for( i = 65;i<=90;i++)
	{
        ch[nch] = i;
        chi[i] = nch;
        nch = nch + 1;
    }
    for( i = 97;i<=122;i++)
	{
        ch[nch] = i;
        chi[i] = nch;
        nch = nch + 1;
    }
    ch[nch] = 32;
    chi[32] = nch;
    nch = nch + 1;
    
    strcpy(s1,str);
	s1[npass]=0;
    strcpy(s2,str+npass);
  
    pass[0]=0;
	buf[1]='\0';
	
    for( j = 0;j<npass;j++)
	{
        i2 = chi[(BYTE)(s2[j])] - chi[(BYTE)(s1[j])];
        if( i2 < 0 )
		{
			i2 = i2 + nch;
		}
		buf[0]=ch[i2];
        strcat(pass,buf);
    }
	strcpy(pass1,pass);
	while(pass1[0]==' ')
    {
		strcpy(pass,pass1+1);
		strcpy(pass1,pass);
		if(pass[0]==0) break;
    }
	while(pass1[strlen(pass1)-1]==' ')
	{
		pass1[strlen(pass1)-1]=0;
		if(strlen(pass1)<=0) break;
	}
	strcpy(pass,pass1);
}
/*
 * Copyright 1997, Regents of the University of Minnesota
 *
 * pmetis.c
 *
 * This file contains the driving routine for multilevel method
 *
 * Started 8/28/94
 * George
 *
 * $Id: pmetis.c,v 1.1 1998/11/27 17:59:39 karypis Exp $
 *
 */

#include <metis.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>

/*************************************************************************
* Let the game begin
**************************************************************************/
void pmetismain_(int *np,int *nodpart,int *nn,int *ne,idxtype *numcol,idxtype *na,int strlenth)
{
  int i, options[10], nparts, itemp;
  idxtype *part;
  float lbvec[MAXNCON];
  GraphType graph;
  int numflag = 0, wgtflag = 0, edgecut;
  timer TOTALTmr, METISTmr, IOTmr;
  char filename[200], *argv;
/*******************************************************/
  int fmt, readew, readvw, ncon;
  int knode=*nn, nedges=*ne;
/*******************************************************/

  nparts=*np;

  if (nparts < 2) {
    printf("The number of partitions should be greater than 1!\n");
    exit(0);
  }

  cleartimer(TOTALTmr);
  cleartimer(METISTmr);
  cleartimer(IOTmr);

  starttimer(TOTALTmr);
  starttimer(IOTmr);

  argv="elem.graph";
  strcpy(filename, argv);
  //ReadGraph_new(&graph, nn, ne, numcol, na, filename, &wgtflag);
/******************************************************************************/
/******************************************************************************/
 
  InitGraph(&graph);
 
  graph.nvtxs=*nn;
  graph.nedges=*ne;
 
  fmt = ncon = 0;
 
  readew = (fmt%10 > 0);
  readvw = ((fmt/10)%10 > 0);
  if (fmt >= 100) {
    printf("Cannot read this type of file format!");
    exit(0);
  }
 
  wgtflag = 0;
  if (readew)
    wgtflag += 1;
  if (readvw)
    wgtflag += 2;
 
  if (ncon > 0 && !readvw) {
    printf("------------------------------------------------------------------------------\n");
    printf("***  I detected an error in your input file  ***\n\n");
    printf("You specified ncon=%d, but the fmt parameter does not specify vertex weights\n", ncon);
    printf("Make sure that the fmt parameter is set to either 10 or 11.\n");
    printf("------------------------------------------------------------------------------\n");
    exit(0);
  }
 
  graph.nedges *=2;
  ncon = graph.ncon = (ncon == 0 ? 1 : ncon);

  //printf("fmt, fmt%10, (fmt/10)%10, ncon, graph->ncon, [readew, readvw]=");
  //printf("%d %d %d %d %d [%d %d]\n", fmt, fmt%10, (fmt/10)%10, ncon, graph.ncon, readew, readvw);

  if (graph.nvtxs > MAXIDX)
    errexit("\nThe matrix is too big: %d [%d %d]\n", graph.nvtxs, MAXIDX, sizeof(idxtype));
  
  graph.adjncy = idxmalloc(graph.nedges, "ReadGraph: adjncy");
 
  graph.vwgt = (readvw ? idxmalloc(ncon*graph.nvtxs, "ReadGraph: vwgt") : NULL);
  graph.adjwgt = (readew ? idxmalloc(graph.nedges, "ReadGraph: adjwgt") : NULL);

  /* Start reading the graph file */

 graph.xadj=numcol;
  for(i=0;i<numcol[*nn];i++)
     graph.adjncy[i]=na[i]-1;

/******************************************************************************/
/******************************************************************************/
  if (graph.nvtxs <= 0) {
    printf("Empty graph. Nothing to do.\n");
    exit(0);
  }
  stoptimer(IOTmr);
  printf("**********************************************************************\n");
  printf("%s", METISTITLE);
  printf("Graph Information ---------------------------------------------------\n");
  printf("  Name: %s, #Vertices: %d, #Edges: %d, #Parts: %d\n", filename, graph.nvtxs, graph.nedges/2, nparts);

  if (graph.ncon > 1)
    printf("  Balancing Constraints: %d\n", graph.ncon);
  printf("\nRecursive Partitioning... -------------------------------------------\n");

  //part = idxmalloc(graph.nvtxs, "main: part");
  part = nodpart;
  options[0] = 0;

  starttimer(METISTmr);
  if (graph.ncon == 1) {
    METIS_PartGraphRecursive(&graph.nvtxs, graph.xadj, graph.adjncy, NULL, NULL,
          &wgtflag, &numflag, &nparts, options, &edgecut, part);
  }
  else {
    METIS_mCPartGraphRecursive(&graph.nvtxs, &graph.ncon, graph.xadj, graph.adjncy, graph.vwgt,
          graph.adjwgt, &wgtflag, &numflag, &nparts, options, &edgecut, part);
  }

  stoptimer(METISTmr);

  ComputePartitionBalance(&graph, nparts, part, lbvec);

  printf("  %d-way Edge-Cut: %7d, Balance: ", nparts, edgecut);
  for (i=0; i<graph.ncon; i++)
    printf("%5.2f ", lbvec[i]);
  printf("\n");

  starttimer(IOTmr);
  //WritePartition(filename, part, graph.nvtxs, nparts);

  stoptimer(IOTmr);
  stoptimer(TOTALTmr);

  printf("\nTiming Information --------------------------------------------------\n");
  printf("  I/O:          \t\t %7.3f\n", gettimer(IOTmr));
  printf("  Partitioning: \t\t %7.3f   (time)\n", gettimer(METISTmr));
  printf("  Total:        \t\t %7.3f\n", gettimer(TOTALTmr));
  printf("**********************************************************************\n");

  GKfree(&graph.adjncy, LTERM);
}
