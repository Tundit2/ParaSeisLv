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
#define verify verify_
#define BYTE unsigned char
void EncodePass(char *str,char *pass);

void verify_(int *rtnvalue, char *licfname1,int strlenth)
{
	FILE *fp;
	char str[50],pass[50],licpathname[1000];
	char hostid[50];
	*rtnvalue = 0;
	struct ifreq ifr;
	struct sockaddr *shard;
	int sockfd,i;

/*************      liushaopeng modifyed ********************/
        for(i=0;i<1000;i++)
        {
          if(licfname1[i]=='\0') break;
          if(licfname1[i]=='\\'&&licfname1=='0') break;
          licpathname[i]=licfname1[i];
        }
        licpathname[i]='\0';
       // printf("In paraverfy:: licpathname == %s\n",licpathname);
/************************************************************/
	sockfd=socket(AF_INET,SOCK_DGRAM,0);
	strcpy(ifr.ifr_name, "eth0");
	if (ioctl(sockfd, SIOCGIFHWADDR, &ifr) < 0)
	{  
	   exit(-1);
	}
	shard=&ifr.ifr_hwaddr;
	char adds[6];
	adds[0]='d';adds[1]=',';
	adds[2]='1';adds[3]='7';
	adds[4]='y';adds[5]='k';
	char temp[100];
	for(i=0;i<100;i++)
		temp[i]='\0';
	sprintf(temp,"%02X%02X%02X%02X%02X%02X",
		shard->sa_data[0],
		shard->sa_data[1],
		shard->sa_data[2],
		shard->sa_data[3],
		shard->sa_data[4],
		shard->sa_data[5]
	);
	int j;
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
		int deduce;
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
//	printf("%s, %d, %d,  %d",licpathname,sizeof(licpathname),strlen(licpathname),strlenth);

	i = strlenth;
	for(j=i-1;j>=0;j--)
	{
//		printf("%d,%d\n",j,licpathname[j]);
			
		if(licpathname[j]==32)
		{
			licpathname[j]='\0';
			continue;
		}else{
			break;
		}
	}
	fp=fopen(licpathname,"r");
	if(!fp)
	{
		*rtnvalue = -2;
		return;
	}
	memset(str,0,sizeof(str));
	int fillnum = 0;
	while(feof(fp)==0){
		if(fillnum == 20)
		{
//			printf("license=(%s)\n",str);
			//解开明文
			DecodePass(str,pass);
			int length=strlen(pass);
			if((length == strlen(temp)) && (strncmp(pass,temp,length)==0))
			{	
				*rtnvalue = 1;
				break;
			}
		}
		char c = fgetc(fp);
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
