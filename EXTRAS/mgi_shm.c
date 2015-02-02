/*
 * Copyright (C) 2014       ESCER center, UQAM
 *
 * This code is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation,
 * version 2.1 of the License.
 *
 * This code is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this code; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

/*
Author: Michel Valin, UQAM, 2014/03/10

This program runs in the background and terminates when the parent process terminates.
It will also terminate if a specified number of "attaches" to the memory segment occurred.

Before it terminates, the program will mark the memory segment for destruction

It expects two or three input parameters:
Arg1  : -q    (quiet monitoring mode)
Arg1/2: size of a shared memory segment to create and watch
Arg2/3: channel name for mgi

The program will wait 20 milliseconds between checks of parent process existence.

*/

#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <mgi.h>
#include <sys/time.h>

#define EXIT_SUCCESS 0
#define EXIT_FAILURE 1

static void usage(char *my_name)
{
  fprintf(stderr,"usage: %s [-q] shm_size[M] mgi_channel_name\n",my_name);
  fprintf(stderr,"   shm_size = shared memory segment size in [K/M]Bytes\n");
  exit(1);
}

/* unused as yet borrowed code */
int mgi_shm(int argc, char **argv){
char *my_name=argv[0];
int i;
pid_t pp=getppid();  /* get pid of my parent */
#if defined(DAEMONIZE)
pid_t sid;
#endif
useconds_t sleep_duration=10000;  /* 10 milliseconds */
int to_watch, to_create;
struct shmid_ds shm_stat;
size_t shm_size;
int shm_size_k;
mgi_shm_buf *shm;
char channel_filename[1024];
char channel_filename_d[1024];
char channel_filename_e[1024];
int fd;
FILE *FD, *stddiag;
int nints;
char multiplier='K';
int items=0;
int read_att=0;
int writ_att=0;
int verbose = 0;
int error_status = 0;
int attach;
volatile int in,out,read_status,write_status;
long long time0, time1;
double rtime;
struct timeval timetag;

stddiag = stderr;

if(argc < 2) usage(my_name);

if(strcmp("-v",argv[1])==0) {
  fprintf(stderr,"INFO: verbose mode active\n");
  verbose = 1;
  argv++;
  argc--;
}

if(argc != 3) usage(my_name);

items = sscanf(argv[1],"%d %1c",&to_create,&multiplier);
if(multiplier=='M' || multiplier=='m') to_create *= 1024;   /* megabytes, not kilobytes */

to_watch=shmget(IPC_PRIVATE,to_create*1024,0600);
if(to_watch == -1) {
  fprintf(stderr,"ERROR: shared memory segment creation failed\n");
  exit(1);
}

shm = (mgi_shm_buf *) shmat(to_watch, NULL, 0);  /* attach segment just created */
if(shm == (void *) -1) {
  fprintf(stderr,"ERROR: memory segment %d cannot be attached\n",to_watch);
  exit(1);
}else{
  if(verbose)fprintf(stderr,"INFO: memory segment %d successfully attached\n",to_watch);
}

FD=NULL;
snprintf(channel_filename_e,sizeof(channel_filename_e),"%s/.gossip/SHM/%s.msg",getenv("HOME"),argv[2]);
snprintf(channel_filename,sizeof(channel_filename),"%s/.gossip/SHM/%s.id",getenv("HOME"),argv[2]);
unlink(channel_filename);   /* remove previously existing file, ignore errors */
FD=fopen(channel_filename,"w");
if(FD==NULL){
  fprintf(stderr,"ERROR: cannot open %s\n",channel_filename);
  i = shmctl(to_watch,IPC_RMID,NULL);   /* mark segment for removal */
  i = shmdt(shm);                       /* detach memory segment, we do not want to prevent release */
  exit(1);
}else{
  fprintf(FD,"%d",to_watch);
  fclose(FD);
}

usleep(50000);  /* 50 milliseconds */
/* on non linux systems, it is not possible to attach a shared memory area that is marked for deletion */
#if defined(linux)
i = shmctl(to_watch,IPC_RMID,NULL);   /* immediately mark segment for removal if linux */
#endif
usleep(50000);  /* 50 milliseconds */

i = shmctl(to_watch,IPC_STAT,&shm_stat);
shm_size = shm_stat.shm_segsz / 1024;
shm_size_k = shm_size;
if(verbose)fprintf(stderr,"INFO: Using shared memory segment, id = '%d', size = %d KBytes, status=%d\n",to_watch,shm_size_k,i);

/* initialize memory segment for use by mgilib */
shm->read_status  = MGI_SHM_IDLE;
shm->write_status = MGI_SHM_IDLE;
shm->first = 0;                     /* first position in buffer */
shm->in = 0;                        /* insertion position in buffer */
shm->out = 0;                       /* extraction position in buffer */
shm_size = shm_stat.shm_segsz;      /* size of memory area */
shm_size -= sizeof(mgi_shm_buf);    /* minus structure size */
shm_size /= sizeof(unsigned int);   /* convert to number of unsigned int */
shm->limit = shm_size - 1;          /* last usable position in buffer */

snprintf(channel_filename_d,sizeof(channel_filename_d),"%s/.gossip/SHM/%s",getenv("HOME"),argv[2]);
if(verbose)fprintf(stderr,"INFO: trying to open %s\n",channel_filename_d);
fd=open(channel_filename_d,O_RDONLY);
if(fd > 0) {                        /* if channel restart file exists, read it into buffer */
  nints = read(fd,shm->data,shm->limit*sizeof(unsigned int)) / sizeof(unsigned int) ;
  shm->in = nints;
  if(verbose)fprintf(stderr,"INFO: read %d elements from '%s'\n",nints,channel_filename_d);
  if(nints == shm->limit){
    fprintf(stderr,"WARNING: restart file possibly larger than shared memory buffer\n");
  }
  close(fd);
  unlink(channel_filename_d); /* remove what we have just read */
}else{
  if(verbose)fprintf(stderr,"INFO: no letfover data found for channel %s\n",argv[2]);
}

fprintf(stdout,"%d\n",to_watch);
fflush(stderr);
fflush(stdout);
fclose(stdout);

i = fork();
if(i > 0) exit(0);  /* parent exits */
if(i < 0) exit(1);  /* fork failed */

#if defined(DAEMONIZE)
sid = setsid();  /* daemonize */
if(sid < 0) exit(1) ;
i = fork();
if(i > 0) exit(0);  /* parent exits */
if(i < 0) exit(1);  /* fork failed */
#endif
if(verbose)fprintf(stderr,"INFO: opening new diag file %s\n",channel_filename_e);
stddiag = fopen(channel_filename_e,"w");
in = shm->in;
out = shm->out;
read_status = shm->read_status;
write_status = shm->write_status;
i = shmctl(to_watch,IPC_STAT,&shm_stat);
if( i == -1){
  fprintf(stderr,"ERROR: shared memory segment no longer accessible, status = %s\n",i);
  exit(1);      /* segment no longer accessible, quit with error */
}
attach = shm_stat.shm_nattch;
if(verbose)fprintf(stderr,"INFO: attach count = %d\n",attach);

while(1){
  if(kill(pp,0)) goto flush_buffer;              /* original parent no longer exists, time to quit */
  usleep(sleep_duration);            /* 10 milliseconds */
  if(shm->read_status  == MGI_SHM_ACTIVE) read_att = 1;
  if(shm->write_status == MGI_SHM_ACTIVE) writ_att = 1;

  if(shmctl(to_watch,IPC_STAT,&shm_stat) == -1){
    fprintf(stddiag,"ERROR: shared memory segment no longer accessible\n");
    exit(1);      /* segment no longer accessible, quit with error */
  }

  flush_buffer:
  if(verbose){   /* monitor changes in status  */
    if(in != shm->in || out != shm->out || read_status != shm->read_status || write_status != shm->write_status || attach != shm_stat.shm_nattch){
      gettimeofday(&timetag,NULL);
      time1 = timetag.tv_sec;
      time1 *= 1000000;
      time1 += timetag.tv_usec;
      rtime = time1 - time0;
      rtime /= 1000000.;
      fprintf(stddiag,"%12.3f: size = %d, attach count = %d, creator=%d, last attach=%d ",rtime,
              (int)shm_stat.shm_segsz,(int)shm_stat.shm_nattch,shm_stat.shm_cpid,shm_stat.shm_lpid);
      fprintf(stddiag," first=%d, in=%d, out=%d, limit=%d, rs=%d, ws=%d\n",
              shm->first,shm->in,shm->out,shm->limit,shm->read_status,shm->write_status);
      fflush(stddiag);
    }
    in = shm->in;
    out = shm->out;
    read_status = shm->read_status;
    write_status = shm->write_status;
    attach = shm_stat.shm_nattch;
  } /* verbose */
  if(read_att==1 && writ_att==1 && shm->read_status==MGI_SHM_IDLE && shm->write_status==MGI_SHM_IDLE){
    if(shm->in == shm->out) break;    /* everybody opened, everything is closed, buffer is empty */
                                      /* dump what's left into file */
    fd=open(channel_filename_d,O_RDWR | O_CREAT, 0644);
    if(fd < 0){
      fprintf(stddiag,"ERROR: cannot open %s to save leftover data\n",channel_filename_d);
      error_status = 1;
      break;
    }
    if(shm->out < shm->in) {   /* write from out to in-1 */
      write(fd,shm->data+shm->out,sizeof(shm->data[0])*(shm->in-shm->out));
    }else{                     /* write from out to limit, then first to in-1 */
      write(fd,shm->data+shm->out,sizeof(shm->data[0])*(shm->limit-shm->out+1));
      write(fd,shm->data+shm->first,sizeof(shm->data[0])*(shm->in-shm->first));
    }
    close(fd);
    fprintf(stddiag,"INFO: leftover data saved to '%s'\n",channel_filename_d);
    break;
  }
}  /* while */
i = shmctl(to_watch,IPC_RMID,NULL);   /* mark for removal if not already done */
i = shmdt(shm);                       /* do not keep the memory area attached, we do not want to prevent release */
fprintf(stddiag,"INFO: nullifying channel %s\n",argv[2]);
//unlink(channel_filename);
FD=fopen(channel_filename,"w");
fprintf(FD,"-111111");
fclose(FD);
exit (error_status);
}
