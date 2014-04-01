/* 
Author: Michel Valin, UQAM, 2014/03/10

This program runs in the background and terminates when the parent process terminates.
It will also terminate if a specified number of "attaches" to the memory segment occurred.

Before it terminates, the program will mark the memory segment for destruction

It expects one or two input parameters:
Arg1: size of a shared memory segment to create and watch
Arg2: channel name for mgi

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
  
#define EXIT_SUCCESS 0  
#define EXIT_FAILURE 1  
  
/* unused as yet borrowed code */
int main(int argc, char **argv){
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
//char *shm;
mgi_shm_buf *shm;
char channel_filename[1024];
int fd;
int nints;
int max_attach;

if(argc < 2 || argc >3) {
  fprintf(stderr,"ERROR: there must be one or two arguments \n");
  fprintf(stderr,"usage: %s shm_size [mgi_channel_name]\n",argv[0]);
  fprintf(stderr,"    shm_size     : shared memory segment size in KBytes\n");
  exit(1);
}

to_create=atoi(argv[1]);
to_watch=shmget(IPC_PRIVATE,to_create*1024,0600);
if(to_watch == -1) {
  fprintf(stderr,"ERROR: shared memory segment creation failed\n");
  exit(1);
}
fprintf(stdout,"%d\n",to_watch);
fflush(stdout);
fclose(stdout);

shm = (mgi_shm_buf *) shmat(to_watch, NULL, 0);  /* attach segment just created */
if(shm == (void *) -1) {
  fprintf(stderr,"ERROR: memory segment %d cannot be attached\n",to_watch);
  exit(1);
}else{
  fprintf(stderr,"INFO: memory segment %d successfully attached\n",to_watch);
}
i = shmctl(to_watch,IPC_STAT,&shm_stat);
shm_size = shm_stat.shm_segsz / 1024;
shm_size_k = shm_size;
fprintf(stderr,"Monitoring shared memory id '%d' of size %d KBytes\n",to_watch,shm_size_k);

/* initialize memory segment for use by mgilib */
shm->read_status = -2;
shm->write_status = -2;
shm->first = 0;                     /* first position in buffer */
shm->in = 0;                        /* insertion position in buffer */
shm->out = 0;                       /* extraction position in buffer */
shm_size = shm_stat.shm_segsz;      /* size of memory area */
shm_size -= sizeof(mgi_shm_buf);    /* minus structure size */
shm_size /= sizeof(unsigned int);   /* convert to number of unsigned int */
shm->limit = shm_size;              /* last position in buffer */

snprintf(channel_filename,sizeof(channel_filename),"%s/.gossip/SHM/%s",getenv("HOME"),(argc>2) ? argv[2] : "default");
fprintf(stderr,"INFO: trying to open %s\n",channel_filename);
fd=open(channel_filename,O_RDONLY);
if(fd > 0) {                        /* channel restart file exists, read it into buffer */
  nints = read(fd,shm->data,shm->limit*sizeof(unsigned int)) / sizeof(unsigned int) ;
  shm->in = nints;
  fprintf(stderr,"INFO: read %d elements from '%s'\n",nints,channel_filename);
  if(nints == shm->limit){
    fprintf(stderr,"WARNING: restart file possibly larger than shared memory buffer\n");
  }
  close(fd);
}

sleep(1);
max_attach=1000000;  /* deactivate feature on non linux systems */
#if defined(linux)
i = shmctl(to_watch,IPC_RMID,NULL);   /* immediately mark segment for removal if linux */
max_attach = 2;
#endif
sleep(1);

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
while(1){
  if(kill(pp,0)) break;    /* original parent no longer exists, time to quit */
  usleep(sleep_duration);  /* 10 milliseconds */
  i = shmctl(to_watch,IPC_STAT,&shm_stat);
  if(i == -1) exit(0);    /* segment no longer accessible, quit */
  if(shm_stat.shm_nattch >= max_attach) {  /* segment attached by enough other processes */
    break;                      /* job done, exit */
  }
  }  /* while */
i = shmctl(to_watch,IPC_RMID,NULL);
exit (0);
}
