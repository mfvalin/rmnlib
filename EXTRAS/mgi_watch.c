#include <stdio.h>
#include <stdlib.h>
#include <unistd.h> 
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <mgi.h>
#include <sys/time.h>

main(int argc, char **argv)
{
  int shmid;
  mgi_shm_buf *shm;
  struct shmid_ds shm_stat;
  struct timeval timetag;
  size_t shm_size;
  int nrep = 100;
  int i;
  int delay=2;
  int attach,read_status,write_status;
  volatile int in,out;
  long long time0;
  long long time1;
  double rtime;

  if(argc<2) exit(1);
  shmid = atoi(argv[1]);
  if(argc>2) delay=atoi(argv[2]);

  shm = shmat(shmid, NULL, 0);
  if(shm == (void *) -1) exit (1);

  gettimeofday(&timetag,NULL);
  time0 = timetag.tv_sec;
  time0 *= 1000000;
  time0 += timetag.tv_usec;
  rtime = 0.0;

  if(-1 == shmctl(shmid,IPC_STAT,&shm_stat)) exit (0);
  fprintf(stderr,"%12.3f: size = %d, attach count = %d, creator=%d, last attach=%d ",rtime,
          (int)shm_stat.shm_segsz,(int)shm_stat.shm_nattch,shm_stat.shm_cpid,shm_stat.shm_lpid);
  fprintf(stderr,"first=%d, in=%d, out=%d, limit=%d, rs=%d, ws=%d\n",
          shm->first,shm->in,shm->out,shm->limit,shm->read_status,shm->write_status);
  in = shm->in;
  out = shm->out;
  read_status = shm->read_status;
  write_status = shm->write_status;
  attach = shm_stat.shm_nattch;

  while(attach >=1) {   /* loop until shared memory area released by all but me */
    usleep(1000);   /* 1 millisec sleep */
    if(-1 == shmctl(shmid,IPC_STAT,&shm_stat)) exit (0);  /* shared memory area has disappeared */
    /* TODO: add a time tag to get a good idea of traffic timing */
    if(in != shm->in || out != shm->out || read_status != shm->read_status || write_status != shm->write_status || attach != shm_stat.shm_nattch){
      gettimeofday(&timetag,NULL);
      time1 = timetag.tv_sec;
      time1 *= 1000000;
      time1 += timetag.tv_usec;
      rtime = time1 - time0;
      rtime /= 1000000.;
      fprintf(stderr,"%12.3f: size = %d, attach count = %d, creator=%d, last attach=%d ",rtime,
              (int)shm_stat.shm_segsz,(int)shm_stat.shm_nattch,shm_stat.shm_cpid,shm_stat.shm_lpid);
      fprintf(stderr," first=%d, in=%d, out=%d, limit=%d, rs=%d, ws=%d\n",
              shm->first,shm->in,shm->out,shm->limit,shm->read_status,shm->write_status);
    }
    in = shm->in;
    out = shm->out;
    read_status = shm->read_status;
    write_status = shm->write_status;
    attach = shm_stat.shm_nattch;
  }
}
