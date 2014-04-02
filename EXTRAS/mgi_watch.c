#include <stdio.h>
#include <stdlib.h>
#include <unistd.h> 
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <mgi.h>

main(int argc, char **argv)
{
  int shmid;
  mgi_shm_buf *shm;
  struct shmid_ds shm_stat;
  size_t shm_size;
  int nrep = 100;
  int i;

  shmid = atoi(argv[1]);
  shm = shmat(shmid, NULL, 0);
  if(shm == (void *) -1) exit (1);
  while(nrep-- > 0) {
    if(-1 == shmctl(shmid,IPC_STAT,&shm_stat)) exit (0);
    fprintf(stderr,"size = %d, attach count = %d, creator=%d, last attach=%d ",
            (int)shm_stat.shm_segsz,(int)shm_stat.shm_nattch,shm_stat.shm_cpid,shm_stat.shm_lpid);
    fprintf(stderr,"first=%d, in=%d, out=%d, limit=%d, rs=%d, ws=%d\n",
            shm->first,shm->in,shm->out,shm->limit,shm->read_status,shm->write_status);
    sleep(2);
  }
}
