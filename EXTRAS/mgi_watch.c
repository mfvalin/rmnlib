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

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <mgi.h>
#include <sys/time.h>

int mgi_watch(int argc, char **argv)
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
  char channel_filename[1024];
  char channel_filename_d[1024];
  FILE *FD=NULL;
  int fd=-1;
  int read_att=0;
  int writ_att=0;
  int monitor=1;

  if(argc<2) {
    fprintf(stderr,"usage: mgi_watch [-q] shared_memory_id|mgi_channel_name [delay]\n");
    exit(1);
  }
  if(strcmp("-q",argv[1])==0) {
    fprintf(stderr,"INFO: quiet mode active\n");
    monitor=0;
    argv++;
    argc--;
  }
  shmid = atoi(argv[1]);
  channel_filename[0] = '\0';   /* null filename */
  if(shmid == 0){   /* channel name ? */
    snprintf(channel_filename,sizeof(channel_filename),"%s/.gossip/SHM/%s.id",getenv("HOME"),argv[1]);
    FD=fopen(channel_filename,"r");
    if(FD==NULL){
      fprintf(stderr,"ERROR: cannot open %s\n",channel_filename);
      exit(1);
    }
    i=fscanf(FD,"%d",&shmid);
    fclose(FD);
    if(i<1 || shmid==0){
      fprintf(stderr,"ERROR: shared memory id unreadable for channel %s\n",channel_filename);
      exit(1);
    }
    snprintf(channel_filename_d,sizeof(channel_filename),"%s/.gossip/SHM/%s",getenv("HOME"),argv[1]);  /* prepare filename to dump channel data */
  }
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
    usleep(10000);      /* 10 millisec sleep */
    if(shm->read_status == MGI_SHM_ACTIVE) read_att = 1;
    if(shm->write_status == MGI_SHM_ACTIVE) writ_att = 1;
    if(read_att==1 && writ_att==1 && shm->read_status==MGI_SHM_IDLE && shm->write_status==MGI_SHM_IDLE){
      if(shm->in == shm->out) goto exit;  /* everybody opened, everything is closed, buffer is empty */
                                        /* dump what's left into file */
      fd=open(channel_filename_d,O_RDWR | O_CREAT, 0644);
      if(fd < 0){
        fprintf(stderr,"ERROR: cannot open %s to save leftover data\n",channel_filename);
        exit(1);
      }
      if(shm->out < shm->in) {   /* write from out to in-1 */
        write(fd,shm->data+shm->out,sizeof(shm->data[0])*(shm->in-shm->out));
      }else{                     /* write from out to limit, then first to in-1 */
        write(fd,shm->data+shm->out,sizeof(shm->data[0])*(shm->limit-shm->out+1));
        write(fd,shm->data+shm->first,sizeof(shm->data[0])*(shm->in-shm->first));
      }
      close(fd);
      fprintf(stderr,"INFO: leftover data saved to '%s'\n",channel_filename);
      goto exit;
    }
    if(-1 == shmctl(shmid,IPC_STAT,&shm_stat)) exit (0);  /* shared memory area has disappeared */
    /* TODO: add a time tag to get a good idea of traffic timing */
    if(in != shm->in || out != shm->out || read_status != shm->read_status || write_status != shm->write_status || attach != shm_stat.shm_nattch){
      gettimeofday(&timetag,NULL);
      time1 = timetag.tv_sec;
      time1 *= 1000000;
      time1 += timetag.tv_usec;
      rtime = time1 - time0;
      rtime /= 1000000.;
      if(monitor) {
        fprintf(stderr,"%12.3f: size = %d, attach count = %d, creator=%d, last attach=%d ",rtime,
                (int)shm_stat.shm_segsz,(int)shm_stat.shm_nattch,shm_stat.shm_cpid,shm_stat.shm_lpid);
        fprintf(stderr," first=%d, in=%d, out=%d, limit=%d, rs=%d, ws=%d\n",
                shm->first,shm->in,shm->out,shm->limit,shm->read_status,shm->write_status);
      }
    }
    in = shm->in;
    out = shm->out;
    read_status = shm->read_status;
    write_status = shm->write_status;
    attach = shm_stat.shm_nattch;
  }
exit:
  unlink(channel_filename);
  return(0);
}
