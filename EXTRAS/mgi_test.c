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
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <fcntl.h>
#include <cgossip.h>
#include <mgi.h>

void usage(char *pgm) {
  fprintf(stderr,"usage: %s shared_memory_id|channel_name R|W scenario_file\n",pgm);
}

int mgi_test(int argc, char **argv)
{
  int shmid;
  mgi_shm_buf *shm;
  char mode;
  FILE *scenario = NULL;
  char line[1024];
  char *ptr;
  char type;
  int i0, in, idelta, ibuf[1024], ibuf2[1024];
  float f0, fn, fdelta, fbuf[1024], fbuf2[1024];
  double d0, dn, ddelta, dbuf[1024], dbuf2[1024];
  int nc, ni, nf, nd;
  int i;
  char *mgi_mode;
  int delay=1;
  int errors;
  int status;
  int channel;
  char channel_filename[1024];
  FILE *FD;
  
  if(argc<4) {
    usage(argv[0]);
    exit(1);
  }
  shmid = atoi(argv[1]);
  if(shmid <= 0){   /* channel name ? */
    snprintf(channel_filename,sizeof(channel_filename),"%s/.gossip/SHM/%s.id",getenv("HOME"),argv[1]);
    fprintf(stderr,"INFO: opening %s\n",channel_filename);
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
  }
  fprintf(stderr,"INFO: shared memory id : %d\n",shmid);
  mode = *argv[2];   /*  R = reader mode, W = writer mode */
  if(mode != 'R' && mode != 'W') {
    fprintf(stderr," %s: invalid mode %c\n",argv[0],*argv[2]);
    usage(argv[0]);
    exit(1);
  }
  if(mode == 'R') mgi_mode = "expecting";
  else  mgi_mode = "sending";

  scenario=fopen(argv[3],"r");
  if(scenario == NULL) {
    fprintf(stderr," %s: cannot open file %s\n",argv[0],argv[3]);
    exit (1);
  }
  shm = shmat(shmid, NULL, 0);
  if(shm == (void *) -1) {
    fprintf(stderr," %s: cannot attach shared memory area %d\n",argv[0],shmid);
    exit (1);
  }
  if(mode == 'W') {
    shm->write_status = -2;    /* reinitialize to inactive state */
  }
  if(mode == 'R') {
    shm->read_status = -2;    /* reinitialize to inactive state */
  }
  channel = C_mgi_init("test");
  fprintf(stderr,"channel %d successfully initialized\n",channel);
  if(mode == 'W') {
    C_mgi_open (channel, 'W');
    shm->write_status = 1;
  }
  if(mode == 'R') {
    C_mgi_open (channel, 'R');
    shm->read_status = 1;
  }
    while(fgets(line,sizeof(line)-1,scenario) != NULL) {  /* read scenario file */
    if(mode == 'W') sleep(delay);    /* make sure that reader will have to wait */
    ptr = &line[0];
    while(*ptr == '\'') ptr++;
    type = *ptr++;
    while(*ptr == '\'') ptr++;
    switch(type){
      case 'I':
        ni = sscanf(ptr,"%d %d %d",&i0,&in,&idelta);
        if(ni == 3){
          ni = 1 + (in-i0)/idelta ;
          fprintf(stderr,"%s %d integer values from %d to %d by %d\n",mgi_mode,ni,i0,in,idelta);
          for(i=0;i<ni;i++) { ibuf[i] = i0 + i * idelta ; fprintf(stderr," %d",ibuf[i]); }
          fprintf(stderr,"\n");
        }
        if(mode == 'R'){
//          fprintf(stderr,"found %d I words in buffer\n",shm->in-shm->out);
          status = ShmReadBuf(shm,ibuf2,ni,'I',ni,10);
          fprintf(stderr,"Read status = %d\n",status);
          if(status == READ_TIMEOUT) {
            fprintf(stderr,"ERROR: Read timeout\n");
            exit(1);
          }
          errors = 0;
          for(i=0;i<ni;i++) { if(ibuf[i] != ibuf2[i]) { fprintf(stderr,"I=%d, expected %d, got %d\n",i,ibuf[i],ibuf2[i]); errors++; } }
          fprintf(stderr,"INFO: errors=%d\n",errors);
        }else{
          status = ShmWriteBuf(shm,ibuf,ni,'I',10);
          fprintf(stderr,"Write status = %d\n",status);
        }
        break;
      case 'R':
        nf = sscanf(ptr,"%f %f %f",&f0,&fn,&fdelta);
        if(nf == 3){
          nf = 1 + (fn-f0)/fdelta ;
          fprintf(stderr,"%s %d real values from %f to %f by %f\n",mgi_mode,nf,f0,fn,fdelta);
          for(i=0;i<nf;i++) { fbuf[i] = f0 + i * fdelta ; fprintf(stderr," %f",fbuf[i]); }
          fprintf(stderr,"\n");
          if(mode == 'R'){
//            fprintf(stderr,"found %d R words in buffer\n",shm->in-shm->out);
            status = ShmReadBuf(shm,fbuf2,nf,'R',nf,10);
            fprintf(stderr,"Read status = %d\n",status);
            errors = 0;
            for(i=0;i<nf;i++) { if(fbuf[i] != fbuf2[i]) { fprintf(stderr,"I=%d, expected %f, got %f\n",i,fbuf[i],fbuf2[i]); errors++; } }
            fprintf(stderr,"INFO: errors=%d\n",errors);
          }else{
            status = ShmWriteBuf(shm,fbuf,nf,'R',10);
            fprintf(stderr,"Write status = %d\n",status);
          }
        }
        break;
      case 'D':
        nd = sscanf(ptr,"%lf %lf %lf",&d0,&dn,&ddelta);
        if(nd == 3){
          nd = 1 + (dn-d0)/ddelta ;
          fprintf(stderr,"%s %d double values from %lf to %lf by %lf\n",mgi_mode,nd,d0,dn,ddelta);
          for(i=0;i<nd;i++) { dbuf[i] = d0 + i * ddelta ; fprintf(stderr," %lf",dbuf[i]); }
          fprintf(stderr,"\n");
          if(mode == 'R'){
//            fprintf(stderr,"found %d D words in buffer\n",(shm->in-shm->out)/2);
            status = ShmReadBuf(shm,dbuf2,nd,'D',nd,10);
            fprintf(stderr,"Read status = %d\n",status);
            errors = 0;
            for(i=0;i<nf;i++) { if(dbuf[i] != dbuf2[i]) { fprintf(stderr,"I=%d, expected %f, got %f\n",i,dbuf[i],dbuf2[i]); errors++; } }
            fprintf(stderr,"INFO: errors=%d\n",errors);
          }else{
            status = ShmWriteBuf(shm,dbuf,nd,'D',10);
            fprintf(stderr,"Write status = %d\n",status);
          }
        }
        break;
      case 'C':
        break;
    }
  }
  fprintf(stderr,"preparing to close shared memory channel %d \n",channel);
  C_mgi_clos(channel);
  fprintf(stderr,"preparing to terminate all shared memory channels %d \n",channel);
  C_mgi_term();
  return(0);
}
