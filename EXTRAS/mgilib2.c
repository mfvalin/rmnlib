/* RMNLIB - Library of useful routines for C and FORTRAN programming
 * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
 *                          Environnement Canada
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation,
 * version 2.1 of the License.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

/* MGILIB.C
   
   Functions written for programs that run in "parallel" and need 
   to exchange information simultaneously. MGI stands for Model Gossip
   Interface. Each program using these functions will have to compile 
   with this library. There are 6 functions:

   MGI_INIT
   to initialize a channel using the name given and open the socket.
   It will also allocate a writing buffer dynamically and if all is
   successful, it will return a channel number (socket descriptor).

   MGI_OPEN
   to open the channel in a certain mode:
   'R' for read: returns 0 to signal that data been written.
                 or returns nblks to be read
   'W' for write: returns 1 if open is ok.
   'S' for storing a restart file:returns 1 if open is ok.

   MGI_READ
   to read from a channel that is open for READ mode.
   It accepts the following type of data:
   'C' for CHARACTER
   'I' for INTEGER
   'R' for REAL
   'D' for REAL*8
   It returns the number of blocks left to read from channel.

   MGI_WRITE
   to write to a channel that is open for WRITE mode.
   It accepts the same type of data as MGI_READ.
   It returns the number of blocks written to channel.

   MGI_CLOS
   to close the mode of a channel and check to make sure all is
   transmitted as requested. It returns the status of the data
   file after it is closed.

   MGI_TERM
   to delete the PID file that was created in the beginning and 
   to release all the memory allocated dynamically. It closes all
   the filepipes therefore, breaking all the pipe connections with
   the other programs.

   ***NOTE: These functions are written to keep enough bits for the
   equivalent of an integer/float in C or integer/real*4 in FORTRAN.
   In other words, you will lose some precision with the 64-bit
   compilation unless real*8 is used.

*/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <rpnmacros.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/time.h> 
#include <string.h>
#include <ctype.h>
#include "mgi.h"
#include <gossip.h>

/* error codes header file */
#include "cgossip.h"

#define  CLOSE      -5
#define  TIMEOUT    -5

static channel chn[MAX_CHANNELS];

static int ichan = 0;
static int init = 0;
static int SIG_ACTIVE = 1;
#ifdef NOT_USED
static char PID_file[MAX_STR];
static char *mgidir;
#endif
static int *intBuffer;

#ifdef NOT_USED
static void getmgidir ();
static int makepidfile ();
static void removepidfile ();
static int validchan (int chan);
static void strcopy (char *s, char *t, int charlen);
#endif
static int bwrite (int chan, void *buffer, int nelem, char *dtype);
ftnword f77name (mgi_init) (char *channel_name, F2Cl lname);
ftnword f77name (mgi_open) (ftnword *f_chan, char *mode, F2Cl lmode);
ftnword f77name (mgi_read) (ftnword *f_chan, void *data, ftnword *f_nelm, char *dtype, F2Cl ltype);
ftnword f77name (mgi_write) (ftnword *f_chan, void *data, ftnword *f_nelm, char *dtype, F2Cl ltype);
ftnword f77name (mgi_clos) (ftnword *f_chan);
ftnword f77name (mgi_term) ();
void f77name (mgi_set_timeout) (ftnword *chan, ftnword *timeout);

extern int connect_to_subchannel_by_name (char *channel, char *subchannel, char *mode);
extern int GET_ack_nack (int socket, char *message);
extern int write_record (int fclient, void *buf, int longueur, int tokensize);
extern void *read_record (int fclient, void *buf, int *longueur, int maxlongueur, int tokensize);
extern char *get_gossip_dir (int display);

extern void init_client_table ();
extern void set_client_timeout (int fclient, int timeout);
extern int get_client_timeout (int fclient);
extern int close_channel (int fclient, char *channel);

ftnword f77name (mgi_read_oob) ();
ftnword f77name (mgi_write_oob) ();

/* --------------------------------------------------------------------------- */
/* #define DEBUG */

/*********************************************************************************************/

void f77name (mgi_nosig) ()
     /* to disable the signals between filepipes */
{
  /* SIG_ACTIVE = 0; */
  fprintf(stderr,"MGI_NOSIG: deprecated call\n");
}
#ifdef NOTUSED
/* to copy a string given by a fortran routine, 
   the space character is ignored, it is taken 
   here as the end of the string */
static void strcopy_( char *s1, char *s2, int lengths1, int lengths2 )    
{
  int i = 0;

  while ( (*s1++ = *s2++) != ' ' && i++ < lengths2 );
  
}
#endif


int check_ends(unsigned char *s1,unsigned char *s2, int s1length, int s2length, int i )
{
  if(*s2 == ' ' )
  {
    return (*(s1 - 1) - *(s2 - 1));
  }
  else
  {
    if(i == s2length)
      return (*(s1 - 1) - *(s2 - 1));
    else
      return -1;
  }
}
#ifdef NOTUSED
/* to compare two strings given by a fortran routine, considering 
   the space character as the end of the string*/
static int f_strcmp( unsigned char *s1, unsigned char *s2, int s1length, int s2length )
{
  int i = 0;	 
  int length;
  
  if(s1length <= s2length)
    length = s1length;
  else
    length = s2length;

  while( i < length && *s1 != ' ' && *s2 != ' ' && *s1 == *s2 && *s1 != '\0' && *s2 != '\0')
    {
      s1++;
      s2++;
      i++;
    }

  
  if(*s1 == ' ')
    {
      fprintf(stderr, "mgilib2::f_strcmp(), before return if(*s1 == ' '), s1 => %s\n ", s1);
      return check_ends(s1, s2, s1length, s2length, i);
     
    }
  else if(*s2 == ' ')
    {
      fprintf(stderr, "mgilib2::f_strcmp(), before return if(*s2 == ' '), s2 => %s\n ", s2);
      return 2 + check_ends(s2, s1, s2length, s1length, i);
    }

  return (*s1 - *s2);

}
#endif
/***********************************************************************************************/


static void strcopy( char *s, char *t, int charlen )
     /* to copy a string given by a fortran routine and place the NULL 
	character at the end of the true (charlen) length of the string */
{
  int i;
  
  i = 0;
  while ( (*s++ = *t++) != ' ' && i++ < charlen);
  if (*s-- == ' ') *s = '\0';
  else *s++ = '\0';
}

#ifdef NOT_USED
static int validchan( int chan )
     /* to validate the channel number; it must be greater than
	zero and less than or equal to ICHAN*/
{
  if ( chn[chan].buffer == NULL )
    return (-1);
  return(0);
}

static void getmgidir()
     /* to get the value of the environment variable "MGI_DIR" */
{
  if ( (mgidir = getenv("MGI_DIR")) == NULL)
    {
      fprintf(stderr,"Environment variable \"MGI_DIR\" undefined --\n");
      /* exit(1); */
    }
}

static int makepidfile()
     /* to make the PID file */
{
  char stuff[MAX_STR];
  sprintf( PID_file, "%s/%d", mgidir, getpid() );
  sprintf( stuff, "%s/%s", mgidir, "PROCS" );
  fprintf(stderr, "linking :%s: to :%s:\n", PID_file, stuff );
  return ( link ( stuff, PID_file ) );
}

static void removepidfile()
     /* to remove the PID file */
{
  fprintf(stderr, "removing %s\n", PID_file );
  unlink( PID_file );
}
#endif

static int bwrite ( int chan, void *buffer, int nelem, char *dtype )
     /* To fill the write buffer of initialized channel "chan" on the server */
{
  int nb, ier;

  fd_set wfds, rfds;
  struct timeval tv;

  FD_ZERO(&wfds);
  FD_SET(chn[chan].gchannel, &wfds);
  
  tv.tv_sec = get_stream_timeout(chn[chan].gchannel);
  tv.tv_usec = 0;
    
  if (select(chn[chan].gchannel + 1, NULL, &wfds, NULL, &tv))
    {
      ier = send_command_to_server(chn[chan].gchannel, "WRITE");
      
    }
  else
    {
      /* return ier = signal_timeout(chn[chan].gchannel); */
      return ier = WRITE_TIMEOUT;
    }
  
 
  if(ier < 0 )
    {
      fprintf(stderr,"bwrite, unable to send write command\n");
      /* return -1; */
      return WRITE_ERROR;
    }

#ifdef DEBUG
  fprintf(stderr,"mgilib2::bwrite(), ==\n");
#endif
  nb = 0 ;
  if(*dtype == 'I' || *dtype == 'R')
    {
      nb = write_record(chn[chan].gchannel, (unsigned char *)buffer, nelem, sizeof(int));
    }

  else if(*dtype == 'D')
    {
      nb = write_record(chn[chan].gchannel, (char *)buffer, nelem, sizeof(double));
    }
  
  else if(*dtype == 'C')
    {
      nb = write_record(chn[chan].gchannel, buffer, nelem, 1);
    }
  
  /* get_ack_nack(chn[chan].gchannel); */
    
  FD_ZERO(&rfds);
  FD_SET(chn[chan].gchannel, &rfds);
  
  tv.tv_sec = get_stream_timeout(chn[chan].gchannel);
  tv.tv_usec = 0;
  
  if (select(chn[chan].gchannel + 1, &rfds, NULL, NULL, &tv))
    {
      get_ack_nack(chn[chan].gchannel);
      
    }
  else
    {
      fprintf(stderr, "bwrite, timeout = %d, get_ack_nack(), else\n", (int) tv.tv_sec);
      /* return ier = signal_timeout(chn[chan].gchannel); */
      return ier = WRITE_TIMEOUT;
    }


  return nb;
}

ftnword f77name (mgi_clos) (ftnword *f_chan)
     /* close a channel and signal that it can be opened in another mode */
{
  int ier = 0, chan;
  char buf[1024];
  chan = (int) *f_chan;

  if(chn[chan].gchannel != 0)
    {
      snprintf(buf, 1023, "%s %s", "END", chn[chan].name);
      ier = send_command(buf);
      fprintf(stderr,"MGI_CLOS: subchannel \"%s\" is closed \n", chn[chan].name);
    }
  
   if(chn[chan].buffer)
    {
      free(chn[chan].buffer);
      chn[chan].buffer = NULL;
    }  
  return ier;
  
}

ftnword f77name (mgi_term) ()
{
  /* close all channels */
  int chan, ier = -1;

  /* if memory channel, the last one to "term" a channel dumps contents into appropriate file */
  /* chn[chan].mode == 'W' / 'R' in this case */
  /* $HOME/.gossip/SHM/chn[chan].name is the appropriate file */

  for (chan = 0; chan <= ichan; chan++)
    {
      if(chn[chan].name && strcmp((char *)chn[chan].name, "") && chn[chan].gchannel > 0)
	{
	  ier = send_command("END");
	  fprintf(stderr,"MGI_TERM: subchannel \"%s\" has been closed!\n", chn[chan].name);
	  
	  if(chn[chan].buffer)
	    {
	      free(chn[chan].buffer);
	      chn[chan].buffer = NULL;
	    }
	}
    }

  return ier;
}

ftnword f77name (mgi_init) (char *channel_name, int lname)
     /* To initialize a channel given a channel_name.
	It will return a number to represent this channel (1 to MAX_CHANNELS-1 */
{
  int chan;
  char env_var_name[1024];
  char *env_var_value;

  if (init == 0)
    {
      init = 1;
    }

#ifdef DEBUG
  fprintf(stderr,"MGI_INIT ** \n"); 
#endif
  ichan++;
  if (ichan >= MAX_CHANNELS)
    {
      fprintf(stderr,"MGI_INIT: ERROR, Too many channels assigned; MAX = %d\n", MAX_CHANNELS);
      ichan--;
      /* return -1; */
      return INIT_ERROR;
    }
  else
    {
      chan = ichan;
      if (lname < MAX_NAME)
      {
	strcopy(chn[chan].name, channel_name, lname);
      }
      else 
      {
        fprintf(stderr,"MGI_INIT: ERROR, Length of channel name > %d chars.\n",MAX_NAME-1);
        return INIT_ERROR;   /* return -1; */
      }
      chn[chan].fd_data = -1;
      if (SIG_ACTIVE)
	{
	  fprintf(stderr,"MGI_INIT: Opening channel: \"%s\" \n", chn[chan].name);
	}
    
      /* initialize channel */
      chn[chan].msgno_W = 0;
      chn[chan].msgno_R = 0;
      chn[chan].nblks = 0;
      chn[chan].mode = ' ';
      chn[chan].pos = 0;
      chn[chan].gchannel = 0;
      chn[chan].shmid = -1;   /* set shared memory id to unused */
      chn[chan].shmbuf = NULL;

      snprintf(env_var_name,sizeof(env_var_name),"SHM_%s",chn[chan].name);
      env_var_name[sizeof(env_var_name)-1]='\0';
      env_var_value = getenv(env_var_name);
      if(env_var_value != NULL) {                /* it is a shared memory channel */
        chn[chan].shmid = atoi(env_var_value);   /* get shared memory segment ID */
        chn[chan].shmbuf = shmat(chn[chan].shmid, NULL, 0);
        if(chn[chan].shmbuf == (void *) -1) chn[chan].shmbuf = NULL;  /* cannot attach shared memory segment */
        /* chn[chan].shmbuf->limit must not be 0 if memory segment was properly initialized at creation  */
      }

    if ((intBuffer = (int *) malloc(BUFSIZE * sizeof(int))) == NULL)
      {
	fprintf(stderr,"MGI_INIT: ERROR on channel %s: Cannot allocate memory for intBuffer\n",
	       chn[chan].name);
	/* return -1; */
	return INIT_ERROR;
      }

    chn[chan].buffer = intBuffer;
    }

  return(chan);
}

ftnword f77name (mgi_open) (ftnword *f_chan, char *mode, int lmode)
     /* to open a channel in mode "mode"; where mode can be:
	'R' for reading
	'W' for writing
	'S' for storing
     */
{
  int chan;
  chan = (int) *f_chan;
  mgi_shm_buf *shm;

  if (*mode == 'W') 
    {
      if(chn[chan].shmbuf != NULL) {   /* not a shared memory channel */
        chn[chan].gchannel = connect_to_subchannel_by_name( get_gossip_dir(0), chn[chan].name, "write" );

        if( chn[chan].gchannel < 0 )
          chn[chan].gchannel = retry_connect( chan );
      } else {   /* it is  a shared memory channel, initialize it for write  */
        shm = chn[chan].shmbuf;
        /* add error code if already connected for write or not ready for write */
        /* id(shm->write_status != -1 ) ... */
        shm->write_status = 1;  /* connected for write */
        chn[chan].gchannel = 123456 ;
        chn[chan].mode = 'W';
      }

    }
  else if (*mode == 'R') 
    {
      if(chn[chan].shmbuf != NULL) {   /* not a shared memory channel */
        chn[chan].gchannel = connect_to_subchannel_by_name( get_gossip_dir(0), chn[chan].name, "read" );

        if( chn[chan].gchannel < 0 )
          chn[chan].gchannel = retry_connect( chan );
      } else {   /* it is  a shared memory channel, initialize it for read */
        shm = chn[chan].shmbuf;
        /* add error code if already connected for read or not ready for read */
        /* id(shm->read_status != -1 ) ... */
        shm->read_status = 1;  /* connected for read */
        chn[chan].gchannel = 123456 ;
        chn[chan].mode = 'R';
      }
    }
  else if (*mode == 'S') 
    { /* store mode (for restart files) */
      chn[chan].mode = 'S';
      chn[chan].nblks = 0;
      chn[chan].msgno_W++;
      chn[chan].pos = 0;
    }

  if(chn[chan].gchannel < 0)
    {
      fprintf(stderr, "MGI_OPEN, Connection Failed, the Server may be down !!\n" );
      /* exit(-1); */
      return CONNECTION_ERROR; 
    }

  /* initialize timeout table */
  init_client_table( chn[chan].gchannel );

  return chan;
}


/* if connection to server fails          */
/* default:  retry 10 times after a sleep */
/* else use user value                    */

int USER_TRY_CONNECT = 10;
void f77name (mgi_set_retry_connect) (ftnword *try_nbr)
{
  printf( "MGI_OPEN, setting try to connect USER_TRY_CONNECT: \"%d\" times\n", (int) *try_nbr );  
  if((int) *try_nbr > 0 && (int) *try_nbr < 10)
    USER_TRY_CONNECT = (int) *try_nbr;
}

int mgi_get_retry_connect(int chan)
{
  
  return USER_TRY_CONNECT;

}

/* if connection to server fails          */
/* default: retry 10 times after a sleep  */
/* interval of 10 secs                    */
int retry_connect( int chan )
{
  int PING_INTERVAL = 10;
  int ping_ord0 = mgi_get_retry_connect(chan);
  int ping_ord =  mgi_get_retry_connect(chan);

  while( chn[chan].gchannel < 0 && ping_ord > 0 )
    {
      sleep( PING_INTERVAL );
      fprintf(stderr, "MGI_OPEN, Connection to Server Failed,  retry to connect: \"%d/%d\" \n", ping_ord0 - ping_ord + 1, ping_ord0 );
	  chn[chan].gchannel = connect_to_subchannel_by_name( get_gossip_dir(0), chn[chan].name, "write" );
	  ping_ord--;
    }
  return chn[chan].gchannel; 
  
}

/* write into shared memory buffer, character version */
static int shm_write_c(mgi_shm_buf *shm,void *buf,int nelem){
  int in, out, limit, inplus;
  int ntok=nelem;
  unsigned char *str = (unsigned char *) buf;
  unsigned int token;
  
  in = shm->in ; out = shm->out ; limit = shm->limit;
  while(ntok > 0){
    inplus = (in+1 > limit) ? 0 : in+1 ;
    while(inplus == out) {       /* shared memory circular buffer is full */
      shm->in = in;              /* update in pointer in shared memory */
      usleep(1000);              /* sleep for 1 millisecond */
      out = shm->out;
    }
    token = *str++ ;
    token <<= 8 ; if(ntok >  2) token |= *str++ ;
    token <<= 8 ; if(ntok >  1) token |= *str++ ;
    token <<= 8 ; if(ntok >  0) token |= *str++ ;
    ntok = ntok -4;
    shm->data[in] = token;
    in = inplus;
  }
  shm->in = in;  /* update in pointer in shared memory */
  return(ntok);
}

/* write into shared memory buffer, integer/real/double/character version */
static int shm_write(mgi_shm_buf *shm,void *buf,int nelem,int type){
  int in, out, limit, inplus;
  int ntok;
  unsigned int *buffer = (unsigned int *) buf ;
  
  if(shm->write_status != 1) return(WRITE_ERROR) ; /* not properly setup to write */
    
  if(type == 'C') return ( shm_write_c(shm,buf,nelem) );                    /* characters */
  if(type != 'I' && type != 'R' && type != 'D') return(WRITE_ERROR) ;       /* unsupported type */
  ntok = nelem;
  if(type == 'D') ntok = nelem*2 ;                 /* 8 byte tokens */

  in = shm->in ; out = shm->out ; limit = shm->limit;
  while(ntok > 0){
    inplus = (in+1 > limit) ? 0 : in+1 ;
    while(inplus == out) {       /* shared memory circular buffer is full */
      shm->in = in;              /* update in pointer in shared memory */
      usleep(1000);              /* sleep for 1 millisecond */
      out = shm->out;
    }
    shm->data[in] = *buffer;
    in = inplus;
    ntok--;
    buffer++;
  }
  shm->in = in;  /* update in pointer in shared memory */
  return(ntok);
}


/* read from shared memory buffer, character version */
static int shm_read_c(mgi_shm_buf *shm,void *buf,int nelem, int len){
  int in, out, limit;
  int ntok=nelem;
  unsigned char *str = (unsigned char *) buf;
  unsigned int token;
  int pad = len - nelem;
  
  in = shm->in ; out = shm->out ; limit = shm->limit;
  while(ntok > 0){
    while(in == out) {           /* shared memory circular buffer is empty */
      shm->out = out;            /* update out pointer in shared memory */
      usleep(1000);              /* sleep for 1 millisecond */
      in = shm->in;              /* update in pointer from shared memory */
    }
    token = shm->data[out] ;
    if(ntok >  3) {*str++ = (token>>24)&0xFF ; *str++ = (token>>16)&0xFF ; *str++ = (token>>8)&0xFF ; *str++ = token&0xFF ; } ;
    if(ntok == 3) {*str++ = (token>>24)&0xFF ; *str++ = (token>>16)&0xFF ; *str++ = (token>>8)&0xFF ; };
    if(ntok == 2) {*str++ = (token>>24)&0xFF ; *str++ = (token>>16)&0xFF ; };
    if(ntok == 1) {*str++ = (token>>24)&0xFF ; };
    ntok = ntok -4;
    out = (out+1 > limit) ? 0 : out+1;   /* bump out */
  }
  shm->out = out;  /* update out pointer in shared memory */
  while(pad-- > 0) *str++ = ' ';
  return(ntok);
}

/* read from shared memory buffer, integer/real/double/character version */
static int shm_read(mgi_shm_buf *shm,void *buf,int nelem,int type, int len){
  int in, out, limit;
  int ntok;
  unsigned int *buffer = (unsigned int *) buf ;
  
  if(shm->read_status != 1) return(READ_ERROR) ; /* not properly setup to read */
    
  if(type == 'C') return ( shm_read_c(shm,buf,nelem,len) );                    /* characters */
  if(type != 'I' && type != 'R' && type != 'D') return(READ_ERROR) ;       /* unsupported type */
  ntok = nelem;
  if(type == 'D') ntok = nelem*2 ;                 /* 8 byte tokens */

  in = shm->in ; out = shm->out ; limit = shm->limit;
  while(ntok > 0){
    while(in == out) {           /* shared memory circular buffer is empty */
      shm->out = out;            /* update out pointer in shared memory */
      usleep(1000);              /* sleep for 1 millisecond */
      in = shm->in;              /* update in pointer from shared memory */
    }
    *buffer = shm->data[out] ;
    out = (out+1 > limit) ? 0 : out+1;   /* bump out */
    ntok--;
    buffer++;
  }
  shm->out = out;  /* update out pointer in shared memory */
  return(ntok);
}

ftnword f77name (mgi_write) (ftnword *f_chan, void *buffer, ftnword *f_nelem, char *dtype, F2Cl ltype)
     /* to write elements from "buffer" into the specified channel
	opened for WRITEMODE. It actually writes
	
	The following data types (dtype) are accepted:
	'C': character
	'I': integer
	'R': real
	'D': real*8 ; note that only the precision of a real would be kept
     */
{
  int nb, chan, nelem;
  int lnblnk_();

  chan = (int) *f_chan;
  nelem = (int) *f_nelem;
  char *tmpstr;
  
  if( nelem <= 0 )
    {
      fprintf(stderr,"MGI_WRITE, Error, cannot write data with length = %d\n", nelem);
      
      return WRITE_ERROR;
    }

  if( chn[chan].gchannel < 0 )
    {
      fprintf(stderr,"MGI_WRITE, Error, cannot connect to server using descriptor: \"%d\"!!!\n", chn[chan].gchannel);
      
      return WRITE_ERROR;
    }

  /* if shared memory channel (chn[chan].shmbuf != NULL) intercept here */
  /* call mgi_shm_write(chn[chan].shmbuf,buffer,nelem                    ,*dtype) (*dtype != 'C') */
  /* call mgi_shm_write(chn[chan].shmbuf,buffer,(nelem<ltype)?nelem:ltype,*dtype) (*dtype == 'C')  */

  if ( *dtype == 'C' )
    {
      nelem = ( *f_nelem < ltype ) ? (int) *f_nelem:ltype;

      tmpstr = (char *)malloc(nelem + 1);

#ifdef DEBUG
      fprintf(stderr,"MGI_WRITE: data type = %c, elts Nbr = %d, strlen = %d,  subchannel = %s\n", dtype[0], nelem, ltype, chn[chan].name);
#endif

      strncpy( tmpstr, (char *)buffer, nelem);
      tmpstr[nelem] = '\0';

      if ((nb = bwrite(chan, (unsigned char *)tmpstr, nelem, dtype)) > 0)
	{
	  fprintf(stderr,"MGI_WRITE: ERROR on %s: %d bytes written (character) \n", chn[chan].name, nb);
	  free( tmpstr );
	  /* return number of bytes not sent */
	  return WRITE_ERROR;
	}
      free( tmpstr );

    }


  else if (*dtype == 'I' || *dtype == 'R' || *dtype == 'D' ) 
    {
      chn[chan].nblks++;
      
#ifdef DEBUG
      fprintf(stderr,"MGI_WRITE: data type = %c, elts Nbr = %d, subchannel = %s\n", dtype[0], nelem, chn[chan].name);
      fprintf(stderr,"MGI_WRITE: data type = %s\n", dtype);
      fprintf(stderr,"MGI_WRITE: elts Nbr = %d\n", nelem);
#endif

      if ((nb = bwrite(chan, (unsigned char *)buffer, nelem, dtype)) > 0)
	{
	  fprintf(stderr,"MGI_WRITE: ERROR on %s: %d bytes written\n", chn[chan].name, nb);
	  /* return number of bytes not sent */
	  return WRITE_ERROR;
	}
     
    }

  else 
    {
      fprintf(stderr,"MGI_WRITE: ERROR on channel %s: Unknown data type: %c\n", chn[chan].name, *dtype);
      /* return -1; */
      return WRITE_TYPE_ERROR;
    }


  if(nb == TIMEOUT)
    {
      if(get_timeout_signal(chn[chan].gchannel))
	{
	  if (*dtype == 'C')
	    fprintf(stderr, "MGI_WRITE: TIMEOUT for write \"%d of Character data\" \n", nelem);

	  else if(*dtype == 'I')
	    fprintf(stderr, "MGI_WRITE: TIMEOUT for write \"%d of Integer data\", nb = %d \n", nelem, nb);

	  else if(*dtype == 'R')
	    fprintf(stderr, "MGI_WRITE: TIMEOUT for write \"%d of Real data\" \n", nelem);

	  else if(*dtype == 'D')
	    fprintf(stderr, "MGI_WRITE: TIMEOUT for write \"%d of Double data\" \n", nelem);

	  return signal_timeout(chn[chan].gchannel);
	}
    }

  return nb;
}

void f77name (mgi_set_timeout) (ftnword *chan, ftnword *timeout)
{
  set_client_timeout(chn[(int) *chan].gchannel, (int) *timeout);

}

ftnword f77name (mgi_read) (ftnword *f_chan, void *buffer, ftnword *f_nelem, char *dtype, F2Cl ltype)

     /* to read elements directly from the data file related to the 
	specified channel into "buffer". The channel must be opened for 
	READMODE only.
	The following data types (dtype) are accepted:
	'C': character
	'I': integer (int)
	'R': real    (float)
	'D': real*8  (double)
     */
{
  int ier, chan, nelem;
  
  chan = (int) *f_chan;
  nelem = (int) *f_nelem;

  if(nelem <= 0)
    {
      fprintf(stderr,"MGI_READ, Error: cannot read data with length = %d\n", nelem);
      /* return -1; */
      return DATA_LENGTH_ERROR;
    }

  bzero(buffer, nelem);

  /* if shared memory channel (chn[chan].shmbuf != NULL), intercept here */
  /* call mgi_shm_read(chn[chan].shmbuf,buffer,nelem,*dtype,nelem) (*dtype != 'C' )*/
  /* call mgi_shm_read(chn[chan].shmbuf,buffer,nelem,*dtype,ltype) (*dtype == 'C') */

  ier = send_command_to_server(chn[chan].gchannel, "READ");

  if(ier < 0)
    {
      fprintf(stderr,"MGI_READ, Error: unable to send write command for channel: \"%s\"\n", chn[chan].name);
     
      return SEND_COMMAND_ERROR;
    }
  
  if (*dtype == 'I')
    { /* integer */
  
      fprintf(stderr, "MGI_READ: \"Integer\", elts Nbr = %d, channel = \"%s\"\n", nelem, chn[chan].name);
  
      buffer = (int *)read_record( chn[chan].gchannel, (int *)buffer, &nelem, nelem, sizeof(int) );

  
      if(buffer != NULL)
	{
	  get_ack_nack( chn[chan].gchannel );
	
	  return ier = nelem;
	}
      else
	{
	  if( get_timeout_signal(chn[chan].gchannel) )
	    {
	      fprintf(stderr, "MGI_READ: TIMEOUT for read \"Integer\" \n" );
	   
	      ier = READ_TIMEOUT;
	    }
	  else
	    {
	      fprintf( stderr, "MGI_READ: Problem read Integer\n" );
	   
	      return READ_ERROR;
	    }
	}

    }

  else if (*dtype == 'R')
    { /* float */
      
      fprintf(stderr, "MGI_READ: \"Real\", elts Nbr = %d, channel = \"%s\"\n", nelem, chn[chan].name);

      buffer = (float *)read_record(chn[chan].gchannel, (float *)buffer, &nelem, nelem, sizeof(int));

      
      if(buffer != NULL)
	{
	  get_ack_nack(chn[chan].gchannel);
	
	  return ier = nelem;

	}
      else
	{
	  
	  if( get_timeout_signal( chn[chan].gchannel ) )
	    {
	      fprintf(stderr, "MGI_READ:  TIMEOUT for read \"Real\" \n");

	      ier = READ_TIMEOUT;
	    }
	  else
	    {
	      fprintf( stderr, "MGI_READ: problem read Real data\n" );

	      return READ_ERROR;
	    }
	}
      
    }
  else if (*dtype == 'D')
    { /* double */

      fprintf(stderr, "MGI_READ: \"Double\", Element's Nbr = %d, channel = \"%s\"\n", nelem, chn[chan].name);

      buffer = (double *)read_record(chn[chan].gchannel, (double *)buffer, &nelem, nelem, sizeof(double));

      if(buffer != NULL)
	{
	  get_ack_nack(chn[chan].gchannel);
	
	  return ier = nelem;
	}
      else
	{
	  
	  if( get_timeout_signal( chn[chan].gchannel ) )
	    {
	      fprintf(stderr, "MGI_READ: TIMEOUT for read \"Double\"\n");

	      ier = READ_TIMEOUT;
	    }
	  else
	    {
	      fprintf( stderr, "MGI_READ: Problem read Double data\n" );

	      return READ_ERROR;
	    }
 	}
     }
  
  else if (*dtype == 'C')
    { /* character */
      int i;
      char *temp = (char *)buffer;

      for(i = 0; i < ltype ; i++ ) 
	{
	  temp[i] = ' ';
	}

     
      buffer = (char *)read_record(chn[chan].gchannel, (char *)buffer, &nelem, nelem, sizeof(char));

      for(i = nelem+1 ; i < ltype ; i++ ) 
	{
	  temp[i] = ' ';
	}
      
      if(buffer != NULL)
	{
	  get_ack_nack(chn[chan].gchannel);
	  
	  return ier = nelem;
	} 
      else
	{
	  
	  if( get_timeout_signal( chn[chan].gchannel ) )
	    {
	      fprintf(stderr, "MGI_READ: TIMEOUT for read \"Character\"\n");
	      
	      ier = READ_TIMEOUT;
	    }
	  else
	    {
	      fprintf( stderr, "MGI_READ: Problem read Character data\n" );
	      
	      return READ_ERROR;
	    }
	}

    }
  
  else
    {
      fprintf(stderr,"MGI_READ: ERROR on channel %s: Unknown data type: %c\n", chn[chan].name, *dtype);
      
      return READ_TYPE_ERROR;
    }
  
  if(ier == CLOSE)
    {
      close_channel(chn[chan].gchannel, chn[chan].name);
    }


  return ier;
}


