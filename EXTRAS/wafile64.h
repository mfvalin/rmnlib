#define MAX_NAME     256
#define MAXWAFILES  1024
#define MAXPAGES      10


#define new_age_rd(age) (age+256)
#define new_age_wr(age) (age+512)
#define decay(age)      (age - (age >> 2))
#define LLSK long long
#define LSEEK lseek64
#define WSEEK(fdesc,offst,posi)\
 {\
  LLSK local_off;\
  local_off = offst;\
  LSEEK(fdesc,local_off * sizeof(int),posi);\
 }

#define CMCARC_SIGN "CMCARCHS"  /* signature du debut d'un fichier cmcarc */
#define CMCARC_SIGN_V5 "CMCARCH5"  /* signature du debut d'un fichier cmcarc version 5 */

typedef struct{
  int l;      /* last logical page when this entry was created */
  int p;      /* physical page this page is remapped into */
} REMAP_ENTRY;
/* new structure name since 64 bit WA/WAP file addressing was implemented */
/* as this gets written to disk, this is why 64 bit tokens are split into 2 parts */
/* to make big<->little endian conversion easier */
/* [0] is the MOST significant part, [1] is the LEAST significant part */
typedef struct {
   unsigned char signature[4];
   unsigned int p0[2];         /* offset to partition 0 for WAP files (split into 2 32 bit tokens) */
   unsigned int p1[2];         /* offset to partition 1 for WAP files (split into 2 32 bit tokens) */
   int last_lp;                /* last logical page */
   int last_pp;                /* last physical page */
   int pgsz;                   /* page size  */
   int npg;                    /* number of active entries in remap table */
   REMAP_ENTRY map[128];       /* remapping table */
} WAPINFO;

typedef struct {
   INT_32 *page_adr;
   int wa0;
   int walast;
   int access_count;
   int last_access;
   int touch_flag;
   int not_used_pad_for_word_alignment;  /* in case pointers are 64 bit wide */
   } PAGEINFO;

typedef struct {
   long long last_addr;        /* last logical address for partition */
   long long offset;           /* offset to partition for WAP files, unused(zero) for WA */
   } PARTINFO;                 /* partition info */

typedef struct {
   long long offset;           /* offset to "official" beginning of WA/WAP file */
   PAGEINFO *page;
   PARTINFO *part;             /* if file is WAP, this will point to the partition table */
   WAPINFO *wap;               /* if file is WAP, this will point to its control table */
   int *core;                  /* address of file if "in core". WAP partition 0 is often "in core" */
   int file_desc;              /* fd for this file */
   int nb_page_in_use;         /* number of pages in use for this file */
   int maxpages;               /* maximum number of pages allowed for this file */
   int written_into;           /* has file only be read from ? */
   } FILEINFO;

typedef struct {
   unsigned char ntc[4];        /* nt (longueur totale du fichier) en unites 64 bits */
   unsigned char ndc[4];        /* nd (longueur des donnees) en unites 64 bits */
   char cmcarc_name[MAX_NAME];
   } ENTETE_CMCARC;

typedef struct {
   unsigned char ntc[8];        /* nt (64 bits) (longueur totale du fichier) en unites 64 bits */
   unsigned char ndc[8];        /* nd (64 bits) (longueur des donnees) en unites 64 bits */
   char cmcarc_name[MAX_NAME];
   } ENTETE_CMCARC_V5;
   
