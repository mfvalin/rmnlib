// nvc -c -O2 xdf98_bug.c # fails
// nvc -c -O2 -Mnovect xdf98_bug.c # O.K.
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#include <unistd.h>
#include <sys/types.h>

#include <pthread.h>

void c_wawrit(
    const int iun,
    const void * const buf,
    const unsigned int offset,
    const int nwords
);

//! \todo Rename this type to something more specific.  Is it used by client apps?
typedef struct {
    unsigned int
        stream:1,
        std:1,
        rsf:1,
        burp:1,
        rnd:1,
        wa:1,
        ftn:1,
        unf:1,
        read_only:1,
        old:1,
        scratch:1,
        notpaged:1,
        pipe:1,
        write_mode:1,
        remote:1,
        volatil:1,
        padding:16;
} attributs;

typedef struct{   // this struct only contains a pointer to the actual full control structure
    void *p ;
} RSF_handle ;

//! \todo Rename this type to something more specific.  Is it used by client apps?
typedef struct {
    //! Complete file name
    char *file_name;
    //! Sub file name for cmcarc files
    char *subname;
    //! File type and options
    char * file_type;
    //! Handle to open file (RSF version)
    RSF_handle rsf_fh;
    //! fnom unit number
    int32_t iun;
    //! C file descriptor
    int32_t fd;
    //! File size in words
    int32_t file_size;
    //! effective file size in words
    int32_t eff_file_size;
    //! Record length when appliable
    int32_t lrec;
    //! Open/close flag
    int32_t open_flag;
    //! Slot in the set of wafile entries
    int32_t wa_slot;

    attributs attr;
} general_file_info;

//! No more memory can be allocated
#define ERR_MEM_FULL -24

typedef struct {
    uint32_t wd1;
    uint32_t wd2;
} word_2;

//! Key descriptor structure, 64 bits per key description
typedef struct {
    uint32_t ncle:32, reserved:8, tcle:6, lcle:5, bit1:13;
} key_descriptor;

//! XDF file header template, provision is made for up to 1024 keys
typedef struct {
    // Each line (except last one) describes 64 bits
    // Standard XDF record header
    uint32_t lng:24,   idtyp:8,   addr:32;
    // char[4]
    uint32_t vrsn,     sign;
    uint32_t fsiz:32,  nrwr:32;
    uint32_t nxtn:32,  nbd:32;
    uint32_t plst:32,  nbig:32;
    uint32_t lprm:16,  nprm:16,  laux:16, naux:16;
    uint32_t neff:32,  nrec:32;
    uint32_t rwflg:32, reserved:32;
    key_descriptor keys[1024];
} file_header;

//! Maximum length of primary keys
#define MAX_PRIMARY_LNG 16
//! Maximum length of info keys
#define MAX_SECONDARY_LNG 8
//! Maximum of 256K records in a random access XDF file
#define MAX_DIR_PAGES 1024

//! Maximum size of the primary key entry
typedef uint32_t max_dir_keys[MAX_PRIMARY_LNG];
typedef uint32_t max_info_keys[MAX_SECONDARY_LNG];

//! Directory page record
typedef struct {
    //! Header length (in 64 bit units)
    uint32_t lng:24;
    //! Type ID (usualy 0)
    uint32_t idtyp:8;
    //! Address of directory page (origin 1, 64 bit units)
    uint32_t addr:32;

    //! idrep (4 ascii char 'DIR0')
    uint32_t reserved1:32;
    //! Reserved (0)
    uint32_t reserved2:32;

    //! Address of next directory page (origin 1, 64 bit units)
    uint32_t nxt_addr:32;
    //! Number of entries in page
    uint32_t nent:32;

    //! Checksum (not valid when in core)
    uint32_t chksum:32;
    //! page_no, record_no, file_index: handle templage
    uint32_t reserved3:32;

    //! (real allocated dimension will be ENTRIES_PER_PAGE * primary_len)
    uint32_t entry[];
} xdf_dir_page;

//! Zero size directory page record
typedef struct {
    // Each line describes 64 bits
    uint32_t lng:24, idtyp:8, addr:32;
    uint32_t reserved1:32, reserved2:32;
    uint32_t nxt_addr:32,  nent:32;
    uint32_t chksum:32, file_index:8, record_no:8, page_no:16;
} base_dir_page;

//! Directory page + forward/backward chain ptrs
typedef struct full_dir_page {
    struct full_dir_page* next_page;
    struct full_dir_page* prev_page;
    int modified;
    int true_file_index;
    //! \bug warning: invalid use of structure with flexible array member
    // GCC doen't like this!  It probably works as expected, but it generates a warning when compiling
    xdf_dir_page dir;
} full_dir_page;

//! Pointer to a chainable directory page
typedef full_dir_page* page_ptr;

typedef struct {
    //! Pointer to directory pages
    page_ptr dir_page[MAX_DIR_PAGES];
    //! Pointer to current directory page
    page_ptr cur_dir_page;
    //! Pointer to primary key building function
    void* build_primary;
    //! Pointer to info building function
    void *build_info;
    //! Pointer to file scan function
    void* scan_file;
    //! Pointer to record filter function
    void *file_filter;
    //! Pointer to current directory entry
    uint32_t* cur_entry;
    //! Pointer to file header
    file_header* header;
    //! Next write address (in word units)
    int32_t nxtadr;
    //! Length in 64 bit units of primary keys (including 64 bit header)
    int primary_len;
    //! Length in 64 bit units of info keys
    int info_len;
    //! File index to next linked file,-1 if none
    int link;
    //! Pointer to current general file desc entry
    general_file_info* cur_info;
    //! FORTRAN unit number, -1 if not open, 0 if C file
    int iun;
    //! Index into file table, -1 if not open
    int file_index;
    //! Modified flag
    int modified;
    //! Number of allocated directory pages
    int npages;
    //! Number of records in file
    int nrecords;
    //! Current page number
    int cur_pageno;
    //! Record number within current page
    int page_record;
    //! Number of records in current page
    int page_nrecords;
    //! Version number
    int file_version;
    //! Last search target valid flag
    int valid_target;
    //! File is sequential xdf
    int xdf_seq;
    //! Last position valid flag (seq only)
    int valid_pos;
    //! Current address (WA, sequential xdf)
    int cur_addr;
    //! Address (WA) of first record (seq xdf)
    int seq_bof;
    //! Old standard file flag
    int fstd_vintage_89;
    //! Header & primary keys for last record
    max_dir_keys head_keys;
    //! Info for last read/written record
    max_info_keys info_keys;
    //! Keys for current operation
    max_dir_keys cur_keys;
    //! Current search target
    max_dir_keys target;
    //! Permanent search mask for this file
    max_dir_keys srch_mask;
    //! Current search mask for this file
    max_dir_keys cur_mask;
} file_table_entry;

typedef file_table_entry* file_table_entry_ptr;

//! File table, exported symbol
extern file_table_entry_ptr* file_table;

//! Create a new XDF file.
//! \return 0 on success, error code otherwise
/*static*/ int create_new_xdf_bad(
    //! [in] File index in table
    int index,
    //! [in] Unit number associated to the file
    int iun,
    //! [in] Primary keys
    word_2 *pri,
    //! [in] Number of primary keys
    int npri,
    //! [in] Auxiliary keys
    word_2 *aux,
    //! [in] Number of auxiliary keys
    int naux,
    //! [in] Application signature
    char *appl
) {

    file_header *file;
    int ikle = 0, lprm = 0, laux = 0;
    int lng_header = naux + npri + 512 / 64;

    if ((file_table[index]->header = malloc(lng_header * 8)) == NULL) {
//         Lib_Log(APP_LIBFST,APP_FATAL,"%s: memory is full\n",__func__);
        return(ERR_MEM_FULL);
    }
    file = file_table[index]->header;
    file->vrsn = 'X' << 24 | 'D' << 16 | 'F' << 8 | '0';
    file->sign = appl[0] << 24 | appl[1] << 16 | appl[2] << 8 | appl[3];
    file->idtyp = 0;
    // keys + fixed part of 512 bits
    file->lng = lng_header;
    file->addr = 0;
    // keys + fixed part of 512 bits
    file->fsiz = lng_header;
    file->nrwr = 0;
    file->nxtn = 0;
    file->nbd = 0;
    file->plst = 0;
    file->nbig = 0;
    file->nprm = npri;
    file->naux = naux;
    file->neff = 0;
    file->nrec = 0;
    file->rwflg = 0;
    file->reserved = 0;

   {
        int i = 0 , bit1 = 0, lcle = 0, tcle = 0;
        while (npri--) {
            file->keys[ikle].ncle = pri[i].wd1;
            bit1 = pri[i].wd2 >> 19;
            lcle = 0x1F & (pri[i].wd2 >> 14);
            tcle = 0x3F  & (pri[i].wd2 >> 8);
            file->keys[ikle].bit1 = bit1;
            file->keys[ikle].lcle = lcle;
            file->keys[ikle].tcle = tcle;
            file->keys[ikle].reserved = 0;
            lprm += lcle;
            i++;
            ikle++;
        }
   }

   /* primary keys + 64 bit header */
   lprm = (lprm + 63) / 64 + 1;
   file->lprm = lprm;
   file_table[index]->primary_len = lprm;
   {
       int i=0 , bit1=0, lcle=0, tcle=0;
        while (naux--) {
            file->keys[ikle].ncle=aux[i].wd1;
            bit1=aux[i].wd2 >> 19;
            lcle=0x1F & (aux[i].wd2 >> 14);
            tcle=0x3F  & (aux[i].wd2 >> 8);
            file->keys[ikle].bit1=bit1;
            file->keys[ikle].lcle=lcle;
            file->keys[ikle].tcle=tcle;
            file->keys[ikle].reserved=0;
            laux += lcle;
            i++; ikle++;
        }
   }
   laux = (laux + 63) / 64;
   file->laux = laux;
   file_table[index]->info_len = laux;
   if (! file_table[index]->cur_info->attr.read_only) {
        {
            int unit = iun, waddress = 1, nwords = file->fsiz * 8 / sizeof(uint32_t);
            c_wawrit(unit, file_table[index]->header, waddress, nwords);
            file_table[index]->nxtadr += nwords;
        }
   }
   return 0;
}
