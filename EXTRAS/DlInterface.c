#include <stdlib.h>
#include <dlfcn.h>
/*
  rmnlib interface to the dynamic loading functions
  if this module is compiled without -DLIVE
  it provides stubs that return a failure code when called
  the purpose is to have a default version in the library that does not
  necessitate -ldl at link time for applications
  if this module is compiled with -DLIVE
  it becomes a direct interface do dlopen/dlsym/dlerror/dlclose
*/
#define ERR_NOT_ACTIVE "ERROR: this is the dummy dynamic loader\n"

static void *(*P_DlOpen)(const char *,int) = NULL;
static void *(*P_DlSym)(void *,const char *) = NULL;
static int (*P_DlClose)(void *) = NULL;
static char *(*P_DlError)(void) = NULL;

void *DlOpen(const char *filename, int flag)
{
if(P_DlOpen != NULL)
  return((*P_DlOpen)(filename,flag));
else
  return(NULL);
}
void *DlSym(void *handle, const char *symbol)
{
if(P_DlSym != NULL)
  return ((*P_DlSym)(handle,symbol));
else
  return(NULL);
}
char *DlError(void)
{
if(P_DlError != NULL)
  return ((*P_DlError)());
else
  return(ERR_NOT_ACTIVE);
}
int DlClose(void *handle)
{
if(P_DlClose != NULL)
  return ((*P_DlClose)(handle));
else
  return(-1);
}

void DlRegister(void *open, void *sym, void *error, void *close)
{
  P_DlOpen = (void *(*)(const char *,int)) open;
  P_DlSym = (void *(*)(void *,const char *)) sym;
  P_DlError = (char *(*)(void)) error;
  P_DlClose = (int (*)(void *)) close;
}
