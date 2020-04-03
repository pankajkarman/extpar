#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include <unistd.h>
#include <fcntl.h>

#include <sys/types.h>
#include <sys/mman.h>

#include "util_mmap_cache.h"

void *allocate_cache(char *variable, size_t length)
{
  int fd;
  void *addr;
  char filename[256];  
  
  strcpy(filename, variable);
  strcat(filename, ".map");
  
  fd = open(filename, O_RDWR | O_CREAT, 0644);
  if (fd < 0)
    {
      perror("open");
      exit(EXIT_FAILURE);
    }
  
  if (ftruncate(fd, length) < 0)
    {
      perror("ftruncate");
      exit(EXIT_FAILURE);
    }
  
  addr = mmap(NULL, length, PROT_READ | PROT_WRITE, MAP_SHARED, fd, (off_t) 0);
  if (addr == MAP_FAILED)
    {
      perror("mmap");
      exit(EXIT_FAILURE);
    }
  
  if (unlink(filename) < 0)
    {
      perror("unlink");
      exit(EXIT_FAILURE);
    }
  
  return addr;
}

void deallocate_cache(void *addr, size_t length)
{
  if (munmap(addr, (getpagesize())) < 0)
    {
      perror("munmap");
      exit(EXIT_FAILURE);
    }

  return;
}

#if 0
int main(int argc, char *argv[])
{
  double *sst;

  sst = allocate_cache("sst", (size_t) (2 * sizeof(double)));

  sst[0] = 273.15;
  sst[1] = 323.67;
  
  deallocate_cache(sst, (size_t) (2 * sizeof(double)));
  
  return 0;
}
#endif

