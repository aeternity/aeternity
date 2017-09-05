#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include <assert.h>
#include <string.h>
#include "cuckoo_base.h"
#include "pow_cuckoo.hpp"

#ifndef PART_BITS
// #bits used to partition edge set processing to save memory
// a value of 0 does no partitioning and is fastest
// a value of 1 partitions in two, making twice_set the
// same size as shrinkingset at about 33% slowdown
// higher values are not that interesting
#define PART_BITS 0
#endif

// arbitrary length of header hashed into siphash key
#define HEADERLEN 80


extern pow_cuckoo_result* generate(char* header, int nonce, int ntrims, int nthreads);
extern int verify(uint64_t key0, uint64_t key1, node_t soln[PROOFSIZE]);


int main(int argc, char ** argv) {
  int nthreads = 1;
  int ntrims   = 1 + (PART_BITS+3)*(PART_BITS+4)/2;
  int nonce = 0;
  int retries = 1;
  char header[HEADERLEN];
  unsigned len;
  int c;

  memset(header, 0, sizeof(header));
  while ((c = getopt (argc, argv, "h:m:n:r:t:")) != -1) {
    switch (c) {
      case 'h':
        len = strlen(optarg);
        assert(len <= sizeof(header));
        memcpy(header, optarg, len);
        break;
      case 'n':
        nonce = atoi(optarg);
        break;
      case 'r':
        retries = atoi(optarg);
        break;
      case 'm':
        ntrims = atoi(optarg);
        break;
      case 't':
        nthreads = atoi(optarg);
        break;
    }
  }

  for (int retry = 0; retry < retries; retry++) {
    pow_cuckoo_result* result = generate(header, nonce + retry, ntrims, nthreads);
    if (result) {
      printf("Solution found with nonce %d, header %s, verifying...\n", nonce, header);

      // verification
      int ver = verify(result->key1, result->key2, result->soln);
      printf("Verification result: %d\n", ver);
      return 0;
    } else {
      printf("No solution found with nonce %d, header %s\n", nonce, header);
    }
  }
  return 0;
}
