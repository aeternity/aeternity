//-------------------------------------------------------------------
// Copyright (C) 2017, Aeternity Anstalt
// Based on Cuckoo Cycle of John Tromp, a memory-hard proof-of-work
//-------------------------------------------------------------------

#include <inttypes.h> // for SCNx64 macro
#include <stdio.h>    // printf/scanf
#include <stdlib.h>   // exit
#include <unistd.h>   // getopt
#include <assert.h>
#include <string.h>
#include "siphash.h"
#include "cuckoo_miner.hpp"


#define MAXSOLS 1
// arbitrary length of header hashed into siphash key
#define HEADERLEN 80


node_t* generate(char* header, int nonce, int ntrims, int nthreads) {
  printf("Looking for %d-cycle on cuckoo%d(\"%s\",%d) with 50%% edges, %d trims, %d threads\n",
         PROOFSIZE, EDGEBITS + 1, header, nonce, ntrims, nthreads);

  u64 edgeBytes = NEDGES/8, nodeBytes = TWICE_ATOMS*sizeof(atwice);
  int edgeUnit, nodeUnit;
  for (edgeUnit=0; edgeBytes >= 1024; edgeBytes>>=10, edgeUnit++) ;
  for (nodeUnit=0; nodeBytes >= 1024; nodeBytes>>=10, nodeUnit++) ;

  printf("Using %d%cB edge and %d%cB node memory, %d-way siphash, and %d-byte counters\n",
         (int)edgeBytes, " KMGT"[edgeUnit], (int)nodeBytes, " KMGT"[nodeUnit],
         NSIPHASH, SIZEOF_TWICE_ATOM);

  thread_ctx *threads = (thread_ctx *)calloc(nthreads, sizeof(thread_ctx));
  assert(threads);
  cuckoo_ctx ctx(nthreads, ntrims, MAXSOLS);

  // Initiate context using header and nonce
  ctx.setheadernonce(header, sizeof(header), nonce);
  printf("k0 %llx k1 %llx\n", ctx.sip_keys.k0, ctx.sip_keys.k1);

  // Spawn threads with this context
  for (int t = 0; t < nthreads; t++) {
    threads[t].id = t;
    threads[t].ctx = &ctx;
    int err = pthread_create(&threads[t].thread, NULL, worker, (void *)&threads[t]);
    if (err != 0)
      return NULL;
  }

  // Wait for threads to finish
  for (int t = 0; t < nthreads; t++) {
    int err = pthread_join(threads[t].thread, NULL);
    if (err != 0)
      return NULL;
  }

  // Look for solutions
  if (ctx.nsols == 0) {
    printf("No solution found for nonce %d\n", nonce);

    // Failed to find a solution
    free(threads);
    return NULL;
  }
  else {
    printf("%d solutions found for nonce %d\n", ctx.nsols, nonce);

    // Return struct
    for (unsigned s = 0; s < ctx.nsols; s++) {
      for (int i = 0; i < PROOFSIZE; i++) {
        printf(" %jx", (uintmax_t)ctx.sols[s][i]);
      }
      printf("\n");
    }

    free(threads);
    // Return the 1st solution as result, dropping possible others
    node_t* result = new node_t[PROOFSIZE];
    for (int i = 0; i < PROOFSIZE; i++)
      result[i] = ctx.sols[0][i];
    return result;
  }
}

int verify(char* header, int nonce, node_t soln[PROOFSIZE]) {
  // Initiate a context using header and nonce, we just need the keys
  cuckoo_ctx ctx(1, 1, 1);
  ctx.setheadernonce(header, sizeof(header), nonce);
  printf("k0 %llx k1 %llx\n", ctx.sip_keys.k0, ctx.sip_keys.k1);

  edge_t nonces[PROOFSIZE];
  for (int i = 0; i < PROOFSIZE; i++) {
    nonces[i] = soln[i];
  }

  int pow_rc = verify(nonces, &ctx.sip_keys);

  if (pow_rc == POW_OK) {
    printf("Verified with cyclehash ");
    unsigned char cyclehash[32];
    blake2b((void *)cyclehash, sizeof(cyclehash), (const void *)nonces, sizeof(nonces), 0, 0);
    for (int i=0; i<32; i++)
      printf("%02x", cyclehash[i]);
    printf("\n");

    return true;
  } else {
    printf("FAILED due to %s\n", errstr[pow_rc]);

      return false;
  }
}
