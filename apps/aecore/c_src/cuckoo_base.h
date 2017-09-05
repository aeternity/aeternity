//-------------------------------------------------------------------
// Copyright (C) 2017, Aeternity Anstalt
// Based on Cuckoo Cycle of John Tromp, a memory-hard proof-of-work
//-------------------------------------------------------------------

typedef uint32_t u32;
typedef uint64_t u64;

// proof-of-work parameters
#ifndef EDGEBITS
// the main parameter is the 2-log of the graph size,
// which is the size in bits of the node identifiers
#define EDGEBITS 27
#endif
#ifndef PROOFSIZE
// the next most important parameter is the (even) length
// of the cycle to be found. a minimum of 12 is recommended
#define PROOFSIZE 42
#endif

#if EDGEBITS > 32
typedef u64 edge_t;
#else
typedef u32 edge_t;
#endif
#if EDGEBITS > 31
typedef u64 node_t;
#else
typedef u32 node_t;
#endif

// number of edges
#define NEDGES ((node_t)1 << EDGEBITS)
// used to mask siphash output
#define EDGEMASK ((edge_t)NEDGES - 1)
