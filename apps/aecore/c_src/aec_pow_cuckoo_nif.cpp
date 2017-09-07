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
#include <erl_nif.h>
#include "cuckoo_base.h"

// arbitrary length of header hashed into siphash key
#define HEADERLEN 80


extern node_t* generate(char* header, int nonce, int ntrims, int nthreads);
extern int verify(char* header, int nonce, node_t soln[PROOFSIZE]);

int read_solution(ErlNifEnv* env, const ERL_NIF_TERM e_soln, node_t soln[PROOFSIZE]);
int get_uint64(ErlNifEnv* env, const ERL_NIF_TERM from, uint64_t* to);


///=============================================================================
/// API
///=============================================================================

static ERL_NIF_TERM generate_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int nonce, ntrims, nthreads;
  char header[HEADERLEN];

  // decode args
  if (argc != 4 ||
      enif_get_string(env, argv[0], header, HEADERLEN, ERL_NIF_LATIN1) <= 0 ||
      !enif_get_int(env, argv[1], &nonce) ||
      !enif_get_int(env, argv[2], &ntrims) ||
      !enif_get_int(env, argv[3], &nthreads))
    return enif_make_badarg(env);

  node_t* result = generate(header, nonce, ntrims, nthreads);

  if (result) {
    // success: encode result
    ERL_NIF_TERM arr[PROOFSIZE];
    for (int i2 = 0; i2 < PROOFSIZE; i2++) {
#if EDGEBITS > 31
      arr[i2] = enif_make_uint64(env, result[i2]);
#else
      arr[i2] = enif_make_uint(env, result[i2]);
#endif
    }
    ERL_NIF_TERM e_soln = enif_make_list_from_array(env, arr, PROOFSIZE);

    delete[] result;

    // solution found: return {ok, Key1, Key2, Solution}
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), e_soln);
  } else {
    // failed to find solution, return {error, no_solutions}
    ERL_NIF_TERM failure = enif_make_tuple2(env,
                                            enif_make_atom(env, "error"),
                                            enif_make_atom(env, "no_solutions"));
    return failure;
  }
}

static ERL_NIF_TERM verify_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  int nonce;
  char header[HEADERLEN];
  node_t soln[PROOFSIZE];

  if (argc != 3 ||
      enif_get_string(env, argv[0], header, HEADERLEN, ERL_NIF_LATIN1) <= 0 ||
      !enif_get_int(env, argv[1], &nonce) ||
      !read_solution(env, argv[2], soln))
    return enif_make_badarg(env);

  int result = verify(header, nonce, soln);

  if (result != 0)
    return enif_make_atom(env, "true");
  else
    return enif_make_atom(env, "false");
}

static ERL_NIF_TERM get_node_size_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  int size = sizeof(node_t);
  return enif_make_int(env, size);
}

static ErlNifFunc nif_funcs[] = {
  {"generate", 4, generate_nif, ERL_NIF_DIRTY_JOB_CPU_BOUND},
  {"verify", 3, verify_nif, 0},
  {"get_node_size", 0, get_node_size_nif, 0}
};

ERL_NIF_INIT(aec_pow_cuckoo, nif_funcs, NULL, NULL, NULL, NULL);

///=============================================================================
/// Internal functions
///=============================================================================

int read_solution(ErlNifEnv* env, const ERL_NIF_TERM e_soln, node_t c_soln[PROOFSIZE]) {
  ERL_NIF_TERM term, head, tail;

  term = e_soln;
  for (int i = 0; i < PROOFSIZE; i++) {
    if (!enif_get_list_cell(env, term, &head, &tail))
      return false;
    c_soln[i] = head;
    if (
#if EDGEBITS > 31
        !enif_get_uint64(env, head, c_soln + i)
#else
        !enif_get_uint(env, head, c_soln + i)
#endif
        )
      return false;
    term = tail;
  }

  return true;
}

// Fix for clang: force cast from u64 (unsigned long long) to unsigned long on
// 64-bit architectures
int get_uint64(ErlNifEnv* env, const ERL_NIF_TERM from, uint64_t* to) {
  int result;
#if SIZEOF_LONG == 8
  result = enif_get_ulong(env, from, (unsigned long *)to);
#else
  result = enif_get_uint64(env, from, to);
#endif
  return result;
}
