#!/bin/bash

TS=(2 3 4 5 6 7 8 9)
RP=(56 57 56 60 60 63 64 63)
RF=8

CURVE="BLS12_381"
PRIME="0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001"
SIZE=255

# CURVE="BN128"
# PRIME="0x30644e72e131a029b85045b68181585d2833e84879b9709143e1f593f0000001"
# SIZE=254

git clone https://extgit.iaik.tugraz.at/krypto/hadeshash.git /tmp/hadeshash

SCRIPT=/tmp/hadeshash/code/generate_parameters_grain.sage

NMS=$(( ${#TS[@]} + 1 ))

export BC_LINE_LENGTH=144

for I in "${!TS[@]}"; do
  echo "Generate (${TS[I]}; ${RP[I]}) for PRIME=${PRIME}..."
  sage ${SCRIPT} 1 0 ${SIZE} ${TS[I]} ${RF} ${RP[I]} ${PRIME} > "/tmp/tmpdata${I}"
done

for I in "${!TS[@]}"; do
  CS=`cat /tmp/tmpdata${I} | head -3 | tail -1 | sed -e "s/'//g; s/\]//g; s/,//g; s/\[//g; s/0x/16#/g"`
  CSIZE=`cat /tmp/tmpdata${I} | head -1 | cut -d' ' -f5`
  echo "params_c_${CURVE}(${TS[$I]}) ->"
  echo "  ["
  N=0
  for X in $CS; do
    N=$(( ${N} + 1 ))
    if [ "$N" -lt "$CSIZE" ]; then
      echo "    $X,"
    else
      echo "    $X"
    fi
  done
  if [ "$I" -lt "$(( ${#TS[@]} - 1 ))" ]; then
    echo "  ];"
  else
    echo "  ]."
  fi
done

echo ""
for I in "${!TS[@]}"; do
  echo "params_m_${CURVE}(${TS[$I]}) ->"
  echo "  ["
  CSIZE=`cat /tmp/tmpdata${I} | head -1 | cut -d' ' -f5`
  MALL=`cat /tmp/tmpdata${I} | tail -1 | sed -e "s/'//g; s/,//g; s/0x/16#/g; s/\]\]//g; s/\[//g"`
  IFS=']' read -ra MSS <<< "${MALL}"
  N1=0
  for MS in "${MSS[@]}"; do
    echo "    ["
    N2=0
    for M in $MS; do
      N2=$(( ${N2} + 1 ))
      if [ "${N2}" -lt "$(( ${I} + 2 ))" ]; then
        echo "      ${M},"
      else
        echo "      ${M}"
      fi
    done
    N1=$(( ${N1} + 1 ))
    if [ "${N1}" -lt "$(( ${I} + 2 ))" ]; then
      echo "    ],"
    else
      echo "    ]"
    fi
  done
  if [ "$I" -lt "$(( ${#TS[@]} - 1 ))" ]; then
    echo "  ];"
  else
    echo "  ]."
  fi
done
