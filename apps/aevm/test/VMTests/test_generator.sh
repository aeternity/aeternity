#!/bin/bash

if [ $# -lt 2 ]; then
  echo "usage: $0 <template> <outdir>"
  exit 1
fi

TEMPLATE=$1
OUTDIR=$2
TESTDATA=${OUTDIR}/_tests.data

if [ ! -e ${TESTDATA} ]; then
  echo "${OUTDIR} does not contain a file '_tests.data'"
  exit 1
fi

echo "Tests in ${TESTDATA}"

for L in `cat ${TESTDATA} | tail -n +2`; do
  echo "Generating: $L"
  TEST=`echo $L | cut -d';' -f1`
  CODE=`echo $L | cut -d';' -f2`
  STOR=`echo $L | cut -d';' -f3`
  cat $TEMPLATE \
    | sed -e "s/___CODE___/${CODE}/" \
    | sed -e "s/___STORAGE___/${STOR}/" \
    | sed -e "s/___TEST___/${TEST}/" > ${OUTDIR}/${TEST}.json
done
