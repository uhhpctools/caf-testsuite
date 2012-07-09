#! /bin/bash

echo "entered shell "
ROOT=`pwd`/..
for cfg in `ls CONFIG*` ; do
  postfix="`echo $cfg | sed 's/CONFIG//g'`"
  sed "s/\"//g" $cfg > tmp1
  sed 's/=/:=/' tmp1 > tmp2
  sed 's/{/(/g' tmp2 > tmp1
  sed 's/}/)/g' tmp1 > tmp2
  sed 's/`//g'  tmp2 > tmp1
  sed "s|pwd/..|$ROOT|g" tmp1 > make$postfix.def
  rm tmp1 tmp2
done
