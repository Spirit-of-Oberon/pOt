#!/bin/sh
if [ "_$1" = "_" ]
then
  echo "one command line parameter, designating the directory where"
  echo "pot resides must be specified, e.g."
  echo ""
  echo "        pot /usr/lang/pot "
  echo ""
  echo "parameters are NOT set."
else
  POT=$1 ; export POT
  POTLAND=$1/lib/:$1/lib/Core/ ; export POTLAND
  PATH=$1/bin:$PATH
fi
cd $POT
