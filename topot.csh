#!/usr/bin/csh
if ( "_$1" == "_"  ) then
  echo "one command line parameter, designating the directory where"
  echo "pot resides must be specified, e.g."
  echo ""
  echo "        pot /usr/lang/pot "
  echo ""
  echo "parameters are NOT set."
else
  setenv POT $1
  setenv POTLAND $1/lib/:$1/lib/Core/
  set path = ($1/bin $path)
endif
cd $POT
