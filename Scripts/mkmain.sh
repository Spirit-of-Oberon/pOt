sed -e "s/<ModName>/${1}/g" -e "s/<Command>/${2}/g" < ${POT}/lib/Core/main.tpl > ${1}_${2}.c
