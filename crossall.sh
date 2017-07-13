#!/bin/bash -e

fpc makeversion.pas

./makeversion >mapparium.version
ver=$(cat mapparium.version)
base="MUIMapparium_${ver}_"

rm -f md5.txt
rm -f errmsg.log


function copyit {
  cp Catalogs/deutsch/MUIMapparium.catalog Release.${plat}/MUIMapparium/Catalogs/deutsch/
  cp MUIMapparium.readme Release.${plat}/MUIMapparium/
  cd Release.${plat}
  rm -f *.lha
  echo "  create archive ${base}${plat}.lha"
  lha ao5 ${base}${plat}.lha MUIMapparium MUIMapparium.info >>errmsg.log
  md5sum ${base}${plat}.lha >>md5.txt
  cd ..
}


# AROS
echo ""
echo "################# AROS ##########################"
plat="i386-aros"
mkdir -p units/${plat}
echo "  Compile ${plat}"
fpc4aros.sh -B -Xs -FUunits/${plat} MUIMapparium.pas >>errmsg.log
cp MUIMapparium Release.${plat}/MUIMapparium/
copyit

# AROS ARM
echo ""
echo "################# ARM AROS ######################"
plat="arm-aros"
mkdir -p units/${plat}
echo "  Compile ${plat}"
fpc4arosarm.sh -B -Xs -FUunits/${plat} MUIMapparium.pas >>errmsg.log
cp MUIMapparium Release.${plat}/MUIMapparium/
copyit

# AROS x64
echo ""
echo "################# AROS64 ########################"
plat="x86_64-aros"
mkdir -p units/${plat}
echo "  Compile ${plat}"
fpc4aros64.sh -B -Xs -FUunits/${plat} MUIMapparium.pas >>errmsg.log
cp MUIMapparium Release.${plat}/MUIMapparium/
copyit

# Amiga
echo ""
echo "################# Amiga #########################"
plat="m68k-amiga"
mkdir -p units/m68k-amiga
echo "  Compile ${plat}"
fpc4amiga.sh -B -Xs -FUunits/${plat} MUIMapparium.pas >>errmsg.log
m68k-amigaos-strip --strip-all MUIMapparium
cp MUIMapparium Release.${plat}/MUIMapparium/MUIMapparium_NoFPU
#noFPU version
mkdir -p units/m68k-amiga-fpu
echo "  Compile ${plat}-fpu"
fpc4amigafpu.sh -B -Xs -FUunits/m68k-amiga-fpu MUIMapparium.pas >>errmsg.log
m68k-amigaos-strip --strip-all MUIMapparium
cp MUIMapparium Release.${plat}/MUIMapparium/MUIMapparium
# copy rest
copyit

# MorphOS
echo ""
echo "################# MorphOS #######################"
plat="powerpc-morphos"
mkdir -p units/${plat}
echo "  Compile ${plat}"
fpc4mos.sh -B -Xs -FUunits/${plat} MUIMapparium.pas >>errmsg.log
powerpc-morphos-strip --strip-all MUIMapparium
cp MUIMapparium Release.${plat}/MUIMapparium/
copyit

# Amiga OS4
echo ""
echo "################# AmigaOS4 ######################"
plat="powerpc-amiga"
mkdir -p units/${plat}
echo "  Compile ${plat}"
fpc4os4.sh -B -Xs -FUunits/${plat} MUIMapparium.pas >>errmsg.log
powerpc-amiga-strip --strip-all MUIMapparium
cp MUIMapparium Release.${plat}/MUIMapparium/
copyit

echo "############### Finished"