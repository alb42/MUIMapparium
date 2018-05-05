#!/bin/bash -e

fpc makeversion.pas

./makeversion >mapparium.version
ver=$(cat mapparium.version)
base="MUIMapparium_${ver}_"

rm -f md5.txt
rm -f errmsg.log


function copyit {
  cp Catalogs/deutsch/MUIMapparium.catalog Release.${plat}/MUIMapparium/Catalogs/deutsch/
  cp locale/MUIMapparium.cd Release.${plat}/MUIMapparium/Catalogs/
  cp MUIMapparium.guide Release.${plat}/MUIMapparium/
  cd Release.${plat}
  rm -f *.lha
  echo "  create archive ${base}${plat}.lha"
  lha ao5 ${base}${plat}.lha MUIMapparium MUIMapparium.info >>../errmsg.log
  md5sum ${base}${plat}.lha >>../md5.txt
  cd ..
}


# AROS
echo ""
echo "################# AROS ##########################"
echo "" >>errmsg.log
echo "################# AROS ##########################" >>errmsg.log
plat="i386-aros"
mkdir -p units/${plat}
echo "  Compile ${plat}"
fpc4aros.sh -B -Xs -FUunits/${plat} -oRelease.${plat}/MUIMapparium/MUIMapparium MUIMapparium.pas >>errmsg.log
chmod a+rwx Release.${plat}/MUIMapparium/MUIMapparium
copyit

# AROS ARM
echo ""
echo "################# ARM AROS ######################"
echo "" >>errmsg.log
echo "################# ARM AROS ##########################" >>errmsg.log
plat="arm-aros"
mkdir -p units/${plat}
echo "  Compile ${plat}"
fpc4arosarm.sh -B -Xs -FUunits/${plat} -oRelease.${plat}/MUIMapparium/MUIMapparium MUIMapparium.pas >>errmsg.log
chmod a+rwx Release.${plat}/MUIMapparium/MUIMapparium
copyit

# AROS x64
echo ""
echo "################# AROS64 ########################"
echo "" >>errmsg.log
echo "################# AROS64 ##########################" >>errmsg.log
plat="x86_64-aros"
mkdir -p units/${plat}
echo "  Compile ${plat}"
fpc4aros64.sh -B -Xs -FUunits/${plat} -oRelease.${plat}/MUIMapparium/MUIMapparium MUIMapparium.pas >>errmsg.log
chmod a+rwx Release.${plat}/MUIMapparium/MUIMapparium
copyit

# Amiga
echo ""
echo "################# Amiga #########################"
echo "" >>errmsg.log
echo "################# Amiga #########################" >>errmsg.log
plat="m68k-amiga"
mkdir -p units/m68k-amiga
echo "  Compile ${plat}"
fpc4amiga.sh -B -Xs -FUunits/${plat} -oRelease.${plat}/MUIMapparium/MUIMapparium_NoFPU MUIMapparium.pas >>errmsg.log
m68k-amigaos-strip --strip-all Release.${plat}/MUIMapparium/MUIMapparium_NoFPU
chmod a+rwx Release.${plat}/MUIMapparium/MUIMapparium_NoFPU
#noFPU version
mkdir -p units/m68k-amiga-fpu
echo "  Compile ${plat}-fpu"
fpc4amigafpu.sh -B -Xs -FUunits/m68k-amiga-fpu -oRelease.${plat}/MUIMapparium/MUIMapparium MUIMapparium.pas >>errmsg.log
m68k-amigaos-strip --strip-all Release.${plat}/MUIMapparium/MUIMapparium
chmod a+rwx Release.${plat}/MUIMapparium/MUIMapparium
# copy rest
copyit

# MorphOS
echo ""
echo "################# MorphOS #######################"
echo "" >>errmsg.log
echo "################# MorphOS #######################" >>errmsg.log
plat="powerpc-morphos"
mkdir -p units/${plat}
echo "  Compile ${plat}"
fpc4mos.sh -B -Xs -FUunits/${plat} -oRelease.${plat}/MUIMapparium/MUIMapparium MUIMapparium.pas >>errmsg.log
powerpc-morphos-strip --strip-all Release.${plat}/MUIMapparium/MUIMapparium
chmod a+rwx Release.${plat}/MUIMapparium/MUIMapparium
copyit

# Amiga OS4
echo ""
echo "################# AmigaOS4 ######################"
echo "" >>errmsg.log
echo "################# AmigaOS4 ######################" >>errmsg.log
plat="powerpc-amiga"
mkdir -p units/${plat}
echo "  Compile ${plat}"
fpc4os4.sh -B -Xs -FUunits/${plat} -oRelease.${plat}/MUIMapparium/MUIMapparium MUIMapparium.pas >>errmsg.log
powerpc-amiga-strip --strip-all Release.${plat}/MUIMapparium/MUIMapparium
chmod a+rwx Release.${plat}/MUIMapparium/MUIMapparium
copyit

echo "############### Finished"
echo "############### Finished" >>errmsg.log
