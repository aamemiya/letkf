#!/bin/sh
set -e
OBSNAME=regular16
F90=ifort
CDIR=`pwd`
cd ..
KSDIR=`pwd`
cd ..
ENKFDIR=`pwd`
COMDIR=$ENKFDIR/common
OUTDIR=$KSDIR/DATA
WKDIR=$KSDIR/tmp
rm -rf $WKDIR
mkdir -p $WKDIR
cd $WKDIR
cp $COMDIR/SFMT.f90 .
cp $COMDIR/common.f90 .
cp $KSDIR/model/KS.f90 .
cp $KSDIR/obs/h_ope.f90 .
cp $KSDIR/obs/obsmake.f90 .
$F90 -o obsmake SFMT.f90 common.f90 KS.f90 h_ope.f90 obsmake.f90
rm *.mod
#rm *.o
ln -s $OUTDIR/nature.dat fort.10
time ./obsmake
mkdir -p $OUTDIR/$OBSNAME
mv fort.91 $OUTDIR/$OBSNAME/obs.dat

