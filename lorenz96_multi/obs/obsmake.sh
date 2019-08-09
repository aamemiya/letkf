#!/bin/sh
#set -e
ORO=
#OBSNAME=regular13
OBSNAME=all_01
F90="ifort"
CDIR=`pwd`
cd ..
L96DIR=`pwd`
cd ..
ENKFDIR=`pwd`
COMDIR=$ENKFDIR/common
OUTDIR=$L96DIR/DATA
WKDIR=$L96DIR/tmp
rm -rf $WKDIR
mkdir -p $WKDIR
cd $WKDIR
cp $COMDIR/SFMT.f90 .
cp $COMDIR/common.f90 .
cp $L96DIR/model/lorenz96_multi.f90 .
cp $L96DIR/obs/h_ope.f90 .
cp $L96DIR/obs/obsmake.f90 .
$F90 -o obsmake SFMT.f90 common.f90 lorenz96_multi.f90 h_ope.f90 obsmake.f90
rm *.mod
rm *.o
ln -s $OUTDIR/nature.dat fort.10
time ./obsmake
mkdir -p $OUTDIR/$OBSNAME
mv fort.91 $OUTDIR/$OBSNAME/obs.dat

