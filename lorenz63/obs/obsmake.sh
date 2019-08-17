#!/bin/sh
#set -e
ORO=
OBSNAME=single_x
VAR=""
F90="ifort"
CDIR=`pwd`
cd ..
L63DIR=`pwd`
cd ..
ENKFDIR=`pwd`
COMDIR=$ENKFDIR/common
OUTDIR=$L63DIR/DATA
WKDIR=$L63DIR/tmp
rm -rf $WKDIR
mkdir -p $WKDIR
cd $WKDIR
cp $COMDIR/SFMT.f90 .
cp $COMDIR/common.f90 .
cp $L63DIR/model/lorenz63${VAR}.f90 .
cp $L63DIR/obs/h_ope_x.f90 .
cp $L63DIR/obs/obsmake.f90 .
$F90 -o obsmake SFMT.f90 common.f90 lorenz63${VAR}.f90 h_ope_x.f90 obsmake.f90
rm *.mod
rm *.o
ln -s $OUTDIR/nature.dat fort.10
time ./obsmake
mkdir -p $OUTDIR/$OBSNAME
mv fort.91 $OUTDIR/$OBSNAME/obs.dat

