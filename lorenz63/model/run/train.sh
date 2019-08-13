#!/bin/sh
#set -e
F90=ifort
VAR=''
CDIR=`pwd`
cd ../..
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
cp $L63DIR/model/run/train.f90 .
$F90 -o nature SFMT.f90 common.f90 lorenz63${VAR}.f90 train.f90
rm *.mod
rm *.o
ln -s $OUTDIR/spinup/init01.dat fort.10
time ./nature
mv fort.90 $OUTDIR/train.dat
cp $CDIR/train.ctl $OUTDIR
