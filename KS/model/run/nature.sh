#!/bin/sh
set -e
F90=ifort
CDIR=`pwd`
cd ../..
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
cp $KSDIR/model/run/nature.f90 .
$F90 -o nature SFMT.f90 common.f90 KS.f90 nature.f90
rm *.mod
#rm *.o
ln -s $OUTDIR/spinup/init18.dat fort.10
time ./nature
mv fort.90 $OUTDIR/nature_train.dat
cp $CDIR/nature.ctl $OUTDIR
