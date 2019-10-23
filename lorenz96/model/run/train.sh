#!/bin/sh
#set -e
F90=ifort
#ORO='_oro'
ORO=
CDIR=`pwd`
cd ../..
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
cp $L96DIR/model/lorenz96$ORO.f90 .
cp $L96DIR/model/run/train.f90 .
$F90 -o nature SFMT.f90 common.f90 lorenz96$ORO.f90 train.f90 -lnetcdf -lnetcdff
rm *.mod
rm *.o
ln -s $OUTDIR/spinup/init.nc .
time ./nature
mv nature.nc $OUTDIR/train.nc

