#!/bin/sh
#set -e
F90=ifort
VAR='_multi'
CONFIG=X40F18
CDIR=`pwd`
cd ../..
L96DIR=`pwd`
cd ..
ENKFDIR=`pwd`
COMDIR=$ENKFDIR/common
OUTDIR=$L96DIR/DATA/$CONFIG
WKDIR=$L96DIR/tmp
rm -rf $WKDIR
mkdir -p $WKDIR
cd $WKDIR
cp $COMDIR/SFMT.f90 .
cp $COMDIR/common.f90 .
cp $L96DIR/model/lorenz96${VAR}.f90 .
cp $L96DIR/model/run/nature.f90 .
$F90 -o nature SFMT.f90 common.f90 lorenz96${VAR}.f90 nature.f90 -lnetcdf -lnetcdff -CU -CB -traceback 
rm *.mod
rm *.o
ln -s $OUTDIR/spinup/init_nature.nc .
time ./nature
mv nature.nc $OUTDIR/
