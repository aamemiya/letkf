#!/bin/sh
#set -e
F90=ifort
VAR=_multi
CONFIG=X40F14
CDIR=`pwd`
cd ../..
L96DIR=`pwd`
cd ..
ENKFDIR=`pwd`
COMDIR=$ENKFDIR/common
OUTDIR=$L96DIR/DATA/${CONFIG}/spinup
WKDIR=$L96DIR/tmp
rm -rf $WKDIR
mkdir -p $WKDIR
cd $WKDIR
cp $COMDIR/SFMT.f90 .
cp $COMDIR/common.f90 .
cp $L96DIR/model/lorenz96${VAR}.f90 .
cp $L96DIR/model/run/spinup_nature.f90 .
$F90 -o spinup SFMT.f90 common.f90 lorenz96${VAR}.f90 spinup_nature.f90 -lnetcdf -lnetcdff
rm *.mod
rm *.o
time ./spinup
mkdir -p $OUTDIR
mv init_nature.nc $OUTDIR/init_nature.nc
I=0
