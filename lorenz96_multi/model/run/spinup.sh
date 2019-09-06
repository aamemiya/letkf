#!/bin/sh
#set -e
MEM=20
F90=ifort
VAR=_biased
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
cp $L96DIR/model/run/spinup.f90 .
$F90 -o spinup SFMT.f90 common.f90 lorenz96${VAR}.f90 spinup.f90 -lnetcdf -lnetcdff
rm *.mod
rm *.o
time ./spinup
mkdir -p $OUTDIR
mv init.nc $OUTDIR/init.nc
I=0
while test $I -lt $MEM
do
if test $I -lt 10
then
I=0$I
fi
time ./spinup
mv init.nc $OUTDIR/init$I.nc
I=`expr $I + 1`
done

