#!/bin/sh
#set -e
MEM=20
F90=ifort
VAR=''
CDIR=`pwd`
cd ../..
L63DIR=`pwd`
cd ..
ENKFDIR=`pwd`
COMDIR=$ENKFDIR/common
OUTDIR=$L63DIR/DATA/spinup
WKDIR=$L63DIR/tmp
rm -rf $WKDIR
mkdir -p $WKDIR
cd $WKDIR
cp $COMDIR/SFMT.f90 .
cp $COMDIR/common.f90 .
cp $L63DIR/model/lorenz63${VAR}.f90 .
cp $L63DIR/model/run/spinup.f90 .
$F90 -o spinup SFMT.f90 common.f90 lorenz63${VAR}.f90 spinup.f90
rm *.mod
rm *.o
time ./spinup
mkdir -p $OUTDIR
mv fort.90 $OUTDIR/init.dat
I=0
while test $I -lt $MEM
do
if test $I -lt 10
then
I=0$I
fi
time ./spinup
mv fort.90 $OUTDIR/init$I.dat
I=`expr $I + 1`
done

