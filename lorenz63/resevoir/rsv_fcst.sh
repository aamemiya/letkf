#!/bin/sh
#set -e
F90=ifort
VAR=''
CDIR=`pwd`
cd ..
L63DIR=`pwd`
cd ..
ENKFDIR=`pwd`
COMDIR=$ENKFDIR/common
NATDIR=$L63DIR/DATA
OUTDIR=$L63DIR/DATA/fcst
WKDIR=$L63DIR/tmp
TESTDIR=$L63DIR/test
rm -rf $WKDIR
mkdir -p $WKDIR
cd $WKDIR
cp $CDIR/rsv_fcst.f90 .
cp $COMDIR/SFMT.f90 .
cp $COMDIR/netlib.f .
cp $COMDIR/common.f90 .
cp $COMDIR/common_mtx.f90 .
cp $COMDIR/common_reg.f90 .
cp $COMDIR/common_rsv_rnet.f90 .
cp $NATDIR/nature_train.dat ./fort.10
cp $NATDIR/nature_test.dat ./fort.30
$F90 -o rsv_fcst SFMT.f90 netlib.f common.f90 common_mtx.f90 common_reg.f90 common_rsv_rnet.f90 rsv_fcst.f90 -mkl
rm *.mod
time ./rsv_fcst
mkdir -p $OUTDIR
mv fort.90 $OUTDIR/fcst.dat
