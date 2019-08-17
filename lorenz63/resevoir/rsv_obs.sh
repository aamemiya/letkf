#!/bin/sh
#set -e
F90=ifort
VAR=''
OBS='single_x'
CDIR=`pwd`
cd ..
L63DIR=`pwd`
cd ..
ENKFDIR=`pwd`
DIMDIR=$L63DIR
COMDIR=$ENKFDIR/common
OBSDIR=$L63DIR/DATA/$OBS
OUTDIR=$L63DIR/DATA/$OBS/fcst
WKDIR=$L63DIR/tmp
TESTDIR=$L63DIR/test
rm -rf $WKDIR
mkdir -p $WKDIR
cd $WKDIR
cp $CDIR/rsv_obs.f90 .
cp $COMDIR/SFMT.f90 .
cp $COMDIR/netlib.f .
cp $COMDIR/common.f90 .
cp $COMDIR/common_mtx.f90 .
cp $TESTDIR/test_reg.f90 .
cp $TESTDIR/test_rsv_rnet.f90 .
cp $OBSDIR/obs.dat ./fort.10
$F90 -o rsv_obs SFMT.f90 netlib.f common.f90 common_mtx.f90 test_reg.f90 test_rsv_rnet.f90 rsv_obs.f90 -mkl -traceback -CU -CB
rm *.mod
time ./rsv_obs
mkdir -p $OUTDIR
mv fort.90 $OUTDIR/fcst.dat

