#!/bin/sh
set -e
ORO=
OBS=regular13
EXP=M10L30I05
F90='ifort -mkl -lnetcdf -lnetcdff'
CDIR=`pwd`
cd ..
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
cp $COMDIR/netlib.f .
cp $COMDIR/common_mtx.f90 .
cp $COMDIR/common_letkf.f90 .
cp $L96DIR/model/lorenz96$ORO.f90 .
cp $L96DIR/obs/h_ope.f90 .
cp $CDIR/letkf.f90 .
$F90 -o letkf SFMT.f90 common.f90 netlib.f common_mtx.f90 common_letkf.f90 lorenz96$ORO.f90 h_ope.f90 letkf.f90
rm *.mod
#rm *.o
ln -s $OUTDIR/$OBS/obs.nc .
ln -s $OUTDIR/spinup/init*.nc .
ln -s $OUTDIR/nature.nc .
time ./letkf
rm -rf $OUTDIR/$OBS/$EXP
mkdir -p $OUTDIR/$OBS/$EXP
mv assim.nc $OUTDIR/$OBS/$EXP
for FILE in infl rmse_t rmse_x
do
if test -f $FILE.dat
then
mv $FILE.dat $OUTDIR/$OBS/$EXP
fi
done

echo "NORMAL END"
