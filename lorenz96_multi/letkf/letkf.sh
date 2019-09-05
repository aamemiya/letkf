#!/bin/sh
#set -e
VAR=_biased
#METHOD=
#METHOD=_DdSM
METHOD=_reg
OBS=all_02
#EXP=M20L30I20
#EXP=M20L30I20_A80NB99
#EXP=nocorr
#EXP=nocorr_I80
#EXP=DdSM
EXP=reg
#EXP=test

MONITOR=F

if [ "$MONITOR" == "T" ] ;then 
 F90="dclfrt -mkl -DMONITOR_YSPACE"
else
 F90="ifort -mkl -fpp -CB -CU -traceback"
fi

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
cp $L96DIR/model/lorenz96$VAR.f90 .
cp $L96DIR/obs/h_ope.f90 .
cp $CDIR/letkf${METHOD}.f90 .

if [ "$MONITOR" == "T" ] ;then 
cp $CDIR/monitor_yspace.f90 .
$F90 -o letkf SFMT.f90 common.f90 netlib.f common_mtx.f90 common_letkf.f90 lorenz96$VAR.f90 h_ope.f90 letkf${METHOD}.f90 monitor_yspace.f90 -lnetcdf -lnetcdff
else
$F90 -o letkf SFMT.f90 common.f90 netlib.f common_mtx.f90 common_letkf.f90 lorenz96$VAR.f90 h_ope.f90 letkf${METHOD}.f90 -lnetcdf -lnetcdff 
fi

rm *.mod
rm *.o
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


[ "$MONITOR" == "T" ] && mv monitor_obs_*.png $OUTDIR/$OBS/$EXP

echo "NORMAL END"
