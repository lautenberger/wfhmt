#!/bin/bash

VER='0.537'

# The variables INSTALL_DIR and COMPILER are typically exported from
# another bash script, but they can be set here as well.
#
# Set the directory to which the compiled executables will be copied.
# Make sure it's in your PATH
if [ -z $INSTALL_DIR ]; then
   INSTALL_DIR=/usr/local/bin
fi

# Uncomment one of these depending on whether you want to compile with the
# gnu (gfortran) or intel compiler:
#COMPILER=intel
if [ -z $COMPILER ]; then
   COMPILER=gnu
fi

echo "waf"
cd waf
rm -f *.o *.mod
make ${COMPILER}_64
sudo cp -f ./waf ${INSTALL_DIR}/waf_$VER
rm -f *.o *.mod waf
cd ..

echo "fire dynamics"
cd fire_dynamics
rm -f *.o *.mod
make ${COMPILER}_64
sudo cp -f ./fire_dynamics ${INSTALL_DIR}/fire_dynamics_$VER
rm -f *.o *.mod fire_dynamics
cd ..

echo "map_generator"
cd map_generator
rm -f *.o *.mod
make ${COMPILER}_64
sudo cp -f ./map_generator ${INSTALL_DIR}/map_generator_$VER
rm -f *.o *.mod map_generator
cd ..

echo "fosberg"
cd fosberg
rm -f *.o *.mod
make ${COMPILER}_64
sudo cp -f ./fosberg ${INSTALL_DIR}/fosberg_$VER
rm -f *.o *.mod fosberg
cd ..

echo "hdw"
cd hdw
rm -f *.o *.mod
make ${COMPILER}_64
sudo cp -f ./hdw ${INSTALL_DIR}/hdw_$VER
rm -f *.o *.mod hdw
cd ..

echo "kbdi"
cd kbdi
rm -f *.o *.mod
make ${COMPILER}_64
sudo cp -f ./kbdi ${INSTALL_DIR}/kbdi_$VER
rm -f *.o *.mod kbdi
cd ..

echo "ignition_axis"
cd ignition_axis
rm -f *.o *.mod
make ${COMPILER}_64
sudo cp -f ./ignition_axis ${INSTALL_DIR}/ignition_axis_$VER
rm -f *.o *.mod ignition_axis
cd ..

echo "rank_rasters"
cd rank_rasters
rm -f *.o *.mod
make ${COMPILER}_64
sudo cp -f ./rank_rasters ${INSTALL_DIR}/rank_rasters_$VER
rm -f *.o *.mod rank_rasters
cd ..

echo "rank_rasters_openmpno"
cd rank_rasters
rm -f *.o *.mod
make ${COMPILER}_64_openmpno
sudo cp -f ./rank_rasters_openmpno ${INSTALL_DIR}/rank_rasters_openmpno_$VER
rm -f *.o *.mod rank_rasters_openmpno
cd ..

echo "raster_percentiles"
cd raster_percentiles
rm -f *.o *.mod
make ${COMPILER}_64
sudo cp -f ./raster_percentiles ${INSTALL_DIR}/raster_percentiles_$VER
rm -f *.o *.mod raster_percentiles
cd ..

echo "raster_hours"
cd raster_hours
rm -f *.o *.mod
make ${COMPILER}_64
sudo cp -f ./raster_hours ${INSTALL_DIR}/raster_hours_$VER
rm -f *.o *.mod raster_hours
cd ..

echo "ish_to_met"
cd ish_to_met
rm -f *.o *.mod
make ${COMPILER}_64
sudo cp -f ./ish_to_met ${INSTALL_DIR}/ish_to_met_$VER
rm -f *.o *.mod ish_to_met
cd ..

echo "ws_and_wd"
cd ws_and_wd
rm -f *.o *.mod
make ${COMPILER}_64
sudo cp -f ./ws_and_wd ${INSTALL_DIR}/ws_and_wd_$VER
rm -f *.o *.mod ws_and_wd
cd ..

echo "focal_stats"
cd focal_stats
rm -f *.o *.mod
make ${COMPILER}_64
sudo cp -f ./focal_stats ${INSTALL_DIR}/focal_stats_$VER
rm -f *.o *.mod focal_stats
cd ..

echo "moving_average"
cd moving_average
rm -f *.o *.mod
make ${COMPILER}_64
sudo cp -f ./moving_average ${INSTALL_DIR}/moving_average_$VER
rm -f *.o *.mod moving_average
cd ..

echo "spfh_to_rh"
cd spfh_to_rh
rm -f *.o *.mod
make ${COMPILER}_64
sudo cp -f ./spfh_to_rh ${INSTALL_DIR}/spfh_to_rh_$VER
rm -f *.o *.mod spfh_to_rh
cd ..

echo "rotate_dem"
cd rotate_dem
rm -f *.o *.mod
make ${COMPILER}_64
sudo cp -f ./rotate_dem ${INSTALL_DIR}/rotate_dem_$VER
rm -f *.o *.mod rotate_dem
cd ..

echo "csv_percentiles"
cd csv_percentiles
rm -f *.o *.mod
make ${COMPILER}_64
sudo cp -f ./csv_percentiles ${INSTALL_DIR}/csv_percentiles_$VER
rm -f *.o *.mod csv_percentiles
cd ..
