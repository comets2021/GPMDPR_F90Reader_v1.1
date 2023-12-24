#!/bin/sh

exe=gpm_driver
data=../testdata
filelist=filelist.txt
#-------------------------------
make clean
rm -f ${filelist}
make

#--- Preprocess for Input Data Preparation:
for year in 2014 2015 2016 2017 2018 2019 2020 2021 2022 ; do
    for month in 01 02 03 04 05 06 07 08 09 10 11 12 ; do
        echo "================================================"
        echo "             Input Data:  " ${year}/${month}
        echo "================================================"
        #--- Prepare the input filename list
        find ${data}/${year}/${month} | sort | grep h5 >> ${filelist}
    done
done

#--- Main Procedures:
echo '*** EXECUTION MAIN'
./${exe} ${filelist}

#--- Finalize:
make clean
rm -f ${filelist}

exit 0
