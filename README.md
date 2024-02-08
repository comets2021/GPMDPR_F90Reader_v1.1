# About GPM/DPR F90Reader (2A.DPR)

The package, GPM/DPR F90Reader, consists of F90 codes to create monthly and annual mean Level3 datasets from the Level2 GPM/DPR granule files. Several major output formats are supported (e.g., NetCDF4, GrADS binary, and plain ASCII for Generic Mapping Tools and Gnuplot) to visualize the climatology from GPM/DPR. The package also includes multi-parameter statistics (CFAD; Contoured Frequency by Altitude Diagram) for more process-oriented model evaluation. Visualization scripts are also prepared in the package.


## Required Library

- [HDF5 Library](https://www.hdfgroup.org/solutions/hdf5/)
- [NetCDF4 Library](https://downloads.unidata.ucar.edu/netcdf/)
- [GrADS (Grid Analysis and Display System)](http://cola.gmu.edu/grads/) [OPTIONAL]
- [GMT6 (The Generic Mapping Tools)](https://www.generic-mapping-tools.org) [OPTIONAL]
- [Gnuplot](http://www.gnuplot.info) [OPTIONAL]
- [GTOOL3/DCL5 Library](https://www.gfd-dennou.org/library/gtool/index.htm.en) [OPTIONAL]


## Supported OS/Machine

- AlmaLinux release 8.8 system (36CPU, 512GB Memory)
- MacOS release 12.3 or later (10CPU/16GPU, M2Pro 32GB Memory)


## Package Structure

The repository includes directories below:
- `src/` contains the F90 routines to read the original GPM/DPR dataset using the HDF5 library.
- `output/` is the place for output, and it also contains example scripts for visualization.

Users will need to add the GPM/DPR data and specify the directory path at `src/main.sh` (see [Test Run](#test-run) guide below for details).


## Test Run

1. Building the drivers
    1. Edit the files `src/` if necessary. By default, the package is built using Intel Fortran Compilar (ifort) on the AlmaLinux release 8.8 system.
    1. In `src/`, edit `Makefile` to reflect the choice of compiler, optimization level, compiler flags, and library names and locations associated with the HDF5 and NetCDF4.
    1. In `src/`, edit `main.sh` to specify the path of the original GPM/DPR HDF5 data and analysis period.
    1. In `src/`, edit `gpm_driver.f90` and `parameters.f90` to modify the horizontal resolution of outputs (reso), scan-mode (i_scanmode), and output options (e.g., outascii, odump3d).

1. Running the test program
    1. Compilation test. In `src/`, `make` will build the main driver. If the compile passes through, the execution file `gpm_driver` will appear in the same directory.
    1. Execution. In `src/`, `./main.sh` will start to read the HDF files, and automatically create monthly mean, annual mean, and mean over the whole period.
    1. Plot. Outputs can be found at `output/Level3/` for various formats. Details of the [output variables](#available-outputs) and [how to visualize the output](#visualization) are summarized below.


## Available Outputs

**Variables included in this package<sup>1</sup>**
| Variable | HDF5 Attributes | Description | Units |
| -------------- | -------------- | ------------------------- | ------- |
| n_obs          | -                             | \# of observed pixels within the grid.  | none |
| prcp_sfc_uc    | /FS/SLV/precipRateESurface    | surface precipitation (unconditional) | mm hr<sup>-1</sup> |
| prcp_sfc_co    | -                             | surface precipitation (conditional)  | mm hr<sup>-1</sup> |
| prcpl_path     | /FS/SLV/precipWaterIntegrated | rain water path | g m<sup>-2</sup> |
| prcps_path     | /FS/SLV/precipWaterIntegrated | snow water path | g m<sup>-2</sup> |
| pdf_precip     | -                             | occurrence frequency of precipitation (PoP) | % |
| pdf_rain       | -                             | occurrence of rain for precipitating column | % |
| pdf_snowsfc    | /FS/Experimental/flagSurfaceSnowfall | occurrence frequency of snowfall | % |
| pdf_grpl       | /FS/Experimental/flagGraupelHail | occurrence frequency of graupel | % |
| pdf_grpsnow    | -                             | cases where both snow and graupel are detected | % |
| pdf_hip*       | /FS/CSF/flagHeavyIcePrecip | occurrence frequencies of heavy ice precipitation | % |
| cfad_Ka        | /FS/PRE/zFactorMeasured | Ka band CFAD | dBZ |
| cfad_Ku        | /FS/PRE/zFactorMeasured | Ku band CFAD | dBZ |

<sup>1</sup>monthly, annual, and mean over the whole period are available


## Visualization

- GrADS

  `$ cd output/Level3/grads/`

  `$ grads`

  `ga-> run plot_map2d_grads.gs`

  for GrADS binary format, or

  `ga-> run plot_map2d_grads_nc.gs`

  for NetCDF format. The result is the same.

- Generic Mapping Tools (GMT6)

  `$ cd output/Level3/gmt/`

  `$ ./plot_map2d_gmt6.sh gpm_map2d_202001.txt`

- Gnuplot

  `$ cd output/Level3/gnuplot/`

  `$ ./plot_map2d.sh`

- GTOOL3

  `$ cd output/Level3/gtool/`

  `$ gtcont pdf_precip_2020 map=1 color=20 -nocnt`


## Contributions and Bug Report

Please report any bugs to [Takuro Michibata](https://orcid.org/0000-0002-1491-0297) via E-mail or the [developer team](https://github.com/comets2021) via pull request if you find a defect in the package.


## References

- [GPM/DPR Level 2 Algorithm Theoretical Basis Document (ATBD)](https://gpm.nasa.gov/resources/documents/gpmdpr-level-2-algorithm-theoretical-basis-document-atbd)
- [G-Portal data download system](https://gportal.jaxa.jp/gpr/index)


## License

This work is distributed under the Creative Commons Attribution 4.0 License.

[![License: CC BY-SA 4.0](https://img.shields.io/badge/License-CC%20BY--SA%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by-sa/4.0/)
