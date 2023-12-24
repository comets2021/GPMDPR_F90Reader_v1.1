module parameters

  integer, parameter :: sp = kind(0.0)                      ! single precision
  integer, parameter :: dp = kind(0.0d0)                    ! double precision

  integer, parameter :: ndset = 10                          ! # of read dataset
  integer, parameter :: i_scanmode = 1                      ! 1 = FS mode (Full Scan)
  !integer, parameter :: i_scanmode = 2                     ! 2 = HS mode (High Sensitivity)

  integer, parameter :: nmon = 12
  integer, parameter :: nfreq = 2                           ! 1 = Ku band; 2 = Ka band
  integer, parameter :: LS    = 2                           ! 1 = Liquid;  2 = Solid

  integer, parameter :: input_file_idx = 11

  !--- I/O option for users
  ! output to ASCII files?
  logical :: outascii
  !data outascii / .true. /
  data outascii / .false. /

  ! output to GTOOL files for GENERATOR?
  logical :: ogtool
  data ogtool / .true. /

  ! read/write 3dim/4dim variables?
  logical :: odump3d
  data odump3d / .true. /
  !data odump3d / .false. /

  ! output directory
  character(len = 128) :: outpath_lev2, outpath_lev3
  data                    outpath_lev2  / "../output/Level2" /
  data                    outpath_lev3  / "../output/Level3" /

  !--- User Defined Configs ---
  !real, parameter    :: reso = 2.50_sp                             ! resolution for grid-box
  real, parameter    :: reso = 1.50_sp                             ! resolution for grid-box
  real, parameter    :: cfad_ddbz = 1.0_sp                         ! resolution for CFAD dBZ
  real, parameter    :: cfad_dlev = 0.25_sp                        ! resolution for CFAD Height (Unit = km)
  !real, parameter    :: cfad_dbzmin =  0.0_sp                      ! minimum value of CFAD dbZ bin
  real, parameter    :: cfad_dbzmin = 12.0_sp                      ! minimum value of CFAD dbZ bin
  real, parameter    :: cfad_dbzmax = 50.0_sp                      ! maximum value of CFAD dbZ bin
  real, parameter    :: cfad_levmin =  0.0_sp                      ! minimum value of CFAD Lev bin (Unit = km)
  real, parameter    :: cfad_levmax = 20.0_sp                      ! maximum value of CFAD Lev bin (Unit = km)
  real, parameter    :: gintv  = reso / 2.0_sp                     ! grid interval for lon/lat
  real, parameter    :: gintvdbz = cfad_ddbz / 2.0_sp              ! grid interval for radar    (Unit = dBZ)
  real, parameter    :: gintvlev = cfad_dlev / 2.0_sp              ! grid interval for altitude (Unit = km)
  integer, parameter :: ilon_max = int( 360 / ( reso / 2.0_sp ) )  ! total # of grid for longitude
  integer, parameter :: ilat_max = int( 180 / ( reso / 2.0_sp ) )  ! total # of grid for latitude
  integer, parameter :: idbz_max = int( ( cfad_dbzmax - cfad_dbzmin ) &
                                 & / ( cfad_ddbz / 2.0_sp ) )      ! total # of grid for CFAD dBZ
  integer, parameter :: ilev_max = int( ( cfad_levmax - cfad_levmin ) &
                                 & / ( cfad_dlev / 2.0_sp ) )      ! total # of grid for CFAD Lev
  integer, parameter :: dimx = int( 360 / reso )                   ! dimension size for longitude
  integer, parameter :: dimy = int( 180 / reso )                   ! dimension size for latitude
  integer, parameter :: dimdbz = idbz_max / 2                      ! dimension size for CFAD dBZ
  integer, parameter :: dimlev = ilev_max / 2                      ! dimension size for CFAD Lev
  !integer, parameter :: nreg = 3                                  ! 1=Tropics; 2=Midlatitudes; 3=Global
  integer, parameter :: nreg = 14                                  ! Based on Michibata et al. (2019, GMD)
                                                                   !-  1. Tropical Warm Pool (5S-15N,70E-135E)
                                                                   !-  2. ITCZ (5N-15N,140E-140W)
                                                                   !-  3. SPCZ (15S-5S,150E-130W)
                                                                   !-  4. North East Pacific (25N-50N,160W-135W)
                                                                   !-  5. California Stratocumulus (15N-35N,130W-110W)
                                                                   !-  6. Peruvian (10S-30S,90W-70W)
                                                                   !-  7. North Atlantic (30N-60N,45W-10W)
                                                                   !-  8. Namibian (30S-0S,25W-15E)
                                                                   !-  9. Australian (15S-35S,80E-115E)
                                                                   !- 10. Japan (25N-50N,125E-150E)
                                                                   !- 11. Eqt. cold tongue (5S-5N,130W-85W)
                                                                   !- 12. Eastern Asia (20N-40N,105E-120E)
                                                                   !- 13. Southern Ocean (40S-60S)
                                                                   !- 14. Global
  integer, parameter :: nvar = 20                                  ! total output vars
  real, parameter    :: undef = -999.0000_sp                       ! missing undef value

  character(len=4), parameter :: &
    file_suffix_bin = '.bin', &
    file_suffix_txt = '.txt', &
    file_suffix_ctl = '.ctl'
  character(len=3), parameter :: &
    file_suffix_nc  = '.nc'

end module parameters
