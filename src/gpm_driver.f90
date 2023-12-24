!=======================================================================!
  !--- Purpose:
  !      * Create Levels-3 and 4 products from the GPM/DPR Level2 (V07A)
  !
  !--- Required:
  !      * HDF5 Library
  !        (https://portal.hdfgroup.org/display/support)
  !      * NetCDF4 Library
  !        (https://www.unidata.ucar.edu/software/netcdf/)
  !        - netCDF:         Version 4.9.2 or later
  !        - netCDF-Fortran: Version 4.6.1 or later
  !      * GTOOL3 Library (Optional)
  !      * Input Argument:
  !          - Filelist for input HDF5 files
  !
  !--- History:
  !      * Jul 25, 2022 - Takuro Michibata: Test Run
  !      * Sep 07, 2022 - Takuro Michibata: Inline Generators
  !                                         - Global Maps
  !                                         - CFADs
  !      * Oct 12, 2022 - Takuro Michibata: Monthly/Annual Output
  !      * Dec 03, 2022 - Takuro Michibata: First Release via GitHub
  !      * Nov 30, 2023 - Takuro Michibata: NetCDF Output
  !
  !--- Contact:
  !    tmichibata@okayama-u.ac.jp
  !
  !--- License:
  !      * Creative Commons Attribution 4.0
  !
!=======================================================================!
program gpmdpr_level2

   use parameters
   use numbers
   use input
   use output

   implicit none

   integer :: imon
   integer :: n_arg, iargc, istat
   character(len =  52) :: filelist
   character(len = 128), dimension(ndset) :: varname
   character(len =   6) :: cgranule
   character(len = 128) :: fname
   character(len =   4) :: year
   character(len =   2) :: month
   character(len =   3) :: cmon
   character(len =   7) :: ym
   character(len =  16) :: obsstr, obsnext
   character(len =  16) :: avgstr_y, avgstr, avgend

   logical :: ofirst, ofirst_gt, ooutyr, ooutmon, ofinal

   integer :: nbin, nray, nscan

   real(sp), dimension(:,:), allocatable :: lat                  ! Latitude
   real(sp), dimension(:,:), allocatable :: lon                  ! Longitude
   real(sp), dimension(:,:,:), allocatable :: height             ! Height [m]
   real(sp), dimension(:,:), allocatable :: precip_surf          ! precipRateESurface [mm/hr]
   real(sp), dimension(:,:,:), allocatable :: precip_path        ! precipWaterIntegrated [g/m2]
   integer, dimension(:,:), allocatable :: flag_graupel          ! flagGraupelHail
                                                                   ! (0 = no grpl or hail;
                                                                   !  1 = grpl or hail is detected)
   integer, dimension(:,:), allocatable :: flag_SurfSnow         ! flagSurfaceSnowfall
                                                                   ! (0 = no snowfall;
                                                                   !  1 = snowfall is detected)
   integer, dimension(:,:), allocatable :: flag_HeavyIcePre      ! flagHeavyIcePrecip
                                                                   ! (0 = no heavy ice precip;
                                                                   !  1 = heavy ice precip exists)
   real(sp), dimension(:,:,:,:), allocatable :: zFactorMeasured  ! zFactorMeasured [dBZ]

   !-----------------------------------------------------------------!
   !  INLINE GENERATORS
   !-----------------------------------------------------------------!
   !--- Main Routine
   real               :: glon( ilon_max + 1 )                 ! grid value (longitude)
   real               :: glat( ilat_max + 1 )                 ! grid value (latitude)
   real               :: gdbz( idbz_max + 1 )                 ! grid value (CFAD dBZ)
   real               :: glev( ilev_max + 1 )                 ! grid value (CFAD Lev)
   integer            :: ilon, ilat                           ! grid boundary number for lon/lat
   integer            :: ix, iy                               ! dimension number for lon/lat
   integer            :: idbz, ilev                           ! dimension number for dbz/lev for CFAD
   integer            :: iix                                  ! for GTOOL GLON (axis modification)
   integer            :: nx, ny, nz                           ! dimension number for lon/lat/lev

   !--- GrADS/GTOOL Configs
   real(sp), dimension( dimx,dimy ) :: regmap

   !--- I/O Settings (Arguments, Filename, Path, etc.)
   ! Confirm the Arguments
   n_arg = iargc()
   if( n_arg /= 1 ) then
      write( *,* ) "### Wrong Arguments!"
      write( *,* ) "Usage: ./gpm_driver [FILELIST]"
      stop
   endif

   !--- For Inline Generators
   ! Initialized (Monthly Output)
   n_obs                = 0; n_rain               = 0
   n_precip_surf        = 0; sum_precip_surf      = 0.0_dp
   n_precip_path        = 0; sum_precip_path      = 0.0_dp
   n_flag_graupel0      = 0; n_flag_graupel1      = 0
   n_flag_SurfSnow0     = 0; n_flag_SurfSnow1     = 0
   n_flag_graupelsnow   = 0; n_hip                = 0
   n_flag_HeavyIcePre0  = 0; n_flag_HeavyIcePre1  = 0
   n_flag_HeavyIcePre2  = 0; n_flag_HeavyIcePre3  = 0
   n_flag_HeavyIcePre4  = 0; n_flag_HeavyIcePre8  = 0
   n_flag_HeavyIcePre12 = 0; n_flag_HeavyIcePre16 = 0
   n_flag_HeavyIcePre25 = 0; n_flag_HeavyIcePre30 = 0
   n_cfad_obs           = 0; n_cfad_bin           = 0
   ! Initialized (Annual Output)
   n_obs_all                = 0; n_rain_all               = 0
   n_precip_surf_all        = 0; sum_precip_surf_all      = 0.0_dp
   n_precip_path_all        = 0; sum_precip_path_all      = 0.0_dp
   n_flag_graupel0_all      = 0; n_flag_graupel1_all      = 0
   n_flag_SurfSnow0_all     = 0; n_flag_SurfSnow1_all     = 0
   n_flag_graupelsnow_all   = 0; n_hip_all                = 0
   n_flag_HeavyIcePre0_all  = 0; n_flag_HeavyIcePre1_all  = 0
   n_flag_HeavyIcePre2_all  = 0; n_flag_HeavyIcePre3_all  = 0
   n_flag_HeavyIcePre4_all  = 0; n_flag_HeavyIcePre8_all  = 0
   n_flag_HeavyIcePre12_all = 0; n_flag_HeavyIcePre16_all = 0
   n_flag_HeavyIcePre25_all = 0; n_flag_HeavyIcePre30_all = 0
   n_cfad_obs_all           = 0; n_cfad_bin_all           = 0
   regmap                   = 0

   ! Grid Settings
   do ilon = 1, ilon_max + 1
      glon( ilon ) = ( ilon - 1 ) * gintv - 180.0_sp
   enddo
   do ilat = 1, ilat_max + 1
      glat( ilat ) = ( ilat - 1 ) * gintv - 90.0_sp
   enddo
   do idbz = 1, idbz_max + 1
      gdbz( idbz ) = ( idbz - 1 ) * gintvdbz + cfad_dbzmin
   enddo
   do ilev = 1, ilev_max + 1
      glev( ilev ) = ( ilev - 1 ) * gintvlev + cfad_levmin
   enddo

   !--- Preprocess
   ! Populate Variable Name in Original HDF5 Files
   if ( i_scanmode .eq. 1 ) then
      varname( 1) = "/FS/Latitude"
      varname( 2) = "/FS/Longitude"
      varname( 3) = "/FS/PRE/height"
      varname( 4) = "/FS/SLV/precipRateESurface"
      varname( 5) = "/FS/SLV/precipWaterIntegrated"
      varname( 6) = "/FS/Experimental/flagGraupelHail"
      varname( 7) = "/FS/Experimental/flagSurfaceSnowfall"
      varname( 8) = "/FS/CSF/flagHeavyIcePrecip"
      varname( 9) = "/FS/PRE/zFactorMeasured"
      varname(10) = "/FS/PRE/adjustFactor"
   elseif ( i_scanmode .eq. 2 ) then
      varname( 1) = "/HS/Latitude"
      varname( 2) = "/HS/Longitude"
      varname( 3) = "/HS/PRE/height"
      varname( 4) = "/HS/SLV/precipRateESurface"
      varname( 5) = "/HS/SLV/precipWaterIntegrated"
!       varname( 6) = "/HS/Experimental/flagGraupelHail"
!       varname( 7) = "/HS/Experimental/flagSurfaceSnowfall"
      varname( 6) = "/HS/Experimental/precipRateESurface2Status"
      varname( 7) = "/HS/Experimental/precipRateESurface2Status"
      varname( 8) = "/HS/CSF/flagHeavyIcePrecip"
      varname( 9) = "/HS/PRE/zFactorMeasured"
      varname(10) = "/HS/PRE/adjustFactor"
   endif

   ! Get Arguments and Read the Filelist
   call getarg( 1, filelist )
   open( input_file_idx, file = trim( filelist ), status = "old", action = "read" )
   istat   = 0
   ofirst  = .true.
   ofirst_gt = .true.
   ooutyr  = .false.
   ooutmon = .false.
   ofinal  = .false.

   !--- Main Loop
   do
      if (ofinal) then
            close(input_file_idx)
      else
         call analyze_file_name( &
            cgranule, year, month, cmon, ym, imon, &
            obsstr, obsnext, avgstr_y, avgstr, avgend, &
            fname, &
            ooutyr, ooutmon, &
            istat, ofirst, ofinal)

         call read_HDF5_files( &
            varname, fname, &
            nbin, nray, nscan, &
            lat, lon, height, precip_surf, precip_path, &
            flag_graupel, flag_SurfSnow, flag_HeavyIcePre, zFactorMeasured)

         call output_level2( &
            year, month, cgranule, imon, &
            precip_surf, precip_path, &
            flag_graupel, flag_SurfSnow, flag_HeavyIcePre, zFactorMeasured, &
            nbin, nray, nscan, lat, lon, height, regmap)

         !--- Release the Array
         deallocate( lat, lon, precip_surf, precip_path, flag_graupel, flag_SurfSnow, flag_HeavyIcePre )
         if ( odump3d ) then !--- Optional Processes ---
            deallocate( height, zFactorMeasured )
         endif ! !--- Optional Processes (odump3d) ---

         !--- Check Output?
         if ( ooutyr .or. ooutmon ) then
            continue
         else
            cycle
         endif

      endif ! if ( ofinal )

      call output_level3( &
         ooutyr, ooutmon, ofinal, &
         ym, year, cmon, imon, &
         avgstr_y, avgstr, avgend, obsstr, &
         glon, glat, gdbz, glev, regmap, &
         ofirst_gt)

      if ( .not. ofinal ) then
         if ( ooutyr ) then
            ooutmon = .false.  ! reset flag
            ooutyr  = .false.  ! reset flag
            avgstr_y  = trim( obsnext )
            avgstr    = trim( obsnext )
            ! reset for next year
            n_obs                = 0; n_rain               = 0
            n_precip_surf        = 0; sum_precip_surf      = 0.0_dp
            n_precip_path        = 0; sum_precip_path      = 0.0_dp
            n_flag_graupel0      = 0; n_flag_graupel1      = 0
            n_flag_SurfSnow0     = 0; n_flag_SurfSnow1     = 0
            n_flag_graupelsnow   = 0; n_hip                = 0
            n_flag_HeavyIcePre0  = 0; n_flag_HeavyIcePre1  = 0
            n_flag_HeavyIcePre2  = 0; n_flag_HeavyIcePre3  = 0
            n_flag_HeavyIcePre4  = 0; n_flag_HeavyIcePre8  = 0
            n_flag_HeavyIcePre12 = 0; n_flag_HeavyIcePre16 = 0
            n_flag_HeavyIcePre25 = 0; n_flag_HeavyIcePre30 = 0
            n_cfad_obs =           0; n_cfad_bin           = 0
            cycle ! back to reading HDF file
         else
            ooutmon = .false.
            avgstr = trim( obsnext )
            cycle ! back to reading HDF file
         endif
      else
         exit
      endif

   end do ! back to reading HDF file

   stop

end program gpmdpr_level2
