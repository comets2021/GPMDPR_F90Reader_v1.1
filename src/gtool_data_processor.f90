module gtool_data_processor
   use parameters
   use netcdf_writer
   use pdfs

   private outgt3

contains

! Summary Output for GTOOL (Optional)
subroutine output_gtool_data( &
      ooutyr, ooutmon, ofinal, &
      ym, year, &
      glon, glat, &
      avgstr_y, avgstr, avgend, obsstr, imon, &
      regmap, n_obs, n_obs_all, &
      ofirst_gt, &
      plot_tool)

   implicit none

   logical, intent(in) :: ooutyr, ooutmon, ofinal
   character(len=*), intent(in) :: ym, year
   character(len=*), intent(in) :: avgstr_y, avgstr, avgend, obsstr
   integer, intent(in) :: imon

   real(sp), dimension(:), intent(in) :: glon                 ! grid value (longitude)
   real(sp), dimension(:), intent(in) :: glat                 ! grid value (latitude)
   real(sp), dimension(:, :), intent(in) :: regmap
   real(sp), dimension(:, :, 0:), intent(in) :: n_obs
   real(sp), dimension(:, :), intent(in) :: n_obs_all

   logical, intent(inout) :: ofirst_gt

   character(len=*), intent(in) :: plot_tool
   character(len = 128) :: outfname, file_prefix

   !--- Main Routine
   integer :: iix                                  ! for GTOOL GLON (axis modification)
   integer :: ix, iy                               ! dimension number for lon/lat
   integer :: nx, ny, nz                           ! dimension number for lon/lat/lev

   !--- GrADS/GTOOL Configs
   integer :: id, itime, ivar, ionum
   character(len = 64) :: gtfp
   character(len = 16) :: clev, clon, clat
   character(len = 16) :: datstr, datend, cdate
   character(len = 16) :: cdset, citem, cunit, cdum
   character(len = 32) :: cvar
   real(sp), dimension( dimx,dimy ) :: gtdata2d, gtdata2d_ann, gtdata2d_all, dum2d, dum2d_ann, dum2d_all
   data  cdset     / "GPM V07A 2A.DPR " /

   if ( ogtool ) then
      write( *,* ) "*** GTOOL OUTPUT"
      nx = dimx
      ny = dimy
      nz = 1
      itime = 1
      datstr = trim( avgstr )
      datend = trim( avgend )
      cdate  = "00000000 000000"
      gtfp = trim(outpath_lev3) // "/gtool/"
      if ( ofirst_gt .and. ofinal ) then
         ionum = 3
      elseif ( ofirst_gt ) then
         ionum = 2
      elseif ( ofinal ) then
         ionum = 4
      else
         ionum = 1
      endif
      ofirst_gt = .false.

      if ( dimx .eq. 144 ) then
         clon = "GLON144"
      elseif ( dimx .eq. 240 ) then
         clon = "GLON240"
      else
         write( *,* ) "### GTOOL AXIS NOT SUPPORTED! << CLON >>"
      endif
      if ( dimy .eq. 72 ) then
         clat = "GLAT72I"
      elseif ( dimy .eq. 120 ) then
         clat = "GLAT120I"
      else
         write( *,* ) "### GTOOL AXIS NOT SUPPORTED! << CLAT >>"
      endif
      clev = "SFC1"

      do ivar = 1, nvar
         id = 60 + ivar
         if ( ivar .eq. 1 ) then
            cvar  = "# of Observation"
            citem = "n_obs"
            cunit = "#"
            gtdata2d(:,:) = n_obs(:,:,imon)
            gtdata2d_ann(:,:) = n_obs(:,:,0)
            gtdata2d_all(:,:) = n_obs_all(:,:)
         elseif ( ivar .eq. 2 ) then
            cvar  = "Occ Freq of Precip"
            citem = "pdf_precip"
            cunit = "%"
            gtdata2d(:,:) = pdf_precip(:,:,imon)
            gtdata2d_ann(:,:) = pdf_precip(:,:,0)
            gtdata2d_all(:,:) = pdf_precip_all(:,:)
         elseif ( ivar .eq. 3 ) then
            cvar  = "Occ Freq of Rain"
            citem = "pdf_rain"
            cunit = "%"
            gtdata2d(:,:) = pdf_rain(:,:,imon)
            gtdata2d_ann(:,:) = pdf_rain(:,:,0)
            gtdata2d_all(:,:) = pdf_rain_all(:,:)
         elseif ( ivar .eq. 4 ) then
            cvar  = "precipRateESurface"
            citem = "prcp_sfc_co"
            cunit = "mm/hr (con)"
            gtdata2d(:,:) = mean_precip_surf(:,:,1,imon)
            gtdata2d_ann(:,:) = mean_precip_surf(:,:,1,0)
            gtdata2d_all(:,:) = mean_precip_surf_all(:,:,1)
         elseif ( ivar .eq. 5 ) then
            cvar  = "precipRateESurface"
            citem = "prcp_sfc_uc"
            cunit = "mm/hr (ucon)"
            gtdata2d(:,:) = mean_precip_surf(:,:,2,imon)
            gtdata2d_ann(:,:) = mean_precip_surf(:,:,2,0)
            gtdata2d_all(:,:) = mean_precip_surf_all(:,:,2)
         elseif ( ivar .eq. 6 ) then
            cvar  = "precipWaterIntegrated1"
            citem = "prcpl_path"
            cunit = "g/m2"
            gtdata2d(:,:) = mean_precip_path(:,:,1,imon)
            gtdata2d_ann(:,:) = mean_precip_path(:,:,1,0)
            gtdata2d_all(:,:) = mean_precip_path_all(:,:,1)
         elseif ( ivar .eq. 7 ) then
            cvar  = "precipWaterIntegrated2"
            citem = "prcps_path"
            cunit = "g/m2"
            gtdata2d(:,:) = mean_precip_path(:,:,2,imon)
            gtdata2d_ann(:,:) = mean_precip_path(:,:,2,0)
            gtdata2d_all(:,:) = mean_precip_path_all(:,:,2)
         elseif ( ivar .eq. 8 ) then
            cvar  = "Occ Freq of Graupel"
            citem = "pdf_grpl"
            cunit = "%"
            gtdata2d(:,:) = pdf_grpl(:,:,imon)
            gtdata2d_ann(:,:) = pdf_grpl(:,:,0)
            gtdata2d_all(:,:) = pdf_grpl_all(:,:)
         elseif ( ivar .eq. 9 ) then
            cvar  = "Occ Freq of Snowfall"
            citem = "pdf_snowsfc"
            cunit = "%"
            gtdata2d(:,:) = pdf_snowsfc(:,:,imon)
            gtdata2d_ann(:,:) = pdf_snowsfc(:,:,0)
            gtdata2d_all(:,:) = pdf_snowsfc_all(:,:)
         elseif ( ivar .eq. 10 ) then
            cvar  = "Occ Freq of HeavyIcePrecip0"
            citem = "pdf_hip0"
            cunit = "%"
            gtdata2d(:,:) = pdf_hip0(:,:,imon)
            gtdata2d_ann(:,:) = pdf_hip0(:,:,0)
            gtdata2d_all(:,:) = pdf_hip0_all(:,:)
         elseif ( ivar .eq. 11 ) then
            cvar  = "Occ Freq of HeavyIcePrecip1"
            citem = "pdf_hip1"
            cunit = "%"
            gtdata2d(:,:) = pdf_hip1(:,:,imon)
            gtdata2d_ann(:,:) = pdf_hip1(:,:,0)
            gtdata2d_all(:,:) = pdf_hip1_all(:,:)
         elseif ( ivar .eq. 12 ) then
            cvar  = "Occ Freq of HeavyIcePrecip2"
            citem = "pdf_hip2"
            cunit = "%"
            gtdata2d(:,:) = pdf_hip2(:,:,imon)
            gtdata2d_ann(:,:) = pdf_hip2(:,:,0)
            gtdata2d_all(:,:) = pdf_hip2_all(:,:)
         elseif ( ivar .eq. 13 ) then
            cvar  = "Occ Freq of HeavyIcePrecip3"
            citem = "pdf_hip3"
            cunit = "%"
            gtdata2d(:,:) = pdf_hip3(:,:,imon)
            gtdata2d_ann(:,:) = pdf_hip3(:,:,0)
            gtdata2d_all(:,:) = pdf_hip3_all(:,:)
         elseif ( ivar .eq. 14 ) then
            cvar  = "Occ Freq of HeavyIcePrecip4"
            citem = "pdf_hip4"
            cunit = "%"
            gtdata2d(:,:) = pdf_hip4(:,:,imon)
            gtdata2d_ann(:,:) = pdf_hip4(:,:,0)
            gtdata2d_all(:,:) = pdf_hip4_all(:,:)
         elseif ( ivar .eq. 15 ) then
            cvar  = "Occ Freq of HeavyIcePrecip8"
            citem = "pdf_hip8"
            cunit = "%"
            gtdata2d(:,:) = pdf_hip8(:,:,imon)
            gtdata2d_ann(:,:) = pdf_hip8(:,:,0)
            gtdata2d_all(:,:) = pdf_hip8_all(:,:)
         elseif ( ivar .eq. 16 ) then
            cvar  = "Occ Freq of HeavyIcePrecip12"
            citem = "pdf_hip12"
            cunit = "%"
            gtdata2d(:,:) = pdf_hip12(:,:,imon)
            gtdata2d_ann(:,:) = pdf_hip12(:,:,0)
            gtdata2d_all(:,:) = pdf_hip12_all(:,:)
         elseif ( ivar .eq. 17 ) then
            cvar  = "Occ Freq of HeavyIcePrecip16"
            citem = "pdf_hip16"
            cunit = "%"
            gtdata2d(:,:) = pdf_hip16(:,:,imon)
            gtdata2d_ann(:,:) = pdf_hip16(:,:,0)
            gtdata2d_all(:,:) = pdf_hip16_all(:,:)
         elseif ( ivar .eq. 18 ) then
            cvar  = "Occ Freq of HeavyIcePrecip25"
            citem = "pdf_hip25"
            cunit = "%"
            gtdata2d(:,:) = pdf_hip25(:,:,imon)
            gtdata2d_ann(:,:) = pdf_hip25(:,:,0)
            gtdata2d_all(:,:) = pdf_hip25_all(:,:)
         elseif ( ivar .eq. 19 ) then
            cvar  = "Occ Freq of HeavyIcePrecip30"
            citem = "pdf_hip30"
            cunit = "%"
            gtdata2d(:,:) = pdf_hip30(:,:,imon)
            gtdata2d_ann(:,:) = pdf_hip30(:,:,0)
            gtdata2d_all(:,:) = pdf_hip30_all(:,:)
         elseif ( ivar .eq. 20 ) then
            cvar  = "Occ Freq of Graupel with Snow"
            citem = "pdf_grpsnow"
            cunit = "%"
            gtdata2d(:,:) = pdf_grpsnow(:,:,imon)
            gtdata2d_ann(:,:) = pdf_grpsnow(:,:,0)
            gtdata2d_all(:,:) = pdf_grpsnow_all(:,:)
         endif
         ! AXIS Modification for GLON (-180:180 => 0:360)
         dum2d    ( :,: ) = gtdata2d    ( :,: )  ! buffer
         dum2d_ann( :,: ) = gtdata2d_ann( :,: )  ! buffer
         dum2d_all( :,: ) = gtdata2d_all( :,: )  ! buffer
         do ix = 1, dimx
            ! Shift 
            if ( ix .le. dimx/2 ) then
               iix = ix + dimx/2
            else
               iix = ix - dimx/2
            endif
            gtdata2d    ( iix,: ) = dum2d    ( ix,: )
            gtdata2d_ann( iix,: ) = dum2d_ann( ix,: )
            gtdata2d_all( iix,: ) = dum2d_all( ix,: )
         enddo
         ! Monthly Output
         if ( ooutmon ) then
            datstr = trim( avgstr )
            call outgt3( id,      gtfp,    citem,   gtdata2d,                 &
                         nx,      ny,      nz,      itime,                    &
                         datstr,  datend,  cdset,   cdate,    cvar,    cunit, &
                         clon,    clat,    clev,    citem,    ionum           )
         endif
         ! Annual Output
         if ( ooutyr ) then
            id = id + 100
            cdum = trim( citem ) // "_" // trim( year )
            datstr = trim( avgstr_y )
            call outgt3( id,      gtfp,    cdum,    gtdata2d_ann,             &
                         nx,      ny,      nz,      itime,                    &
                         datstr,  datend,  cdset,   cdate,    cvar,    cunit, &
                         clon,    clat,    clev,    citem,    3               )
         endif
         ! All-Term Output
         if ( ofinal ) then
            id = id + 200
            cdum = trim( citem ) // "_all"
            datstr = trim( obsstr )
            call outgt3( id,      gtfp,    cdum,    gtdata2d_all,             &
                         nx,      ny,      nz,      itime,                    &
                         datstr,  datend,  cdset,   cdate,    cvar,    cunit, &
                         clon,    clat,    clev,    citem,    3               )
         endif
      enddo

      file_prefix = trim(outpath_lev3) // "/" // plot_tool // "/" // "gpm_map2d_"
      !! Monthly Output
      if ( ooutmon ) then
         outfname = trim(file_prefix) // trim(ym) // file_suffix_nc
         call output_netcdf_map2d( &
             outpath_lev3, year, &
             glon,                              glat,                              & ! 01-02
             n_obs           ( :, :, imon ),                                       & ! 03
             pdf_precip      ( :, :, imon ),    pdf_rain( :, :, imon ),            & ! 04-05
             mean_precip_surf( :, :, 1, imon ), mean_precip_surf( :, :, 2, imon ), & ! 06-07
             mean_precip_path( :, :, 1, imon ), mean_precip_path( :, :, 2, imon ), & ! 08-09
             pdf_grpl        ( :, :, imon ),    pdf_snowsfc( :, :, imon ),         & ! 10-11
             pdf_hip0        ( :, :, imon ),    pdf_hip1 ( :, :, imon ),           & ! 12-13
             pdf_hip2        ( :, :, imon ),    pdf_hip3 ( :, :, imon ),           & ! 14-15
             pdf_hip4        ( :, :, imon ),    pdf_hip8 ( :, :, imon ),           & ! 16-17
             pdf_hip12       ( :, :, imon ),    pdf_hip16( :, :, imon ),           & ! 18-19
             pdf_hip25       ( :, :, imon ),    pdf_hip30( :, :, imon ),           & ! 20-21
             pdf_grpsnow     ( :, :, imon ),                                       & ! 22
             outfname, plot_tool, output_type='monthly', imon=imon)
      endif

      !! Annual Output
      if ( ooutyr ) then
         outfname = trim(file_prefix) // trim(year) // file_suffix_nc
         call output_netcdf_map2d( &
             outpath_lev3, year, &
             glon,                           glat,                           & ! 01-02
             n_obs           ( :, :, 0 ),                                    & ! 03
             pdf_precip      ( :, :, 0 ),    pdf_rain( :, :, 0 ),            & ! 04-05
             mean_precip_surf( :, :, 1, 0 ), mean_precip_surf( :, :, 2, 0 ), & ! 06-07
             mean_precip_path( :, :, 1, 0 ), mean_precip_path( :, :, 2, 0 ), & ! 08-09
             pdf_grpl        ( :, :, 0 ),    pdf_snowsfc( :, :, 0 ),         & ! 10-11
             pdf_hip0        ( :, :, 0 ),    pdf_hip1 ( :, :, 0 ),           & ! 12-13
             pdf_hip2        ( :, :, 0 ),    pdf_hip3 ( :, :, 0 ),           & ! 14-15
             pdf_hip4        ( :, :, 0 ),    pdf_hip8 ( :, :, 0 ),           & ! 16-17
             pdf_hip12       ( :, :, 0 ),    pdf_hip16( :, :, 0 ),           & ! 18-19
             pdf_hip25       ( :, :, 0 ),    pdf_hip30( :, :, 0 ),           & ! 20-21
             pdf_grpsnow     ( :, :, 0 ),                                    & ! 22
             outfname, plot_tool, output_type='annual')
      endif

      !! All-Term Output
      if ( ofinal ) then

         outfname = trim(file_prefix) // "all" // file_suffix_nc
         call output_netcdf_map2d( &
             outpath_lev3, year, &
             glon,                            glat,                            & ! 01-02
             n_obs_all           ( :, : ),                                     & ! 03
             pdf_precip_all      ( :, : ),    pdf_rain_all( :, : ),            & ! 04-05
             mean_precip_surf_all( :, :, 1 ), mean_precip_surf_all( :, :, 2 ), & ! 06-07
             mean_precip_path_all( :, :, 1 ), mean_precip_path_all( :, :, 2 ), & ! 08-09
             pdf_grpl_all        ( :, : ),    pdf_snowsfc_all( :, : ),         & ! 10-11
             pdf_hip0_all        ( :, : ),    pdf_hip1_all ( :, : ),           & ! 12-13
             pdf_hip2_all        ( :, : ),    pdf_hip3_all ( :, : ),           & ! 14-15
             pdf_hip4_all        ( :, : ),    pdf_hip8_all ( :, : ),           & ! 16-17
             pdf_hip12_all       ( :, : ),    pdf_hip16_all( :, : ),           & ! 18-19
             pdf_hip25_all       ( :, : ),    pdf_hip30_all( :, : ),           & ! 20-21
             pdf_grpsnow_all     ( :, : ),                                     & ! 22
             outfname, plot_tool, output_type='all')
      endif
   endif  ! ogtool

   if (ofinal) then ! ofinal
      ! Region Map
      ! AXIS Modification for GLON (-180:180 => 0:360)
      dum2d( :,: ) = regmap( :,: )  ! buffer
      do ix = 1, dimx
         ! Shift
         if ( ix .le. dimx/2 ) then
            iix = ix + dimx/2
         else
            iix = ix - dimx/2
         endif
         gtdata2d    ( iix,: ) = dum2d    ( ix,: )
      enddo
      id = 199
      citem = "regmap"
      datstr = "00000000 0000"
      datend = "00000000 0000"
      cvar = "regid"
      cunit = "region ID"
      call outgt3( id,      gtfp,    citem,   gtdata2d,                 &
                   nx,      ny,      nz,      itime,                    &
                   datstr,  datend,  cdset,   cdate,    cvar,    cunit, &
                   clon,    clat,    clev,    citem,    3               )
   endif

end subroutine output_gtool_data

! PACKAGE OUTPUT !!
!
!   [HIS] 19/07/10 (michibata) for MIROC SCM configuration
!         21/05/12 (michibata) add ascii output option
!         22/08/24 (michibata) F77 to F90 for GPM output
!
!************************************************************************
!************************************************************************
SUBROUTINE OUTGT3  &
               & (  IONUM,  GTFP,   GTFN,  GTDATA,                     &
               &    DIMX,   DIMY,   DIMZ,  ITIME,                      &
               &    DATSTR, DATEND, DSET,  DDATE,  DNAME,  DUNIT,      &
               &    AXLON,  AXLAT,  AXLEV, ITEM,   IOMODE              )

   IMPLICIT NONE

!   [INPUT]
   CHARACTER(LEN=64) :: GTFP               !! GTOOL I/O
   CHARACTER(LEN=16) :: GTFN               !! GTOOL I/O
   INTEGER :: IONUM                        !! I/O file unit number
   INTEGER :: DIMX                         !! dimension size of X
   INTEGER :: DIMY                         !! dimension size of Y
   INTEGER :: DIMZ                         !! dimension size of Z
   INTEGER :: ITIME                        !! time
   INTEGER :: IOMODE                       !! I/O mode:
                                             ! --> 1 = WRITE ONLY
                                             ! --> 2 = OPEN AND WRITE
                                             ! --> 3 = OPEN, WRITE, AND CLOSE
                                             ! --> 4 = WRITE AND CLOSE
   real(sp)  :: GTDATA(DIMX,DIMY,DIMZ)      !! output GTOOL data
   CHARACTER(LEN=16) :: ITEM               !! input var name (item)
   CHARACTER(LEN=16) :: DATSTR             !! data analysis start date
   CHARACTER(LEN=16) :: DATEND             !! data analysis end date
   CHARACTER(LEN=16) :: DSET               !! data product name (title)
   CHARACTER(LEN=16) :: DDATE              !! date: e.g. "20000101 000000 "
   CHARACTER(LEN=32) :: DNAME              !! data name (title)
   CHARACTER(LEN=16) :: DUNIT              !! data unit
   CHARACTER(LEN=16) :: AXLON              !! axis name used for longitude (x-axis)
   CHARACTER(LEN=16) :: AXLAT              !! axis name used for latitude  (y-axis) 
   CHARACTER(LEN=16) :: AXLEV              !! axis name used for altitude  (z-axis)

!   [INTERNAL WORK]
   CHARACTER(LEN=16), dimension(64) :: GTHEAD           !! GTOOL header
   CHARACTER(LEN=16) :: CAUTHOR                         !! program author
   CHARACTER(LEN=16) :: CMAKER                          !! data provider
   DATA  CMAKER   / "GPM V07A 2A.DPR " /
   DATA  CAUTHOR  / "Takuro Michibata" /

   WRITE(*,*) "    *** ITEM = ", TRIM(ITEM)

   !--- I/O diagnostic:
   IF ( IOMODE == 2 .OR. IOMODE == 3 ) THEN
      OPEN ( IONUM, FILE = TRIM(GTFP) // TRIM(GTFN),   &
   &          FORM = "UNFORMATTED", STATUS = "UNKNOWN", &
   &          ACTION = "WRITE", CONVERT = "BIG_ENDIAN"  )
   ENDIF

   !--- GTOOL3 output:
   GTHEAD(:) = ' '
   ITEM  = TRIM( adjustl(ITEM) )
   WRITE(GTHEAD( 1),'(i16)') 9010
   WRITE(GTHEAD( 2),'(a16)') DSET                !! product/dataset
   WRITE(GTHEAD( 3),'(a16)') ITEM                !! input var name (item)
   WRITE(GTHEAD(12),'(i16)') 1
   WRITE(GTHEAD(13),'(i16)') 1
   WRITE(GTHEAD(14),'(a16)') DNAME( 1:16)        !! title1
   WRITE(GTHEAD(15),'(a16)') DNAME(17:32)        !! title2
   WRITE(GTHEAD(16),'(a16)') DUNIT               !! unit
   WRITE(GTHEAD(25),'(i16)') ITIME               !! time
   WRITE(GTHEAD(26),'(a16)') 'MONTH           '  !! utime
   WRITE(GTHEAD(27),'(a16)') DDATE               !! date
   WRITE(GTHEAD(28),'(i16)') 1                   !! tdur
   WRITE(GTHEAD(29),'(a16)') AXLON               !! aitm1
   WRITE(GTHEAD(30),'(i16)') 1                   !! astr1 
   WRITE(GTHEAD(31),'(i16)') DIMX                !! aend1
   WRITE(GTHEAD(32),'(a16)') AXLAT               !! aitm2
   WRITE(GTHEAD(33),'(i16)') 1                   !! astr2
   WRITE(GTHEAD(34),'(i16)') DIMY                !! aend2
   WRITE(GTHEAD(35),'(a16)') AXLEV               !! aitm3
   WRITE(GTHEAD(36),'(i16)') 1                   !! astr3
   WRITE(GTHEAD(37),'(i16)') DIMZ                !! aend3
   WRITE(GTHEAD(38),'(a16)') 'UR4             '  !! format
   WRITE(GTHEAD(39),'(a16)') ' -0.9990000E+03 '  !! UNDEF
   WRITE(GTHEAD(40),'(a16)') ' -0.9990000E+03 '  !! UNDEF
   WRITE(GTHEAD(41),'(a16)') ' -0.9990000E+03 '  !! UNDEF
   WRITE(GTHEAD(42),'(a16)') ' -0.9990000E+03 '  !! UNDEF
   WRITE(GTHEAD(43),'(a16)') ' -0.9990000E+03 '  !! UNDEF
   IF ( TRIM(DNAME) .EQ. 'pressure' ) THEN
      WRITE(GTHEAD(44),'(i16)') -1               !! STYP
   ELSE
      WRITE(GTHEAD(44),'(i16)') 1                !! STYP
   ENDIF
   WRITE(GTHEAD(48),'(a16)') DATSTR              !! start date
   WRITE(GTHEAD(49),'(a16)') DATEND              !! end date
   WRITE(GTHEAD(50),'(a16)') CMAKER              !! memo1
   WRITE(GTHEAD(61),'(a16)') CAUTHOR             !! author
   WRITE(GTHEAD(63),'(a16)') CAUTHOR             !! modifier
   WRITE(GTHEAD(64),'(i16)') DIMX*DIMY*DIMZ      !! size

   WRITE(IONUM) GTHEAD
   WRITE(IONUM) GTDATA

   IF ( IOMODE == 3 .OR. IOMODE == 4 ) THEN
      CLOSE(IONUM)
   ENDIF

   RETURN
END SUBROUTINE OUTGT3

end module gtool_data_processor
