module output
   use netcdf
   use parameters
   use numbers
   use pdfs

   private get_regid, calc_pdf, calc_ratio

contains

subroutine output_level2( &
   year, month, cgranule, imon, &
   precip_surf, precip_path, &
   flag_graupel, flag_SurfSnow, flag_HeavyIcePre, zFactorMeasured, &
   nbin, nray, nscan, lat, lon, height, regmap)

   implicit none
   character(*), intent(in) :: year, month, cgranule
   integer, intent(in) :: nbin, nray, nscan, imon
   real(sp), dimension(:, :, :), intent(in) :: height               ! Height [m]
   real(sp), dimension(:, :), intent(in) :: precip_surf             ! precipRateESurface [mm/hr]
   real(sp), dimension(:, :, :), intent(in)  :: precip_path(:,:,:)  ! precipWaterIntegrated [g/m2]
   integer, dimension(:, :), intent(in) :: flag_graupel(:,:)        ! flagGraupelHail
                                                                    ! (0 = no grpl or hail; 1 = grpl or hail is detected)
   integer, dimension(:, :), intent(in) :: flag_SurfSnow(:,:)       ! flagSurfaceSnowfall
                                                                    ! (0 = no snowfall; 1 = snowfall is detected)
   integer, dimension(:, :), intent(in) :: flag_HeavyIcePre(:,:)    ! flagHeavyIcePrecip
                                                                    ! (0 = no heavy ice precip; 1 = heavy ice precip exists)
   real(sp), dimension(:, :, :, :), intent(in) :: zFactorMeasured   ! zFactorMeasured [dBZ]

   real(sp), dimension(:, :), intent(inout) :: lat                  ! Latitude
   real(sp), dimension(:, :), intent(inout) :: lon                  ! Longitude
   real(sp), dimension(:, :), intent(inout) :: regmap

   !--- Main Routine
   integer :: ireg, ifreq               ! dimension number for reg/freq for CFAD
   integer :: ix, iy                    ! dimension number for lon/lat
   integer :: i, j, k
   integer :: idbz, ilev                ! dimension number for dbz/lev for CFAD
   character(len = 128) :: outfname

   !--- Inline Generators (Cumulating Level2)
   do k = 1, nscan
      do j = 1, nray

         ! Search Gridbox
         if ( lon(j,k) .eq. 180.00000_sp ) lon(j,k) = lon(j,k) - 1e-2_dp
         ix = int( ( lon(j,k) + 180.0_sp ) / reso ) + 1
         iy = int( ( lat(j,k) +  90.0_sp ) / reso ) + 1
         if ( ix .gt. dimx  .or.  iy .gt. dimy  .or.  ix .le. 0 .or.  iy .le. 0 ) then
            write( *,* ) "### CANNOT DETERMINE THE GRID ID!", ix, iy, lon(j,k), lat(j,k)
            cycle
            !stop
         endif

         ireg = 0
         if ( nreg .eq. 3 ) then
            ! Define Region ID (Riley Dellaripa et al., 2021)
            if ( lat(j,k) .ge. -35.0_sp  .and.  lat(j,k) .le.  35.0_sp ) ireg = 1  ! Tropics
            if ( lat(j,k) .gt.  35.0_sp  .and.  lat(j,k) .le.  90.0_sp ) ireg = 2  ! Mid-Latitudes
            if ( lat(j,k) .lt. -35.0_sp  .and.  lat(j,k) .ge. -90.0_sp ) ireg = 2  ! Mid-Latitudes
            if ( ireg .eq. 0 ) then
               write(*,*) "### ERROR IN REGION ID: ", ireg, lat(j,k)
               stop
            endif
         elseif ( nreg .eq. 14 ) then
            ! Define Region ID (Michibata et al., 2019, GMD)
            call get_regid( lon(j,k),  & ! [IN]    longitude
                          & lat(j,k),  & ! [IN]    latitude
                          & ireg       ) ! [INOUT] region ID
            regmap( ix,iy ) = real( ireg )
         else
            write(*,*) "### ERROR IN REGION DEFINITION: ", nreg
            stop
         endif

         ! # of observation
         n_obs( ix,iy,imon ) = n_obs( ix,iy,imon ) + 1

         ! precip_surf (conditional)
         if ( precip_surf(j,k) .gt. 0.0_dp ) then
            n_precip_surf( ix,iy,1,imon ) = n_precip_surf( ix,iy,1,imon ) + 1
            sum_precip_surf( ix,iy,1,imon ) = sum_precip_surf( ix,iy,1,imon ) + precip_surf(j,k)
            ! for Rain/Snow Ratio
            if ( flag_SurfSnow(j,k) .eq. 0 ) then  ! detecting rain but not snow
               n_rain( ix,iy,imon ) = n_rain( ix,iy,imon ) + 1
            endif
         endif

         ! precip_surf (unconditional)
         if ( precip_surf(j,k) .ge. 0.0_dp ) then
            n_precip_surf( ix,iy,2,imon ) = n_precip_surf( ix,iy,2,imon ) + 1
            sum_precip_surf( ix,iy,2,imon ) = sum_precip_surf( ix,iy,2,imon ) + precip_surf(j,k)
         endif

         ! precipWaterIntegrated (Liquid)
         if ( precip_path(1,j,k) .ge. 0.0_dp ) then
            n_precip_path( ix,iy,1,imon ) = n_precip_path( ix,iy,1,imon ) + 1
            sum_precip_path( ix,iy,1,imon ) = sum_precip_path( ix,iy,1,imon ) + precip_path(1,j,k)
         endif

         ! precipWaterIntegrated (Solid)
         if ( precip_path(2,j,k) .ge. 0.0_dp ) then
            n_precip_path( ix,iy,2,imon ) = n_precip_path( ix,iy,2,imon ) + 1
            sum_precip_path( ix,iy,2,imon ) = sum_precip_path( ix,iy,2,imon ) + precip_path(2,j,k)
         endif

         ! flag_graupel
         ! Note: 0 includes missing values
         if ( flag_graupel(j,k) .eq. 0 ) then
            n_flag_graupel0( ix,iy,imon ) = n_flag_graupel0( ix,iy,imon ) + 1
         elseif ( flag_graupel(j,k) .eq. 1 ) then
            n_flag_graupel1( ix,iy,imon ) = n_flag_graupel1( ix,iy,imon ) + 1
         endif

         ! flag_SurfSnow
         ! Note: 0 includes missing values
         if ( flag_SurfSnow(j,k) .eq. 0 ) then
            n_flag_SurfSnow0( ix,iy,imon ) = n_flag_SurfSnow0( ix,iy,imon ) + 1
         elseif ( flag_SurfSnow(j,k) .eq. 1 ) then
            n_flag_SurfSnow1( ix,iy,imon ) = n_flag_SurfSnow1( ix,iy,imon ) + 1
         endif

         ! flag_HeavyIcePre
         ! Note: 0 includes missing values
         if ( flag_HeavyIcePre(j,k) .eq. 0 ) then
            n_flag_HeavyIcePre0( ix,iy,imon ) = n_flag_HeavyIcePre0( ix,iy,imon ) + 1
         elseif ( flag_HeavyIcePre(j,k) .eq. 1 ) then
            n_flag_HeavyIcePre1( ix,iy,imon ) = n_flag_HeavyIcePre1( ix,iy,imon ) + 1
         elseif ( flag_HeavyIcePre(j,k) .eq. 2 ) then
            n_flag_HeavyIcePre2( ix,iy,imon ) = n_flag_HeavyIcePre2( ix,iy,imon ) + 1
         elseif ( flag_HeavyIcePre(j,k) .eq. 3 ) then
            n_flag_HeavyIcePre3( ix,iy,imon ) = n_flag_HeavyIcePre3( ix,iy,imon ) + 1
         elseif ( flag_HeavyIcePre(j,k) .eq. 4 ) then
            n_flag_HeavyIcePre4( ix,iy,imon ) = n_flag_HeavyIcePre4( ix,iy,imon ) + 1
         elseif ( flag_HeavyIcePre(j,k) .eq. 8 ) then
            n_flag_HeavyIcePre8( ix,iy,imon ) = n_flag_HeavyIcePre8( ix,iy,imon ) + 1
         elseif ( flag_HeavyIcePre(j,k) .eq. 12 ) then
            n_flag_HeavyIcePre12( ix,iy,imon ) = n_flag_HeavyIcePre12( ix,iy,imon ) + 1
         elseif ( flag_HeavyIcePre(j,k) .eq. 16 ) then
            n_flag_HeavyIcePre16( ix,iy,imon ) = n_flag_HeavyIcePre16( ix,iy,imon ) + 1
         elseif ( flag_HeavyIcePre(j,k) .eq. 25 ) then
            n_flag_HeavyIcePre25( ix,iy,imon ) = n_flag_HeavyIcePre25( ix,iy,imon ) + 1
         elseif ( flag_HeavyIcePre(j,k) .eq. 30 ) then
            n_flag_HeavyIcePre30( ix,iy,imon ) = n_flag_HeavyIcePre30( ix,iy,imon ) + 1
         endif

         ! cases where both snow and graupel are observed
         if ( flag_SurfSnow(j,k) .eq. 1 .and. flag_graupel(j,k) .eq. 1 ) then
            n_flag_graupelsnow( ix,iy,imon ) = n_flag_graupelsnow( ix,iy,imon ) + 1
         endif

         ! CFAD
         if ( odump3d ) then !--- Optional Processes ---
            do ifreq = 1, nfreq
               do i = 1, nbin
                  if ( precip_surf(j,k) .gt. 0.0_dp                   .and. &
                     & height(i,j,k) .ge. 1.0_dp                      .and. &  ! remove ground clutter
                     & height(i,j,k) .le. cfad_levmax                 .and. &
                     & zFactorMeasured(ifreq,i,j,k) .ge. cfad_dbzmin  .and. &
                     & zFactorMeasured(ifreq,i,j,k) .lt. cfad_dbzmax        ) then
                     idbz = int( ( zFactorMeasured(ifreq,i,j,k) - cfad_dbzmin ) / cfad_ddbz ) + 1
                     ilev = int( ( height(i,j,k) - cfad_levmin ) / cfad_dlev ) + 1
                     ! Regional CFAD
                     if ( ireg .ne. 0 ) then
                        n_cfad_obs( ireg,ifreq,imon ) = n_cfad_obs( ireg,ifreq,imon ) + 1
                        n_cfad_bin( idbz,ilev,ireg,ifreq,imon ) = n_cfad_bin( idbz,ilev,ireg,ifreq,imon ) + 1
                     endif
                     ! Global CFAD
                     n_cfad_obs( nreg,ifreq,imon ) = n_cfad_obs( nreg,ifreq,imon ) + 1
                     n_cfad_bin( idbz,ilev,nreg,ifreq,imon ) = n_cfad_bin( idbz,ilev,nreg,ifreq,imon ) + 1
                  endif
               enddo
            enddo
         endif !--- Optional Processes ---

      enddo
   enddo

   !--- Output to Ascii File
   if ( outascii ) then
      outfname = trim( outpath_lev2 ) // "/" // trim(year) // "/" // trim(month) // "/" // "gpm_" // cgranule // ".txt"
      open( 21, file = trim( outfname ), status = "unknown", action = "write" )
      write( *,* ) "*** FILENAME (OUT): ", trim( outfname )
      write( *,* ) "*** Output to text file..."
      do k = 1, nscan
         do j = 1, nray
            write( 21,* ) lon(j,k), lat(j,k), &
                        & precip_surf(j,k),   &
                        & precip_path(1,j,k), &
                        & precip_path(2,j,k), &
                        & flag_graupel(j,k),  &
                        & flag_SurfSnow(j,k), &
                        & flag_HeavyIcePre(j,k)
         enddo
      enddo
      close( 21 )
      write( *,* ) "    ==> Completed!"
      write( *,* )
   endif

end subroutine output_level2

subroutine get_regid( r4lon, r4lat, id )
   implicit none

   real(sp), intent(in)    :: r4lon
   real(sp), intent(in)    :: r4lat
   integer, intent(inout) :: id
   real :: lon, lat

   lon = r4lon
   lat = r4lat
   if( lon < 0 ) lon = lon + 360.0_dp  !! -180 to 180 --> 0 to 360
   ! region definition following Michibata et al. (2019, GMD)
   if ( lon >=  70.0_sp .and. lon <= 135.0_sp .and. lat >=  -5.0_sp .and. lat <=  15.0_sp ) id =  1  ! Tropical Warm Pool (5S-15N,70E-135E)
   if ( lon >= 140.0_sp .and. lon <= 220.0_sp .and. lat >=   5.0_sp .and. lat <=  15.0_sp ) id =  2  ! ITCZ (5N-15N,140E-140W)
   if ( lon >= 150.0_sp .and. lon <= 230.0_sp .and. lat >= -15.0_sp .and. lat <=  -5.0_sp ) id =  3  ! SPCZ (15S-5S,150E-130W)
   if ( lon >= 200.0_sp .and. lon <= 225.0_sp .and. lat >=  25.0_sp .and. lat <=  50.0_sp ) id =  4  ! North East Pacific (25N-50N,160W-135W)
   if ( lon >= 230.0_sp .and. lon <= 250.0_sp .and. lat >=  15.0_sp .and. lat <=  35.0_sp ) id =  5  ! California StCu (15N-35N,130W-110W)
   if ( lon >= 270.0_sp .and. lon <= 290.0_sp .and. lat >= -30.0_sp .and. lat <= -10.0_sp ) id =  6  ! Peruvian (10S-30S,90W-70W)
   if ( lon >= 315.0_sp .and. lon <= 350.0_sp .and. lat >=  30.0_sp .and. lat <=  60.0_sp ) id =  7  ! North Atlantic (30N-60N,45W-10W)
   if ( lon >= 335.0_sp .and. lon <= 360.0_sp .and. lat >= -30.0_sp .and. lat <=   0.0_sp ) id =  8  ! Namibian (30S-0S,25W-15E)
   if ( lon >=   0.0_sp .and. lon <=  15.0_sp .and. lat >= -30.0_sp .and. lat <=   0.0_sp ) id =  8  ! Namibian (30S-0S,25W-15E)
   if ( lon >=  80.0_sp .and. lon <= 115.0_sp .and. lat >= -35.0_sp .and. lat <= -15.0_sp ) id =  9  ! Australian (15S-35S,80E-115E)
   if ( lon >= 125.0_sp .and. lon <= 150.0_sp .and. lat >=  25.0_sp .and. lat <=  50.0_sp ) id = 10  ! Japan (25N-50N,125E-150E)
   if ( lon >= 230.0_sp .and. lon <= 275.0_sp .and. lat >=  -5.0_sp .and. lat <=   5.0_sp ) id = 11  ! Eqt. cold tongue (5S-5N,130W-85W)
   if ( lon >= 105.0_sp .and. lon <= 120.0_sp .and. lat >=  20.0_sp .and. lat <=  40.0_sp ) id = 12  ! Eastern Asia (20N-40N,105E-120E)
   if ( lon >=   0.0_sp .and. lon <= 360.0_sp .and. lat >= -60.0_sp .and. lat <= -40.0_sp ) id = 13  ! Southern Ocean (40S-60S)

   return
end subroutine get_regid

subroutine output_level3( &
   ooutyr, ooutmon, ofinal, &
   ym, year, cmon, imon, &
   avgstr_y, avgstr, avgend, &
   obsstr, &
   glon, glat, gdbz, glev, regmap, &
   ofirst_gt)

   use gmt_data_processor, only: output_gmt_data
   use gnuplot_data_processor, only: output_gnuplot_data
   use grads_data_processor, only: output_grads_data
   use gtool_data_processor, only: output_gtool_data

   implicit none
   logical, intent(in) :: ooutyr, ooutmon, ofinal
   character(*), intent(in) :: ym, year, cmon
   ! character(*), intent(in) :: &
   !    ym, year, outpath_lev3, cmon
   integer, intent(in) :: imon
   real(sp), dimension(:), intent(in) :: glon                 ! grid value (longitude)
   real(sp), dimension(:), intent(in) :: glat                 ! grid value (latitude)
   real(sp), dimension(:), intent(in) :: gdbz( idbz_max + 1 ) ! grid value (CFAD dBZ)
   real(sp), dimension(:), intent(in) :: glev( ilev_max + 1 ) ! grid value (CFAD Lev)
   real(sp), dimension(:, :), intent(in) :: regmap

   character(*), intent(in) :: avgstr_y, avgstr, avgend, obsstr
   logical, intent(inout) :: ofirst_gt

   call calc_pdf(ooutyr, ooutmon, ofinal, imon)

   call output_gmt_data( &
      ooutyr, ooutmon, ofinal, &
      ym, year, imon, &
      glon, glat, gdbz, glev, &
      n_obs, n_obs_all, &
      plot_tool='gmt')

   call output_gnuplot_data( &
      ooutyr, ooutmon, ofinal, &
      ym, year, imon, &
      glon, glat, gdbz, glev, &
      n_obs, n_obs_all, &
      plot_tool='gnuplot')

   call output_grads_data( &
      ooutyr, ooutmon, ofinal, &
      ym, year, cmon, imon, &
      glon, glat, gdbz, glev, &
      n_obs, n_obs_all, &
      plot_tool='grads')

   call output_gtool_data( &
      ooutyr, ooutmon, ofinal, &
      ym, year, &
      glon, glat, &
      avgstr_y, avgstr, avgend, obsstr, imon, &
      regmap, n_obs, n_obs_all, &
      ofirst_gt, &
      plot_tool='gtool')

end subroutine output_level3

subroutine calc_pdf(ooutyr, ooutmon, ofinal, imon)
   implicit none
   logical, intent(in) :: ooutyr, ooutmon, ofinal
   integer, intent(in) :: imon

   integer :: itype
   integer :: idbz, ilev  ! dimension number for dbz/lev for CFAD

   ! initialize variables
   if (imon==1) then
      mean_precip_path = 0
      mean_precip_surf = 0
      pdf_precip = 0
      pdf_rain = 0
      pdf_grpl = 0
      pdf_snowsfc = 0
      pdf_grpsnow = 0
      pdf_hip0 = 0
      pdf_hip1 = 0
      pdf_hip2 = 0
      pdf_hip3 = 0
      pdf_hip4 = 0
      pdf_hip8 = 0
      pdf_hip12 = 0
      pdf_hip16 = 0
      pdf_hip25 = 0
      pdf_hip30 = 0
   endif

   !--- Summary Statistics
   !--- Inline Generators (Level2 ==> Level3)

   ! # of observation and probability of precipitation (pop)
   where ( n_obs( :,:,imon ) .eq. 0 )
      n_obs( :,:,imon ) = undef
      pdf_precip( :,:,imon ) = undef
   else where
      pdf_precip( :,:,imon ) = &
         100.0_dp * real( n_precip_surf( :,:,1,imon ) ) / real( n_obs( :,:,imon ) )
   end where

   ! occurrence frequencies of rain
   pdf_rain( :,:,imon ) = &
      calc_ratio( 100.0_sp * real(n_rain( :,:,imon )), &
                  real(n_precip_surf(:,:,1,imon)) )

   ! mean_precip_surf (conditional=1; unconditional=2)
   do itype = 1, 2
      mean_precip_surf( :,:,itype,imon ) = &
         calc_ratio( real( sum_precip_surf( :,:,itype,imon ) ), &
                     real( n_precip_surf( :,:,itype,imon ) ) )
   enddo

   ! mean_precip_path (Liquid=1; Solid=2)
   do itype = 1, 2
      mean_precip_path( :,:,itype,imon ) = &
         calc_ratio( real(sum_precip_path( :,:,itype,imon ) ), &
                     real( n_precip_path( :,:,itype,imon ) ) )
   enddo

   ! occurrence frequencies of graupel and snow
   pdf_grpl( :,:,imon ) = &
      calc_ratio( 100.0_sp * real( n_flag_graupel1( :,:,imon ) ),&
                  real( n_flag_graupel0( :,:,imon ) + n_flag_graupel1( :,:,imon ) ) )
   pdf_snowsfc( :,:,imon ) = &
      calc_ratio( 100.0_sp * real( n_flag_SurfSnow1( :,:,imon ) ),&
                  real( n_flag_SurfSnow0( :,:,imon ) + n_flag_SurfSnow1( :,:,imon ) ) )
   pdf_grpsnow( :,:,imon ) = &
      calc_ratio( 100.0_sp * real( n_flag_graupelsnow( :,:,imon ) ), &
                  real( n_flag_SurfSnow1( :,:,imon ) ) )

   ! occurrence frequencies of heavy ice precipitation (HIP)
   n_hip( :,:,imon ) = real( n_flag_HeavyIcePre0 ( :,:,imon ) +    &
                  &       n_flag_HeavyIcePre1 ( :,:,imon ) +    &
                  &       n_flag_HeavyIcePre2 ( :,:,imon ) +    &
                  &       n_flag_HeavyIcePre3 ( :,:,imon ) +    &
                  &       n_flag_HeavyIcePre4 ( :,:,imon ) +    &
                  &       n_flag_HeavyIcePre8 ( :,:,imon ) +    &
                  &       n_flag_HeavyIcePre12( :,:,imon ) +    &
                  &       n_flag_HeavyIcePre16( :,:,imon ) +    &
                  &       n_flag_HeavyIcePre25( :,:,imon ) +    &
                  &       n_flag_HeavyIcePre30( :,:,imon ) )
   pdf_hip0( :,:,imon ) = &
      calc_ratio( 100.0_sp * real( n_flag_HeavyIcePre0( :,:,imon ) ), &
                  real( n_hip( :,:,imon ) ) )
   pdf_hip1( :,:,imon ) = &
      calc_ratio( 100.0_sp * real( n_flag_HeavyIcePre1( :,:,imon ) ), &
                  real( n_hip( :,:,imon ) ) )
   pdf_hip2( :,:,imon ) = &
      calc_ratio( 100.0_sp * real( n_flag_HeavyIcePre2( :,:,imon ) ), &
                  real( n_hip( :,:,imon ) ) )
   pdf_hip3( :,:,imon ) = &
      calc_ratio( 100.0_sp * real( n_flag_HeavyIcePre3( :,:,imon ) ), &
                  real( n_hip( :,:,imon ) ) )
   pdf_hip4( :,:,imon ) = &
      calc_ratio( 100.0_sp * real( n_flag_HeavyIcePre4( :,:,imon ) ), &
                  real( n_hip( :,:,imon ) ) )
   pdf_hip8( :,:,imon ) = &
      calc_ratio( 100.0_sp * real( n_flag_HeavyIcePre8( :,:,imon ) ), &
                  real( n_hip( :,:,imon ) ) )
   pdf_hip12( :,:,imon ) = &
      calc_ratio( 100.0_sp * real( n_flag_HeavyIcePre12( :,:,imon ) ), &
                  real( n_hip( :,:,imon ) ) )
   pdf_hip16( :,:,imon ) = &
      calc_ratio( 100.0_sp * real( n_flag_HeavyIcePre16( :,:,imon ) ), &
                  real( n_hip( :,:,imon ) ) )
   pdf_hip25( :,:,imon ) = &
      calc_ratio( 100.0_sp * real( n_flag_HeavyIcePre25( :,:,imon ) ), &
                  real( n_hip( :,:,imon ) ) )
   pdf_hip30( :,:,imon ) = &
      calc_ratio( 100.0_sp * real( n_flag_HeavyIcePre30( :,:,imon ) ), &
                  real( n_hip( :,:,imon ) ) )

   !! Annual Output
   if ( ooutyr ) then
      ! Averaging
      n_obs( :,:,0 ) = sum( n_obs( :,:,0:12 ), DIM=3 )

      ! # of observation and probability of precipitation (pop)
      where ( n_obs( :,:,0 ) .eq. 0 )
         n_obs( :,:,0 ) = undef
         pdf_precip( :,:,0 ) = undef
      else where
         pdf_precip( :,:,0 ) = &
            100.0_sp * real( sum( n_precip_surf( :,:,1,1:12 ), DIM=3 ) ) / real( n_obs( :,:,0 ) )
      end where

      ! occurrence frequencies of rain
      pdf_rain( :,:,0 ) = &
         calc_ratio( 100.0_sp * real(sum(n_rain( :,:,1:12 ), DIM=3) ), &
                     real(sum(n_precip_surf(:,:,1,1:12), DIM=3) ))

      ! mean_precip_surf (conditional=1; unconditional=2)
      do itype = 1, 2
         mean_precip_surf( :,:,itype,0 ) = &
            calc_ratio( real( sum( sum_precip_surf( :,:,itype,1:12 ), DIM=3 ) ), &
                        real( sum( n_precip_surf( :,:,itype,1:12 ), DIM=3 ) ) )
      enddo

      ! mean_precip_path (Liquid=1; Solid=2)
      do itype = 1, 2
         mean_precip_path( :,:,itype,0 ) = &
            calc_ratio( real( sum( sum_precip_path( :,:,itype,1:12 ), DIM=3 ) ), &
                        real( sum( n_precip_path( :,:,itype,1:12 ), DIM=3 ) ) )
      enddo

      ! occurrence frequencies of graupel and snow
      pdf_grpl( :,:,0 ) = &
         calc_ratio( 100.0_sp * real( sum( n_flag_graupel1( :,:,1:12 ), DIM=3 ) ), &
                     real( sum( n_flag_graupel0( :,:,1:12 ), DIM=3 ) &
                        + sum( n_flag_graupel1( :,:,1:12 ), DIM=3 ) ))
      where ( sum( n_flag_graupel0(:,:,1:12), DIM=3 ) + &
              sum( n_flag_graupel1(:,:,1:12), DIM=3 ) .gt. 0 )
         pdf_snowsfc( :,:,0 ) = &
            100.0_sp * real( sum( n_flag_SurfSnow1( :,:,1:12 ), DIM=3 ) ) &
                     / real( sum( n_flag_SurfSnow0( :,:,1:12 ), DIM=3 ) &
                           + sum( n_flag_SurfSnow1( :,:,1:12 ), DIM=3 ) )
      else where
         pdf_snowsfc( :,:,0 ) = undef
      end where

      pdf_grpsnow( :,:,0 ) = &
         calc_ratio( 100.0_sp * real( sum( n_flag_graupelsnow( :,:,1:12 ), DIM=3 ) ), &
                     real( sum( n_flag_SurfSnow1( :,:,1:12 ), DIM=3 ) ))

      ! occurrence frequencies of heavy ice precipitation (HIP)
      n_hip( :,:,0 ) = real( sum( n_flag_HeavyIcePre0 ( :,:,1:12 ), DIM=3 ) +    &
                             sum( n_flag_HeavyIcePre1 ( :,:,1:12 ), DIM=3 ) +    &
                             sum( n_flag_HeavyIcePre2 ( :,:,1:12 ), DIM=3 ) +    &
                             sum( n_flag_HeavyIcePre3 ( :,:,1:12 ), DIM=3 ) +    &
                             sum( n_flag_HeavyIcePre4 ( :,:,1:12 ), DIM=3 ) +    &
                             sum( n_flag_HeavyIcePre8 ( :,:,1:12 ), DIM=3 ) +    &
                             sum( n_flag_HeavyIcePre12( :,:,1:12 ), DIM=3 ) +    &
                             sum( n_flag_HeavyIcePre16( :,:,1:12 ), DIM=3 ) +    &
                             sum( n_flag_HeavyIcePre25( :,:,1:12 ), DIM=3 ) +    &
                             sum( n_flag_HeavyIcePre30( :,:,1:12 ), DIM=3 ) )
      pdf_hip0( :,:,0 ) = &
         calc_ratio( 100.0_sp * real( sum( n_flag_HeavyIcePre0( :,:,1:12 ), DIM=3 ) ), &
                     real( sum( n_hip( :,:,1:12 ), DIM=3 ) ))
      pdf_hip1( :,:,0 ) = &
         calc_ratio( 100.0_sp * real( sum( n_flag_HeavyIcePre1( :,:,1:12 ), DIM=3 ) ), &
                     real( sum( n_hip( :,:,1:12 ), DIM=3 ) ))
      pdf_hip2( :,:,0 ) = &
         calc_ratio( 100.0_sp * real( sum( n_flag_HeavyIcePre2( :,:,1:12 ), DIM=3 ) ), &
                     real( sum( n_hip( :,:,1:12 ), DIM=3 ) ))
      pdf_hip3( :,:,0 ) = &
         calc_ratio( 100.0_sp * real( sum( n_flag_HeavyIcePre3( :,:,1:12 ), DIM=3 ) ), &
                     real( sum( n_hip( :,:,1:12 ), DIM=3 ) ))
      pdf_hip4( :,:,0 ) = &
         calc_ratio( 100.0_sp * real( sum( n_flag_HeavyIcePre4( :,:,1:12 ), DIM=3 ) ), &
                     real( sum( n_hip( :,:,1:12 ), DIM=3 ) ))
      pdf_hip8( :,:,0 ) = &
         calc_ratio( 100.0_sp * real( sum( n_flag_HeavyIcePre8( :,:,1:12 ), DIM=3 ) ), &
                     real( sum( n_hip( :,:,1:12 ), DIM=3 ) ))
      pdf_hip12( :,:,0 ) = &
         calc_ratio( 100.0_sp * real( sum( n_flag_HeavyIcePre12( :,:,1:12 ), DIM=3 ) ), &
                     real( sum( n_hip( :,:,1:12 ), DIM=3 ) ))
      pdf_hip16( :,:,0 ) = &
         calc_ratio( 100.0_sp * real( sum( n_flag_HeavyIcePre16( :,:,1:12 ), DIM=3 ) ), &
                     real( sum( n_hip( :,:,1:12 ), DIM=3 ) ))
      pdf_hip25( :,:,0 ) = &
         calc_ratio( 100.0_sp * real( sum( n_flag_HeavyIcePre25( :,:,1:12 ), DIM=3 ) ), &
                     real( sum( n_hip( :,:,1:12 ), DIM=3 ) ))
      pdf_hip30( :,:,0 ) = &
         calc_ratio( 100.0_sp * real( sum( n_flag_HeavyIcePre30( :,:,1:12 ), DIM=3 ) ), &
                     real( sum( n_hip( :,:,1:12 ), DIM=3 ) ))
   endif ! ooutyr

   !! Annual Output
   if ( ooutyr ) then
      ! update for accumulating whole analysis period
      where ( n_obs( :,:,0 ) .ne. undef )
         n_obs_all( :,: ) = n_obs_all( :,: ) + n_obs( :,:,0 )
      end where
      n_rain_all( :,: ) = n_rain_all( :,: ) + sum( n_rain( :,:,1:12 ), DIM=3 )
      n_precip_surf_all( :,:,1 ) = n_precip_surf_all( :,:,1 ) + sum( n_precip_surf( :,:,1,1:12 ), DIM=3 )
      n_precip_surf_all( :,:,2 ) = n_precip_surf_all( :,:,2 ) + sum( n_precip_surf( :,:,2,1:12 ), DIM=3 )
      sum_precip_surf_all( :,:,1 ) = sum_precip_surf_all( :,:,1 ) + sum( sum_precip_surf( :,:,1,1:12 ), DIM=3 )
      sum_precip_surf_all( :,:,2 ) = sum_precip_surf_all( :,:,2 ) + sum( sum_precip_surf( :,:,2,1:12 ), DIM=3 )
      n_precip_path_all( :,:,1 ) = n_precip_path_all( :,:,1 ) + sum( n_precip_path( :,:,1,1:12 ), DIM=3 )
      n_precip_path_all( :,:,2 ) = n_precip_path_all( :,:,2 ) + sum( n_precip_path( :,:,2,1:12 ), DIM=3 )
      sum_precip_path_all( :,:,1 ) = sum_precip_path_all( :,:,1 ) + sum( sum_precip_path( :,:,1,1:12 ), DIM=3 )
      sum_precip_path_all( :,:,2 ) = sum_precip_path_all( :,:,2 ) + sum( sum_precip_path( :,:,2,1:12 ), DIM=3 )
      n_flag_graupel0_all( :,: ) = n_flag_graupel0_all( :,: ) + sum( n_flag_graupel0( :,:,1:12 ), DIM=3 )
      n_flag_graupel1_all( :,: ) = n_flag_graupel1_all( :,: ) + sum( n_flag_graupel1( :,:,1:12 ), DIM=3 )
      n_flag_SurfSnow0_all( :,: ) = n_flag_SurfSnow0_all( :,: ) + sum( n_flag_SurfSnow0( :,:,1:12 ), DIM=3 )
      n_flag_SurfSnow1_all( :,: ) = n_flag_SurfSnow1_all( :,: ) + sum( n_flag_SurfSnow1( :,:,1:12 ), DIM=3 )
      n_flag_graupelsnow_all( :,: ) = n_flag_graupelsnow_all( :,: ) + sum( n_flag_graupelsnow( :,:,1:12 ), DIM=3 )
      n_hip_all( :,: ) = n_hip_all( :,: ) + sum( n_hip( :,:,1:12 ), DIM=3 )
      n_flag_HeavyIcePre0_all ( :,: ) = n_flag_HeavyIcePre0_all ( :,: ) + sum( n_flag_HeavyIcePre0 ( :,:,1:12 ), DIM=3 )
      n_flag_HeavyIcePre1_all ( :,: ) = n_flag_HeavyIcePre1_all ( :,: ) + sum( n_flag_HeavyIcePre1 ( :,:,1:12 ), DIM=3 )
      n_flag_HeavyIcePre2_all ( :,: ) = n_flag_HeavyIcePre2_all ( :,: ) + sum( n_flag_HeavyIcePre2 ( :,:,1:12 ), DIM=3 )
      n_flag_HeavyIcePre3_all ( :,: ) = n_flag_HeavyIcePre3_all ( :,: ) + sum( n_flag_HeavyIcePre3 ( :,:,1:12 ), DIM=3 )
      n_flag_HeavyIcePre4_all ( :,: ) = n_flag_HeavyIcePre4_all ( :,: ) + sum( n_flag_HeavyIcePre4 ( :,:,1:12 ), DIM=3 )
      n_flag_HeavyIcePre8_all ( :,: ) = n_flag_HeavyIcePre8_all ( :,: ) + sum( n_flag_HeavyIcePre8 ( :,:,1:12 ), DIM=3 )
      n_flag_HeavyIcePre12_all( :,: ) = n_flag_HeavyIcePre12_all( :,: ) + sum( n_flag_HeavyIcePre12( :,:,1:12 ), DIM=3 )
      n_flag_HeavyIcePre16_all( :,: ) = n_flag_HeavyIcePre16_all( :,: ) + sum( n_flag_HeavyIcePre16( :,:,1:12 ), DIM=3 )
      n_flag_HeavyIcePre25_all( :,: ) = n_flag_HeavyIcePre25_all( :,: ) + sum( n_flag_HeavyIcePre25( :,:,1:12 ), DIM=3 )
      n_flag_HeavyIcePre30_all( :,: ) = n_flag_HeavyIcePre30_all( :,: ) + sum( n_flag_HeavyIcePre30( :,:,1:12 ), DIM=3 )
   endif ! ooutyr

   !! All-Term Output
   if ( ofinal ) then
      ! Averaging

      ! # of observation and probability of precipitation (pop)
      where ( n_obs_all( :,: ) .eq. 0 )
         n_obs_all( :,: ) = undef
         pdf_precip_all( :,: ) = undef
      else where
         pdf_precip_all( :,: ) = &
            100.0_sp * real( n_precip_surf_all( :,:,1 ) ) / real( n_obs_all( :,: ) )
      end where

      ! occurrence frequencies of rain
      pdf_rain_all( :,: ) = &
         calc_ratio( 100.0_sp * real( n_rain_all( :,: ) ), &
                     real( n_precip_surf_all( :,:,1 ) ))

      ! mean_precip_surf (conditional=1; unconditional=2)
      do itype = 1, 2
         mean_precip_surf_all( :,:,itype ) = &
            calc_ratio( real( sum_precip_surf_all( :,:,itype ) ), &
                        real( n_precip_surf_all( :,:,itype ) ) )
      enddo

      ! mean_precip_path (Liquid=1; Solid=2)
      do itype = 1, 2
         mean_precip_path_all( :,:,itype ) = &
            calc_ratio( real( sum_precip_path_all( :,:,itype ) ), &
                        real( n_precip_path_all( :,:,itype ) ))
      enddo

      ! occurrence frequencies of graupel and snow
      pdf_grpl_all( :,: ) = &
         calc_ratio( 100.0_sp * real( n_flag_graupel1_all( :,: ) ), &
                     real( n_flag_graupel0_all( :,: ) + n_flag_graupel1_all( :,: ) ))
      where ( real( n_flag_graupel0_all(:,:) + n_flag_graupel1_all(:,:) ) .gt. 0 )
         pdf_snowsfc_all( :,: ) = &
            100.0_sp * real( n_flag_SurfSnow1_all( :,: ) ) &
                     / real( n_flag_SurfSnow0_all( :,: ) + n_flag_SurfSnow1_all( :,: ) )
      else where
         pdf_snowsfc_all( :,: ) = undef
      end where

      pdf_grpsnow_all( :,: ) = &
         calc_ratio( 100.0_sp * real( n_flag_graupelsnow_all( :,: ) ), &
                     real( n_flag_SurfSnow1_all( :,: ) ) )

      ! occurrence frequencies of heavy ice precipitation (HIP)
      n_hip_all( :,: ) = real( n_flag_HeavyIcePre0_all ( :,: ) + &
                               n_flag_HeavyIcePre1_all ( :,: ) + &
                               n_flag_HeavyIcePre2_all ( :,: ) + &
                               n_flag_HeavyIcePre3_all ( :,: ) + &
                               n_flag_HeavyIcePre4_all ( :,: ) + &
                               n_flag_HeavyIcePre8_all ( :,: ) + &
                               n_flag_HeavyIcePre12_all( :,: ) + &
                               n_flag_HeavyIcePre16_all( :,: ) + &
                               n_flag_HeavyIcePre25_all( :,: ) + &
                               n_flag_HeavyIcePre30_all( :,: ) )
      pdf_hip0_all( :,: ) = &
         calc_ratio( 100.0_sp * real( n_flag_HeavyIcePre0_all( :,: ) ), &
                     real( n_hip_all( :,: ) ) )
      pdf_hip1_all( :,: ) = &
         calc_ratio( 100.0_sp * real( n_flag_HeavyIcePre1_all( :,: ) ), &
                     real( n_hip_all( :,: ) ) )
      pdf_hip2_all( :,: ) = &
         calc_ratio( 100.0_sp * real( n_flag_HeavyIcePre2_all( :,: ) ), &
                     real( n_hip_all( :,: ) ) )
      pdf_hip3_all( :,: ) = &
         calc_ratio( 100.0_sp * real( n_flag_HeavyIcePre3_all( :,: ) ), &
                     real( n_hip_all( :,: ) ) )
      pdf_hip4_all( :,: ) = &
         calc_ratio( 100.0_sp * real( n_flag_HeavyIcePre4_all( :,: ) ), &
                     real( n_hip_all( :,: ) ) )
      pdf_hip8_all( :,: ) = &
         calc_ratio( 100.0_sp * real( n_flag_HeavyIcePre8_all( :,: ) ), &
                     real( n_hip_all( :,: ) ) )
      pdf_hip12_all( :,: ) = &
         calc_ratio( 100.0_sp * real( n_flag_HeavyIcePre12_all( :,: ) ), &
                     real( n_hip_all( :,: ) ) )
      pdf_hip16_all( :,: ) = &
         calc_ratio( 100.0_sp * real( n_flag_HeavyIcePre16_all( :,: ) ), &
                     real( n_hip_all( :,: ) ) )
      pdf_hip25_all( :,: ) = &
         calc_ratio( 100.0_sp * real( n_flag_HeavyIcePre25_all( :,: ) ), &
                     real( n_hip_all( :,: ) ) )
      pdf_hip30_all( :,: ) = &
         calc_ratio( 100.0_sp * real( n_flag_HeavyIcePre30_all( :,: ) ), &
                     real( n_hip_all( :,: ) ) )
   endif  ! ofinal

   !--- CFAD
   if ( odump3d ) then
      ! Monthly CFAD
      if ( ooutmon ) then
         do ilev = 1, dimlev
            do idbz = 1, dimdbz
               where ( n_cfad_bin( idbz,ilev,:,:,imon ) .eq. 0 )
                  pdf_cfad( idbz,ilev,:,:,imon ) = undef
               else where
                  pdf_cfad( idbz,ilev,:,:,imon ) = &
                     calc_ratio( 100.0_sp * real( n_cfad_bin( idbz,ilev,:,:,imon ) ), &
                                 real( n_cfad_obs( :,:,imon ) ) )
               end where
            enddo
         enddo
      endif

      ! Annual CFAD
      if ( ooutyr ) then
         do ilev = 1, dimlev
            do idbz = 1, dimdbz
               where ( sum( n_cfad_bin( idbz,ilev,:,:,1:12 ), DIM=3 ) .eq. 0 )
                  pdf_cfad( idbz,ilev,:,:,0 ) = undef
               else where
                  pdf_cfad( idbz,ilev,:,:,0 ) = &
                     calc_ratio( 100.0_sp * real( sum( n_cfad_bin( idbz,ilev,:,:,1:12 ), DIM=3 ) ), &
                                 real( sum( n_cfad_obs( :,:,1:12 ), DIM=3 ) ) )
               end where
            enddo
         enddo

         ! update for accumulating whole analysis period
         n_cfad_obs_all( :,: ) = n_cfad_obs_all( :,: ) + sum( n_cfad_obs( :,:,1:12 ), DIM=3 )
         n_cfad_bin_all( :,:,:,: ) = n_cfad_bin_all( :,:,:,: ) + &
                                     sum( n_cfad_bin( :,:,:,:,1:12 ), DIM=5 )
      endif

      ! All-Term Output
      if ( ofinal ) then
         do ilev = 1, dimlev
            do idbz = 1, dimdbz
               where ( n_cfad_bin_all( idbz,ilev,:,: ) .eq. 0 )
                  pdf_cfad_all( idbz,ilev,:,: ) = undef
               else where
                  pdf_cfad_all( idbz,ilev,:,: ) = &
                     calc_ratio( 100.0_sp * real( n_cfad_bin_all( idbz,ilev,:,: ) ), &
                                 real( n_cfad_obs_all( :,: ) ) )
               end where               
            enddo
         enddo
      endif
   endif

end subroutine calc_pdf

function calc_ratio(numerator, denominator) result(ratio)
   implicit none
   real(sp), dimension(:,:), intent(in) :: numerator, denominator
   real(sp), dimension(:,:), allocatable :: ratio
   integer :: size1, size2
   integer, dimension(2) :: array_shape

   array_shape = shape(denominator)
   size1 = array_shape(1)
   size2 = array_shape(2)
   allocate( ratio(size1, size2) )

   where ( denominator .gt. 0 )
      ratio = numerator / denominator
   else where
      ratio = undef
   end where

end function calc_ratio

end module output
