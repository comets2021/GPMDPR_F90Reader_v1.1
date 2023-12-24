module gmt_data_processor
    use parameters
    use netcdf_writer
    use pdfs

contains

! Summary Output for GMT
subroutine output_gmt_data( &
    ooutyr, ooutmon, ofinal, &
    ym, year, imon, &
    glon, glat, gdbz, glev, &
    n_obs, n_obs_all, &
    plot_tool)

    implicit none

    logical, intent(in) :: ooutyr, ooutmon, ofinal
    character(*), intent(in) :: ym, year
    integer, intent(in) :: imon
    real(sp), dimension(:), intent(in) :: glon                 ! grid value (longitude)
    real(sp), dimension(:), intent(in) :: glat                 ! grid value (latitude)
    real(sp), dimension(:), intent(in) :: gdbz( idbz_max + 1 ) ! grid value (CFAD dBZ)
    real(sp), dimension(:), intent(in) :: glev( ilev_max + 1 ) ! grid value (CFAD Lev)
    real(sp), dimension(:, :, 0:), intent(in) :: n_obs
    real(sp), dimension(:, :), intent(in) :: n_obs_all

    character(len=*), intent(in) :: plot_tool

    character(len = 128) :: outfname, file_prefix
    character(len = 32) :: creg, cfreq, cname

    file_prefix = trim(outpath_lev3) // "/" // plot_tool // "/" // "gpm_map2d_"

    !! Monthly Output
    if ( ooutmon ) then
        outfname = trim(file_prefix) // trim(ym) // file_suffix_txt
        call output_vars(&
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
            outfname, plot_tool)

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
        outfname = trim(file_prefix) // trim(year) // file_suffix_txt
        call output_vars(&
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
            outfname, plot_tool)

        outfname = trim(file_prefix) // trim(year) // file_suffix_nc
        call output_netcdf_map2d( &
            outpath_lev3, year, &
            glon,                              glat,                              & ! 01-02
            n_obs           ( :, :, 0 ),                                       & ! 03
            pdf_precip      ( :, :, 0 ),    pdf_rain( :, :, 0 ),            & ! 04-05
            mean_precip_surf( :, :, 1, 0 ), mean_precip_surf( :, :, 2, 0 ), & ! 06-07
            mean_precip_path( :, :, 1, 0 ), mean_precip_path( :, :, 2, 0 ), & ! 08-09
            pdf_grpl        ( :, :, 0 ),    pdf_snowsfc( :, :, 0 ),         & ! 10-11
            pdf_hip0        ( :, :, 0 ),    pdf_hip1 ( :, :, 0 ),           & ! 12-13
            pdf_hip2        ( :, :, 0 ),    pdf_hip3 ( :, :, 0 ),           & ! 14-15
            pdf_hip4        ( :, :, 0 ),    pdf_hip8 ( :, :, 0 ),           & ! 16-17
            pdf_hip12       ( :, :, 0 ),    pdf_hip16( :, :, 0 ),           & ! 18-19
            pdf_hip25       ( :, :, 0 ),    pdf_hip30( :, :, 0 ),           & ! 20-21
            pdf_grpsnow     ( :, :, 0 ),                                       & ! 22
            outfname, plot_tool, output_type='annual')
    endif

    !! All-Term Output
    if ( ofinal ) then
        outfname = trim(file_prefix) // "all" // file_suffix_txt
        call output_vars(&
            glon,                             glat,                            & ! 01-02
            n_obs_all,                                                         & ! 03
            pdf_precip_all,                   pdf_rain_all,                    & ! 04-05
            mean_precip_surf_all( :, :, 1 ),  mean_precip_surf_all( :, :, 2 ), & ! 06-07
            mean_precip_path_all( :, :, 1 ),  mean_precip_path_all( :, :, 2 ), & ! 08-09
            pdf_grpl_all,                     pdf_snowsfc_all,                 & ! 10-11
            pdf_hip0_all,                     pdf_hip1_all,                    & ! 12-13
            pdf_hip2_all,                     pdf_hip3_all,                    & ! 14-15
            pdf_hip4_all,                     pdf_hip8_all,                    & ! 16-17
            pdf_hip12_all,                    pdf_hip16_all,                   & ! 18-19
            pdf_hip25_all,                    pdf_hip30_all,                   & ! 20-21
            pdf_grpsnow_all,                                                   & ! 22
            outfname, plot_tool)

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

end subroutine output_gmt_data

subroutine output_vars( &
    var1,  var2,  var3,  var4,  var5,  &
    var6,  var7,  var8,  var9,  var10, &
    var11, var12, var13, var14, var15, &
    var16, var17, var18, var19, var20, &
    var21, var22, &
    outfname, plot_tool )

    implicit none
    character(len=*), intent(in) :: outfname
    real(sp), dimension(:), intent(in) :: var1, var2
    real(sp), dimension(:, :), intent(in) :: &
        var3, var4, var5, &
        var6, var7, var8, var9, var10, var11, &
        var12, var13, var14, var15, var16, &
        var17, var18, var19, var20, var21, &
        var22
    character(len=*), intent(in) :: plot_tool
    integer :: file_idx = 31
    integer :: ix, iy                               ! dimension number for lon/lat

    open( file_idx, file = outfname, status = "unknown" )

    do iy = 1, dimy
        do ix = 1, dimx
            write( file_idx,'(22f15.7)' ) &
                var1( ix*2 ), var2( iy*2 ), &
                var3 ( ix,iy ),  var4 ( ix,iy ),  var5 ( ix,iy ),  &
                var6 ( ix,iy ),  var7 ( ix,iy ),  var8 ( ix,iy ),  &
                var9 ( ix,iy ),  var10( ix,iy ),  var11( ix,iy ),  &
                var12( ix,iy ),  var13( ix,iy ),  var14( ix,iy ),  &
                var15( ix,iy ),  var16( ix,iy ),  var17( ix,iy ),  &
                var18( ix,iy ),  var19( ix,iy ),  var20( ix,iy ),  &
                var21( ix,iy ),  var22( ix,iy )
        enddo
        if ( plot_tool == 'gnuplot' ) then
            write( file_idx,* )
        endif
    enddo

    close( file_idx )

end subroutine output_vars

end module gmt_data_processor
