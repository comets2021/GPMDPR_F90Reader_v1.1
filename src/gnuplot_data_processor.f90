module gnuplot_data_processor
    use parameters
    use gmt_data_processor, only: output_vars
    use pdfs

contains

! Summary Output for gnuplot
subroutine output_gnuplot_data( &
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

    character(len = 128) :: outfname
    character(len = 32) :: creg, cfreq, cname
    integer :: ireg, ifreq                          ! dimension number for reg/freq for CFAD
    integer :: idbz, ilev                           ! dimension number for dbz/lev for CFAD

    !! Monthly Output
    if ( ooutmon ) then
        outfname = trim(outpath_lev3) // "/gnuplot/gpm_map2d_" // trim(ym) // "_gpl.txt"
        call output_vars(&
            glon,                              glat,                             & ! 01-02
            n_obs           ( :, :, imon ),                                      & ! 03
            pdf_precip      ( :, :, imon ),    pdf_rain( :, :, imon ),           & ! 04-05
            mean_precip_surf( :, :, 1,imon ),  mean_precip_surf( :, :, 2,imon ), & ! 06-07
            mean_precip_path( :, :, 1,imon ),  mean_precip_path( :, :, 2,imon ), & ! 08-09
            pdf_grpl        ( :, :, imon ),    pdf_snowsfc( :, :, imon ),        & ! 10-11
            pdf_hip0        ( :, :, imon ),    pdf_hip1 ( :, :, imon ),          & ! 12-13
            pdf_hip2        ( :, :, imon ),    pdf_hip3 ( :, :, imon ),          & ! 14-15
            pdf_hip4        ( :, :, imon ),    pdf_hip8 ( :, :, imon ),          & ! 16-17
            pdf_hip12       ( :, :, imon ),    pdf_hip16( :, :, imon ),          & ! 18-19
            pdf_hip25       ( :, :, imon ),    pdf_hip30( :, :, imon ),          & ! 20-21
            pdf_grpsnow     ( :, :, imon ),                                      & ! 22
            outfname, plot_tool)
    endif

    !! Annual Output
    if ( ooutyr ) then
        outfname = trim(outpath_lev3) // "/gnuplot/gpm_map2d_" // trim(year) // "_gpl.txt"
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
    endif

    !! All-Term Output
    if ( ofinal ) then
        outfname = trim(outpath_lev3) // "/gnuplot/gpm_map2d_all_gpl.txt"
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
    endif

    if ( odump3d ) then
        ! gnuplot format
        do ifreq = 1, nfreq
           if ( ifreq .eq. 1 ) cfreq = "Ku"
           if ( ifreq .eq. 2 ) cfreq = "Ka"
           do ireg = 1, nreg
              if ( nreg .eq. 3 ) then
                 if ( ireg .eq.  1 ) creg = "tropic"
                 if ( ireg .eq.  2 ) creg = "midlat"
                 if ( ireg .eq.  3 ) creg = "global"
              elseif ( nreg .eq. 14 ) then
                 if ( ireg .eq.  1 ) creg = "trwp"
                 if ( ireg .eq.  2 ) creg = "itcz"
                 if ( ireg .eq.  3 ) creg = "spcz"
                 if ( ireg .eq.  4 ) creg = "nepa"
                 if ( ireg .eq.  5 ) creg = "cast"
                 if ( ireg .eq.  6 ) creg = "peru"
                 if ( ireg .eq.  7 ) creg = "natl"
                 if ( ireg .eq.  8 ) creg = "nami"
                 if ( ireg .eq.  9 ) creg = "aust"
                 if ( ireg .eq. 10 ) creg = "japn"
                 if ( ireg .eq. 11 ) creg = "eqct"
                 if ( ireg .eq. 12 ) creg = "easi"
                 if ( ireg .eq. 13 ) creg = "soce"
                 if ( ireg .eq. 14 ) creg = "glob"
              else
                   write(*,*) "### ERROR IN REGION DEFINITION (#2): ", nreg
                   stop
              endif

              cname = trim( cfreq ) // "_" // trim( creg )
              ! Monthly CFAD
              if ( ooutmon ) then
                 outfname = trim( outpath_lev3 ) // "/gnuplot/gpm_cfad_" // trim(ym) // "_gpl_" // trim(cname) // ".txt"
                 open( 33, file = trim( outfname ), status = "unknown" ) ! for gnuplot
                 do ilev = 1, dimlev
                    do idbz = 1, dimdbz
                       write( 33,* ) gdbz( idbz*2 ), glev( ilev*2 ), pdf_cfad( idbz,ilev,ireg,ifreq,imon )
                    enddo
                    write( 33,* )
                 enddo
                 close( 33 )
              endif

              ! Annual CFAD
              if ( ooutyr ) then
                 outfname = trim( outpath_lev3 ) // "/gnuplot/gpm_cfad_" // trim(year) // "_gpl_" // trim(cname) // ".txt"
                 open( 34, file = trim( outfname ), status = "unknown" ) ! for gnuplot
                 do ilev = 1, dimlev
                    do idbz = 1, dimdbz
                       write( 34,* ) gdbz( idbz*2 ), glev( ilev*2 ), pdf_cfad( idbz,ilev,ireg,ifreq,0 )
                    enddo
                    write( 34,* )
                 enddo
                 close( 34 )
              endif

              ! All-Term Output
              if ( ofinal ) then
                 outfname = trim( outpath_lev3 ) // "/gnuplot/gpm_cfad_all_gpl_" // trim(cname) // ".txt"
                 open( 35, file = trim( outfname ), status = "unknown" ) ! for gnuplot
                 do ilev = 1, dimlev
                    do idbz = 1, dimdbz
                       write( 35,* ) gdbz( idbz*2 ), glev( ilev*2 ), pdf_cfad_all( idbz,ilev,ireg,ifreq )
                    enddo
                    write( 35,* )
                 enddo
                 close( 35 )
              endif

           enddo  ! ireg loop
        enddo     ! ifreq loop
     endif 

end subroutine output_gnuplot_data

end module gnuplot_data_processor