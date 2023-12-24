module grads_data_processor
    use parameters
    use netcdf_writer
    use pdfs

    private output_2d, output_3d, output_4d, output_ctl_map2d, output_ctl_cfad

contains

! Summary Output for GrADS
subroutine output_grads_data( &
    ooutyr, ooutmon, ofinal, &
    ym, year, cmon, imon, &
    glon, glat, gdbz, glev, &
    n_obs, n_obs_all, &
    plot_tool)

    implicit none

    logical, intent(in) :: ooutyr, ooutmon, ofinal
    character(len=*), intent(in) :: ym, year, cmon
    integer, intent(in) :: imon
    real(sp), dimension(:), intent(in) :: glon                 ! grid value (longitude)
    real(sp), dimension(:), intent(in) :: glat                 ! grid value (latitude)
    real(sp), dimension(:), intent(in) :: gdbz( idbz_max + 1 ) ! grid value (CFAD dBZ)
    real(sp), dimension(:), intent(in) :: glev( ilev_max + 1 ) ! grid value (CFAD Lev)

    real(sp), dimension(:, :, 0:), intent(in) :: n_obs
    real(sp), dimension(:, :), intent(in) :: n_obs_all

    character(len=*), intent(in) :: plot_tool

    character(len = 128) :: outfname, file_prefix_map2d, file_suffix_bin

    ! Monthly 2D Map: Binary (Unformatted Direct Access)
    file_prefix_map2d = trim(outpath_lev3) // "/" // plot_tool // "/" // "gpm_map2d_"
    outfname = trim(file_prefix_map2d) // trim(ym) // file_suffix_bin
    call output_2d(&
        outfname, &
        n_obs           ( 1:dimx,1:dimy,imon ),                                              &
        pdf_precip      ( 1:dimx,1:dimy,imon ),    pdf_rain        ( 1:dimx,1:dimy,imon ),   &
        mean_precip_surf( 1:dimx,1:dimy,1,imon ),  mean_precip_surf( 1:dimx,1:dimy,2,imon ), &
        mean_precip_path( 1:dimx,1:dimy,1,imon ),  mean_precip_path( 1:dimx,1:dimy,2,imon ), &
        pdf_grpl        ( 1:dimx,1:dimy,imon ),    pdf_snowsfc     ( 1:dimx,1:dimy,imon ),   &
        pdf_hip0        ( 1:dimx,1:dimy,imon ),    pdf_hip1        ( 1:dimx,1:dimy,imon ),   &
        pdf_hip2        ( 1:dimx,1:dimy,imon ),    pdf_hip3        ( 1:dimx,1:dimy,imon ),   &
        pdf_hip4        ( 1:dimx,1:dimy,imon ),    pdf_hip8        ( 1:dimx,1:dimy,imon ),   &
        pdf_hip12       ( 1:dimx,1:dimy,imon ),    pdf_hip16       ( 1:dimx,1:dimy,imon ),   &
        pdf_hip25       ( 1:dimx,1:dimy,imon ),    pdf_hip30       ( 1:dimx,1:dimy,imon ),   &
        pdf_grpsnow     ( 1:dimx,1:dimy,imon ))

        outfname = trim(file_prefix_map2d) // trim(ym) // file_suffix_nc
        call output_netcdf_map2d( &
            outpath_lev3, year, &
            glon, glat, &
            n_obs           ( 1:dimx,1:dimy,imon ),                                           &
            pdf_precip      ( 1:dimx,1:dimy,imon ),    pdf_rain        ( 1:dimx,1:dimy,imon ),   &
            mean_precip_surf( 1:dimx,1:dimy,1,imon ),  mean_precip_surf( 1:dimx,1:dimy,2,imon ), &
            mean_precip_path( 1:dimx,1:dimy,1,imon ),  mean_precip_path( 1:dimx,1:dimy,2,imon ), &
            pdf_grpl        ( 1:dimx,1:dimy,imon ),    pdf_snowsfc     ( 1:dimx,1:dimy,imon ),   &
            pdf_hip0        ( 1:dimx,1:dimy,imon ),    pdf_hip1        ( 1:dimx,1:dimy,imon ),   &
            pdf_hip2        ( 1:dimx,1:dimy,imon ),    pdf_hip3        ( 1:dimx,1:dimy,imon ),   &
            pdf_hip4        ( 1:dimx,1:dimy,imon ),    pdf_hip8        ( 1:dimx,1:dimy,imon ),   &
            pdf_hip12       ( 1:dimx,1:dimy,imon ),    pdf_hip16       ( 1:dimx,1:dimy,imon ),   &
            pdf_hip25       ( 1:dimx,1:dimy,imon ),    pdf_hip30       ( 1:dimx,1:dimy,imon ),   &
            pdf_grpsnow     ( 1:dimx,1:dimy,imon ),                                              &
            outfname, plot_tool, output_type="monthly", imon=imon)

    ! Monthly 2D Map: CTL File for GrADS
    call output_ctl_map2d( &
        outpath_lev3, "binary", "monthly", ym, year, cmon)
    call output_ctl_map2d( &
        outpath_lev3, "netcdf", "monthly", ym, year, cmon)

    if ( ooutyr ) then
        ! Annual 2D Map: Binary (Unformatted Direct Access)
        outfname = trim(file_prefix_map2d) // trim(year) // file_suffix_bin
        call output_3d(&
            outfname, &
            n_obs           ( 1:dimx, 1:dimy, 1:nmon ),                                                    &
            pdf_precip      ( 1:dimx, 1:dimy, 1:nmon ),    pdf_rain        ( 1:dimx, 1:dimy, 1:nmon ),     &
            mean_precip_surf( 1:dimx, 1:dimy, 1, 1:nmon ),  mean_precip_surf( 1:dimx, 1:dimy, 2, 1:nmon ), &
            mean_precip_path( 1:dimx, 1:dimy, 1, 1:nmon ),  mean_precip_path( 1:dimx, 1:dimy, 2, 1:nmon ), &
            pdf_grpl        ( 1:dimx, 1:dimy, 1:nmon ),    pdf_snowsfc     ( 1:dimx, 1:dimy, 1:nmon ),     &
            pdf_hip0        ( 1:dimx, 1:dimy, 1:nmon ),    pdf_hip1        ( 1:dimx, 1:dimy, 1:nmon ),     &
            pdf_hip2        ( 1:dimx, 1:dimy, 1:nmon ),    pdf_hip3        ( 1:dimx, 1:dimy, 1:nmon ),     &
            pdf_hip4        ( 1:dimx, 1:dimy, 1:nmon ),    pdf_hip8        ( 1:dimx, 1:dimy, 1:nmon ),     &
            pdf_hip12       ( 1:dimx, 1:dimy, 1:nmon ),    pdf_hip16       ( 1:dimx, 1:dimy, 1:nmon ),     &
            pdf_hip25       ( 1:dimx, 1:dimy, 1:nmon ),    pdf_hip30       ( 1:dimx, 1:dimy, 1:nmon ),     &
            pdf_grpsnow     ( 1:dimx, 1:dimy, 1:nmon ))

        outfname = trim(file_prefix_map2d) // trim(year) // file_suffix_nc
        call output_netcdf_map2d_12months( &
            outpath_lev3, year, &
            glon, glat, &
            n_obs           ( 1:dimx, 1:dimy, 1:nmon ),                                                   &
            pdf_precip      ( 1:dimx, 1:dimy, 1:nmon ),    pdf_rain        ( 1:dimx, 1:dimy, 1:nmon ),    &
            mean_precip_surf( 1:dimx, 1:dimy, 1, 1:nmon ), mean_precip_surf( 1:dimx, 1:dimy, 2, 1:nmon ), &
            mean_precip_path( 1:dimx, 1:dimy, 1, 1:nmon ), mean_precip_path( 1:dimx, 1:dimy, 2, 1:nmon ), &
            pdf_grpl        ( 1:dimx, 1:dimy, 1:nmon ),    pdf_snowsfc     ( 1:dimx, 1:dimy, 1:nmon ),    &
            pdf_hip0        ( 1:dimx, 1:dimy, 1:nmon ),    pdf_hip1        ( 1:dimx, 1:dimy, 1:nmon ),    &
            pdf_hip2        ( 1:dimx, 1:dimy, 1:nmon ),    pdf_hip3        ( 1:dimx, 1:dimy, 1:nmon ),    &
            pdf_hip4        ( 1:dimx, 1:dimy, 1:nmon ),    pdf_hip8        ( 1:dimx, 1:dimy, 1:nmon ),    &
            pdf_hip12       ( 1:dimx, 1:dimy, 1:nmon ),    pdf_hip16       ( 1:dimx, 1:dimy, 1:nmon ),    &
            pdf_hip25       ( 1:dimx, 1:dimy, 1:nmon ),    pdf_hip30       ( 1:dimx, 1:dimy, 1:nmon ),    &
            pdf_grpsnow     ( 1:dimx, 1:dimy, 1:nmon ),                                                   &
            outfname, plot_tool, output_type="12_months")

        outfname = trim(file_prefix_map2d) // trim(year) // "_ann" // file_suffix_bin
        call output_2d(&
            outfname, &
            n_obs           ( 1:dimx,1:dimy,0 ),                                           &
            pdf_precip      ( 1:dimx,1:dimy,0 ),    pdf_rain        ( 1:dimx,1:dimy,0 ),   &
            mean_precip_surf( 1:dimx,1:dimy,1,0 ),  mean_precip_surf( 1:dimx,1:dimy,2,0 ), &
            mean_precip_path( 1:dimx,1:dimy,1,0 ),  mean_precip_path( 1:dimx,1:dimy,2,0 ), &
            pdf_grpl        ( 1:dimx,1:dimy,0 ),    pdf_snowsfc     ( 1:dimx,1:dimy,0 ),   &
            pdf_hip0        ( 1:dimx,1:dimy,0 ),    pdf_hip1        ( 1:dimx,1:dimy,0 ),   &
            pdf_hip2        ( 1:dimx,1:dimy,0 ),    pdf_hip3        ( 1:dimx,1:dimy,0 ),   &
            pdf_hip4        ( 1:dimx,1:dimy,0 ),    pdf_hip8        ( 1:dimx,1:dimy,0 ),   &
            pdf_hip12       ( 1:dimx,1:dimy,0 ),    pdf_hip16       ( 1:dimx,1:dimy,0 ),   &
            pdf_hip25       ( 1:dimx,1:dimy,0 ),    pdf_hip30       ( 1:dimx,1:dimy,0 ),   &
            pdf_grpsnow     ( 1:dimx,1:dimy,0 ))

        outfname = trim(file_prefix_map2d) // trim(year) // "_ann" // file_suffix_nc
        call output_netcdf_map2d( &
            outpath_lev3, year, &
            glon, glat, &
            n_obs           ( 1:dimx,1:dimy,0 ),                                           &
            pdf_precip      ( 1:dimx,1:dimy,0 ),    pdf_rain        ( 1:dimx,1:dimy,0 ),   &
            mean_precip_surf( 1:dimx,1:dimy,1,0 ),  mean_precip_surf( 1:dimx,1:dimy,2,0 ), &
            mean_precip_path( 1:dimx,1:dimy,1,0 ),  mean_precip_path( 1:dimx,1:dimy,2,0 ), &
            pdf_grpl        ( 1:dimx,1:dimy,0 ),    pdf_snowsfc     ( 1:dimx,1:dimy,0 ),   &
            pdf_hip0        ( 1:dimx,1:dimy,0 ),    pdf_hip1        ( 1:dimx,1:dimy,0 ),   &
            pdf_hip2        ( 1:dimx,1:dimy,0 ),    pdf_hip3        ( 1:dimx,1:dimy,0 ),   &
            pdf_hip4        ( 1:dimx,1:dimy,0 ),    pdf_hip8        ( 1:dimx,1:dimy,0 ),   &
            pdf_hip12       ( 1:dimx,1:dimy,0 ),    pdf_hip16       ( 1:dimx,1:dimy,0 ),   &
            pdf_hip25       ( 1:dimx,1:dimy,0 ),    pdf_hip30       ( 1:dimx,1:dimy,0 ),   &
            pdf_grpsnow     ( 1:dimx,1:dimy,0 ),                                           &
            outfname, plot_tool, output_type="annual")

        ! Annual 2D Map: CTL File for GrADS
        call output_ctl_map2d( &
            outpath_lev3, "binary", "12_months", ym, year, cmon)
        call output_ctl_map2d( &
            outpath_lev3, "netcdf", "12_months", ym, year, cmon)

        call output_ctl_map2d( &
            outpath_lev3, "binary", "annual", ym, year, cmon)
        call output_ctl_map2d( &
            outpath_lev3, "netcdf", "annual", ym, year, cmon)
        ! call output_ctl_map2d_annual(outpath_lev3, year)

    endif  ! ooutyr

    if ( ofinal ) then
        ! All-Term 2D Map: Binary (Unformatted Direct Access)
        outfname = trim( outpath_lev3 ) // "/grads/gpm_map2d_all" // file_suffix_bin
        call output_2d(&
            outfname, &
            n_obs_all           ( 1:dimx,1:dimy ),                                             &
            pdf_precip_all      ( 1:dimx,1:dimy ),    pdf_rain_all        ( 1:dimx,1:dimy ),   &
            mean_precip_surf_all( 1:dimx,1:dimy,1 ),  mean_precip_surf_all( 1:dimx,1:dimy,2 ), &
            mean_precip_path_all( 1:dimx,1:dimy,1 ),  mean_precip_path_all( 1:dimx,1:dimy,2 ), &
            pdf_grpl_all        ( 1:dimx,1:dimy ),    pdf_snowsfc_all     ( 1:dimx,1:dimy ),   &
            pdf_hip0_all        ( 1:dimx,1:dimy ),    pdf_hip1_all        ( 1:dimx,1:dimy ),   &
            pdf_hip2_all        ( 1:dimx,1:dimy ),    pdf_hip3_all        ( 1:dimx,1:dimy ),   &
            pdf_hip4_all        ( 1:dimx,1:dimy ),    pdf_hip8_all        ( 1:dimx,1:dimy ),   &
            pdf_hip12_all       ( 1:dimx,1:dimy ),    pdf_hip16_all       ( 1:dimx,1:dimy ),   &
            pdf_hip25_all       ( 1:dimx,1:dimy ),    pdf_hip30_all       ( 1:dimx,1:dimy ),   &
            pdf_grpsnow_all     ( 1:dimx,1:dimy ))

        outfname = trim( outpath_lev3 ) // "/grads/gpm_map2d_all" // file_suffix_nc
        call output_netcdf_map2d( &
            outpath_lev3, year, &
            glon, glat, &
            n_obs_all           ( 1:dimx,1:dimy ),                                             &
            pdf_precip_all      ( 1:dimx,1:dimy ),    pdf_rain_all        ( 1:dimx,1:dimy ),   &
            mean_precip_surf_all( 1:dimx,1:dimy,1 ),  mean_precip_surf_all( 1:dimx,1:dimy,2 ), &
            mean_precip_path_all( 1:dimx,1:dimy,1 ),  mean_precip_path_all( 1:dimx,1:dimy,2 ), &
            pdf_grpl_all        ( 1:dimx,1:dimy ),    pdf_snowsfc_all     ( 1:dimx,1:dimy ),   &
            pdf_hip0_all        ( 1:dimx,1:dimy ),    pdf_hip1_all        ( 1:dimx,1:dimy ),   &
            pdf_hip2_all        ( 1:dimx,1:dimy ),    pdf_hip3_all        ( 1:dimx,1:dimy ),   &
            pdf_hip4_all        ( 1:dimx,1:dimy ),    pdf_hip8_all        ( 1:dimx,1:dimy ),   &
            pdf_hip12_all       ( 1:dimx,1:dimy ),    pdf_hip16_all       ( 1:dimx,1:dimy ),   &
            pdf_hip25_all       ( 1:dimx,1:dimy ),    pdf_hip30_all       ( 1:dimx,1:dimy ),   &
            pdf_grpsnow_all     ( 1:dimx,1:dimy ),                                           &
            outfname, plot_tool, output_type="all")

       ! All-Term 2D Map: CTL File for GrADS
        call output_ctl_map2d( &
            outpath_lev3, "binary", "all", ym, year, cmon)
        call output_ctl_map2d( &
            outpath_lev3, "netcdf", "all", ym, year, cmon)

    endif  ! ofinal

    !--- CFAD
    if ( odump3d ) then
       ! GrADS format
       ! Monthly CFAD
       if ( ooutmon ) then
          outfname = trim( outpath_lev3 ) // "/grads/gpm_cfad_" // trim(ym) // file_suffix_bin
          call output_4d( outfname, pdf_cfad( 1:dimdbz, 1:dimlev, 1:nreg, 1:nfreq, imon ) )

          outfname = trim( outpath_lev3 ) // "/grads/gpm_cfad_" // trim(ym) // file_suffix_nc
          call output_netcdf_cfad( outfname, plot_tool, gdbz, glev, &
                                   pdf_cfad( 1:dimdbz, 1:dimlev, 1:nreg, 1:nfreq, imon ) )

          ! Monthly CFAD: CTL File for GrADS
          call output_ctl_cfad(outpath_lev3, "binary", "monthly", ym, year, cmon)
          call output_ctl_cfad(outpath_lev3, "netcdf", "monthly", ym, year, cmon)

       endif ! ooutmon

       ! Annual CFAD
       if ( ooutyr ) then
          outfname = trim( outpath_lev3 ) // "/grads/gpm_cfad_" // trim(year) // file_suffix_bin
          call output_4d( outfname, pdf_cfad( 1:dimdbz, 1:dimlev, 1:nreg, 1:nfreq, 0 ) )

          outfname = trim( outpath_lev3 ) // "/grads/gpm_cfad_" // trim(year) // file_suffix_nc
          call output_netcdf_cfad( outfname, plot_tool, gdbz, glev, &
                                   pdf_cfad( 1:dimdbz, 1:dimlev, 1:nreg, 1:nfreq, 0 ) )

          ! Annual CFAD: CTL File for GrADS
          call output_ctl_cfad(outpath_lev3, "binary", "annual", ym, year, cmon)
          call output_ctl_cfad(outpath_lev3, "netcdf", "annual", ym, year, cmon)

        endif ! ooutyr

       ! All-Term CFAD
       if ( ofinal ) then
          outfname = trim( outpath_lev3 ) // "/grads/gpm_cfad_all" // file_suffix_bin
          call output_4d( outfname, pdf_cfad_all( 1:dimdbz, 1:dimlev, 1:nreg, 1:nfreq ) )

          outfname = trim( outpath_lev3 ) // "/grads/gpm_cfad_all" // file_suffix_nc
          call output_netcdf_cfad( outfname, plot_tool, gdbz, glev, &
                                   pdf_cfad_all( 1:dimdbz, 1:dimlev, 1:nreg, 1:nfreq ) )

          ! All-Term CFAD: CTL File for GrADS
          call output_ctl_cfad(outpath_lev3, "binary", "all", ym, year, cmon)
          call output_ctl_cfad(outpath_lev3, "netcdf", "all", ym, year, cmon)

        endif ! ofinal
    endif    ! odump3d

end subroutine output_grads_data

subroutine output_2d( &
    outfname, &
    var1,  var2,  var3,  var4,  var5,  &
    var6,  var7,  var8,  var9,  var10, &
    var11, var12, var13, var14, var15, &
    var16, var17, var18, var19, var20  )

    character(len=*), intent(in) :: outfname
    real(sp), dimension(:, :), intent(in) :: &
        var1,  var2,  var3,  var4,  var5,  &
        var6,  var7,  var8,  var9,  var10, &
        var11, var12, var13, var14, var15, &
        var16, var17, var18, var19, var20
    integer :: file_idx = 43

    open( file_idx, file = trim( outfname ), access = "direct", status = "unknown", recl = 4*dimx*dimy*nvar )
    write( file_idx, rec=1 ) &
        var1,  var2,  var3,  var4,  var5,  &
        var6,  var7,  var8,  var9,  var10, &
        var11, var12, var13, var14, var15, &
        var16, var17, var18, var19, var20
    close( file_idx )

end subroutine output_2d

subroutine output_3d( &
    outfname, &
    var1,  var2,  var3,  var4,  var5,  &
    var6,  var7,  var8,  var9,  var10, &
    var11, var12, var13, var14, var15, &
    var16, var17, var18, var19, var20  )

    character(len=*), intent(in) :: outfname
    real(sp), dimension(:, :, :), intent(in) :: &
        var1,  var2,  var3,  var4,  var5,  &
        var6,  var7,  var8,  var9,  var10, &
        var11, var12, var13, var14, var15, &
        var16, var17, var18, var19, var20
    integer :: file_idx = 43

    open( file_idx, file = trim( outfname ), access = "direct", status = "unknown", recl = 4*dimx*dimy*nvar*nmon )
    write( file_idx, rec=1 ) &
        var1,  var2,  var3,  var4,  var5,  &
        var6,  var7,  var8,  var9,  var10, &
        var11, var12, var13, var14, var15, &
        var16, var17, var18, var19, var20
    close( file_idx )

end subroutine output_3d

subroutine output_4d(outfname, var)

    character(len=*), intent(in) :: outfname
    real(sp), dimension(:, :, :, :), intent(in) :: var
    integer :: file_idx = 45

    open( file_idx, file = trim( outfname ), access = "direct", status = "unknown", recl = 4*dimdbz*dimlev*nfreq*nreg )
    write( file_idx, rec=1 ) var
    close( file_idx )

end subroutine output_4d

! output CTL file for 2D map
subroutine output_ctl_map2d(outpath_lev3, data_type, output_type, ym, year, cmon)
    implicit none
    character(len=*), intent(in) :: outpath_lev3, data_type, output_type
    character(len=*), intent(in) :: ym, year, cmon
    character(len = 128) :: outfname, dset, tdef
    character(len = 20) :: title, level
    character(len = 6) :: data_suffix, data_label
    integer :: file_idx = 51

    select case(data_type)
    case("binary")
        data_suffix = file_suffix_bin
        data_label = ""
        level = "0 99"
    case("netcdf")
        data_suffix = file_suffix_nc
        data_label = "_nc"
        if (output_type == "12_months") then
            level = "1 t,y,x"
        else
            level = "1 y,x"
        endif
    end select

    select case(output_type)
    case("monthly")
        outfname = trim( outpath_lev3 ) // "/grads/gpm_map2d_" // trim(ym) // trim(data_label) // file_suffix_ctl
        dset = "^gpm_map2d_" // trim(ym) // data_suffix
        title = "Monthly"
        tdef = "1 linear 01" // trim(cmon) // trim(year) // " 1mo"
    case("12_months")
        outfname = trim( outpath_lev3 ) // "/grads/gpm_map2d_" // trim(year) // trim(data_label) // file_suffix_ctl
        dset = "^gpm_map2d_" // trim(year) // data_suffix
        title = "Monthly"
        tdef = "12 linear 01jan"//trim(year)//" 1mo"
    case("annual")
        outfname = trim( outpath_lev3 ) // "/grads/gpm_map2d_" // trim(year) // "_ann" // trim(data_label) // file_suffix_ctl
        dset = "^gpm_map2d_" // trim(year) // "_ann" // data_suffix
        title = "Annual"
        tdef = "1 linear 01jan"//trim(year)//" 1yr"
    case("all")
        outfname = trim( outpath_lev3 ) // "/grads/gpm_map2d_all" // trim(data_label) // file_suffix_ctl
        dset = "^gpm_map2d_all" // data_suffix
        title = "(Climatology)"
        tdef = "1 linear 01jan2000 1yr"  ! dummy
    end select

    open(  file_idx, file = trim( outfname ), status = "unknown" )
    write( file_idx,* ) "DSET " // trim(dset)
    if (data_type == "netcdf") then
        write( file_idx,* ) "DTYPE netcdf"
    endif
    write( file_idx,* ) "TITLE   GPM V07A Level3 " // trim(title)
    write( file_idx,* ) "UNDEF", undef
    write( file_idx,* ) "XDEF ", dimx, "linear ", -180.0_sp + ( reso/2.0_sp ), reso
    write( file_idx,* ) "YDEF ", dimy, "linear ",  -90.0_sp + ( reso/2.0_sp ), reso
    write( file_idx,* ) "ZDEF 1 levels 1000"
    write( file_idx,* ) "TDEF " // trim(tdef)
    write( file_idx,* ) "VARS  ", nvar
    write( file_idx,* ) "n_obs           " // trim(level) // " unit [#]"
    write( file_idx,* ) "pdf_precip      " // trim(level) // " unit [%]"
    write( file_idx,* ) "pdf_rain        " // trim(level) // " unit [%]"
    write( file_idx,* ) "prcp_sfc_con    " // trim(level) // " unit [mm/hr]"
    write( file_idx,* ) "prcp_sfc_uco    " // trim(level) // " unit [mm/hr]"
    write( file_idx,* ) "prcpl_path      " // trim(level) // " unit [g/m2]"
    write( file_idx,* ) "prcps_path      " // trim(level) // " unit [g/m2]"
    write( file_idx,* ) "pdf_grpl        " // trim(level) // " unit [%]"
    write( file_idx,* ) "pdf_snowsfc     " // trim(level) // " unit [%]"
    write( file_idx,* ) "pdf_hip0        " // trim(level) // " unit [%]"
    write( file_idx,* ) "pdf_hip1        " // trim(level) // " unit [%]"
    write( file_idx,* ) "pdf_hip2        " // trim(level) // " unit [%]"
    write( file_idx,* ) "pdf_hip3        " // trim(level) // " unit [%]"
    write( file_idx,* ) "pdf_hip4        " // trim(level) // " unit [%]"
    write( file_idx,* ) "pdf_hip8        " // trim(level) // " unit [%]"
    write( file_idx,* ) "pdf_hip12       " // trim(level) // " unit [%]"
    write( file_idx,* ) "pdf_hip16       " // trim(level) // " unit [%]"
    write( file_idx,* ) "pdf_hip25       " // trim(level) // " unit [%]"
    write( file_idx,* ) "pdf_hip30       " // trim(level) // " unit [%]"
    write( file_idx,* ) "pdf_grpsnow     " // trim(level) // " unit [%]"
    write( file_idx,* ) "ENDVARS"
    close( file_idx )

end subroutine output_ctl_map2d

! output CTL file for CFAD
subroutine output_ctl_cfad(outpath_lev3, data_type, output_type, ym, year, cmon)
    implicit none
    character(len=*), intent(in) :: outpath_lev3, data_type, output_type
    character(len=*), intent(in) :: ym, year, cmon
    character(len = 128) :: outfname, dset, tdef
    character(len = 20) :: title, level
    character(len = 6) :: data_suffix, data_label
    integer :: file_idx = 55

    select case(data_type)
    case("binary")
        data_suffix = file_suffix_bin
        data_label = ""
        level = "0 99"
    case("netcdf")
        data_suffix = file_suffix_nc
        data_label = "_nc"
        level = "1 y,x"
    end select

    select case(output_type)
    case("monthly")
        outfname = trim( outpath_lev3 ) // "/grads/gpm_cfad_" // trim(ym) // trim(data_label) // file_suffix_ctl
        dset = "^gpm_cfad_" // trim(ym) // data_suffix
        title = "Monthly CFAD"
        tdef = "1 linear 01" // trim(cmon) // trim(year) // " 1mo"
    case("annual")
        outfname = trim( outpath_lev3 ) // "/grads/gpm_cfad_" // trim(year) // trim(data_label) // file_suffix_ctl
        dset = "^gpm_cfad_" // trim(year) // data_suffix
        title = "Annual CFAD"
        tdef = "1 linear 01jan"//trim(year)// " 1yr"
    case("all")
        outfname = trim( outpath_lev3 ) // "/grads/gpm_cfad_all" // trim(data_label) // file_suffix_ctl
        dset = "^gpm_cfad_all" // data_suffix
        title = "CFAD (Climatology)"
        tdef = "1 linear 01jan2000  1yr" ! dummy
    end select

    open(  file_idx, file = trim( outfname ), status = "unknown" )
    write( file_idx,* ) "DSET " // trim(dset)
    if (data_type == "netcdf") then
        write( file_idx,* ) "DTYPE netcdf"
    endif
    write( file_idx,* ) "TITLE   GPM V07A Level3 " // trim(title)
    write( file_idx,* ) "UNDEF", undef
    write( file_idx,* ) "XDEF ", dimdbz, "linear ", cfad_dbzmin + ( cfad_ddbz/2.0_sp ), cfad_ddbz
    write( file_idx,* ) "YDEF ", dimlev, "linear ", cfad_levmin + ( cfad_dlev/2.0_sp ), cfad_dlev
    write( file_idx,* ) "ZDEF 1 levels 1000"
    write( file_idx,* ) "TDEF " // trim(tdef)
    write( file_idx,* ) "VARS  ", nfreq*nreg
    write( file_idx,* ) "cfad_ku_trwp      " // trim(level) // " unit [none]"
    write( file_idx,* ) "cfad_ku_itcz      " // trim(level) // " unit [none]"
    write( file_idx,* ) "cfad_ku_spcz      " // trim(level) // " unit [none]"
    write( file_idx,* ) "cfad_ku_nepa      " // trim(level) // " unit [none]"
    write( file_idx,* ) "cfad_ku_cast      " // trim(level) // " unit [none]"
    write( file_idx,* ) "cfad_ku_peru      " // trim(level) // " unit [none]"
    write( file_idx,* ) "cfad_ku_natl      " // trim(level) // " unit [none]"
    write( file_idx,* ) "cfad_ku_nami      " // trim(level) // " unit [none]"
    write( file_idx,* ) "cfad_ku_aust      " // trim(level) // " unit [none]"
    write( file_idx,* ) "cfad_ku_japn      " // trim(level) // " unit [none]"
    write( file_idx,* ) "cfad_ku_eqct      " // trim(level) // " unit [none]"
    write( file_idx,* ) "cfad_ku_easi      " // trim(level) // " unit [none]"
    write( file_idx,* ) "cfad_ku_soce      " // trim(level) // " unit [none]"
    write( file_idx,* ) "cfad_ku_glob      " // trim(level) // " unit [none]"
    write( file_idx,* ) "cfad_ka_trwp      " // trim(level) // " unit [none]"
    write( file_idx,* ) "cfad_ka_itcz      " // trim(level) // " unit [none]"
    write( file_idx,* ) "cfad_ka_spcz      " // trim(level) // " unit [none]"
    write( file_idx,* ) "cfad_ka_nepa      " // trim(level) // " unit [none]"
    write( file_idx,* ) "cfad_ka_cast      " // trim(level) // " unit [none]"
    write( file_idx,* ) "cfad_ka_peru      " // trim(level) // " unit [none]"
    write( file_idx,* ) "cfad_ka_natl      " // trim(level) // " unit [none]"
    write( file_idx,* ) "cfad_ka_nami      " // trim(level) // " unit [none]"
    write( file_idx,* ) "cfad_ka_aust      " // trim(level) // " unit [none]"
    write( file_idx,* ) "cfad_ka_japn      " // trim(level) // " unit [none]"
    write( file_idx,* ) "cfad_ka_eqct      " // trim(level) // " unit [none]"
    write( file_idx,* ) "cfad_ka_easi      " // trim(level) // " unit [none]"
    write( file_idx,* ) "cfad_ka_soce      " // trim(level) // " unit [none]"
    write( file_idx,* ) "cfad_ka_glob      " // trim(level) // " unit [none]"
    write( file_idx,* ) "ENDVARS"
    close( file_idx )

end subroutine output_ctl_cfad
 
end module grads_data_processor
