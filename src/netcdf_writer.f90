module netcdf_writer
    use parameters
    use netcdf

    implicit none
    private check

contains

subroutine output_netcdf_cfad( outfname, plot_tool, gdbz, glev, pdf_cfad )
    implicit none
    character(len=*), intent(in) :: outfname
    character(len=*), intent(in) :: plot_tool
    real(sp), dimension(:), intent(in) :: gdbz( idbz_max + 1 ) ! grid value (CFAD dBZ)
    real(sp), dimension(:), intent(in) :: glev( ilev_max + 1 ) ! grid value (CFAD Lev)
    real(sp), dimension(:, :, :, :), intent(in) :: pdf_cfad
    character(len=6), dimension(:), allocatable :: reg_name_list, long_reg_name_list
    character(len=2), dimension(nfreq) :: freq_name_list, capital_freq_name_list

    integer :: ncid, oldmode
    integer :: file_type, dimt, idx, ireg, ifreq
    integer, dimension(2) :: dim_id_list
    integer, dimension(1) :: dbz_dim_id, lev_dim_id, time_dim_id
    integer :: dbz_var_id, lev_var_id, time_var_id
    integer, dimension(nreg*nfreq) :: var_id_list

    ! set dimension of time
    dimt = 1

    ! select NetCDF format
    select case(plot_tool)
    case("grads")
        file_type = NF90_NETCDF4
    case("gmt")
        file_type = NF90_NETCDF4
    case("gtool")
        file_type = NF90_NETCDF4
    end select

    allocate(reg_name_list(nreg))
    allocate(long_reg_name_list(nreg))

    select case(nreg)
    case(3)
        reg_name_list = (/ "tropic", "midlat", "global" /)
        long_reg_name_list = (/ "Tropics", "Midlatitudes", "Global" /)
    case(14)
        reg_name_list = (/ "trwp", "itcz", "spcz", "nepa", "cast", "peru", "natl", &
                           "nami", "aust", "japn", "eqct", "easi", "soce", "glob" /)
        long_reg_name_list = (/ "Tropical Warm Pool (5S-15N,70E-135E)", &
                            "ITCZ (5N-15N,140E-140W)", &
                            "SPCZ (15S-5S,150E-130W)", &
                            "North East Pacific (25N-50N,160W-135W)", &
                            "California Stratocumulus (15N-35N,130W-110W)", &
                            "Peruvian (10S-30S,90W-70W)", &
                            "North Atlantic (30N-60N,45W-10W)", &
                            "Namibian (30S-0S,25W-15E)", &
                            "Australian (15S-35S,80E-115E)", &
                            "Japan (25N-50N,125E-150E)", &
                            "Eqt. cold tongue (5S-5N,130W-85W)", &
                            "Eastern Asia (20N-40N,105E-120E)", &
                            "Southern Ocean (40S-60S)", &
                            "Global"/)
    end select

    freq_name_list = (/ "ku", "ka" /)
    capital_freq_name_list = (/ "Ku", "Ka" /)

    ! create output file
    call check( nf90_create( trim(outfname), file_type, ncid ))

    ! define dimensions
    call check( nf90_def_dim( ncid, "dbz", dimdbz, dbz_dim_id(1) ) )
    call check( nf90_def_dim( ncid, "lev", dimlev, lev_dim_id(1) ) )

    ! define variables
    call define_var( ncid, var_id=dbz_var_id, &
                     dim_id=dbz_dim_id, var_name="dbz", var_type=NF90_FLOAT, &
                     units="dBZe", long_name="Reflectivity", &
                     standard_name="reflectivity", axis="X")

    call define_var( ncid, var_id=lev_var_id, &
                     dim_id=lev_dim_id, var_name="lev", var_type=NF90_FLOAT, &
                     units="km", long_name="Altitude", &
                     standard_name="altitude", axis="Y")

    dim_id_list = (/dbz_dim_id, lev_dim_id/)

    do ifreq = 1, nfreq
        do ireg = 1, nreg
            idx = (ifreq - 1) * nreg + ireg
            call define_var( ncid, var_id=var_id_list(idx), dim_id=dim_id_list, &
                             var_name="cfad_" // trim(freq_name_list(ifreq)) // "_" // trim(reg_name_list(ireg)), &
                             var_type=NF90_FLOAT, &
                             units="none", &
                             long_name=trim(capital_freq_name_list(ifreq)) // "&" // trim(long_reg_name_list(ireg)))
        enddo
    enddo

    ! set global attributes
    call check( nf90_put_att(ncid,nf90_global,"title", &
                "GPM V07A Level3 (CFAD)"))

    call check( nf90_enddef( ncid ) )

    ! write variables
    call check( nf90_put_var(ncid, dbz_var_id, gdbz(2::2)) )
    call check( nf90_put_var(ncid, lev_var_id, glev(2::2)) )

    do ifreq = 1, nfreq
        do ireg = 1, nreg
            idx = (ifreq - 1) * nreg + ireg
            call check( nf90_put_var( ncid, &
                                      var_id_list(idx), &
                                      pdf_cfad( :, :, ireg, ifreq )) )
        enddo
    enddo

    deallocate(reg_name_list)
    deallocate(long_reg_name_list)

end subroutine output_netcdf_cfad

subroutine output_netcdf_map2d( &
    outpath_lev3, year, &
    glon, glat, &
    n_obs, &
    pdf_precip, pdf_rain, &
    prcp_sfc_con, prcp_sfc_uco, &
    prcpl_path, prcps_path, &
    pdf_grpl, pdf_snowsfc, pdf_grpsnow, &
    pdf_hip0, pdf_hip1, pdf_hip2, pdf_hip3, pdf_hip4, &
    pdf_hip8, pdf_hip12, pdf_hip16, pdf_hip25, pdf_hip30, &
    outfname, plot_tool, output_type, imon)

    implicit none
    character(len=*), intent(in) :: year, outpath_lev3
    real(sp), dimension(:), intent(in) :: glon                 ! grid value (longitude)
    real(sp), dimension(:), intent(in) :: glat                 ! grid value (latitude)

    real(sp), dimension(:, :), intent(in) :: n_obs

    !--- Output Variables
    ! monthly
    real(sp), dimension(:, :), intent(in) :: prcp_sfc_con  ! conditional
    real(sp), dimension(:, :), intent(in) :: prcp_sfc_uco  ! unconditional
    real(sp), dimension(:, :), intent(in) :: prcpl_path    ! RWP
    real(sp), dimension(:, :), intent(in) :: prcps_path    ! SWP
    real(sp), dimension(:, :), intent(in) :: pdf_precip    ! PDF of precip. against all profiles within the column (PoP)
    real(sp), dimension(:, :), intent(in) :: pdf_rain      ! occurrence of rain for precipitating column (Nrain/Nprecip [%])
    real(sp), dimension(:, :), intent(in) :: pdf_grpl
    real(sp), dimension(:, :), intent(in) :: pdf_snowsfc
    real(sp), dimension(:, :), intent(in) :: pdf_grpsnow
    real(sp), dimension(:, :), intent(in) :: pdf_hip0,  pdf_hip1,  pdf_hip2,  pdf_hip3,  pdf_hip4,  pdf_hip8, &
                                            pdf_hip12, pdf_hip16, pdf_hip25, pdf_hip30
    character(len=*), intent(in) :: outfname, output_type
    character(len=*), intent(in) :: plot_tool
    integer, intent(in), optional :: imon

    integer, dimension(0:nmon) :: time

    integer :: ncid, oldmode
    integer, dimension(1) :: lon_dim_id, lat_dim_id, time_dim_id
    integer, dimension(nvar) :: var_id_list
    character(len=20), dimension(nvar) :: var_name_list, units_list
    character(len=128), dimension(nvar) :: long_name_list
    integer :: lon_var_id, lat_var_id, time_var_id
    integer, dimension(2) :: dim_id_list
    integer :: file_type, mon_ini, mon_fin, dimt, ivar

    time = (/0:nmon/)
    select case(output_type)
    case("12_months")
        mon_ini = 1
        mon_fin = 12
        dimt = 12
    case("annual")
        mon_ini = 1
        mon_fin = 1
        dimt = 1
    case("monthly")
        mon_ini = 1
        mon_fin = 1
        dimt = 1
    end select

    ! select NetCDF format
    select case(plot_tool)
    case("grads")
        file_type = NF90_NETCDF4
    case("gmt")
        file_type = NF90_NETCDF4
    case("gtool")
        file_type = NF90_CLASSIC_MODEL
    end select

    ! create output file
    call check( nf90_create( trim(outfname), file_type, ncid ))

    ! define dimensions
    call check( nf90_def_dim( ncid, "lon", dimx, lon_dim_id(1) ) )
    call check( nf90_def_dim( ncid, "lat", dimy, lat_dim_id(1) ) )

    ! define variables
    call define_var( ncid, var_id=lon_var_id, &
                     dim_id=lon_dim_id, var_name="lon", var_type=NF90_FLOAT, &
                     units="degrees_east", long_name="Longitude", &
                     standard_name="longitude", axis="X")

    call define_var( ncid, var_id=lat_var_id, &
                     dim_id=lat_dim_id, var_name="lat", var_type=NF90_FLOAT, &
                     units="degrees_north", long_name="Latitude", &
                     standard_name="latitude", axis="Y")

    dim_id_list = (/lon_dim_id, lat_dim_id/)

    var_name_list = (/ "n_obs", "pdf_precip", "pdf_rain", &
                       "prcp_sfc_con", "prcp_sfc_uco", "prcpl_path", "prcps_path", &
                       "pdf_grpl", "pdf_snowsfc", &
                       "pdf_hip0", "pdf_hip1", "pdf_hip2", "pdf_hip3", "pdf_hip4", &
                       "pdf_hip8", "pdf_hip12", "pdf_hip16", "pdf_hip25", "pdf_hip30", &
                       "pdf_grpsnow" /)
    units_list = (/ "#", "%", "%", &
                    "mm/hr (con)", "mm/hr (uco)", "g/m2", "g/m2", &
                    "%", "%", &
                    "%", "%", "%", "%", "%", &
                    "%", "%", "%", "%", "%", &
                    "%" /)
    long_name_list = (/ "# of Observation", &
                        "PDF of precip. against all profiles within the column (PoP)", &
                        "occurrence of rain for precipitating column (Nrain/Nprecip)", &
                        "precipRateESurface (conditional)", &
                        "precipRateESurface (unconditional)", &
                        "precipWaterIntegrated (RWP)", &
                        "precipWaterIntegrated (SWP)", &
                        "Occ Freq of Graupel", &
                        "Occ Freq of Snowfall", &
                        "Occ Freq of HeavyIcePrecip0", &
                        "Occ Freq of HeavyIcePrecip1", &
                        "Occ Freq of HeavyIcePrecip2", &
                        "Occ Freq of HeavyIcePrecip3", &
                        "Occ Freq of HeavyIcePrecip4", &
                        "Occ Freq of HeavyIcePrecip8", &
                        "Occ Freq of HeavyIcePrecip12", &
                        "Occ Freq of HeavyIcePrecip16", &
                        "Occ Freq of HeavyIcePrecip25", &
                        "Occ Freq of HeavyIcePrecip30", &
                        "Occ Freq of Graupel with Snow" /)

    do ivar = 1, nvar
        call define_var( ncid, var_id=var_id_list(ivar), &
                        dim_id=dim_id_list, var_name=var_name_list(ivar), var_type=NF90_FLOAT, &
                        units=units_list(ivar), long_name=long_name_list(ivar))
    enddo

    ! set global attributes
    call check( nf90_put_att(ncid,nf90_global,"title", &
                "GPM V07A Level3 (Global 2-D Map)"))

    call check( nf90_enddef( ncid ) )

    ! write variables
    call check( nf90_put_var(ncid, lon_var_id, glon(2::2)) )
    call check( nf90_put_var(ncid, lat_var_id, glat(2::2)) )

    call check( nf90_put_var(ncid, var_id_list( 1), n_obs       ) )
    call check( nf90_put_var(ncid, var_id_list( 2), pdf_precip  ) )
    call check( nf90_put_var(ncid, var_id_list( 3), pdf_rain    ) )
    call check( nf90_put_var(ncid, var_id_list( 4), prcp_sfc_con) )
    call check( nf90_put_var(ncid, var_id_list( 5), prcp_sfc_uco) )
    call check( nf90_put_var(ncid, var_id_list( 6), prcpl_path  ) )
    call check( nf90_put_var(ncid, var_id_list( 7), prcps_path  ) )
    call check( nf90_put_var(ncid, var_id_list( 8), pdf_grpl    ) )
    call check( nf90_put_var(ncid, var_id_list( 9), pdf_snowsfc ) )
    call check( nf90_put_var(ncid, var_id_list(10), pdf_hip0    ) )
    call check( nf90_put_var(ncid, var_id_list(11), pdf_hip1    ) )
    call check( nf90_put_var(ncid, var_id_list(12), pdf_hip2    ) )
    call check( nf90_put_var(ncid, var_id_list(13), pdf_hip3    ) )
    call check( nf90_put_var(ncid, var_id_list(14), pdf_hip4    ) )
    call check( nf90_put_var(ncid, var_id_list(15), pdf_hip8    ) )
    call check( nf90_put_var(ncid, var_id_list(16), pdf_hip12   ) )
    call check( nf90_put_var(ncid, var_id_list(17), pdf_hip16   ) )
    call check( nf90_put_var(ncid, var_id_list(18), pdf_hip25   ) )
    call check( nf90_put_var(ncid, var_id_list(19), pdf_hip30   ) )
    call check( nf90_put_var(ncid, var_id_list(20), pdf_grpsnow ) )

    ! close file
    call check( nf90_close(ncid) )

end subroutine output_netcdf_map2d

subroutine output_netcdf_map2d_12months( &
    outpath_lev3, year, &
    glon, glat, &
    n_obs, &
    pdf_precip, pdf_rain, &
    prcp_sfc_con, prcp_sfc_uco, &
    prcpl_path, prcps_path, &
    pdf_grpl, pdf_snowsfc, pdf_grpsnow, &
    pdf_hip0, pdf_hip1, pdf_hip2, pdf_hip3, pdf_hip4, &
    pdf_hip8, pdf_hip12, pdf_hip16, pdf_hip25, pdf_hip30, &
    outfname, plot_tool, output_type, imon)

    implicit none
    character(len=*), intent(in) :: year, outpath_lev3
    real(sp), dimension(:), intent(in) :: glon                 ! grid value (longitude)
    real(sp), dimension(:), intent(in) :: glat                 ! grid value (latitude)

    real(sp), dimension(:, :, :), intent(in) :: n_obs

    !--- precipitation and occurence frequency
    real(sp), dimension(:, :, :), intent(in) :: prcp_sfc_con  ! conditional
    real(sp), dimension(:, :, :), intent(in) :: prcp_sfc_uco  ! unconditional
    real(sp), dimension(:, :, :), intent(in) :: prcpl_path    ! RWP
    real(sp), dimension(:, :, :), intent(in) :: prcps_path    ! SWP
    real(sp), dimension(:, :, :), intent(in) :: pdf_precip    ! PDF of precip. against all profiles within the column (PoP)
    real(sp), dimension(:, :, :), intent(in) :: pdf_rain      ! occurrence of rain for precipitating column (Nrain/Nprecip [%])
    real(sp), dimension(:, :, :), intent(in) :: pdf_grpl
    real(sp), dimension(:, :, :), intent(in) :: pdf_snowsfc
    real(sp), dimension(:, :, :), intent(in) :: pdf_grpsnow
    real(sp), dimension(:, :, :), intent(in) :: pdf_hip0,  pdf_hip1,  pdf_hip2,  pdf_hip3,  pdf_hip4,  pdf_hip8, &
                                                pdf_hip12, pdf_hip16, pdf_hip25, pdf_hip30
    character(len=*), intent(in) :: outfname, output_type
    character(len=*), intent(in) :: plot_tool
    integer, intent(in), optional :: imon

    integer, dimension(0:nmon) :: time

    ! ids
    integer :: ncid, oldmode
    integer, dimension(1) :: lon_dim_id, lat_dim_id, time_dim_id
    integer, dimension(nvar) :: var_id_list
    integer :: lon_var_id, lat_var_id, time_var_id
    integer, dimension(3) :: dim_id_list

    ! attributes
    character(len=20), dimension(nvar) :: var_name_list, units_list
    character(len=128), dimension(nvar) :: long_name_list

    integer :: file_type, mon_ini, mon_fin, dimt, ivar

    time = (/0:nmon/)
    select case(output_type)
    case("12_months")
        mon_ini = 1
        mon_fin = 12
        dimt = 12
    case("annual")
        mon_ini = 1
        mon_fin = 1
        dimt = 1
    case("monthly")
        mon_ini = 1
        mon_fin = 1
        dimt = 1
    end select

    ! select NetCDF format
    select case(plot_tool)
    case("grads")
        file_type = NF90_NETCDF4
    case("gmt")
        file_type = NF90_NETCDF4
    case("gtool")
        file_type = NF90_CLASSIC_MODEL
    end select

    ! create output file
    call check( nf90_create( trim(outfname), file_type, ncid ))

    ! define dimensions
    call check( nf90_def_dim( ncid, "lon", dimx, lon_dim_id(1) ) )
    call check( nf90_def_dim( ncid, "lat", dimy, lat_dim_id(1) ) )
    call check( nf90_def_dim( ncid, "time", dimt, time_dim_id(1) ) )

    ! define variables
    call define_var( ncid, var_id=lon_var_id, &
                     dim_id=lon_dim_id, var_name="lon", var_type=NF90_FLOAT, &
                     units="degrees_east", long_name="Longitude", &
                     standard_name="longitude", axis="X")

    call define_var( ncid, var_id=lat_var_id, &
                     dim_id=lat_dim_id, var_name="lat", var_type=NF90_FLOAT, &
                     units="degrees_north", long_name="Latitude", &
                     standard_name="latitude", axis="Y")

    call define_var( ncid, var_id=time_var_id, &
                     dim_id=time_dim_id, var_name="time", var_type=NF90_INT, &
                     units="month", long_name="Time", &
                     standard_name="time")

    dim_id_list = (/lon_dim_id, lat_dim_id, time_dim_id/)

    var_name_list = (/ "n_obs", "pdf_precip", "pdf_rain", &
                       "prcp_sfc_con", "prcp_sfc_uco", "prcpl_path", "prcps_path", &
                       "pdf_grpl", "pdf_snowsfc", &
                       "pdf_hip0", "pdf_hip1", "pdf_hip2", "pdf_hip3", "pdf_hip4", &
                       "pdf_hip8", "pdf_hip12", "pdf_hip16", "pdf_hip25", "pdf_hip30", &
                       "pdf_grpsnow" /)
    units_list = (/ "#", "%", "%", &
                    "mm/hr (con)", "mm/hr (uco)", "g/m2", "g/m2", &
                    "%", "%", &
                    "%", "%", "%", "%", "%", &
                    "%", "%", "%", "%", "%", &
                    "%" /)
    long_name_list = (/ "# of Observation", &
                        "PDF of precip. against all profiles within the column (PoP)", &
                        "occurrence of rain for precipitating column (Nrain/Nprecip)", &
                        "precipRateESurface (conditional)", &
                        "precipRateESurface (unconditional)", &
                        "precipWaterIntegrated (RWP)", &
                        "precipWaterIntegrated (SWP)", &
                        "Occ Freq of Graupel", &
                        "Occ Freq of Snowfall", &
                        "Occ Freq of HeavyIcePrecip0", &
                        "Occ Freq of HeavyIcePrecip1", &
                        "Occ Freq of HeavyIcePrecip2", &
                        "Occ Freq of HeavyIcePrecip3", &
                        "Occ Freq of HeavyIcePrecip4", &
                        "Occ Freq of HeavyIcePrecip8", &
                        "Occ Freq of HeavyIcePrecip12", &
                        "Occ Freq of HeavyIcePrecip16", &
                        "Occ Freq of HeavyIcePrecip25", &
                        "Occ Freq of HeavyIcePrecip30", &
                        "Occ Freq of Graupel with Snow" /)

    do ivar = 1, nvar
        call define_var( ncid, var_id=var_id_list(ivar), &
                        dim_id=dim_id_list, var_name=var_name_list(ivar), var_type=NF90_FLOAT, &
                        units=units_list(ivar), long_name=long_name_list(ivar))
    enddo

    ! set global attributes
    call check( nf90_put_att(ncid,nf90_global,"title", &
                "GPM V07A Level3 (Global 2-D Map)"))

    call check( nf90_enddef( ncid ) )

    ! write variables
    call check( nf90_put_var(ncid, lon_var_id, glon(2::2)) )
    call check( nf90_put_var(ncid, lat_var_id, glat(2::2)) )
    call check( nf90_put_var(ncid, time_var_id, time(mon_ini:mon_fin)) )

    call check( nf90_put_var(ncid, var_id_list( 1), n_obs           ( 1:dimx,1:dimy,mon_ini:mon_fin )) )
    call check( nf90_put_var(ncid, var_id_list( 2), pdf_precip      ( 1:dimx,1:dimy,mon_ini:mon_fin )) )
    call check( nf90_put_var(ncid, var_id_list( 3), pdf_rain        ( 1:dimx,1:dimy,mon_ini:mon_fin )) )
    call check( nf90_put_var(ncid, var_id_list( 4), prcp_sfc_con    ( 1:dimx,1:dimy,mon_ini:mon_fin )) )
    call check( nf90_put_var(ncid, var_id_list( 5), prcp_sfc_uco    ( 1:dimx,1:dimy,mon_ini:mon_fin )) )
    call check( nf90_put_var(ncid, var_id_list( 6), prcpl_path      ( 1:dimx,1:dimy,mon_ini:mon_fin )) )
    call check( nf90_put_var(ncid, var_id_list( 7), prcps_path      ( 1:dimx,1:dimy,mon_ini:mon_fin )) )
    call check( nf90_put_var(ncid, var_id_list( 8), pdf_grpl        ( 1:dimx,1:dimy,mon_ini:mon_fin )) )
    call check( nf90_put_var(ncid, var_id_list( 9), pdf_snowsfc     ( 1:dimx,1:dimy,mon_ini:mon_fin )) )
    call check( nf90_put_var(ncid, var_id_list(10), pdf_hip0        ( 1:dimx,1:dimy,mon_ini:mon_fin )) )
    call check( nf90_put_var(ncid, var_id_list(11), pdf_hip1        ( 1:dimx,1:dimy,mon_ini:mon_fin )) )
    call check( nf90_put_var(ncid, var_id_list(12), pdf_hip2        ( 1:dimx,1:dimy,mon_ini:mon_fin )) )
    call check( nf90_put_var(ncid, var_id_list(13), pdf_hip3        ( 1:dimx,1:dimy,mon_ini:mon_fin )) )
    call check( nf90_put_var(ncid, var_id_list(14), pdf_hip4        ( 1:dimx,1:dimy,mon_ini:mon_fin )) )
    call check( nf90_put_var(ncid, var_id_list(15), pdf_hip8        ( 1:dimx,1:dimy,mon_ini:mon_fin )) )
    call check( nf90_put_var(ncid, var_id_list(16), pdf_hip12       ( 1:dimx,1:dimy,mon_ini:mon_fin )) )
    call check( nf90_put_var(ncid, var_id_list(17), pdf_hip16       ( 1:dimx,1:dimy,mon_ini:mon_fin )) )
    call check( nf90_put_var(ncid, var_id_list(18), pdf_hip25       ( 1:dimx,1:dimy,mon_ini:mon_fin )) )
    call check( nf90_put_var(ncid, var_id_list(19), pdf_hip30       ( 1:dimx,1:dimy,mon_ini:mon_fin )) )
    call check( nf90_put_var(ncid, var_id_list(20), pdf_grpsnow     ( 1:dimx,1:dimy,mon_ini:mon_fin )) )

    ! close file
    call check( nf90_close(ncid) )

end subroutine output_netcdf_map2d_12months

! define netcdf variables and put attributions
subroutine define_var(&
    ncid, var_id, &
    dim_id, var_name, var_type, &
    units, long_name, &
    standard_name, axis)

    implicit none
    integer, intent(out) :: var_id
    integer, intent(in) :: ncid
    integer, dimension(:), intent(in) :: dim_id
    integer, intent(in) :: var_type
    character(len=*), intent(in) :: var_name, units, long_name
    character(len=*), intent(in), optional :: standard_name, axis

    call check( nf90_def_var( ncid, var_name, var_type, dim_id, var_id) )

    ! put attribute
    call check( nf90_put_att( ncid, var_id, "units", units ))
    call check( nf90_put_att( ncid, var_id, "long_name", long_name ))

    if ( present( standard_name ) ) then
        call check( nf90_put_att( ncid, var_id, "standard_name", standard_name ))
    endif

    if ( present( axis ) ) then
        call check( nf90_put_att( ncid, var_id, "axis", axis ))
    endif

    ! specify undefined values
    call check( nf90_def_var_fill( ncid, var_id, 0, undef ))

end subroutine define_var

! check operations
subroutine check(status)
    implicit none
    integer, intent(in) :: status

    if(status /= nf90_noerr) then
       print *, trim(nf90_strerror(status))
       stop "NetCDF Error"
    end if
end subroutine check

end module netcdf_writer
