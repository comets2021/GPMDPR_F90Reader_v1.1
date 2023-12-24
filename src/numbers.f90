module numbers
    use parameters

    implicit none

    ! monthly
    !integer,    dimension( dimx,dimy,0:12 ), save    :: n_obs
    real(sp),   dimension( dimx,dimy,0:12 ),    save :: n_obs
    integer,    dimension( dimx,dimy,0:12 ),    save :: n_rain
    integer,    dimension( dimx,dimy, 2,0:12 ), save :: n_precip_surf        ! both conditional and unconditional
    integer,    dimension( dimx,dimy,LS,0:12 ), save :: n_precip_path        ! only unconditional (1=RWP; 2=SWP)
    real(dp),   dimension( dimx,dimy, 2,0:12 ), save :: sum_precip_surf      ! both conditional and unconditional
    real(dp),   dimension( dimx,dimy,LS,0:12 ), save :: sum_precip_path      ! only unconditional (1=RWP; 2=SWP)
    integer,    dimension( dimx,dimy,0:12 ),    save :: n_flag_graupel0
    integer,    dimension( dimx,dimy,0:12 ),    save :: n_flag_graupel1
    integer,    dimension( dimx,dimy,0:12 ),    save :: n_flag_SurfSnow0
    integer,    dimension( dimx,dimy,0:12 ),    save :: n_flag_SurfSnow1
    integer,    dimension( dimx,dimy,0:12 ),    save :: n_flag_HeavyIcePre0
    integer,    dimension( dimx,dimy,0:12 ),    save :: n_flag_HeavyIcePre1
    integer,    dimension( dimx,dimy,0:12 ),    save :: n_flag_HeavyIcePre2
    integer,    dimension( dimx,dimy,0:12 ),    save :: n_flag_HeavyIcePre3
    integer,    dimension( dimx,dimy,0:12 ),    save :: n_flag_HeavyIcePre4
    integer,    dimension( dimx,dimy,0:12 ),    save :: n_flag_HeavyIcePre8
    integer,    dimension( dimx,dimy,0:12 ),    save :: n_flag_HeavyIcePre12
    integer,    dimension( dimx,dimy,0:12 ),    save :: n_flag_HeavyIcePre16
    integer,    dimension( dimx,dimy,0:12 ),    save :: n_flag_HeavyIcePre25
    integer,    dimension( dimx,dimy,0:12 ),    save :: n_flag_HeavyIcePre30
    integer,    dimension( dimx,dimy,0:12 ),    save :: n_hip
    integer,    dimension( dimx,dimy,0:12 ),    save :: n_flag_graupelsnow
    integer(8), dimension( nreg,nfreq,0:12 ),   save             :: n_cfad_obs  ! CFAD Observed All Samples
    integer(8), dimension( dimdbz,dimlev,nreg,nfreq,0:12 ), save :: n_cfad_bin  ! CFAD Samples Binning by dBZ and Height

    ! annual
    real(sp),   dimension( dimx,dimy ),    save :: n_obs_all
    integer,    dimension( dimx,dimy ),    save :: n_rain_all
    integer,    dimension( dimx,dimy, 2 ), save :: n_precip_surf_all        ! both conditional and unconditional
    integer,    dimension( dimx,dimy,LS ), save :: n_precip_path_all        ! only unconditional (1=RWP; 2=SWP)
    real(dp),   dimension( dimx,dimy, 2 ), save :: sum_precip_surf_all      ! both conditional and unconditional
    real(dp),   dimension( dimx,dimy,LS ), save :: sum_precip_path_all      ! only unconditional (1=RWP; 2=SWP)
    integer,    dimension( dimx,dimy ),    save :: n_flag_graupel0_all
    integer,    dimension( dimx,dimy ),    save :: n_flag_graupel1_all
    integer,    dimension( dimx,dimy ),    save :: n_flag_SurfSnow0_all
    integer,    dimension( dimx,dimy ),    save :: n_flag_SurfSnow1_all
    integer,    dimension( dimx,dimy ),    save :: n_flag_HeavyIcePre0_all
    integer,    dimension( dimx,dimy ),    save :: n_flag_HeavyIcePre1_all
    integer,    dimension( dimx,dimy ),    save :: n_flag_HeavyIcePre2_all
    integer,    dimension( dimx,dimy ),    save :: n_flag_HeavyIcePre3_all
    integer,    dimension( dimx,dimy ),    save :: n_flag_HeavyIcePre4_all
    integer,    dimension( dimx,dimy ),    save :: n_flag_HeavyIcePre8_all
    integer,    dimension( dimx,dimy ),    save :: n_flag_HeavyIcePre12_all
    integer,    dimension( dimx,dimy ),    save :: n_flag_HeavyIcePre16_all
    integer,    dimension( dimx,dimy ),    save :: n_flag_HeavyIcePre25_all
    integer,    dimension( dimx,dimy ),    save :: n_flag_HeavyIcePre30_all
    integer,    dimension( dimx,dimy ),    save :: n_hip_all
    integer,    dimension( dimx,dimy ),    save :: n_flag_graupelsnow_all
    integer(8), dimension( nreg,nfreq ),               save :: n_cfad_obs_all  ! CFAD Observed All Samples
    integer(8), dimension( dimdbz,dimlev,nreg,nfreq ), save :: n_cfad_bin_all  ! CFAD Samples Binning by dBZ and Height
 
end module numbers
