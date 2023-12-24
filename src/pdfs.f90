module pdfs
    use parameters

    implicit none

    !--- Output Variables
    !--- precipitation and occurence frequency

    ! monthly
    real(sp), dimension( dimx,dimy,2,0:12 ), save  :: mean_precip_surf  ! 1=conditional; 2=unconditional
    real(sp), dimension( dimx,dimy,2,0:12 ), save  :: mean_precip_path  ! 1=RWP; 2=SWP
    real(sp), dimension( dimx,dimy,0:12 ),   save  :: pdf_precip        ! PDF of precip. against all profiles
                                                                        ! within the column (PoP)
    real(sp), dimension( dimx,dimy,0:12 ),   save  :: pdf_rain          ! occurrence of rain for precipitating
                                                                        ! column (Nrain/Nprecip [%])
    real(sp), dimension( dimx,dimy,0:12 ),   save  :: pdf_grpl
    real(sp), dimension( dimx,dimy,0:12 ),   save  :: pdf_snowsfc
    real(sp), dimension( dimx,dimy,0:12 ),   save  :: pdf_grpsnow
    real(sp), dimension( dimx,dimy,0:12 ),   save  :: pdf_hip0,  pdf_hip1,  pdf_hip2,  pdf_hip3,  pdf_hip4, &
                                                    & pdf_hip8,  pdf_hip12, pdf_hip16, pdf_hip25, pdf_hip30
    real(sp), dimension( dimdbz,dimlev,nreg,nfreq,0:12 ), save :: pdf_cfad

    ! annual
    real(sp), dimension( dimx,dimy,2 ), save :: mean_precip_surf_all  ! 1=conditional; 2=unconditional
    real(sp), dimension( dimx,dimy,2 ), save :: mean_precip_path_all  ! 1=RWP; 2=SWP
    real(sp), dimension( dimx,dimy ),   save :: pdf_precip_all        ! PDF of precip. against all profiles
                                                                      ! within the column (PoP)
    real(sp), dimension( dimx,dimy ),   save :: pdf_rain_all          ! occurrence of rain for precipitating
                                                                      ! column (Nrain/Nprecip [%])
    real(sp), dimension( dimx,dimy ),   save :: pdf_grpl_all
    real(sp), dimension( dimx,dimy ),   save :: pdf_snowsfc_all
    real(sp), dimension( dimx,dimy ),   save :: pdf_grpsnow_all
    real(sp), dimension( dimx,dimy ),   save :: pdf_hip0_all,  pdf_hip1_all,  pdf_hip2_all,  pdf_hip3_all,  &
                                              & pdf_hip4_all,  pdf_hip8_all,  pdf_hip12_all, pdf_hip16_all, &
                                              & pdf_hip25_all, pdf_hip30_all
    real(sp), dimension( dimdbz,dimlev,nreg,nfreq ), save :: pdf_cfad_all

end module pdfs
