module input
    use hdf5
    use parameters

    private get_dimsize_2d, get_dimsize_3d, &
            read_1int_2d, read_4flt_2d, read_4flt_3d, read_4flt_4d

contains

subroutine analyze_file_name( &
    cgranule, year, month, cmon, ym, imon, &
    obsstr, obsnext, avgstr_y, avgstr, avgend, &
    fname, &
    ooutyr, ooutmon, &
    istat, ofirst, ofinal)

    implicit none

    character(len =   6), intent(out) :: cgranule
    character(len =   4), intent(out) :: year
    character(len =   2), intent(out) :: month
    character(len =   3), intent(out) :: cmon
    character(len =   7), intent(out) :: ym
    character(len =  16), intent(out) :: obsstr, obsnext
    character(len =  16), intent(out) :: avgstr_y, avgstr, avgend
    character(len = 128), intent(out) :: fname
    integer, intent(out) :: imon
    logical, intent(out) :: ooutyr, ooutmon

    integer, intent(inout) :: istat
    logical, intent(inout) :: ofirst, ofinal

    integer :: slash_ix
    character(len = 128) :: fname_next
    character(len =   4) :: year_next, hour, hour_next
    character(len =   2) :: month_next, day, day_next
    character(len =   7) :: ym_next
    character(len =  16) :: obsend

    if ( ofirst ) then
        read( input_file_idx, '(A)', iostat=istat ) fname
        if ( istat .eq. -1 ) then
            write(*,*) "### No input HDF5 file. [Abort]"
            stop
        endif
        read( input_file_idx, '(A)', iostat=istat ) fname_next
        if ( istat .eq. -1 ) then
            ooutyr  = .true.
            ooutmon = .true.
            ofinal  = .true.
        endif
    else
        fname = trim( fname_next )  ! substitute t-1 older filename
        read( input_file_idx, '(A)', iostat=istat ) fname_next
        if ( istat .eq. -1 ) then
            ooutyr  = .true.
            ooutmon = .true.
            ofinal  = .true.
        endif
    endif
    fname = trim( adjustl(fname) )
    write( *,* ) "*** FILENAME (IN) : ", trim( fname )
    slash_ix = scan( fname, "/", .TRUE. )
    cgranule = fname( slash_ix + 28 : slash_ix + 33 )
    year     = "20" // fname( slash_ix + 12 : slash_ix + 13 )
    month    = fname( slash_ix + 14 : slash_ix + 15 )
    ym       = year // month
    day      = fname( slash_ix + 16 : slash_ix + 17 )
    hour     = fname( slash_ix + 18 : slash_ix + 21 )
    read ( month,* ) imon        ! CHARACTER => INTEGER
    write( cmon,'(i2.2)' ) imon  ! INTEGER => CHARACTER
    if ( month .eq. '01' ) cmon = "jan"
    if ( month .eq. '02' ) cmon = "feb"
    if ( month .eq. '03' ) cmon = "mar"
    if ( month .eq. '04' ) cmon = "apr"
    if ( month .eq. '05' ) cmon = "may"
    if ( month .eq. '06' ) cmon = "jun"
    if ( month .eq. '07' ) cmon = "jul"
    if ( month .eq. '08' ) cmon = "aug"
    if ( month .eq. '09' ) cmon = "sep"
    if ( month .eq. '10' ) cmon = "oct"
    if ( month .eq. '11' ) cmon = "nov"
    if ( month .eq. '12' ) cmon = "dec"
    if ( ofirst ) then
        obsstr = trim( year ) // trim( month ) // trim( day ) // " " // trim( hour ) // "00"
        obsend = trim( obsstr )
        avgstr = trim( obsstr )
        avgstr_y = trim( obsstr )
        avgend = trim( obsend )
        ofirst = .false.
    else
        obsend = trim( year ) // trim( month ) // trim( day ) // " " // trim( hour ) // "00"
        avgend = trim( obsend )
    endif
    write( *,* ) "GRANULE = ", cgranule
    write( *,* ) "YYYY = ", year
    write( *,* ) "MM =   ", month, " ", imon, " ", cmon
    write( *,* ) "YM =   ", ym
    write( *,* ) "ISTAT = ", istat
    write( *,* ) "OBSSTR = ", obsstr
    write( *,* ) "AVGSTR_Y = ", avgstr_y
    if ( ofinal ) then
        write(*,*) "OFINAL = .TRUE."
    else
        write(*,*) "OFINAL = .FALSE."
        ! next file
        fname_next = trim( adjustl(fname_next) )
        slash_ix   = scan( fname_next, "/", .TRUE. )
        year_next  = "20" // fname_next( slash_ix + 12 : slash_ix + 13 )
        month_next = fname_next( slash_ix + 14 : slash_ix + 15 )
        ym_next    = year_next // month_next
        day_next   = fname_next( slash_ix + 16 : slash_ix + 17 )
        hour_next  = fname_next( slash_ix + 18 : slash_ix + 21 )
        obsnext = trim( year_next ) // trim( month_next ) // trim( day_next ) // " " // trim( hour_next ) // "00"
        write( *,* ) "*** FILENAME (IN NEXT) : ", trim( obsnext )
    endif
    ! Output flag
    if ( year .ne. year_next  .or.  ofinal ) then
        ooutyr  = .true.
        write(*,*) "OOUTYR = .TRUE."
    else
        write(*,*) "OOUTYR = .FALSE."
    endif
    if ( month .ne. month_next  .or.  ofinal ) then
        ooutmon = .true.
        write(*,*) "OOUTMON = .TRUE."
    else
        write(*,*) "OOUTMON = .FALSE."
    endif

end subroutine analyze_file_name

subroutine read_HDF5_files( &
        varname, fname, &
        nbin, nray, nscan, &
        lat, lon, height, precip_surf, precip_path, &
        flag_graupel, flag_SurfSnow, flag_HeavyIcePre, zFactorMeasured)
    implicit none
    character(*), dimension(:), intent(in) :: varname
    character(*), intent(in) :: fname

    integer, intent(out) :: nbin, nray, nscan
    real(sp), dimension(:,:), allocatable, intent(out) :: lat                  ! Latitude
    real(sp), dimension(:,:), allocatable, intent(out) :: lon                  ! Longitude
    real(sp), dimension(:,:,:), allocatable, intent(out) :: height             ! Height [m]
    real(sp), dimension(:,:), allocatable, intent(out) :: precip_surf          ! precipRateESurface [mm/hr]
    real(sp), dimension(:,:,:), allocatable, intent(out) :: precip_path        ! precipWaterIntegrated [g/m2]
    integer, dimension(:,:), allocatable, intent(out) :: flag_graupel          ! flagGraupelHail
                                                                               ! (0 = no grpl or hail; 1 = grpl or hail is detected)
    integer, dimension(:,:), allocatable, intent(out) :: flag_SurfSnow         ! flagSurfaceSnowfall
                                                                               ! (0 = no snowfall; 1 = snowfall is detected)
    integer, dimension(:,:), allocatable, intent(out) :: flag_HeavyIcePre      ! flagHeavyIcePrecip
                                                                               ! (0 = no heavy ice precip; 1 = heavy ice precip exists)
    real(sp), dimension(:,:,:,:), allocatable, intent(out) :: zFactorMeasured  ! zFactorMeasured [dBZ]

    integer(HSIZE_T), dimension(2) :: dim2d                   ! dim2d(1)=nray, dim2d(2)=nscan
    integer(HSIZE_T), dimension(3) :: dim3d                   ! dim3d(1)=nbin, dim3d(2)=nray, dim3d(3)=nscan
    integer(HSIZE_T), dimension(4) :: dim4d                   ! for dBZ
                                                              ! (dim(4)=1 for KuPR; dim(4)=2 for KaPR)
    integer :: astat  ! status ID from data assignment
    real(sp), dimension(:,:,:), allocatable :: adjustFactor       ! adjustFactor
                                                                  ! (zFactorMeasured = zFactorMeasured - adjustFactor)
    integer :: i, j, k

    !--- Retrieve the Dimensions of a Datafield in the HDF5
    call get_dimsize_2d( fname, varname(1), & ! in
                        & dim2d )             ! out
    call get_dimsize_3d( fname, varname(3), & ! in
                        & dim3d )             ! out
    nbin  = dim3d(1)
    nray  = dim3d(2)
    nscan = dim3d(3)
    if ( nray .ne. dim2d(1)  .or.  nscan .ne. dim2d(2) ) then
        write( *,* ) "### MISMATCHED TO RETRIEVE DIMSIZES:"
        write( *,* ) "    NBIN, NRAY, NSCAN, NFREQ = ", nbin, nray, nscan, nfreq
        write( *,* ) "    DIM2D(1) and DIM2D(2) = ", dim2d(1), dim2d(2)
        write( *,* ) "### ABORT"
        stop
    else
        write( *,* ) "*** NBIN, NRAY, NSCAN, NFREQ = ", nbin, nray, nscan, nfreq
        write( *,* ) "    DIM2D(1) and DIM2D(2) = ", dim2d(1), dim2d(2)
    endif

    !--- Get Geolocation Data
    ! latitude
    allocate( lat( nray,nscan ), stat=astat )
    if ( astat /= 0 ) then
        write( *,* ) "### CANNOT ALLOCATE lat", astat
        stop
    endif
    call read_4flt_2d( fname, trim(varname(1)), dim2d, &  ! in
                     & lat )                              ! out

    ! longitude
    allocate( lon( nray,nscan ), stat=astat )
    if ( astat /= 0 ) then
        write( *,* ) "### CANNOT ALLOCATE lon", astat
        stop
    endif
    call read_4flt_2d( fname, trim(varname(2)), dim2d, &  ! in
                     & lon )                              ! out

    ! height
    if( odump3d ) then
    allocate( height( nbin,nray,nscan ), stat=astat )
    if ( astat /= 0 ) then
        write( *,* ) "### CANNOT ALLOCATE height", astat
        stop
    endif
    call read_4flt_3d( fname, trim(varname(3)), dim3d, &  ! in
                     & height )                           ! out
    endif

    !--- Retrieve the Specific Data
    ! precipRateESurface
    allocate( precip_surf( nray,nscan ), stat=astat )
    if ( astat /= 0 ) then
        write( *,* ) "### CANNOT ALLOCATE precip_surf", astat
        stop
    endif
    call read_4flt_2d( fname, trim(varname(4)), dim2d, &  ! in
                     & precip_surf )                      ! out

    ! precipWaterIntegrated 
    allocate( precip_path( LS,nray,nscan ), stat=astat )
    if ( astat /= 0 ) then
        write( *,* ) "### CANNOT ALLOCATE precip_path", astat
        stop
    endif
    call read_4flt_3d( fname, trim(varname(5)), dim3d, &  ! in
                     & precip_path )                      ! out

    ! flagGraupelHail
    allocate( flag_graupel( nray,nscan ), stat=astat )
    if ( astat /= 0 ) then
        write( *,* ) "### CANNOT ALLOCATE flag_graupel", astat
        stop
    endif
    call read_1int_2d( fname, trim(varname(6)), dim2d, &  ! in
                     & flag_graupel )                     ! out

    ! flagSurfaceSnowfall
    allocate( flag_SurfSnow( nray,nscan ), stat=astat )
    if ( astat /= 0 ) then
        write( *,* ) "### CANNOT ALLOCATE flag_SurfSnow", astat
        stop
    endif
    call read_1int_2d( fname, trim(varname(7)), dim2d, &  ! in
                     & flag_SurfSnow )                    ! out

    ! flagHeavyIcePrecip
    allocate( flag_HeavyIcePre( nray,nscan ), stat=astat )
    if ( astat /= 0 ) then
        write( *,* ) "### CANNOT ALLOCATE flag_HeavyIcePre", astat
        stop
    endif
    call read_1int_2d( fname, trim(varname(8)), dim2d, &  ! in
                     & flag_HeavyIcePre )                 ! out

    if( odump3d ) then !--- Optional Processes ---

        ! zFactorMeasured
        allocate( zFactorMeasured( nfreq,nbin,nray,nscan ), stat=astat )
        if ( astat /= 0 ) then
            write( *,* ) "### CANNOT ALLOCATE zFactorMeasured", astat
            stop
        endif
        dim4d(1) = nfreq; dim4d(2) = nbin; dim4d(3) = nray; dim4d(4) = nscan
        call read_4flt_4d( fname, trim(varname(9)), dim4d, &  ! in
                         & zFactorMeasured )                  ! out      

        ! adjustFactor
        allocate( adjustFactor( nfreq,nray,nscan ), stat=astat )
        if ( astat /= 0 ) then
            write( *,* ) "### CANNOT ALLOCATE adjustFactor", astat
            stop
        endif
        dim3d(1) = nfreq; dim3d(2) = nray; dim3d(3) = nscan
        call read_4flt_3d( fname, trim(varname(10)), dim3d, &  ! in
                         & adjustFactor )                      ! out

        !--- Scaling and Offsetting for dBZ; Unit Conversion for Height
        do k = 1, nscan
        do j = 1, nray
            do i = 1, nbin
                zFactorMeasured(1,i,j,k) = zFactorMeasured(1,i,j,k) - adjustFactor(1,j,k) ! Ku band
                zFactorMeasured(2,i,j,k) = zFactorMeasured(2,i,j,k) - adjustFactor(2,j,k) ! Ka band
            enddo
        enddo
        enddo
        height(:,:,:) = height(:,:,:) * 1e-3_dp  ! Unit: m => km

    endif !--- Optional Processes (odump3d) ---

end subroutine read_HDF5_files

subroutine get_dimsize_2d( filename, dataname, & ! in
                         & dims )                ! out
    implicit none

    integer hdferr
    character(len=*), intent(in) :: filename
    character(len=*), intent(in) :: dataname
    integer(hid_t) :: file_id       ! File identifier
    integer(hid_t) :: data_space_id ! data space identifier
    integer(hid_t) :: dset_id       ! Dataset identifier

    integer(hsize_t), dimension(2)              :: maxdims
    integer(hsize_t), dimension(2), intent(out) :: dims


    ! Initialize/Open HDF
    call h5open_f( hdferr )
    call h5fopen_f( filename, H5F_ACC_RDONLY_F, file_id, hdferr )
    if ( hdferr .eq. -1 ) then
    write( *,* ) "### get_dimsize_2d: CANNOT OPEN FILE", hdferr
    stop
    endif

    ! Access to Dataset
    call h5dopen_f( file_id, dataname, dset_id, hdferr )
    if ( hdferr .eq. -1 ) then
    write( *,* ) "### get_dimsize_2d: CANNOT ACCESS TO DATASET", hdferr
    stop
    endif

    ! Get Data Space
    call h5dget_space_f( dset_id, data_space_id, hdferr )
    if ( hdferr .eq. -1 ) then
    write( *,* ) "### get_dimsize_2d: CANNOT GET DATA SPACE", hdferr
    stop
    endif

    ! Get the Size of Dataset
    call h5sget_simple_extent_dims_f( data_space_id, dims, maxdims, hdferr )
    if ( hdferr .eq. -1 ) then
    write( *,* ) "### get_dimsize_2d: CANNOT GET DIMSIZE", hdferr
    stop
    endif
    write( *,* ) "*** DIMENSION SIZE (2D): ", dims(1:2)
    write( *,* ) "                MAXDIMS: ", maxdims(1:2)

    ! Close Data Space
    call h5sclose_f( data_space_id, hdferr )

    ! Close the Datasets
    call h5dclose_f( dset_id, hdferr )

    ! Close the File
    call h5fclose_f( file_id, hdferr )

    ! Close FORTRAN Interface
    call h5close_f( hdferr )

    return
end subroutine get_dimsize_2d


subroutine get_dimsize_3d( filename, dataname, & ! in
                         & dims )                ! out
    implicit none

    integer hdferr
    character(len=*), intent(in) :: filename
    character(len=*), intent(in) :: dataname
    integer(hid_t) :: file_id       ! File identifier
    integer(hid_t) :: data_space_id ! data space identifier
    integer(hid_t) :: dset_id       ! Dataset identifier

    integer(hsize_t), dimension(3)              :: maxdims
    integer(hsize_t), dimension(3), intent(out) :: dims


    ! Initialize/Open HDF
    call h5open_f( hdferr )
    call h5fopen_f( filename, H5F_ACC_RDONLY_F, file_id, hdferr )
    if ( hdferr .eq. -1 ) then
    write( *,* ) "### get_dimsize_3d: CANNOT OPEN FILE", hdferr
    stop
    endif

    ! Access to Dataset
    call h5dopen_f( file_id, dataname, dset_id, hdferr )
    if ( hdferr .eq. -1 ) then
    write( *,* ) "### get_dimsize_3d: CANNOT ACCESS TO DATASET", hdferr
    stop
    endif

    ! Get Data Space
    call h5dget_space_f( dset_id, data_space_id, hdferr )
    if ( hdferr .eq. -1 ) then
    write( *,* ) "### get_dimsize_3d: CANNOT GET DATA SPACE", hdferr
    stop
    endif

    ! Get the Size of Dataset
    call h5sget_simple_extent_dims_f( data_space_id, dims, maxdims, hdferr )
    if ( hdferr .eq. -1 ) then
    write( *,* ) "### get_dimsize_3d: CANNOT GET DIMSIZE", hdferr
    stop
    endif
    write( *,* ) "*** DIMENSION SIZE (3D): ", dims(1:3)
    write( *,* ) "                MAXDIMS: ", maxdims(1:3)

    ! Close Data Space
    call h5sclose_f( data_space_id, hdferr )

    ! Close the Datasets
    call h5dclose_f( dset_id, hdferr )

    ! Close the File
    call h5fclose_f( file_id, hdferr )

    ! Close FORTRAN Interface
    call h5close_f( hdferr )

    return
end subroutine get_dimsize_3d

subroutine read_1int_2d( filename, dataname, dims, & ! in
                       & data )                      ! out
    implicit none

    integer hdferr
    integer(hid_t) :: file_id  ! File identifier
    integer(hid_t) :: dset_id  ! Dataset identifier

    character(len=*), intent(in)  :: filename
    character(len=*), intent(in)  :: dataname
    integer(hsize_t), intent(in)  :: dims(2)
    integer,          intent(out) :: data(:,:) ! Data buffers

    ! Initialize/Open HDF
    call h5open_f( hdferr )
    call h5fopen_f( filename, H5F_ACC_RDONLY_F, file_id, hdferr )
    if ( hdferr .eq. -1 ) then
    write( *,* ) "### read_1int_2d: CANNOT OPEN FILE", hdferr
    stop
    endif

    ! Access to Dataset
    call h5dopen_f( file_id, dataname, dset_id, hdferr )
    if ( hdferr .eq. -1 ) then
    write( *,* ) "### read_1int_2d: CANNOT ACCESS TO DATASET", hdferr, dataname
    stop
    endif
    write( *,* ) "*** ACCESS TO ... ", dataname

    ! Read Data
    call h5dread_f( dset_id, H5T_NATIVE_INTEGER, data, dims, hdferr )

    ! Close the Datasets
    call h5dclose_f( dset_id, hdferr )

    ! Close the File
    call h5fclose_f( file_id, hdferr )

    ! Close FORTRAN Interface
    call h5close_f( hdferr )

end subroutine read_1int_2d

subroutine read_4flt_2d( filename, dataname, dims, & ! in
                       & data )                      ! out
    implicit none

    integer hdferr
    integer(hid_t) :: file_id  ! File identifier
    integer(hid_t) :: dset_id  ! Dataset identifier

    character(len=*), intent(in)  :: filename
    character(len=*), intent(in)  :: dataname
    integer(hsize_t), intent(in)  :: dims(2)
    real(sp),         intent(inout) :: data(:,:) ! Data buffers

    ! Initialize/Open HDF
    call h5open_f( hdferr )
    call h5fopen_f( filename, H5F_ACC_RDONLY_F, file_id, hdferr )
    if ( hdferr .eq. -1 ) then
    write( *,* ) "### read_4flt_2d: CANNOT OPEN FILE", hdferr
    stop
    endif

    ! Access to Dataset
    call h5dopen_f( file_id, dataname, dset_id, hdferr )
    if ( hdferr .eq. -1 ) then
    write( *,* ) "### read_4flt_2d: CANNOT ACCESS TO DATASET", hdferr, dataname
    stop
    endif
    write( *,* ) "*** ACCESS TO ... ", dataname

    ! Read Data
    call h5dread_f( dset_id, H5T_NATIVE_REAL, data, dims, hdferr )

    ! Close the Datasets
    call h5dclose_f( dset_id, hdferr )

    ! Close the File
    call h5fclose_f( file_id, hdferr )

    ! Close FORTRAN Interface
    call h5close_f( hdferr )

end subroutine read_4flt_2d

subroutine read_4flt_3d( filename, dataname, dims, & ! in
                       & data )                      ! out
    implicit none

    integer hdferr
    integer(hid_t) :: file_id  ! File identifier
    integer(hid_t) :: dset_id  ! Dataset identifier

    character(len=*), intent(in)  :: filename
    character(len=*), intent(in)  :: dataname
    integer(hsize_t), intent(in)  :: dims(3)
    real(sp),         intent(inout) :: data(:,:,:) ! Data buffers

    ! Initialize/Open HDF
    call h5open_f( hdferr )
    call h5fopen_f( filename, H5F_ACC_RDONLY_F, file_id, hdferr )
    if ( hdferr .eq. -1 ) then
    write( *,* ) "### read_4flt_3d: CANNOT OPEN FILE", hdferr
    stop
    endif

    ! Access to Dataset
    call h5dopen_f( file_id, dataname, dset_id, hdferr )
    if ( hdferr .eq. -1 ) then
    write( *,* ) "### read_4flt_3d: CANNOT ACCESS TO DATASET", hdferr, dataname
    stop
    endif
    write( *,* ) "*** ACCESS TO ... ", dataname

    ! Read Data
    call h5dread_f( dset_id, H5T_NATIVE_REAL, data, dims, hdferr )

    ! Close the Datasets
    call h5dclose_f( dset_id, hdferr )

    ! Close the File
    call h5fclose_f( file_id, hdferr )

    ! Close FORTRAN Interface
    call h5close_f( hdferr )

end subroutine read_4flt_3d

subroutine read_4flt_4d( filename, dataname, dims, & ! in
                       & data )                      ! out
    implicit none

    integer hdferr
    integer(hid_t) :: file_id  ! File identifier
    integer(hid_t) :: dset_id  ! Dataset identifier

    character(len=*), intent(in) :: filename
    character(len=*), intent(in) :: dataname
    integer(hsize_t), intent(in) :: dims(4)
    real(sp),         intent(inout) :: data(:,:,:,:) ! Data buffers

    ! Initialize/Open HDF
    call h5open_f( hdferr )
    call h5fopen_f( filename, H5F_ACC_RDONLY_F, file_id, hdferr )
    if ( hdferr .eq. -1 ) then
    write( *,* ) "### read_4flt_4d: CANNOT OPEN FILE", hdferr
    stop
    endif

    ! Access to Dataset
    call h5dopen_f( file_id, dataname, dset_id, hdferr )
    if ( hdferr .eq. -1 ) then
    write( *,* ) "### read_4flt_4d: CANNOT ACCESS TO DATASET", hdferr, dataname
    stop
    endif
    write( *,* ) "*** ACCESS TO ... ", dataname

    ! Read Data
    call h5dread_f( dset_id, H5T_NATIVE_REAL, data, dims, hdferr )

    ! Close the Datasets
    call h5dclose_f( dset_id, hdferr )

    ! Close the File
    call h5fclose_f( file_id, hdferr )

    ! Close FORTRAN Interface
    call h5close_f( hdferr )

end subroutine read_4flt_4d

end module input
