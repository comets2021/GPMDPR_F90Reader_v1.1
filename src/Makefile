# Makefile for GPM/DPR Driver

BINDIR = .
BIN  = $(BINDIR)/gpm_driver

OBJS = parameters.o             \
       numbers.o                \
	   pdfs.o                   \
       input.o                  \
	   netcdf_writer.o          \
	   gmt_data_processor.o     \
	   gnuplot_data_processor.o \
	   grads_data_processor.o   \
	   gtool_data_processor.o   \
       output.o                 \
       gpm_driver.o

FC     = ifort

### Options
FFLAGS = -O2
LDFLAGS = -I/usr/include -I/usr/local/include -L/usr/local/lib
LDLIBS = -lz -lm -ldf -lsz -ljpeg -lc -lpthread -lhdf5_hl -lhdf5_fortran -lhdf5  -lnetcdff -lnetcdf
#FFLAGS = -O2 -g -traceback -check all
FFLAGS = -O2
HDF_INC = /usr/local/include
HDF_LIB = /usr/local/lib
NF_INC = /usr/local/include
NF_LIB = /usr/local/lib
INC = /usr/include
LIB = /usr/lib
HDF5_LDFLAGS = -I$(INC) -I$(HDF_INC) -L$(LIB)
NETCDF_LDFLAGS = -I${NF_INC} -L${NF_LIB}

$(BIN): $(OBJS)
	$(FC) -o $@ $(OBJS) $(FFLAGS) $(LDFLAGS) $(LDLIBS)

parameters.o: parameters.f90
	$(FC) -c $< $(FFLAGS) 

numbers.o: numbers.f90
	$(FC) -c $< $(FFLAGS) 

pdfs.o: pdfs.f90
	$(FC) -c $< $(FFLAGS)

input.o: input.f90 parameters.o
	$(FC) -c $< $(FFLAGS) $(HDF5_LDFLAGS)

netcdf_writer.o: netcdf_writer.f90 parameters.o
	$(FC) -c $< $(FFLAGS) $(NETCDF_LDFLAGS)

gmt_data_processor.o: gmt_data_processor.f90 parameters.o netcdf_writer.o
	$(FC) -c $< $(FFLAGS) $(NETCDF_LDFLAGS)

gnuplot_data_processor.o: gnuplot_data_processor.f90 gmt_data_processor.o parameters.o
	$(FC) -c $< $(FFLAGS) $(NETCDF_LDFLAGS)

grads_data_processor.o: grads_data_processor.f90 parameters.o netcdf_writer.o
	$(FC) -c $< $(FFLAGS) $(NETCDF_LDFLAGS)

gtool_data_processor.o: gtool_data_processor.f90 parameters.o netcdf_writer.o
	$(FC) -c $< $(FFLAGS) $(NETCDF_LDFLAGS)

output.o: output.f90 parameters.o numbers.o pdfs.o gmt_data_processor.o gnuplot_data_processor.o grads_data_processor.o gtool_data_processor.o
	$(FC) -c $< $(FFLAGS) $(NETCDF_LDFLAGS)

gpm_driver.o: gpm_driver.f90 parameters.o numbers.o input.o output.o
	$(FC) -c $< $(FFLAGS) $(HDF5_LDFLAGS) $(NETCDF_LDFLAGS)

clean :
	rm -f $(BIN) core *.o *.mod
