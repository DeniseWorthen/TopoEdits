CDF=/apps/netcdf/4.7.0/intel/18.0.5.274
#####################################################################
# compiler options
# #####################################################################
#FOPT = -C -O0
#fPIC reqd for 008 and is b4b when used on smaller grids
FOPT = -C -O0 -fPIC
#FOPT = -C -warn

F90 = ifort

#opt1 = -Doutput_grid_qdeg
#opt1 = -Doutput_grid_hdeg
#opt1 = -Doutput_grid_072deg
opt1 = -Doutput_grid_1deg
#opt1 = -Doutput_grid_3deg
#opt1 = -Doutput_grid_twelfdeg

optall = $(opt1) $(opt2)
######################################################################
#
#####################################################################
OBJS = param.o charstrings.o grdvars.o maskdefs.o adjust_landmask.o topoedits.o write_mskgrid.o

topoed: $(OBJS)
	$(F90) $(FOPT) -o topoed $(OBJS) -L$(CDF)/lib -lnetcdff -lnetcdf

%.o: %.F90
	$(F90) $(FOPT) $(optall) -c -I$(CDF)/include $<
	cpp $(optall) -I$(CDF)/include $*.F90>$*.i

clean:
	/bin/rm -f topoed *.o *.i *.mod
