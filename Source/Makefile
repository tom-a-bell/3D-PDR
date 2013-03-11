# -----------------------------------------------------------------

SHELL = /bin/sh

CC       = gcc
F90      = ifort
LNKR     = ifort
CPPFLAGS = -cpp
CFLAGS   = -fopenmp -DOPENMP
FFLAGS   = -openmp -DOPENMP
LDFLAGS  = -openmp
LIBS     = -lm

CFLAGS  += -O2
FFLAGS  += -O2
LDFLAGS += -O2

#CFLAGS  += -g
#FFLAGS  += -g -debug -C -traceback #-check arg_temp_created -check bounds -fp-stack-check -warn alignments -warn interfaces -align all -align rec8byte
#LDFLAGS += -g -debug -C -traceback #-check arg_temp_created -check bounds -fp-stack-check -warn alignments -warn interfaces

FFLAGS += -DDUST
FFLAGS += -DCALC_TDUST
FFLAGS += -DGUESS_TEMP
#FFLAGS += -DPHOTO_CROSS_SECTIONS
#FFLAGS += -DUSE_ALI
#CFLAGS += -DLOG_ODES

INCLUDES  = -I/home/tom/sundials/include
LIBRARIES = -L/home/tom/sundials/lib -lsundials_cvode -lsundials_nvecserial ${LIBS}

SOURCE = definitions healpix_types modules convert_number
SOURCE += main setup_logfiles count_lines count_substring swap_function spline
SOURCE += read_parameters read_particles read_field read_species read_reactions read_coolants read_cross_sections
SOURCE += evaluation_points dust_temperature guess_temperature column_densities line_opacities
SOURCE += reaction_rates h2_formation_rate photoreaction_rates collisional_rates heating_rates
SOURCE += update_abundances update_coolants update_linewidths ortho_para_ratio lte_populations
SOURCE += level_populations transition_matrix escape_probability
SOURCE += check_chemistry_convergence check_population_convergence fix_bad_cooling_rates
SOURCE += nearest_neighbours update_temperature write_output analyse_chemistry
SOURCE += photo_cross_sections xray_cross_sections xray_ionization_rates
#SOURCE += lambda_operator
SOURCE += odes jacobian calculate_abundances

OBJECTS = ${SOURCE:=.o}

# -----------------------------------------------------------------------------------------

.SUFFIXES : .o .c .f90

.c.o :
	${CC} ${CFLAGS} ${INCLUDES} -c $<

.f90.o :
	${F90} ${CPPFLAGS} ${FFLAGS} -c $<

# -----------------------------------------------------------------------------------------

3D-PDR: ${OBJECTS}
	${LNKR} -o 3D-PDR ${OBJECTS} ${LDFLAGS} ${LIBRARIES}
	mv 3D-PDR ../

install:
	mv 3D-PDR ../

clean:
	rm -f *.o
	rm -f *.mod
	rm -f 3D-PDR

cleandebug:
	rm -f *__genmod.f90
	rm -f *__genmod.mod

# -----------------------------------------------------------------------------------------
