# Makefile for compiling COSMO
# fuo, 07.03.2008, oliver.fuhrer@meteoswiss.ch
#
# Description: This makefile automagically searches for source files in the src
#              directory and creates dependencies. Afterwards files are compiled
#              and linked together.
#              Suitable for both, lm and int2lm
# History:
#   fuo  07.03.2008   first release


### Macros ###########################

# define shell
SHELL      := /bin/sh

# directory definitions
SRCDIR     := src
OBJDIR     := obj
BINDIR     := bin
ifeq (0,${MAKELEVEL})
  ROOT      := $(shell pwd)
  VPATH     := .:$(ROOT)/$(SRCDIR)
endif

# define targets
TARGETS    := \
  extpar_alb_to_buffer.exe \
  extpar_aot_to_buffer.exe \
  extpar_consistency_check.exe \
  extpar_cru_to_buffer.exe \
  extpar_flake_to_buffer.exe \
  extpar_landuse_to_buffer.exe \
  extpar_ndvi_to_buffer.exe \
  extpar_soil_to_buffer.exe \
  extpar_topo_to_buffer.exe

# generate list of object files
-include $(ROOT)/Objfiles
ifndef OBJ
  $(error Could not load Objfiles file)
endif

# dynamically generated dependency file
DEPF       := .depend
IGN        := 

# if machine is not defined, try to determine it
ifndef MACH
  MACH       := $(shell hostname | sed 's/[0-9]//g')
endif

# select machine dependent stuff
-include $(ROOT)/Options.$(MACH)
ifndef F90
  $(error Host $(MACH) not known. Must create platform specific Options.$(MACH) file)
endif

# handle options
ifdef VERBOSE
  FFLAGS  += $(VFLAGS)
endif
ifdef DEBUG
  FFLAGS  += $(FDBG)
  LIB     += $(DBGL)
  INC     += $(DBGI)
endif
ifdef OPT
  FFLAGS  += $(FOPT)
  LIB     += $(OPTL)
  INC     += $(OPTI)
endif

export ROOT VPATH MACH VERBOSE DEBUG OPT

### Phony targets ###########################

.PHONY : default depend opt debug clean

default : opt

depend :
	@echo "generating dependencies"
	@$(ROOT)/bin/sfmakedepend --case down --longpath $(INC) $(IGN) --file $(ROOT)/$(DEPF) $(ROOT)/$(SRCDIR)/*.f90

opt :
	@$(MAKE) -C $(OBJDIR) -f $(ROOT)/Makefile OPT=1 depend
	@for target in $(TARGETS) ; do \
     echo "generating target $$target" ; \
	   $(MAKE) -C $(OBJDIR) -f $(ROOT)/Makefile OPT=1 $$target ; \
   done

debug :
	@$(MAKE) -C $(OBJDIR) -f $(ROOT)/Makefile DEBUG=1 depend
	@for target in $(TARGETS) ; do \
     echo "generating target $$target" ; \
	   $(MAKE) -C $(OBJDIR) -f $(ROOT)/Makefile DEBUG=1 $$target ; \
   done
                                                            
clean :
	-rm -f $(DEPF) $(DEPF).old $(OBJDIR)/* $(BINDIR)/*.exe

### Suffix Rules ###########################

.SUFFIXES: .exe .o .mod .f90

%.exe : $(notdir %.o) $(OBJ)
	@echo "linking $@"
	@$(LD) $(LFLAGS) $(FFLAGS) $(INC) $(patsubst %.exe,%.o,$(notdir $@)) $(OBJ) $(LIB) -o $(ROOT)/$(BINDIR)/$@

%.o : %.f90
	@echo "compiling $(patsubst %.o,%.f90,$(notdir $@))"
	@$(F90) -c $(PFLAGS) $(FFLAGS) $(INC) -o $@ $(ROOT)/$(SRCDIR)/$(patsubst %.o,%.f90,$(notdir $@))

# include dynamically generated dependency file
-include $(ROOT)/$(DEPF)

# goodbye earthling!
