# compiler
F90      = ftn

# linker
LD       = $(F90)

# global compilation flags
FFLAGS   = -Kieee # Use IEEE division, optionally enable traps
FFLAGS  += -Mfree # Assume free-format source
FFLAGS  += -Mdclchk # check that all variables are declared
FFLAGS  += -byteswapio # swap byte-order for unformatted input/output (== -Mbyteswapio)

# global linking flags
LFLAGS   = -byteswapio # swap byte-order for unformatted input/output (== -Mbyteswapio)

# verbose compilation flags
VFLAGS   = -v # displays invocations of compiler, assembler and linker
VFLAGS  += -Minform=inform # display all error messages (inform, warn, severe and fatal)
VFLAGS  += -Minfo=all # display all information (accel,inline,ipa,loop,lre,mp,opt,par,vect)
VFLAGS  += -Mneginfo=all # display all negative information (see above)
#VFLAGS  += -Mkeepasm # preserve assembly language file

# force preprocessing flags
PFLAGS   = -Mpreprocess # perform preprocessing for all source files

# optimization flags (FOPT0=none, FOPT1=strong, FOPT2=strongest)
FOPT    = -O3 # aggressive global optimization
FOPT   += -fast # common optimizations (includes -O2 -Mvect=sse -Mlre -Mautoinline)
FOPT   += -Mvect=noassoc # dissallow associative transformations (OBSOLETE?)
#FOPT   += -Mvect=noaltcode # generate only vectorized code
#FOPT   += -Msmartalloc # enable optimized malloc routines
#FOPT   += -Mprefetch=distance:8 # set prefetch-ahead distance in cache lines
FOPT   += -Mipa=fast,inline # interprocedural analysis (automatically inline)

# debugging flags
FDBG     = -O0 # inhibit any optimizations
FDBG    += -C # array bounds checking (== -Mbounds)
FDBG    += -g # generate information for debugger (disable optimizations)
FDBG    += -Mchkfpstk # check consistency of floating point stack at subprogram calls
FDBG    += -Mchkptr # check of NULL pointer references
FDBG    += -Ktrap=fp # enable floating point exceptions (inv,divz,ovf)
FDBG    += -traceback # add debug information for runtime traceback
FDBG    += -Mdepchk # check dependence relations for vector or parallel code
#FDBG    += -Mstandard # check standard conformance
FDBG    += -Mchkstk # check for sufficient stack space upon subprogram entry
FDBG    += -Meh_frame # preserve exception-handling frame information (export PGI_TERM=trace)

# global libraries and includes
LIB      = 
INC      = 

# optimized flags, libraries and includes
OPTL     = 
OPTI     = 

# debugging flags, libraries and includes
DBGL     = 
DBGI     = 

# Grib API library
LIB     += -L/oprusers/osm/lib/libgrib_api_1.11.0.1_pgi13.6.0/lib -lgrib_api_f90 -lgrib_api
INC     += -I/oprusers/osm/lib/libgrib_api_1.11.0.1_pgi13.6.0/include 

# NetCDF library
LIB     += 
INC     += 

# JASPER library
LIB     += -L/oprusers/osm/lib/libjasper_1.900.1_gnu/lib -ljasper
INC     +=

# PNG library
LIB     += -L/oprusers/osm/lib/libpng_1.5.12_pgi13.6.0/lib -lpng
INC     += 

# ZLIB library
LIB     += -L/oprusers/osm/lib/zlib_1.2.7_pgi13.6.0/lib -lz
INC     +=
