AC_PREREQ([2.69])
AC_INIT([extpar], [5.3.0])
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_AUX_DIR([config])
AC_CONFIG_SRCDIR([src/extpar_alb_to_buffer.f90])

AC_ARG_ENABLE([silent-rules],
  [AC_HELP_STRING([--enable-silent-rules],
     [less verbose build output (undo: "make V=1") @<:@default=yes@:>@])], [],
  [enable_silent_rules=yes])

AC_ARG_ENABLE([rpaths],
  [AC_HELP_STRING([--enable-rpaths],
     [add directories specified with -L flags in LDFLAGS and LIBS to the
runtime library search paths (RPATH) @<:@default=yes@:>@])], [],
  [enable_rpaths=yes])

AC_ARG_ENABLE([openmp],
  [AC_HELP_STRING([--enable-openmp],
     [enable OpenMP support @<:@default=no@:>@])], [],
  [enable_openmp=no])

AC_PROG_FC
AC_LANG([Fortran])
ACX_FC_PP_SRCEXT([f90])
ACX_COMPILER_FC_VENDOR_SIMPLE
AC_SUBST([FC_VENDOR], [$acx_cv_fc_compiler_vendor])
ACX_COMPILER_FC_VERSION_SIMPLE
AC_SUBST([FC_VERSION], [$acx_cv_fc_compiler_version])

ACX_FC_LINE_LENGTH([unlimited])

ACX_FC_MODULE_NAMING(
  [AC_SUBST([FCMODEXT], ["$acx_cv_fc_module_naming_ext"])
   AC_SUBST([FCMODUC], ["$acx_cv_fc_module_naming_upper"])])

ACX_FC_MODULE_IN_FLAG([AC_SUBST([FCMODINC], ["$acx_cv_fc_module_in_flag"])])
ACX_FC_MODULE_OUT_FLAG([AC_SUBST([FCMODOUT], ["$acx_cv_fc_module_out_flag"])])

ACX_FC_INCLUDE_FLAG([AC_SUBST([FCINCFLAG], ["$acx_cv_fc_ftn_include_flag"])])
ACX_FC_INCLUDE_ORDER(
  [AC_SUBST([FCINCORDER], ["$acx_cv_fc_ftn_include_order"])],
  [AC_SUBST([FCINCORDER], ['src,flg'])])

ACX_FC_INCLUDE_FLAG_PP(
  [AC_SUBST([FCINCFLAG_PP], ["$acx_cv_fc_pp_include_flag"])])
ACX_FC_INCLUDE_ORDER_PP(
  [AC_SUBST([FCINCORDER_PP], ["$acx_cv_fc_pp_include_order"])],
  [AC_SUBST([FCINCORDER_PP], ['inc,flg'])])

ACX_LANG_MACRO_FLAG([AC_SUBST([FCDEF_PP], ["$acx_cv_fc_macro_flag"])])

dnl Additional macro definitions to aid the dependency generator,
dnl e.g. compiler intrinsic macros:
AC_SUBST([DEPGEN_FCFLAGS], [''])

AS_VAR_IF([enable_silent_rules], [yes],
  [AC_SUBST([DEFAULT_VERBOSITY], [0])],
  [AC_SUBST([DEFAULT_VERBOSITY], [1])])

dnl Generate RPATH flags:
AS_VAR_IF([enable_rpaths], [yes],
  [ACX_SHLIB_FC_RPATH_FLAG
   ASX_EXTRACT_ARGS([acx_L_args], ["$LDFLAGS $LIBS"], ['-L@<:@ @:>@*'])
   for acx_L_arg in $acx_L_args; do
     ASX_VAR_APPEND_UNIQ([LDFLAGS],
       ["$acx_cv_fc_rpath_flag$acx_L_arg"], [' '])
   done
   ACX_SHLIB_RPATH_FLAGS_CHECK])

acx_libsearch_flags='-L'
AS_VAR_IF([enable_rpaths], [yes],
  [AS_VAR_APPEND([acx_libsearch_flags], [" '$acx_cv_fc_rpath_flag'"])])
eval "set dummy $acx_libsearch_flags"; shift
ACX_LANG_PACKAGE_INIT([NetCDF], ["$FCMODINC"], ["$@"])
ACX_LANG_PACKAGE_INIT([CDI], ["$FCMODINC"], ["$@"])

AS_VAR_IF([enable_openmp], [yes],
  [ACX_LANG_OPENMP_FLAG(
     [AS_VAR_APPEND([FCFLAGS], [" $acx_cv_fc_openmp_flag"])
      ACX_LANG_MACRO_CHECK_VALUE([_OPENMP],
         [201511 201307 201107 200805 200505 200011 199911 199710])
      AS_VAR_IF([acx_macro_value], [unknown],
        [AC_MSG_WARN([unable to detect supported OpenMP version])
         AS_VAR_APPEND([DEPGEN_FCFLAGS], [" ${FCDEF_PP}_OPENMP"])],
        [AS_VAR_APPEND([DEPGEN_FCFLAGS],
           [" ${FCDEF_PP}_OPENMP=$acx_macro_value"])])])],
  [ACX_LANG_MACRO_CHECK_DEFINED([_OPENMP])
   AS_VAR_IF([acx_macro_defined], [yes],
     [AC_MSG_ERROR([Fortran compiler enables OpenMP support by default: set dnl
FCFLAGS accordingly to disable it])])])

dnl Check for NetCDF
acx_save_FCFLAGS=$FCFLAGS
NETCDF_FCFLAGS=${NETCDF_FCFLAGS-$acx_fc_NetCDF_inc_search_args}
FCFLAGS="$NETCDF_FCFLAGS $acx_save_FCFLAGS"
ACX_FC_MODULE_CHECK([netcdf],
  [ACX_LANG_LIB_SEARCH([NETCDF_FCLIBS], [nf90_open],
     [ASX_PREPEND_LDFLAGS([$acx_fc_NetCDF_lib_search_args],
        [-lnetcdff], [-lnetcdff -lnetcdf])],
     [NETCDF_FCLIBS=$acx_cv_fc_lib_func_NF90_OPEN
      acx_have_netcdf=yes], [],
     [AC_LANG_PROGRAM([],
[[      use netcdf, only: nf90_open, NF90_NOWRITE
      implicit none
      integer :: status, ncid
      status = nf90_open('conftest.nc', NF90_NOWRITE, ncid)]])])])
FCFLAGS=$acx_save_FCFLAGS

dnl Check for CDI
acx_save_FCFLAGS=$FCFLAGS
CDI_FCFLAGS=${CDI_FCFLAGS-$acx_fc_CDI_inc_search_args}
FCFLAGS="$CDI_FCFLAGS $acx_save_FCFLAGS"
ACX_FC_MODULE_CHECK([mo_cdi],
  [ACX_LANG_LIB_SEARCH([CDI_FCLIBS], [vlistDefVarTimetype],
     [ASX_PREPEND_LDFLAGS([$acx_fc_CDI_lib_search_args],
        [-lcdi_f2003], [-lcdi_f2003 -lcdi])],
     [CDI_FCLIBS=$acx_cv_fc_lib_func_VLISTDEFVARTIMETYPE
      acx_have_netcdf=yes], [],
     [AC_LANG_PROGRAM([],
[[      use mo_cdi, only: vlistDefVarTimetype
      implicit none
      integer :: a, b, c
      call vlistDefVarTimetype(a, b, c)]])])])
FCFLAGS=$acx_save_FCFLAGS

dnl Check for file formats supported by CDI.
acx_save_FCFLAGS=$FCFLAGS; FCFLAGS="$CDI_FCFLAGS $acx_save_FCFLAGS"
acx_save_LIBS=$LIBS; LIBS="$CDI_FCLIBS $acx_save_LIBS"
acx_cdi_formats="grb:CDI_FILETYPE_GRB grb2:CDI_FILETYPE_GRB2 nc:CDI_FILETYPE_NC
                 nc2:CDI_FILETYPE_NC2 nc4:CDI_FILETYPE_NC4
                 nc4c:CDI_FILETYPE_NC4C nc5:CDI_FILETYPE_NC5
                 srv:CDI_FILETYPE_SRV ext:CDI_FILETYPE_EXT ieg:CDI_FILETYPE_IEG"
for acx_cdi_format in $acx_cdi_formats; do
  acx_cdi_format_name=`echo "$acx_cdi_format" | cut -d: -f1`
  acx_cdi_format_ID=`echo "$acx_cdi_format" | cut -d: -f2`
  acx_cache_var_name="acx_cv_cdi_have_$acx_cdi_format_name"
  AC_CACHE_CHECK([whether CDI supports file format '$acx_cdi_format_name'],
    [$acx_cache_var_name],
    [AS_VAR_SET([$acx_cache_var_name], [unknown])
     AS_VAR_IF([cross_compiling], [no],
       [AC_LINK_IFELSE([AC_LANG_PROGRAM([],
[[      use mo_cdi, only: cdiHaveFiletype, $acx_cdi_format_ID
      write(*, "(i0)") cdiHaveFiletype($acx_cdi_format_ID)]])],
          [acx_exec_result=`./conftest$ac_exeext 2>/dev/null`
           AS_IF([test $? -eq 0],
             [AS_IF([test $acx_exec_result -eq 1],
                [AS_VAR_SET([$acx_cache_var_name], [yes])],
                [AS_VAR_SET([$acx_cache_var_name], [no])])])])])])
done
FCFLAGS=$acx_save_FCFLAGS; LIBS=$acx_save_LIBS
dnl Here we can check whether the format is supported and do something.
dnl For example, we can check whether NC is supported and emit a warning:
dnl    AS_IF([test x"$acx_cv_cdi_have_nc" != xyes],
dnl      [AC_MSG_WARN([CDI does not support file format 'nc'])])

dnl Check for PYTHON
AC_ARG_VAR([PYTHON], [Python interpreter command])dnl
ACX_PROG_SEARCH([PYTHON], [python python3],
  [acx_python_try="$acx_candidate $srcdir/mkhelper/depgen.py -h dnl
>&AS_MESSAGE_LOG_FD"
   _AC_DO_VAR([acx_python_try])],
  [PYTHON=$acx_candidate])

AC_PROG_INSTALL
AC_PROG_MKDIR_P

AC_CONFIG_FILES([Makefile])

dnl An additional step to generate dummy dependencies:
AC_CONFIG_COMMANDS([depfiles],
  [${MAKE-make} dummy-depend >/dev/null 2>&AS_MESSAGE_LOG_FD
   sleep 1
   touch Makefile])

AC_OUTPUT

