#!/bin/bash

#  Script module updating info_int2lm.f90 (which prints version information
#  on binary call to the stdout)
#
# Author  : Oliver Fuhrer (oliver.fuhrer@meteoswiss.ch)
#
# History :
#   25.10.2010  fuo     First release
#   05.05.2011  roa     Adaptation for int2lm
#
# Notes   :
# 1. This script must receive a fresh .config file as argument 1
# 2. It will update the info_int2lm.f90 file in the directory received as argument 2

# set some globals (need to be manually updated)
code_name="extpar"
code_version="_3.0-rc"

# parse command line arguments
fconfig="$1"
srcdir="$2"

# check if file is available
if [ ! -f "${srcdir}/info_extpar.f90" ] ; then
  echo "ERROR: could not find ${srcdir}/info_extpar.f90 file" 1>&2
  exit 1
fi

# get host and date
creation_host=`hostname | sed 's/[0-9]*$//g'`
creation_date=`date`
creation_user=`whoami`

# get source code checksum (excluding info_int2lm.f90 file)
code_files=`/bin/ls -1d ${srcdir}/*.f90 | grep -v 'info_int2lm\.f90'`
code_checksum=`cksum ${code_files} | cksum | awk '{print $1}'`

# get compiler information
compiler=`cat $fconfig | grep 'Compiler command' | cut -d":" -f2`
cversion=`cat $fconfig | grep 'Compiler version' | cut -d":" -f2`
cincludes=`cat $fconfig | grep 'Compiler includes' | cut -d":" -f2`
cflags=`cat $fconfig | grep 'Compiler flags' | cut -d":" -f2`
linker=`cat $fconfig | grep 'Linker command' | cut -d":" -f2`
lversion=`cat $fconfig | grep 'Linker version' | cut -d":" -f2`
lflags=`cat $fconfig | grep 'Linker flags' | cut -d":" -f2`
llibraries=`cat $fconfig | grep 'Linker libraries' | cut -d":" -f2 |cut -c1-200`
string=`cat $fconfig | grep 'Compiler *:' | cut -d":" -f2 | cut -d" " -f3-`

# information related to version control system
svn info . 1>/dev/null 2>/dev/null
if [ "$?" -ne 0 ] ; then
  
  # this directory is not under version control
  INFO_LibraryName="${code_name}${code_version}"
  INFO_RevisionTag="(missing)"
  INFO_CheckinDate="(missing)"
  INFO_RevisionNumber="(missing)"
  INFO_CheckoutDate="(missing)"
  INFO_ProductionDate="(missing)"
  INFO_CodeIsModified="unknown"

else
  
  # this is a subversion working copy
  INFO_LibraryName="${code_name}${code_version}"
  INFO_RevisionTag=`cd ..; svn info | grep '^URL: ' | sed 's/^URL: //g'`
  INFO_CheckinDate=`cd ..; svn info | grep '^Last Changed Date: ' | sed 's/^Last Changed Date: //g'`
  INFO_RevisionNumber=`cd ..; svnversion`
  INFO_CheckoutDate="(missing)"
  INFO_ProductionDate="(missing)"
  echo "$INFO_RevisionNumber" | grep 'M' 2>/dev/null 1>/dev/null
  if [ "$?" -eq 0 ] ; then
    INFO_CodeIsModified="true"
  else
    INFO_CodeIsModified="false"
  fi
  
fi

# information related to compilation
INFO_CodeChecksum="${code_checksum}"
INFO_CompilerCall="${compiler} ${cflags}"
INFO_CompilerVersion="${cversion}"
INFO_DefinedMacros="${cincludes}"
INFO_UndefinedMacros="(missing)"
INFO_DebugOptions="(missing)"
INFO_LinkOptions="${linker} ${llibraries}"
INFO_CompiledBy="${creation_user}"
INFO_CompileTime="${creation_date}"
INFO_CompileMachine="${creation_host}"

# information which will have to be defined runtime
INFO_StartTime=""
INFO_BinaryName="${code_name}"
INFO_RunMachine=""
INFO_Nodes=""
INFO_Domain=""
INFO_Options=""

# replace the placeholders in info_int2lm.f90 with the correct strings
ftmp="/tmp/info_extpar.`whoami`.$$"
ierr=0
cp ${srcdir}/info_extpar.f90 ${ftmp}
sed "s|\(INFO_LibraryName *= *\)'.*'|\1'""${INFO_LibraryName}""'|g" ${ftmp} > ${ftmp}.1
((ierr=ierr+$?)); mv -f ${ftmp}.1 ${ftmp}; ((ierr=ierr+$?))
sed "s|\(INFO_RevisionTag *= *\)'.*'|\1'""${INFO_RevisionTag}""'|g" ${ftmp} > ${ftmp}.1
((ierr=ierr+$?)); mv -f ${ftmp}.1 ${ftmp}; ((ierr=ierr+$?))
sed "s|\(INFO_CheckinDate *= *\)'.*'|\1'""${INFO_CheckinDate}""'|g" ${ftmp} > ${ftmp}.1
((ierr=ierr+$?)); mv -f ${ftmp}.1 ${ftmp}; ((ierr=ierr+$?))
sed "s|\(INFO_RevisionNumber *= *\)'.*'|\1'""${INFO_RevisionNumber}""'|g" ${ftmp} > ${ftmp}.1
((ierr=ierr+$?)); mv -f ${ftmp}.1 ${ftmp}; ((ierr=ierr+$?))
sed "s|\(INFO_CheckoutDate *= *\)'.*'|\1'""${INFO_CheckoutDate}""'|g" ${ftmp} > ${ftmp}.1
((ierr=ierr+$?)); mv -f ${ftmp}.1 ${ftmp}; ((ierr=ierr+$?))
sed "s|\(INFO_ProductionDate *= *\)'.*'|\1'""${INFO_ProductionDate}""'|g" ${ftmp} > ${ftmp}.1
((ierr=ierr+$?)); mv -f ${ftmp}.1 ${ftmp}; ((ierr=ierr+$?))
sed "s|\(INFO_CodeIsModified *= *\)'.*'|\1'""${INFO_CodeIsModified}""'|g" ${ftmp} > ${ftmp}.1
((ierr=ierr+$?)); mv -f ${ftmp}.1 ${ftmp}; ((ierr=ierr+$?))
sed "s|\(INFO_CodeChecksum *= *\)'.*'|\1'""${INFO_CodeChecksum}""'|g" ${ftmp} > ${ftmp}.1
((ierr=ierr+$?)); mv -f ${ftmp}.1 ${ftmp}; ((ierr=ierr+$?))
sed "s|\(INFO_CompilerCall *= *\)'.*'|\1'""${INFO_CompilerCall}""'|g" ${ftmp} > ${ftmp}.1
((ierr=ierr+$?)); mv -f ${ftmp}.1 ${ftmp}; ((ierr=ierr+$?))
sed "s|\(INFO_CompilerVersion *= *\)'.*'|\1'""${INFO_CompilerVersion}""'|g" ${ftmp} > ${ftmp}.1
((ierr=ierr+$?)); mv -f ${ftmp}.1 ${ftmp}; ((ierr=ierr+$?))
sed "s|\(INFO_DefinedMacros *= *\)'.*'|\1'""${INFO_DefinedMacros}""'|g" ${ftmp} > ${ftmp}.1
((ierr=ierr+$?)); mv -f ${ftmp}.1 ${ftmp}; ((ierr=ierr+$?))
sed "s|\(INFO_UndefinedMacros *= *\)'.*'|\1'""${INFO_UndefinedMacros}""'|g" ${ftmp} > ${ftmp}.1
((ierr=ierr+$?)); mv -f ${ftmp}.1 ${ftmp}; ((ierr=ierr+$?))
sed "s|\(INFO_DebugOptions *= *\)'.*'|\1'""${INFO_DebugOptions}""'|g" ${ftmp} > ${ftmp}.1
((ierr=ierr+$?)); mv -f ${ftmp}.1 ${ftmp}; ((ierr=ierr+$?))
sed "s|\(INFO_LinkOptions *= *\)'.*'|\1'""${INFO_LinkOptions}""'|g" ${ftmp} > ${ftmp}.1
((ierr=ierr+$?)); mv -f ${ftmp}.1 ${ftmp}; ((ierr=ierr+$?))
sed "s|\(INFO_StartTime *= *\)'.*'|\1'""${INFO_StartTime}""'|g" ${ftmp} > ${ftmp}.1
((ierr=ierr+$?)); mv -f ${ftmp}.1 ${ftmp}; ((ierr=ierr+$?))
sed "s|\(INFO_CompiledBy *= *\)'.*'|\1'""${INFO_CompiledBy}""'|g" ${ftmp} > ${ftmp}.1
((ierr=ierr+$?)); mv -f ${ftmp}.1 ${ftmp}; ((ierr=ierr+$?))
sed "s|\(INFO_CompileTime *= *\)'.*'|\1'""${INFO_CompileTime}""'|g" ${ftmp} > ${ftmp}.1
((ierr=ierr+$?)); mv -f ${ftmp}.1 ${ftmp}; ((ierr=ierr+$?))
sed "s|\(INFO_CompileMachine *= *\)'.*'|\1'""${INFO_CompileMachine}""'|g" ${ftmp} > ${ftmp}.1
((ierr=ierr+$?)); mv -f ${ftmp}.1 ${ftmp}; ((ierr=ierr+$?))
sed "s|\(INFO_BinaryName *= *\)'.*'|\1'""${INFO_BinaryName}""'|g" ${ftmp} > ${ftmp}.1
((ierr=ierr+$?)); mv -f ${ftmp}.1 ${ftmp}; ((ierr=ierr+$?))
sed "s|\(INFO_RunMachine *= *\)'.*'|\1'""${INFO_RunMachine}""'|g" ${ftmp} > ${ftmp}.1
((ierr=ierr+$?)); mv -f ${ftmp}.1 ${ftmp}; ((ierr=ierr+$?))
sed "s|\(INFO_Nodes *= *\)'.*'|\1'""${INFO_Nodes}""'|g" ${ftmp} > ${ftmp}.1
((ierr=ierr+$?)); mv -f ${ftmp}.1 ${ftmp}; ((ierr=ierr+$?))
sed "s|\(INFO_Domain *= *\)'.*'|\1'""${INFO_Domain}""'|g" ${ftmp} > ${ftmp}.1
((ierr=ierr+$?)); mv -f ${ftmp}.1 ${ftmp}; ((ierr=ierr+$?))
sed "s|\(INFO_Options *= *\)'.*'|\1'""${INFO_Options}""'|g" ${ftmp} > ${ftmp}.1
((ierr=ierr+$?)); mv -f ${ftmp}.1 ${ftmp}; ((ierr=ierr+$?))

  
# check if everything ok
if [ "$ierr" -eq 0 ] ; then
  mv ${ftmp} ${srcdir}/info_extpar.f90
fi
/bin/rm -f ${ftmp}*

exit 0



