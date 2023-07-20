MODULE info_extpar

  USE mo_logging

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: info_print

  CHARACTER (len=*), PARAMETER :: INFO_PackageName     = 'extpar-v5.12'
  CHARACTER (len=*), PARAMETER :: INFO_RepositoryURL   = 'git@github.com:C2SM-RCM/extpar'
  CHARACTER (len=*), PARAMETER :: INFO_LastCommitDate  = '2023-07-19 08:55:22'
  CHARACTER (len=*), PARAMETER :: INFO_RevisionHash    = '8828b17b693e98392454466beff185735d3b0414'
  CHARACTER (len=*), PARAMETER :: INFO_CodeIsModified  = 'modified'
  CHARACTER (len=*), PARAMETER :: INFO_CompilerVersion = 'GCC 9.1.0'
  CHARACTER (len=*), PARAMETER :: INFO_CompiledBy      = 'jhelmert'
  CHARACTER (len=*), PARAMETER :: INFO_CompileTime     = '2023-07-20 09:49:18'
  CHARACTER (len=*), PARAMETER :: INFO_CompileMachine  = 'rcnl'

  CHARACTER (len=16)   :: INFO_StartTime = ''
  CHARACTER (len=1024) :: INFO_BinaryName  = ''

  PUBLIC :: INFO_RevisionHash, &
       &    INFO_CodeIsModified, &
       &    INFO_PackageName, &
       &    INFO_CompilerVersion
  
CONTAINS

  SUBROUTINE info_print

    CHARACTER (len=8)    :: date
    CHARACTER (len=10)   :: time
    CHARACTER (len=1024) :: arg0
    INTEGER :: len_arg0

    IF ( LEN_TRIM(INFO_StartTime) == 0 ) THEN
      CALL date_and_time(date, time)
      INFO_StartTime = date(1:4) // '-' // date(5:6) // '-' // date(7:8) // &
           &    ' ' // time(1:2) // ':' // time(3:4)
    END IF
    CALL get_command_argument(0, arg0, len_arg0)
    INFO_BinaryName = arg0(1:len_arg0)

    WRITE(logging%fileunit,*)''
    WRITE(logging%fileunit,*)'==== Code information used to build this binary ===='
    WRITE(logging%fileunit,*)''
    WRITE(logging%fileunit,*)'Compile-Date ......: ' // INFO_CompileTime(1:LEN_TRIM(INFO_CompileTime))
    WRITE(logging%fileunit,*)'Code is modified ..: ' // INFO_CodeIsModified(1:LEN_TRIM(INFO_CodeIsModified))
    WRITE(logging%fileunit,*)'Library name ......: ' // INFO_PackageName(1:LEN_TRIM(INFO_PackageName))
    WRITE(logging%fileunit,*)'Compiled on .......: ' // INFO_CompileMachine(1:LEN_TRIM(INFO_CompileMachine))
    WRITE(logging%fileunit,*)'Revision number ...: ' // INFO_RevisionHash(1:LEN_TRIM(INFO_RevisionHash))
    WRITE(logging%fileunit,*)'Checkout-Date .....: ' // INFO_LastCommitDate(1:LEN_TRIM(INFO_LastCommitDate))
    WRITE(logging%fileunit,*)'Binary name .......: ' // INFO_BinaryName(1:LEN_TRIM(INFO_BinaryName))
    WRITE(logging%fileunit,*)'Compiled by .......: ' // INFO_CompiledBy(1:LEN_TRIM(INFO_CompiledBy))
    WRITE(logging%fileunit,*)'Compiler version ..: ' // INFO_CompilerVersion(1:LEN_TRIM(INFO_CompilerVersion))
    WRITE(logging%fileunit,*)'Current start time : ' // INFO_StartTime(1:LEN_TRIM(INFO_StartTime))
    WRITE(logging%fileunit,*)'==== End of code information ===='
    WRITE(logging%fileunit,*)''

  END SUBROUTINE info_print

END MODULE info_extpar
