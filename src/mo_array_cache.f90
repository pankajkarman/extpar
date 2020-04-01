module mo_array_cache

  use, intrinsic :: iso_c_binding, only: c_ptr, c_f_pointer, c_sizeof, c_associated
  use mo_util_mmap_cache
  use mo_kind,                     only: i8, wp
  
  implicit none

  private

  public :: allocate_cached
  public :: deallocate_cached  

  type variable
     integer :: hash
     character(len=64) :: name
     integer :: sizes(3)
     type(c_ptr) :: ptr_for_deallocation     
  end type variable

  type(variable) :: cached_variables(512)
  integer :: number_of_cached_variables = 0

  interface allocate_cached
    module procedure allocate_3d_cached
    module procedure allocate_4d_cached
  end interface allocate_cached
  
contains

  subroutine allocate_3d_cached(variable_name, variable_pointer, variable_size)

    character(len=*), intent(in) :: variable_name
    real(wp), pointer :: variable_pointer(:,:,:) 
    integer, intent(in) :: variable_size(3)
    
    type(c_ptr) :: addr

    if (any(variable_size == 0)) then
      allocate(variable_pointer(variable_size(1), variable_size(2), variable_size(3)))
    else
      addr = allocate_cache(variable_name, int(sum(variable_size)*c_sizeof(0.0_wp),i8))
      call c_f_pointer(addr, variable_pointer, shape=variable_size)

      number_of_cached_variables = number_of_cached_variables + 1

      cached_variables(number_of_cached_variables) = variable(                   &
           &                                         number_of_cached_variables, &
           &                                         variable_name,              &
           &                                         variable_size,              &
           &                                         addr)
    endif

  end subroutine allocate_3d_cached

  subroutine allocate_4d_cached(variable_name, variable_pointer, variable_size)

    character(len=*), intent(in) :: variable_name
    real(wp), pointer :: variable_pointer(:,:,:,:) 
    integer, intent(in) :: variable_size(4)
    
    type(c_ptr) :: addr
    
    if (any(variable_size == 0)) then
      allocate(variable_pointer(variable_size(1), variable_size(2), variable_size(3), variable_size(4)))
    else
      addr = allocate_cache(variable_name, int(sum(variable_size)*c_sizeof(0.0_wp),i8))
      call c_f_pointer(addr, variable_pointer, shape=variable_size)

      number_of_cached_variables = number_of_cached_variables + 1

      cached_variables(number_of_cached_variables) = variable(                   &
           &                                         number_of_cached_variables, &
           &                                         variable_name,              &
           &                                         variable_size,              &
           &                                         addr)
    endif
  end subroutine allocate_4d_cached

  subroutine deallocate_cached(variable_name)
    character(len=*), intent(in) :: variable_name
    integer :: i

    do i = 1, number_of_cached_variables
       if (variable_name == cached_variables(i)%name) then
         if (.not. c_associated(cached_variables(i)%ptr_for_deallocation)) then
           call deallocate_cache(cached_variables(i)%ptr_for_deallocation, int(sum(cached_variables(i)%sizes)*c_sizeof(0.0_wp),i8))     
           exit
         endif
       endif
     enddo
    
  end subroutine deallocate_cached

end module mo_array_cache
