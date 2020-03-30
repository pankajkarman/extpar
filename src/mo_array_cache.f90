module mo_array_cache

  use, intrinsic :: iso_c_binding, only: c_ptr, c_f_pointer, c_loc, &
       &                                 c_size_t, c_sizeof
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
  
contains

  subroutine allocate_cached(variable_name, variable_pointer, variable_size)

    character(len=*), intent(in) :: variable_name
    real(wp), pointer :: variable_pointer(:,:,:) 
    integer, intent(in) :: variable_size(:)
    
    type(c_ptr) :: addr
    
    addr = allocate_cache(variable_name, int(sum(variable_size*c_sizeof(0.0_wp)),i8))
    call c_f_pointer(addr, variable_pointer, shape=variable_size)

    number_of_cached_variables = number_of_cached_variables + 1

    cached_variables(number_of_cached_variables) = variable(                   &
         &                                         number_of_cached_variables, &
         &                                         variable_name,              &
         &                                         variable_size,              &
         &                                         addr)
    
  end subroutine allocate_cached

  subroutine deallocate_cached(variable_name)
    character(len=*), intent(in) :: variable_name
    integer :: i
    
    do i = 1, number_of_cached_variables
       if (variable_name == cached_variables(i)%name) then
          call deallocate_cache(cached_variables(i)%ptr_for_deallocation, int(sum(cached_variables(i)%sizes*c_sizeof(0.0_wp)),i8))     
          exit
       endif
    enddo
    
  end subroutine deallocate_cached

end module mo_array_cache
