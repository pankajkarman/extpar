module mo_array_cache

  use, intrinsic :: iso_c_binding, only: c_ptr, c_f_pointer, c_sizeof, c_associated
  use mo_util_mmap_cache,          only: allocate_cache, deallocate_cache  
  use mo_kind,                     only: i4, i8, sp, dp
  
  implicit none
  
  private
  
  public :: allocate_cached
  public :: deallocate_cached  

  integer, parameter :: of_real_type    = 0
  integer, parameter :: of_integer_type = 1  
  
  type variable
    integer :: hash
    character(len=64) :: name
    integer :: sizes(3)
    type(c_ptr) :: ptr_for_deallocation
    integer :: real_or_integer_type
    integer :: kind_type
  end type variable

  type(variable) :: cached_variables(512)
  integer :: number_of_cached_variables = 0

  interface allocate_cached
    module procedure allocate_3d_sp_cached
    module procedure allocate_4d_sp_cached
    module procedure allocate_5d_sp_cached    
    module procedure allocate_3d_dp_cached
    module procedure allocate_4d_dp_cached
    module procedure allocate_5d_dp_cached    
    module procedure allocate_3d_i4_cached
    module procedure allocate_4d_i4_cached
    module procedure allocate_5d_i4_cached    
  end interface allocate_cached
  
contains

  subroutine allocate_3d_sp_cached(variable_name, variable_pointer, variable_size)

    character(len=*), intent(in) :: variable_name
    real(sp), pointer :: variable_pointer(:,:,:) 
    integer, intent(in) :: variable_size(3)
    
    type(c_ptr) :: addr

    if (any(variable_size == 0)) then
      allocate(variable_pointer(variable_size(1), variable_size(2), variable_size(3)))
    else
      addr = allocate_cache(variable_name, int(sum(variable_size)*c_sizeof(0.0_sp),i8))
      call c_f_pointer(addr, variable_pointer, shape=variable_size)

      number_of_cached_variables = number_of_cached_variables + 1

      cached_variables(number_of_cached_variables) = variable(                   &
           &                                         number_of_cached_variables, &
           &                                         variable_name,              &
           &                                         variable_size,              &
           &                                         addr,                       &
           &                                         of_real_type,               &
           &                                         sp)
    endif

  end subroutine allocate_3d_sp_cached

  subroutine allocate_3d_dp_cached(variable_name, variable_pointer, variable_size)

    character(len=*), intent(in) :: variable_name
    real(dp), pointer :: variable_pointer(:,:,:) 
    integer, intent(in) :: variable_size(3)
    
    type(c_ptr) :: addr

    if (any(variable_size == 0)) then
      allocate(variable_pointer(variable_size(1), variable_size(2), variable_size(3)))
    else
      addr = allocate_cache(variable_name, int(sum(variable_size)*c_sizeof(0.0_dp),i8))
      call c_f_pointer(addr, variable_pointer, shape=variable_size)

      number_of_cached_variables = number_of_cached_variables + 1

      cached_variables(number_of_cached_variables) = variable(                   &
           &                                         number_of_cached_variables, &
           &                                         variable_name,              &
           &                                         variable_size,              &
           &                                         addr,                       &
           &                                         of_real_type,               &           
           &                                         dp)
    endif

  end subroutine allocate_3d_dp_cached

    subroutine allocate_3d_i4_cached(variable_name, variable_pointer, variable_size)

    character(len=*), intent(in) :: variable_name
    integer(i4), pointer :: variable_pointer(:,:,:) 
    integer, intent(in) :: variable_size(3)
    
    type(c_ptr) :: addr

    if (any(variable_size == 0)) then
      allocate(variable_pointer(variable_size(1), variable_size(2), variable_size(3)))
    else
      addr = allocate_cache(variable_name, int(sum(variable_size)*c_sizeof(0_i4),i8))
      call c_f_pointer(addr, variable_pointer, shape=variable_size)

      number_of_cached_variables = number_of_cached_variables + 1

      cached_variables(number_of_cached_variables) = variable(                   &
           &                                         number_of_cached_variables, &
           &                                         variable_name,              &
           &                                         variable_size,              &
           &                                         addr,                       &
           &                                         of_integer_type,            &                      
           &                                         i4)
    endif

  end subroutine allocate_3d_i4_cached

  subroutine allocate_4d_sp_cached(variable_name, variable_pointer, variable_size)

    character(len=*), intent(in) :: variable_name
    real(sp), pointer :: variable_pointer(:,:,:,:) 
    integer, intent(in) :: variable_size(4)
    
    type(c_ptr) :: addr
    
    if (any(variable_size == 0)) then
      allocate(variable_pointer(variable_size(1), variable_size(2), variable_size(3), variable_size(4)))
    else
      addr = allocate_cache(variable_name, int(sum(variable_size)*c_sizeof(0.0_sp),i8))
      call c_f_pointer(addr, variable_pointer, shape=variable_size)
      
      number_of_cached_variables = number_of_cached_variables + 1
      
      cached_variables(number_of_cached_variables) = variable(                   &
           &                                         number_of_cached_variables, &
           &                                         variable_name,              &
           &                                         variable_size,              &
           &                                         addr,                       &
           &                                         of_real_type,               &           
           &                                         sp)
    endif
    
  end subroutine allocate_4d_sp_cached

  subroutine allocate_4d_dp_cached(variable_name, variable_pointer, variable_size)

    character(len=*), intent(in) :: variable_name
    real(dp), pointer :: variable_pointer(:,:,:,:) 
    integer, intent(in) :: variable_size(4)
    
    type(c_ptr) :: addr
    
    if (any(variable_size == 0)) then
      allocate(variable_pointer(variable_size(1), variable_size(2), variable_size(3), variable_size(4)))
    else
      addr = allocate_cache(variable_name, int(sum(variable_size)*c_sizeof(0.0_dp),i8))
      call c_f_pointer(addr, variable_pointer, shape=variable_size)
      
      number_of_cached_variables = number_of_cached_variables + 1
      
      cached_variables(number_of_cached_variables) = variable(                   &
           &                                         number_of_cached_variables, &
           &                                         variable_name,              &
           &                                         variable_size,              &
           &                                         addr,                       &
           &                                         of_real_type,               &           
           &                                         dp)
    endif
    
  end subroutine allocate_4d_dp_cached

    subroutine allocate_4d_i4_cached(variable_name, variable_pointer, variable_size)

    character(len=*), intent(in) :: variable_name
    integer(i4), pointer :: variable_pointer(:,:,:,:) 
    integer, intent(in) :: variable_size(4)
    
    type(c_ptr) :: addr
    
    if (any(variable_size == 0)) then
      allocate(variable_pointer(variable_size(1), variable_size(2), variable_size(3), variable_size(4)))
    else
      addr = allocate_cache(variable_name, int(sum(variable_size)*c_sizeof(0_i4),i8))
      call c_f_pointer(addr, variable_pointer, shape=variable_size)
      
      number_of_cached_variables = number_of_cached_variables + 1
      
      cached_variables(number_of_cached_variables) = variable(                   &
           &                                         number_of_cached_variables, &
           &                                         variable_name,              &
           &                                         variable_size,              &
           &                                         addr,                       &
           &                                         of_integer_type,            &           
           &                                         i4)
    endif
    
  end subroutine allocate_4d_i4_cached

  subroutine allocate_5d_sp_cached(variable_name, variable_pointer, variable_size)

    character(len=*), intent(in) :: variable_name
    real(sp), pointer :: variable_pointer(:,:,:,:,:) 
    integer, intent(in) :: variable_size(5)
    
    type(c_ptr) :: addr
    
    if (any(variable_size == 0)) then
      allocate(variable_pointer(variable_size(1), &
           &                    variable_size(2), &
           &                    variable_size(3), &
           &                    variable_size(4), &
           &                    variable_size(5)))
    else
      addr = allocate_cache(variable_name, int(sum(variable_size)*c_sizeof(0.0_sp),i8))
      call c_f_pointer(addr, variable_pointer, shape=variable_size)
      
      number_of_cached_variables = number_of_cached_variables + 1
      
      cached_variables(number_of_cached_variables) = variable(                   &
           &                                         number_of_cached_variables, &
           &                                         variable_name,              &
           &                                         variable_size,              &
           &                                         addr,                       &
           &                                         of_real_type,               &           
           &                                         sp)
    endif
    
  end subroutine allocate_5d_sp_cached

  subroutine allocate_5d_dp_cached(variable_name, variable_pointer, variable_size)

    character(len=*), intent(in) :: variable_name
    real(dp), pointer :: variable_pointer(:,:,:,:,:) 
    integer, intent(in) :: variable_size(5)
    
    type(c_ptr) :: addr
    
    if (any(variable_size == 0)) then
      allocate(variable_pointer(variable_size(1), &
           &                    variable_size(2), &
           &                    variable_size(3), &
           &                    variable_size(4), &
           &                    variable_size(5)))
    else
      addr = allocate_cache(variable_name, int(sum(variable_size)*c_sizeof(0.0_dp),i8))
      call c_f_pointer(addr, variable_pointer, shape=variable_size)
      
      number_of_cached_variables = number_of_cached_variables + 1
      
      cached_variables(number_of_cached_variables) = variable(                   &
           &                                         number_of_cached_variables, &
           &                                         variable_name,              &
           &                                         variable_size,              &
           &                                         addr,                       &
           &                                         of_real_type,               &           
           &                                         dp)
    endif
    
  end subroutine allocate_5d_dp_cached

  subroutine allocate_5d_i4_cached(variable_name, variable_pointer, variable_size)

    character(len=*), intent(in) :: variable_name
    integer(i4), pointer :: variable_pointer(:,:,:,:,:) 
    integer, intent(in) :: variable_size(5)
    
    type(c_ptr) :: addr
    
    if (any(variable_size == 0)) then
      allocate(variable_pointer(variable_size(1), &
           &                    variable_size(2), &
           &                    variable_size(3), &
           &                    variable_size(4), &
           &                    variable_size(5)))
    else
      addr = allocate_cache(variable_name, int(sum(variable_size)*c_sizeof(0_i4),i8))
      call c_f_pointer(addr, variable_pointer, shape=variable_size)
      
      number_of_cached_variables = number_of_cached_variables + 1
      
      cached_variables(number_of_cached_variables) = variable(                   &
           &                                         number_of_cached_variables, &
           &                                         variable_name,              &
           &                                         variable_size,              &
           &                                         addr,                       &
           &                                         of_integer_type,            &           
           &                                         i4)
    endif
    
  end subroutine allocate_5d_i4_cached

  subroutine deallocate_cached(variable_name)
    character(len=*), intent(in) :: variable_name
    integer :: i
    integer(i8) :: v_sizeof

    v_sizeof = -1

    do i = 1, number_of_cached_variables

      select case (cached_variables(i)%real_or_integer_type)
      case(of_real_type)
        select case (cached_variables(i)%kind_type)
        case(sp)
          v_sizeof = c_sizeof(0.0_sp)
        case(dp)
          v_sizeof = c_sizeof(0.0_dp)
        end select
      case(of_integer_type)
        select case (cached_variables(i)%kind_type)
        case(i4)
          v_sizeof = c_sizeof(0_i4)
        end select
      end select
        
      if (variable_name == cached_variables(i)%name) then
        if (.not. c_associated(cached_variables(i)%ptr_for_deallocation)) then
          call deallocate_cache(cached_variables(i)%ptr_for_deallocation, &
               &                int(sum(cached_variables(i)%sizes)*v_sizeof,i8))
          exit
        endif
      endif
    enddo
    
  end subroutine deallocate_cached

end module mo_array_cache
