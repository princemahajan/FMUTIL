!############################################################################################
!     ________  _____  ______________ 
!    / ____/  |/  / / / /_  __/  _/ / 
!   / /_  / /|_/ / / / / / /  / // /  
!  / __/ / /  / / /_/ / / / _/ // /___
! /_/   /_/  /_/\____/ /_/ /___/_____/
!
!> \mainpage    FMUTIL: Fortran Miscellaneous UTILities
!! \details     A Fortran library for miscellaneous utilities such as data structures
!!              (List, Vector), root-finding for polynomials and nonlinear equations, etc.
!! \author      Bharat Mahajan
!! \version     0.1
!! \date        Created: 02/12/2020    
!! \copyright   Copyright 2020 Bharat Mahajan  
!! \section     sec Introduction 
!!              FMUTil is a modern object-oriented Fortran library that provides (as of now):
!!              - Two data structures
!!                  + Vector, similar to C++ STL Vectors
!!                  + List, similar to Python List
!!              - Root-finding algorithm for polynomials by solving associated eigenvalue
!!                problem
!!              - Root-finding of a nonlinear equation using Brent's algorithm
!!              - A binary search method for sorted arrays
!!
!! \section     usagesec How to Use
!!              + For Vector data structure of any type, define the type of the Vector 
!!              element and the assignment operation as follows.
!!              
!!              \code{.f90}
!!                  module TestModule
!!                  use FMUtil
!!                  type, extends(VecElem) :: relem
!!                      real :: rdata
!!                  contains
!!                      procedure :: AssignVecElem => assign_relem
!!                  end type relem
!!
!!                  contains
!!    
!!                  subroutine assign_relem(lhs, rhs)
!!                      implicit none
!!                      class(relem), intent(out) :: lhs
!!                      class(VecElem), intent(in) :: rhs
!!                      select type (rhs)
!!                      class is (relem)
!!                          lhs%rdata = rhs%rdata
!!                      end select    
!!                  end subroutine assign_relem
!!                  end module TestModule
!!              \endcode
!!
!!              + Then, use Vector using
!!
!!              \code{.f90}
!!                  program Test
!!                  use FMUtil
!!                  use TestModule
!!
!!                  integer :: ctr
!!                  type(Vector) :: vec
!!                  type(relem) :: rd1, rd2
!!
!!                  rd1%rdata = 3.1416
!!                  ctr = vec%PushBack(rd1)    
!!                  rd1%rdata = 2.718
!!                  call vec%Insert(2,[rd1, rd1])
!!
!!                  do ctr = 1, vec%Size()
!!                      rd2 = vec%ElemAt(ctr)
!!                      print *, 'int(', ctr, ')=', rd2%rdata        
!!                  end do    
!!                  end program Test
!!              \endcode
!!
!!              + List can be used as
!!
!!              \code{.f90}
!!                  program Test
!!                  use FMUtil
!!
!!                  integer :: ctr
!!                  type(List) :: list
!!                  class(*), allocatable :: item
!!                  type(relem) :: rd1
!!                  rd1%rdata = 3.1416
!!
!!                  ctr = list%PushBack(100)
!!                  ctr = list%PushBack(rd1)
!!                  ctr = list%PushBack(rd1%rdata)
!!                  ctr = list%Insert(3,'pi')    
!!
!!                  do ctr = 1, list%Size()
!!                      item = list%Item(ctr)
!!                      select type (item)
!!                      type is (real)
!!                          print *, item
!!                      type is (relem)
!!                          print *, item
!!                      type is (integer)
!!                          print *, item
!!                      type is (chanracter(len=*))
!!                          print *, item
!!                      end select
!!                  end do  
!!                  end program Test  
!!              \endcode
!!
!!              + See the test program files, test.f90 and testmod.f90, in tests folder
!!              for more features and functionality.
!!
!!
!! \section     Installation
!!              FMUTIL is tested with Intel Fortran Compiler and MinGW-W64 gfortran. Doxyfile is 
!!              provided for generating extensive API documentation using Doxygen. FLINT has no
!!              dependency on any other library. The CMakeLists file is provided along with 
!!              additional CMake modules for compiler options and a Find module for finding and
!!              linking Intel MKL is also provided. These files can be used to auto generate 
!!              Visual Studio projects or makefiles on Windows and Linux using CMake. 
!!              A find module for FMUTIL is provided that generates CMake config files for 
!!              easily linking FMUTIL using the find_package() command. The steps to link FMUTIL 
!!              in cmake-based projects are:
!!
!!              + In cmake GUI or command-line, set FMUTIL_INSTALL_LIB_DIR to the desired directory, 
!!                      where the compiled library, modules, and cmake config files will be installed.
!!              + In cmake GUI or command-line, set FMUTIL_INSTALL_BIN_DIR to the desired directory, 
!!                      where the compiled test executables will be installed.
!!              + In your project CMakeLists.txt, insert
!!
!!              \code
!!                  find_package(FMUTIL REQUIRED 0.1 CONFIG 
!!                      PATHS "<SAME_PATH_AS_IN_FMUTIL_INSTALL_LIB_DIR>" NO_CMAKE_PACKAGE_REGISTRY)
!!                  target_link_libraries(<YOUR_TARGET_NAME> FMUTIL::FMUTIL)
!!              \endcode
!
!############################################################################################
 
    
module FMUtil

    ! Base Module
    use FMUTILBase
    
    ! Root finding module
    use RootFinding
    
    ! search algorithms
    use Search
    
    ! data structures
    use Vectors
    use Lists

    implicit none


    contains
    
    
end module FMUtil
