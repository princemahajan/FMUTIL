# FMUTIL

## Fortran Miscellaneous UTILities


### Author

Bharat Mahajan

### Introduction

FMUTIL is a modern object-oriented Fortran library that provides (as of now):

- Two data structures
    + Vector, similar to C++ STL Vectors
    + List, similar to Python List
- Root-finding algorithm for polynomials by solving associated eigenvalue problem
- Root-finding of a nonlinear equation using Brent's algorithm
- A binary search method for sorted arrays

### How to Use

+ For Vector data structure of any type, define the type of the Vector element and the assignment operation as follows.

    ```fortran
        module TestModule
        use FMUtil
        type, extends(VecElem) :: relem
            real :: rdata
        contains
            procedure :: AssignVecElem => assign_relem
        end type relem

        contains

        subroutine assign_relem(lhs, rhs)
            implicit none
            class(relem), intent(out) :: lhs
            class(VecElem), intent(in) :: rhs
            select type (rhs)
            class is (relem)
                lhs%rdata = rhs%rdata
            end select    
        end subroutine assign_relem
        end module TestModule
    ```

+ Then, use Vector using

    ```fortran
        program Test
        use FMUtil
        use TestModule

        integer :: ctr
        type(Vector) :: vec
        type(relem) :: rd1, rd2

        rd1%rdata = 3.1416
        ctr = vec%PushBack(rd1)    
        rd1%rdata = 2.718
        call vec%Insert(2,[rd1, rd1])

        do ctr = 1, vec%Size()
            rd2 = vec%ElemAt(ctr)
            print *, 'int(', ctr, ')=', rd2%rdata        
        end do    
        end program Test
    ```

+ List can be used as

    ```fortran
        program Test
        use FMUtil

        integer :: ctr
        type(List) :: list
        class(*), allocatable :: item
        type(relem) :: rd1
        rd1%rdata = 3.1416

        ctr = list%PushBack(100)
        ctr = list%PushBack(rd1)
        ctr = list%PushBack(rd1%rdata)
        ctr = list%Insert(3,'pi')    

        do ctr = 1, list%Size()
            item = list%Item(ctr)
            select type (item)
            type is (real)
                print *, item
            type is (relem)
                print *, item
            type is (integer)
                print *, item
            type is (chanracter(len=*))
                print *, item
            end select
        end do  
        end program Test  
    ```

+ See the test program files, test.f90 and testmod.f90, in tests folder for more features and functionality.

### Installation

FMUTIL is tested with Intel Fortran Compiler and MinGW-W64 gfortran (8.1.0) on Windows platofrm only. Doxyfile is provided for generating extensive API documentation using Doxygen. FLINT has no dependency on any other library. The CMakeLists file is provided along with additional CMake modules for compiler options and a Find module for finding and linking Intel MKL is also provided. These files can be used to auto generate Visual Studio projects or makefiles on Windows and Linux using CMake. A find module for FMUTIL is provided that generates CMake config files for easily linking FMUTIL using the find_package() command. The steps to link FMUTIL in cmake-based projects are:

+ In cmake GUI or command-line, set FMUTIL_INSTALL_LIB_DIR to the desired directory, where the compiled library, modules, and cmake config files will be installed.

+ In cmake GUI or command-line, set FMUTIL_INSTALL_BIN_DIR to the desired directory, where the compiled test executables will be installed.

+ In your project CMakeLists.txt, insert

    ```cmake
        find_package(FMUTIL REQUIRED 0.1 CONFIG PATHS "<SAME_PATH_AS_IN_FMUTIL_INSTALL_LIB_DIR>" NO_CMAKE_PACKAGE_REGISTRY)
        target_link_libraries(<YOUR_TARGET_NAME> FMUTIL::FMUTIL)
    ```
