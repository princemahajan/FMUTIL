#     ________  _____  ______________ 
#    / ____/  |/  / / / /_  __/  _/ / 
#   / /_  / /|_/ / / / / / /  / // /  
#  / __/ / /  / / /_/ / / / _/ // /___
# /_/   /_/  /_/\____/ /_/ /___/_____/                                                                             

# Module for finding Intel Math Kernel Library
# Author: Bharat Mahajan (bharat.mahajan@nasa.gov)

# No support for x86 architecture

# First find the Intel MKL root directory by reading the environment variable
# This requires the environment variable "MKLROOT" set with the path to the 
# MKL installation directory. On typical Windows system, it looks something like
# "C:\Program Files (x86)\IntelSWTools\compilers_and_libraries_2019.0.117\windows\mkl"
set(INTELMKLROOT $ENV{MKLROOT})

if(NOT INTELMKLROOT)
    message(FATAL_ERROR "Environment variable MKLROOT not found" )    
endif()

# Find the Intel MKL parent directory by reading the environment variable
# This requires the environment variable "IFORT_COMPILER20" set with the path to the 
# Compiler installation directory. On typical Windows system, it looks something like
# "C:\Program Files (x86)\IntelSWTools\compilers_and_libraries_2020\windows\"
set(INTELROOT $ENV{IFORT_COMPILER20})

if(NOT INTELROOT)
    message(FATAL_ERROR "Intel Fortran compiler not found" )    
endif()

# Set the following FALSE for dynamic linking of Intel MKL
if(POINCARE_BUILD_SHAREDLIB)
    set(MKL_USE_STATIC_LIBS FALSE)
    message(STATUS "POINCARE: Linking Intel MKL as a shared library")    
else()
    set(MKL_USE_STATIC_LIBS TRUE)
endif()    

# Set the following TRUE for 64-bit default integer data type
set(MKL_ILP_INTERFACE ${FMUTIL_INTSIZE_64})

if(MKL_ILP_INTERFACE AND CMAKE_HOST_WIN32)
    set(MKL_CompileOption "/integer-size:64")
elseif(MKL_ILP_INTERFACE AND CMAKE_HOST_UNIX)
    set(MKL_CompileOption "-integer-size:64")
else()
    unset(MKL_CompileOption)
endif()


# Set the apprpriate MKL library names

if(CMAKE_HOST_WIN32)

    # Windows-specific library names with extensions

    # Interface Layer
    if(MKL_ILP_INTERFACE)

        # LAPACK and BLAS Fortran 95 Wrappers Libraries
        set(MKL_BLAS95_LIB mkl_lapack95_ilp64.lib)
        set(MKL_LAPACK95_LIB mkl_blas95_ilp64.lib)
 
        # MKL Interface layer
        if(MKL_USE_STATIC_LIBS)
            set(MKL_INTERFACE_LIB mkl_intel_ilp64.lib)
        else()
            set(MKL_INTERFACE_LIB mkl_intel_ilp64_dll.lib)            
        endif()
    
    else()
    
        # LAPACK and BLAS Fortran 95 Wrappers Libraries
        set(MKL_BLAS95_LIB mkl_lapack95_lp64.lib)
        set(MKL_LAPACK95_LIB mkl_blas95_lp64.lib)
    
        # MKL Interface layer
        if(MKL_USE_STATIC_LIBS)
            set(MKL_INTERFACE_LIB mkl_intel_lp64.lib)
        else()
            set(MKL_INTERFACE_LIB mkl_intel_lp64_dll.lib)            
        endif()

    endif()

    # OpenMP Threading Libraries
    if(MKL_USE_STATIC_LIBS)
        set(MKL_THREAD_LIB mkl_intel_thread.lib)
    else()
        set(MKL_THREAD_LIB mkl_intel_thread_dll.lib)        
    endif()

    # Computational Layer
    if(MKL_USE_STATIC_LIBS)
        set(MKL_CORE_LIB mkl_core.lib)
    else()
        set(MKL_CORE_LIB mkl_core_dll.lib)        
    endif()    

    # Run time
    set(MKL_RTL_LIB libiomp5md.lib)

elseif(CMAKE_HOST_UNIX)

    # Unix/Linux-specific library names with extensions

    # Interface Layer
    if(MKL_ILP_INTERFACE)
    
        # LAPACK and BLAS Fortran 95 Wrappers Libraries
        set(MKL_BLAS95_LIB libmkl_blas95_ilp64.a)
        set(MKL_LAPACK95_LIB libmkl_lapack95_ilp64.a)

      # MKL Interface layer
        if(MKL_USE_STATIC_LIBS)
            set(MKL_INTERFACE_LIB libmkl_intel_ilp64.a)
        else()
            set(MKL_INTERFACE_LIB lmkl_intel_ilp64)            
        endif()

    else()
        
        # LAPACK and BLAS Fortran 95 Wrappers Libraries
        set(MKL_BLAS95_LIB libmkl_blas95_lp64.a)
        set(MKL_LAPACK95_LIB libmkl_lapack95_lp64.a)
    
        # MKL Interface layer
        if(MKL_USE_STATIC_LIBS)
            set(MKL_INTERFACE_LIB libmkl_intel_lp64.a)
        else()
            set(MKL_INTERFACE_LIB lmkl_intel_lp64)            
        endif()

    endif()
    
    # OpenMP Threading Libraries
    if(MKL_USE_STATIC_LIBS)
        set(MKL_THREAD_LIB libmkl_intel_thread.a)
    else()
        set(MKL_THREAD_LIB mkl_intel_thread)        
    endif()    

    # Computational Layer
    if(MKL_USE_STATIC_LIBS)
        set(MKL_CORE_LIB libmkl_core.a)
    else()
        set(MKL_CORE_LIB mkl_core)        
    endif()

    # Run time library
    set(MKL_RTL_LIB iomp5)
    
 endif()


# Location of Fortran95 wrappers

find_library(BLAS95_WRAPPER_FULLPATH NAMES ${MKL_BLAS95_LIB} 
            PATHS ${INTELMKLROOT}/lib ${INTELMKLROOT}/lib/intel64
            DOC "Location of Intel MKL Fortran 95 Wrappers for BLAS"
            NO_DEFAULT_PATH) 

find_library(LAPACK95_WRAPPER_FULLPATH NAMES ${MKL_LAPACK95_LIB} 
            PATHS ${INTELMKLROOT}/lib ${INTELMKLROOT}/lib/intel64
            DOC "Location of Intel MKL Fortran 95 Wrappers for LAPACK"
            NO_DEFAULT_PATH)

# Location of interface libraries

find_library(INTERFACE_LIBS_FULLPATH NAMES ${MKL_INTERFACE_LIB} 
            PATHS ${INTELMKLROOT}/lib
            PATH_SUFFIXES intel64
            DOC "Location of Intel MKL interface libraries"
            NO_DEFAULT_PATH)

# Location of threading libraries

find_library(THREADING_LIBS_FULLPATH NAMES ${MKL_THREAD_LIB} 
            PATHS ${INTELMKLROOT}/lib
            PATH_SUFFIXES intel64
            DOC "Location of Intel MKL threading libraries"
            NO_DEFAULT_PATH)

# Location of core libraries

find_library(CORE_LIBS_FULLPATH NAMES ${MKL_CORE_LIB} 
            PATHS ${INTELMKLROOT}/lib
            PATH_SUFFIXES intel64
            DOC "Location of Intel MKL core libraries"
            NO_DEFAULT_PATH)

# Location of threading runtime libraries

find_library(RTL_LIBS_FULLPATH NAMES ${MKL_RTL_LIB} 
            PATHS ${INTELROOT}/compiler ${INTELROOT}/compiler/lib
            PATH_SUFFIXES intel64
            DOC "Location of Intel MKL runtime libraries"
            NO_DEFAULT_PATH)         
            
# Include Directories

if(MKL_ILP_INTERFACE)
    set(INTELMKL_INCLUDE_DIRS ${INTELMKLROOT}/include ${INTELMKLROOT}/include/intel64/ilp64)
else()
    set(INTELMKL_INCLUDE_DIRS ${INTELMKLROOT}/include ${INTELMKLROOT}/include/intel64/lp64)
endif()

# MKL Link Line except the core layer
set(INTELMKL_LIBRARIES  ${BLAS95_WRAPPER_FULLPATH} 
                        ${LAPACK95_WRAPPER_FULLPATH} 
                        ${INTERFACE_LIBS_FULLPATH}
                        ${THREADING_LIBS_FULLPATH}
                        ${RTL_LIBS_FULLPATH})

# Some extra link commands for linux systems: see Intel link line advisor                    
if(CMAKE_HOST_UNIX)
    list(APPEND INTELMKL_LIBRARIES "pthread" "m" "dl")
endif()

# Delete cache variables no longer required
unset(BLAS95_WRAPPER_FULLPATH CACHE)
unset(LAPACK95_WRAPPER_FULLPATH CACHE)
unset(INTERFACE_LIBS_FULLPATH CACHE)
unset(THREADING_LIBS_FULLPATH CACHE)
unset(RTL_LIBS_FULLPATH CACHE)

# Handle find_package standard arguments
include(FindPackageHandleStandardArgs)

find_package_handle_standard_args(IntelMKL 
                                  FOUND_VAR IntelMKL_FOUND 
                                  REQUIRED_VARS INTELMKL_LIBRARIES
                                                INTELMKL_INCLUDE_DIRS) 

# Define the imported targets for Intel MKL library

# Target 1: Interface library
if(IntelMKL_FOUND AND (NOT TARGET IntelMKL::IntelMKL))
    add_library(IntelMKL::IntelMKL UNKNOWN IMPORTED)
    set_target_properties(IntelMKL::IntelMKL PROPERTIES
                            IMPORTED_LOCATION "${CORE_LIBS_FULLPATH}"
                            INTERFACE_INCLUDE_DIRECTORIES "${INTELMKL_INCLUDE_DIRS}"
                            INTERFACE_LINK_LIBRARIES "${INTELMKL_LIBRARIES}"
                            INTERFACE_COMPILE_OPTIONS "${MKL_CompileOption}")
endif()

# Dont display these cache variables in the GUI unless show advanced option is TRUE
mark_as_advanced(CORE_LIBS_FULLPATH INTELMKL_INCLUDE_DIRS  INTELMKL_LIBRARIES)


