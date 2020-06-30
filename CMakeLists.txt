#     ________  _____  ______________ 
#    / ____/  |/  / / / /_  __/  _/ / 
#   / /_  / /|_/ / / / / / /  / // /  
#  / __/ / /  / / /_/ / / / _/ // /___
# /_/   /_/  /_/\____/ /_/ /___/_____/                                                                             
                                                                               
 
# Version: 0.1
# Author: Bharat Mahajan

cmake_minimum_required(VERSION 3.12)


# Project
project(FMUTIL VERSION 0.1 LANGUAGES Fortran)
enable_language(Fortran)

# set major and minor version
set(FMUTIL_MAJOR_VERSION 0)
set(FMUTIL_MINOR_VERSION 1)
set(FMUTIL_VERSION  ${FMUTIL_MAJOR_VERSION}.${FMUTIL_MINOR_VERSION})

# Prompt for user options

# Default integer size 64-bits?
option(FMUTIL_INTSIZE_64 "Set to ON for 64-bit default interger type. 
Otherwise, the default is 32-bit."  OFF)

# Build a DLL?
option(FMUTIL_SHARED_INTELMKL "Set to ON for linking Intel MKL as a
shared library or DLL. The default is statically linkage." OFF)

# Cmake module folder path
set(CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake")

# Ask for the directory to install FMUTIL lib
set(FMUTIL_INSTALL_LIB_DIR lib/FMUTIL CACHE PATH "Installation directory for FMUTIL library")

# Ask for the directory to install TestFMUTIL executable
set(FMUTIL_INSTALL_BIN_DIR bin CACHE PATH "Installation directory for FMUTIL test executable")

# Make relative paths absolute
foreach(p LIB BIN)
  set(var FMUTIL_INSTALL_${p}_DIR)
  if(NOT IS_ABSOLUTE "${${var}}")
    set(${var} "${CMAKE_INSTALL_PREFIX}/${${var}}")
  endif()
endforeach()

# Lib dir should contain all the .mod files 
set(CMAKE_Fortran_MODULE_DIRECTORY ${FMUTIL_INSTALL_LIB_DIR})

############################
# Source code subdirectories
############################

# FMUTIL library

set(SRC_SUBDIRS src/math src/datastructs)

# Add all the source files in the current directory
set(FMUTIL_SRC src/fmutil_base.F90 src/fmutil.F90)

# Add sources in the subdirectories one by one
message(STATUS "Adding FMUTIL lib source files from...")

foreach(sdir ${SRC_SUBDIRS})
    message(STATUS ${sdir})
    # find all the sources
    file(GLOB SRC_FILES ./${sdir}/*.F90)
    # append the sources in the master list
    list(APPEND FMUTIL_SRC ${SRC_FILES})    
endforeach(sdir IN SRC_SUBDIRS)

# Define source grouping
source_group(TREE "${CMAKE_SOURCE_DIR}/src" FILES ${FMUTIL_SRC})

# FMUTIL test executable
file(GLOB TestFMUTIL_SRC src/tests/*.F90)

################
# FMUTIL Targets
################

# FMUTIL library
set(FMUTILBIN FMUTIL)

# Define the static library target
add_library(${FMUTILBIN} STATIC ${FMUTIL_SRC})
target_include_directories(${FMUTILBIN} INTERFACE ${FMUTIL_INSTALL_LIB_DIR})

# Find and link Intel MKL Library
message(STATUS "Linking Intel MKL...")
find_package(IntelMKL REQUIRED)
if(IntelMKL_FOUND)
    target_link_libraries(${FMUTILBIN} IntelMKL::IntelMKL)    
endif()


# The test executable 
set(TestFMUTILBIN TestFMUTIL)
add_executable(${TestFMUTILBIN} ${TestFMUTIL_SRC})

# link FMUTIL
target_link_libraries(${TestFMUTILBIN} ${FMUTILBIN})


############################
# Compile Options for FMUTIL
############################

include(CompilerOptions)

# These options are for gfortran
set(COption "-std=f2008ts")
GetPlatformCompilerFlag("${COption}" FALSE Fortran)
add_compile_options("${_Flag}")
set(COption "-ffree-line-length-0")
GetPlatformCompilerFlag("${COption}" FALSE Fortran)
add_compile_options("${_Flag}")

# Add ifort specific options

# # Data alignment always
# set(COption "/align:all" "-align all")
# GetPlatformCompilerFlag("${COption}" FALSE Fortran)
# target_compile_options(${FMUTILBIN} PUBLIC "${_Flag}")

# # Traceback information
# set(COption "/traceback" "-traceback")
# GetPlatformCompilerFlag("${COption}" FALSE Fortran)
# target_compile_options(${FMUTILBIN} PRIVATE ${_Flag})

# # Generate auto-dispatch code paths for better optimization if available 
# set(COption "/QaxCORE-AVX2" "-axCORE-AVX2")
# GetPlatformCompilerFlag("${COption}" FALSE Fortran)
# target_compile_options(${FMUTILBIN} PRIVATE ${_Flag})

#######################
# Debug Compile Options
#######################

# No optimization
set(COption "/Od" "-O0")
GetPlatformCompilerFlag("${COption}" TRUE Fortran)
target_compile_options(${FMUTILBIN} PRIVATE $<$<CONFIG:Debug>:${_Flag}>)

# Generate full debug information, passed to the linker
set(COption "/debug:full" "-debug full")
GetPlatformCompilerFlag("${COption}" FALSE Fortran)
target_compile_options(${FMUTILBIN} PRIVATE $<$<CONFIG:Debug>:${_Flag}>)

# Generate full debug information in object or PDB file, passed to compiler
set(COption "/Zi" "-g2")
GetPlatformCompilerFlag("${COption}" FALSE Fortran)
target_compile_options(${FMUTILBIN} PRIVATE $<$<CONFIG:Debug>:${_Flag}>)

# Debug information for Parameters
set(COption "/debug-parameters" "-debug-parameters")
GetPlatformCompilerFlag("${COption}" FALSE Fortran)
target_compile_options(${FMUTILBIN} PRIVATE $<$<CONFIG:Debug>:${_Flag}>)

# Array bounds checking
set(COption "/check:all" "-check all" "-fcheck=all")
GetPlatformCompilerFlag("${COption}" TRUE Fortran)
target_compile_options(${FMUTILBIN} PRIVATE $<$<CONFIG:Debug>:${_Flag}>)


#########################
# Release Compile Options
#########################

# Aggressive optimization for maximum speed: includes bunch of options
# Make sure we also set the appropriat Intel instruction set to use after this
set(COption "/fast /Qprec-div /fp:fast=1" "-fast -prec-div -fp-model fast=1")
GetPlatformCompilerFlag("${COption}" FALSE Fortran)
target_compile_options(${FMUTILBIN} PRIVATE $<$<CONFIG:Release>:${_Flag}>)

# Most optimization enabled
set(COption "/O3" "-O3")
GetPlatformCompilerFlag("${COption}" TRUE Fortran)
target_compile_options(${FMUTILBIN} PRIVATE $<$<CONFIG:Release>:${_Flag}>)

# Enable auto-parallelizer
set(COption "/Qparallel" "-parallel")
GetPlatformCompilerFlag("${COption}" FALSE Fortran)
target_compile_options(${FMUTILBIN} PRIVATE $<$<CONFIG:Release>:${_Flag}>)



############################
# Install and Export Targets
############################


install(TARGETS ${FMUTILBIN}
  # IMPORTANT: Add the FMUTIL library to the "export-set"
  EXPORT FMUTILTargets
  LIBRARY DESTINATION "${FMUTIL_INSTALL_LIB_DIR}" COMPONENT lib
  ARCHIVE DESTINATION "${FMUTIL_INSTALL_LIB_DIR}" COMPONENT lib
  COMPONENT dev)

install(TARGETS ${TestFMUTILBIN}
  # IMPORTANT: Add the TestFMUTIL executable to the "export-set"
  EXPORT FMUTILargets
  RUNTIME DESTINATION "${FMUTIL_INSTALL_BIN_DIR}" COMPONENT bin)

# Build-tree export set
export(EXPORT FMUTILTargets 
    NAMESPACE FMUTIL::
    FILE "${FMUTIL_INSTALL_LIB_DIR}/cmake/FMUTILTargets.cmake")

##########################
# Config and Version Files
##########################

include(CMakePackageConfigHelpers)

# version file
write_basic_package_version_file(
    ${CMAKE_CURRENT_BINARY_DIR}/FMUTILConfigVersion.cmake
    VERSION ${FMUTIL_VERSION}
    COMPATIBILITY AnyNewerVersion
)

configure_package_config_file(${CMAKE_CURRENT_LIST_DIR}/cmake/FMUTILConfig.cmake.in 
        ${CMAKE_CURRENT_BINARY_DIR}/FMUTILConfig.cmake
        INSTALL_DESTINATION cmake
        INSTALL_PREFIX ${FMUTIL_INSTALL_LIB_DIR})

install(FILES ${CMAKE_CURRENT_BINARY_DIR}/FMUTILConfig.cmake
        ${CMAKE_CURRENT_BINARY_DIR}/FMUTILConfigVersion.cmake
        DESTINATION ${FMUTIL_INSTALL_LIB_DIR}/cmake)