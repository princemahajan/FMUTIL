###############################################################################
#     ________  _____  ______________ 
#    / ____/  |/  / / / /_  __/  _/ / 
#   / /_  / /|_/ / / / / / /  / // /  
#  / __/ / /  / / /_/ / / / _/ // /___
# /_/   /_/  /_/\____/ /_/ /___/_____/                                                                             
#
# Copyright 2020 Bharat Mahajan
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# Compiler Flags CMake Module
# Author: Bharat Mahajan
#
###############################################################################


# Compiler flag check modules
include(CheckCCompilerFlag)
include(CheckCXXCompilerFlag)
include(CheckFortranCompilerFlag)

#[[#################################################################################

GetPlatformCompilerFlag

This function takes a list of compiler flags as an input, checks each of them against
the language compiler specified by the user and sets _Flag with the
correct flag that works, only if the current build configuration matches the provided
value. If the required flag is set to true, then it raises an 
error if the check fails otherwise it unsets the _Flag. 

Inputs:
Flag:           List of flags
IsRequired:     True if the flag must be set
LANG:           Language compiler to be checked against
Config:         The build Config in which the flag is used. Only 2 values
                are supported: Debug and Release.

Outputs:
_Flag:          The correct flag if found

One usage example:

set(COption /Zi -g)
GetPlatformCompilerFlag("${COption}" True Fortran)
target_compile_options(<target> PUBLIC "${_Flag}")

#################################################################################]]

function (GetPlatformCompilerFlag Flags IsRequired LANG)

# check each option one by one and return the first one that works
foreach(flag ${Flags})

    # unset the variable in cache from the previous runs
    unset(FLAG_WORKS CACHE)

    # prints message
    message(STATUS "GetPlatformCompilerFlag: checking flag: " ${flag})

    # check the option against each language compiler
    if(LANG STREQUAL C)
        check_c_compiler_flag("${flag}" FLAG_WORKS)
    elseif(LANG STREQUAL CXX)
        check_cxx_compiler_flag("${flag}" FLAG_WORKS)
    elseif(LANG STREQUAL Fortran)
        check_fortran_compiler_flag("${flag}" FLAG_WORKS)
    else()
        message(FATAL_ERROR "GetPlatformCompilerFlag: Unknown language " ${LANG})
        break()
    endif()
    
    # if option works, set output and stop testing more flags
    if(FLAG_WORKS)
        set(_Flag ${flag} PARENT_SCOPE)
        set(FLAG_FOUND TRUE)        
        # unset the variables
        unset(FLAG_WORKS CACHE)
        break()
    else()
        set(FLAG_FOUND FALSE)
        message("GetPlatformCompilerFlag: unknown flag: " ${flag})
        unset(_Flag PARENT_SCOPE)
    endif()
    
endforeach()

# if option is required and not found in list, raise an error
if(IsRequired AND (NOT FLAG_FOUND))
    message(FATAL_ERROR "GetPlatformCompilerFlag: Wrong required ${LANG} compiler flag " ${flag})
elseif((NOT IsRequired) AND (NOT FLAG_FOUND))
    # flag doesnt work but its ok!
    message(STATUS "GetPlatformCompilerFlag: Wrong ${LANG} compiler flag " ${flag})
endif()

endfunction()

