!############################################################################################
!     ________  _____  ______________ 
!    / ____/  |/  / / / /_  __/  _/ / 
!   / /_  / /|_/ / / / / / /  / // /  
!  / __/ / /  / / /_/ / / / _/ // /___
! /_/   /_/  /_/\____/ /_/ /___/_____/                                     
!
! Copyright 2020 Bharat Mahajan
!
! Licensed under the Apache License, Version 2.0 (the "License");
! you may not use this file except in compliance with the License.
! You may obtain a copy of the License at
!
!    http://www.apache.org/licenses/LICENSE-2.0
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.
!
!> \brief       FMUTIL Base Module
!! \details     This module provides basic objects and functionality common to all the
!!              FMUTIL internal modules.
!! \author      Bharat Mahajan
!! \date        Created: 02/11/2020    
!
!############################################################################################
 
    
module FMUTILBase

    !> import the fortran environment module for precision-related constants
    use, intrinsic :: iso_fortran_env, only: real64, real128

    implicit none
    
    !> By default, use 64-bit double precision (IEEE-754)
    integer, parameter, public :: DP = real64   !> Double precision kind parameter
    integer, parameter, public :: QP = real128  !> Quadruple precision kind parameter
    integer, parameter, public :: WP = DP       !> Default word precision used

    !> Smallest positive real satisfying 1.0_WP + eps > 1.0_WP
    real(WP), parameter, public :: EPS = epsilon(1.0_WP)
    
    !> Infinity definition
    real(WP), parameter, public :: INF = huge(1.0_WP)
    
    !> Identity matrix - 3x3
    integer, private ::  ind
    logical, dimension(*), parameter :: nzind3 = [ (ind == (3*(ind/3)+(ind/3+1)), ind=1,8), .TRUE.]
    real(WP), dimension(*,*), parameter, public :: I3 = &
                         reshape([merge([(1.0_WP, ind=1,9)], [(0.0_WP, ind=1,9)], nzind3)],[3,3])
    
    
    !> Identity matrix - 6x6
    logical, dimension(*), parameter :: nzind6 = [ (ind == (6*(ind/6)+(ind/6+1)), ind=1,6**2-1), .TRUE.]
    real(WP), dimension(*,*), parameter, public :: I6 = &
                         reshape([merge([(1.0_WP, ind=1,6**2)], [(0.0_WP, ind=1,6**2)], nzind6)],[6,6])





    contains
    

    
end module FMUTILBase
