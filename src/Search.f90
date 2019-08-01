!############################################################################################
!
! Copyright 2019 Bharat Mahajan
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
!> \brief       Search Module
!! \details     This module provides functions for search utilities
!! \author      Bharat Mahajan
!! \date        07/31/2019    
!
!############################################################################################
 
    
!> Module containing all the utility functions
module Search

    ! import the fortran environment module for precision-related constants
    use, intrinsic :: iso_fortran_env, only: real64, real128

    !> Smallest positive real satisfying 1.0_WP + eps > 1.0_WP
    real(WP), parameter, public :: EPS = epsilon(1.0_WP)
    
    !> Max iterations for the root-finding
    integer, parameter :: MAX_ITERATIONS = 50
    
    !> Interface of the function whose root needs to be computed
    abstract interface
        pure function func(x) result(y)
            import :: WP
            implicit none
            real(WP), intent(in) :: x !< independent variable
            real(WP) :: y
        end function func
    end interface
    
    contains
    
    
    !> \brief Performs binary search on a provided sorted array
    !! \details TBD
    pure function BinarySearch(SortedX, SortedDataArray)

        !import :: WP
    
        implicit none    
    
        real(WP), dimension(1:), intent(in)  :: SortedX    !< Element to find
        real(WP), dimension(:), intent(in)  :: SortedDataArray   !< Ascending-order sorted vector
        integer, dimension(size(SortedX)) :: BinarySearch

        integer :: lb, ub, a, b, m, n, ctr, xloc
        real(WP) :: x
        
        ! initialize
        lb = 1
        ub = size(SortedDataArray)
        m = lb
        
        ! for each element
        do ctr = 1, size(SortedX)
        
            ! element to search for
            x = SortedX(ctr)
            
            ! current interval. Note we assume that the array of the elements
            ! to search for is also a sorted array.
            a = m
            b = ub
            
            ! loop for finding the element
            do
                ! size of the current interval
                n = b - a + 1
        
                ! mid-point
                m = floor((n+1)/2.0) + (a-1)
        
                if (x < SortedDataArray(m)) then    
                    ! search the lower half
                    b = m
                    cycle
                elseif (x > SortedDataArray(m) .AND. (m+1) /= b) then
                    ! search the upper half
                    a = m
                    cycle
                else
                    ! We either have found the exact match 
                    ! Or x lies in between Data(m) and Data(m+1)
                    xloc = m
                    exit
                end if        
            end do

            ! save the location of the current element
            BinarySearch(ctr) = xloc
        end do
    
    end function BinarySearch    
    
    
    
end module Search
