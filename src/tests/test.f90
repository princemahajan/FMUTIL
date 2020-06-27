!##############################################################################
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
!> \brief       TestFMUtil Main Program
!! \details     Main program to run multiple test cases for FMUTIL
!! \author      Bharat Mahajan
!! \date        Created: 01/25/2019     
!
!##############################################################################
    
    
    program TestFMUtil

    
    use iso_fortran_env, only: output_unit
    use FMUtil
    use TestModule

    implicit none
    
    ! Simulation Parameters
    integer, parameter :: VecOps = 10000

    complex(WP), dimension(:), allocatable :: coeffs, roots
    integer :: error, ctr, status, itr
    logical :: BalanceOn
    
    real(WP) :: StartTime, EndTime
    
    type(Vector) :: rvec, rvec1
    type(relem) :: rd1, rd2
    class(VecElem), dimension(:), allocatable :: vecarr
    
    type(List) :: list1, list2
    class(*), allocatable :: item1
    
    
    
    ! Polynomial Roots Test

    coeffs = [complex(WP):: 1e-200_WP, (0,-1e200_WP), 1, 100]
    
    BalanceOn = .True.
    
     call PolyRoots(coeffs, roots, error, BalanceOn = BalanceOn)
    
     print *, roots
    
     print *, 'Roots accuracy: ', &
             sum(reshape([(coeffs(ctr)*(roots**(ubound(coeffs,1)-ctr)), &
                         ctr=1,ubound(coeffs,1))],[ubound(roots,1),ubound(coeffs,1)]),2)
    
    
    ! Vector Tests

    ! Vector Init: set bucket size, capacity increments, and mold elems
    print '(1A60,3I6)', 'Vector: capacity, size, Used Buckets: ', rvec%Capacity(), rvec%Size(), rvec%NUsedBkts()
    call rvec%Init(1, 5, rd1)
    print '(1A60,3I6)', 'Vector init: capacity, size, Used Buckets: ', rvec%Capacity(), rvec%Size(), rvec%NUsedBkts()    
    call rvec%Init(1, 5, rd1)
    
    ! Vector reserve and shrink
    rd1%rdata = 1.01
    call rvec%Reserve(49)    
    print '(1A60,3I6)', 'Vector reserved 49: capacity, size, Used Buckets: ', rvec%Capacity(), rvec%Size(), rvec%NUsedBkts()        
    ctr = rvec%PushBack(rd1)    
    ctr = rvec%PushBack(rd1)
    rd2 = rvec%PopBack() 
    call rvec%ShrinkToFit()    
    print '(1A60,3I6)', 'added 1 & shrunk: capacity, size, Used Buckets: ', rvec%Capacity(), rvec%Size(), rvec%NUsedBkts()            
    
    ! Vector insert and erase elements
    rd1%rdata = 2.2
    call rvec%Insert(2,[rd1],3)
    rd1%rdata = 3.3
    call rvec%Insert(5,[rd1],3)    
    print '(1A60,3I6)', 'Vector insert 6: capacity, size, Used Buckets: ', rvec%Capacity(), rvec%Size(), rvec%NUsedBkts()            
    rd1%rdata = 10.10
    ctr = rvec%PushBack(rd1)
    call rvec%Erase(3,4)
    print '(1A60,3I6)', 'Vector push 1 & erase 2: capacity, size, Used Buckets: ', rvec%Capacity(), rvec%Size(), rvec%NUsedBkts()        
 
    ! Vector element retrieval
    do ctr = 1, rvec%Size()
        rd2 = rvec%ElemAt(ctr)
        print *, 'int(', ctr, ')=', rd2%rdata        
    end do    
    
    ! Vector slicing
    call CPU_TIME(StartTime)
    do ctr = 1, VecOps
    vecarr = rvec%Slice(1,6,1)
    end do
    call CPU_TIME(EndTime)
    
    select type(vecarr)
    type is (InvalidVecElem)
        print *, 'Invalid Elem'
    type is (relem)
        print *, vecarr
    end select
    print *, 'Slice time: ', (EndTime-StartTime)
    
    call CPU_TIME(StartTime)
    do ctr = 1, VecOps
    itr = 6
    vecarr = rvec%BktSlice(1,itr)
    end do
    call CPU_TIME(EndTime)
    
    select type(vecarr)
    type is (InvalidVecElem)
        print *, 'Invalid Elem'
    type is (relem)
        print *, vecarr(1:size(vecarr))
    end select
    print *, 'Bkt Slice time: ', (EndTime-StartTime)
    
    ! Vector Assigment test
    call rvec1%Init(3, 2, rd1)
    rd1%rdata = 1.01
    ctr = rvec1%PushBack(rd1)    
    print *, 'before assignment'
    do ctr = 1, rvec1%Size()
        rd2 = rvec1%ElemAt(ctr)
        print *, 'int(', ctr, ')=', rd2%rdata        
    end do    
    rvec1 = rvec
    print *, 'after assignment'
    do ctr = 1, rvec1%Size()
        rd2 = rvec1%ElemAt(ctr)
        print *, 'int(', ctr, ')=', rd2%rdata        
    end do    

    ! clear
    call rvec1%Clear()
    print *, 'after clear'
    do ctr = 1, rvec1%Size()
        rd2 = rvec1%ElemAt(ctr)
        print *, 'int(', ctr, ')=', rd2%rdata        
    end do    
    
    
    ! List Data struture test

    ctr = list1%PushBack(1.8)
    ctr = list1%PushBack(4)
    ctr = list1%PushBack(rd1)
    ctr = list1%PushBack('list item')
    item1 = list1%PopBack()
    select type (item1)
    type is (character(len=*))
        print *, item1
    end select
    call list1%Insert(4,40)
    call list1%Erase(3)
    call list1%ShrinktoFit()
    list2 = list1
    do ctr = 1, list2%Size()
        item1 = list2%Item(ctr)
        select type (item1)
        type is (real)
            print *, item1
        type is (integer)
            print *, item1
        type is (relem)
            print *, item1
        end select        
    end do
    
    
    contains
    
    
    
    
    end program TestFMUtil

