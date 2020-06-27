!##############################################################################
!
! FMUTIL: Fortran Miscellaneous UTILities
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
!> \brief       TestMod Module
!! \details     Test Module to be used by the Main Test program
!! \author      Bharat Mahajan
!! \date        Created: 06/12/2020
!
!##############################################################################


module TestModule
    use FMUtil

type, extends(VecElem) :: relem
    real :: rdata
contains
    procedure :: AssignVecElem => assign_relem
end type relem

type, extends(VecElem) :: ielem
    integer, dimension(10) :: idata
contains
    procedure :: AssignVecElem => assign_ielem
end type ielem
    
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

subroutine assign_ielem(lhs, rhs)
    implicit none
    class(ielem), intent(out) :: lhs
    class(VecElem), intent(in) :: rhs

    select type (rhs)
    class is (ielem)
        lhs%idata = rhs%idata
    end select
end subroutine assign_ielem


end module TestModule
    
    