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
!> \brief       RootFinding Module
!! \details     This module provides functions for root-finding of polynomials
!!              by solving associated eigenvalue problems and nonlinear 
!!              equations using algorithms like Brent's Algorithm.
!! \author      Bharat Mahajan
!! \date        Created: 07/31/2019    
!
!##############################################################################
 
    
module RootFinding

    use FMUTILBase
    
    !> Intel MKL F95 interfaces for LAPACK
    use lapack95, only: gebal, hseqr

    implicit none

    !> Max iterations for all the root-finding
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
    
    
    !> \brief Computes the complex roots of the given polynomial
    !! \details PolyRoots finds the roots of the polynomial defined by complex
    !! coefficients given in array "c" (c(1) is the highest degree coeff.). It 
    !! forms the companion matrix and then solves its eigenvalues using LAPACk 
    !! routines from Intel MKL library. The algorithm is same as that used by
    !! MATLAB's  "roots" command. References:
    !! 1. https://blogs.mathworks.com/cleve/2016/06/27/19-dubious-ways-to-compute-the-zeros-of-a-polynomial/
    !! 2. Math77: http://www.netlib.org/math/docpdf/ch07-01.pdf 
    subroutine PolyRoots(c, r, error, BalanceOn)
    
        implicit none
        
        !> Complex coefficients of the polynomial defined by
        !! p(x) = c(1)*x^n + ... + c(n)*x + c(n+1)
        complex(WP), dimension(1:), intent(in)             :: c
        
        !> Complex roots array
        complex(WP), dimension(:), allocatable, intent(out)    :: r
        
        !> Error status returned to the user. 
        !! "0" : Success, valid results in "r"
        !! "-1": Error, input coefficients array has length less than two
        !! "-2": Error, all the coefficients are zero
        !! "-3": Error, companion matrix balancing failed
        !! "-4": Error, LAPACK routine failed to compute eigenvalues
        integer, intent(out)                            :: error
        
        !> If True, then the companion matrix is balanced first before finding
        !! its eigenvalues, otherwise not. Default is True. It may produce
        !! inaccurate results in case of very poorly-scaled matrices.
        logical, intent(in), optional           :: BalanceOn
        
        integer :: n, nnz, ctr
        integer, dimension(1) :: nzlocl, nzloct
        complex(WP), dimension(:), allocatable :: cnz
        complex(WP), dimension(:,:), allocatable :: H
        logical :: UseBalance
        integer :: ilo, ihi, info
        real(WP), dimension(:), allocatable, target :: scale
        
        error = 0 ! all ok so far!
        
        ! original polynomial degree
        n = size(c) - 1
        
        !> \remark return if input coefficient array is empty
        if (n <= 0) then
            error = -1
            return
        end if
        
        !> \remark Return if all coefficients are zero
        if (all(abs(c) == 0) .EQV. .TRUE.) then
            error = -2
            return
        end if
    
        ! By default, balance the companion matrix
        UseBalance = .True.
        if (present(BalanceOn)) UseBalance = BalanceOn
        
        ! find the location of the first nonzero coefficient at the front and back
        block
            integer, dimension(size(c)) :: c1
            
            c1 = merge(1,0,real(c) /= 0 .OR. aimag(c) /= 0)
            nzlocl = findloc(c1, 1)
            nzloct = findloc(c1, 1, back=.TRUE.)
        end block
    
        !> \remark Leading zeros are thrown away while the trailing zero coefficients
        !! are remembered by introducing roots at zero.
        nnz = nzloct(1) - nzlocl(1)
        
        ! copy the nonzero coefficients
        cnz = c(nzlocl(1):nzloct(1))
        
        ! Relatively small leading coefficients are removed to avoid infinite
        ! values similar to MATLAB's "root" command, ctr will contain the
        ! location of the first valid significant coefficient
        ctr = 1
        do while (any(abs(cnz(ctr+1:)/cnz(ctr)) > INF))
            ctr = ctr + 1
            if (ctr == ubound(cnz,1)) exit
        end do
        nnz = nnz - (ctr - 1)
        
        ! Final polynomial degree is less than 1, so it has only zero roots
        ! corresponding to the trailing zero coefficients
        if (nnz <= 0) then
            r = [(0, ctr = 1,(n+1-nzloct(1)))]
            return
        elseif (nnz == 1) then ! Special case: 1st degree polynomial
            r = [complex(WP):: -cnz(1+ctr)/cnz(ctr), (0, ctr = 1,(n+1-nzloct(1)))]
            return
        end if
    
        ! Form the companion matrix that is an upper Hessenberg matrix
        allocate(H(nnz,nnz))
        H = (0,0)
        H(1,:) = -cnz(ctr+1:)/cnz(ctr)
        do ctr = 2,ubound(H,1)
            H(ctr, ctr-1) = (1,0)
        end do
    
        ! Balance the companion matrix for improved accuracy
        if (UseBalance) then
            allocate(scale(nnz))
            call gebal(H, ilo=ilo, ihi=ihi, job='B',info=info)
            if (info /= 0) then
                ! balancing failed, return error
                error = -3
                return
            end if
        else
            ilo = 1
            ihi = nnz
        end if
        
        ! We have a balanced upper Hessenberg companion matrix, therefore
        ! we can use this LAPACK computational routine directly
        allocate(r(nnz))
        call hseqr(H, r, ilo=ilo, ihi=ihi, job='E', info=info)
        !call geevx(H, r, balanc='B', info=info)
    
        if (info /= 0) error = -4        
        
        ! Gather all the roots
        r = [complex(wp) :: r, (0, ctr = 1,(n+1-nzloct(1)))]
    
    end subroutine PolyRoots
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    !> \brief Subroutine for computing the root using Brent algorithm.
    !! \details For details on the algorithm, see Numerical receipes in C book
    !! at https://www2.units.it/ipl/students_area/imm2/files/Numerical_Recipes.pdf
    pure subroutine Root(a, b, ya, yb, tol, f, x, fx, niter, error)
    
        implicit none
        
        real(WP), intent(in)                 :: a     !< Lower bound on the root
        real(WP), intent(in)                 :: b     !< Upper bound on the root
        real(WP), intent(in)                 :: ya    !< f(a)
        real(WP), intent(in)                 :: yb    !< f(b)
        real(WP), intent(in)                 :: tol   !< tolerance
        procedure(func)                      :: f     !< function
        real(WP), intent(out)                :: x     !< root
        real(WP), intent(out)                :: fx    !< value at root
        integer, intent(out)                 :: niter !< no. of iterations
        
        !> Error status returned to the user. 
        !! "0" means root has been found.
        !! "1" means provided bounds do not contain the root, i.e., 
        !! root is not bracketed.
        !! "2" means maximum number of interations reached.
        integer, intent(out)                 :: error 

        real(WP) :: aa, bb, cc, fa, fb, fc, xm, ss, dd, ee, pp, qq, rr
        
        real(WP) :: tolx
        
        real(WP), parameter :: NEARZERO = 1.0e-20_WP
        
        logical :: done
        integer :: itr
        
        aa = a
        bb = b
        fa = ya
        fb = yb

        error = 0
        niter = 0

        ! test
        fa = f(a)
        fb = f(b)
        
        ! Trivial cases
        if (abs(fa) <= NEARZERO) then
            x = aa
            fx = fa
            return
        else if (abs(fb) <= NEARZERO) then
            x = bb
            fx = fb
            return
        end if
        
        ! if root is not bracketed then return error
        if ((fa>0 .AND. fb>0) .OR. (fa<0 .AND. fb<0)) then
            error = 1
            niter = 0
            return
        end if
        
        ! algorithm start here
        fc = fb
        done = .FALSE.
        itr = 0
        do while (done .EQV. .FALSE. .AND. itr < MAX_ITERATIONS)
            
            ! if root is NOT bracketed between C and B, then C = A
            if ((fc>0 .AND. fb>0) .OR. (fc<0 .AND. fb<0)) then
                cc = aa
                fc = fa
                dd = bb - aa
                ee = dd
            end if
            ! Make sure |f(c)| is bigger than |f(b)|
            if (abs(fc) < abs(fb)) then
                aa = bb
                bb = cc
                cc = aa
                fa = fb
                fb = fc
                fc = fa
            end if
            ! tolerance on the independent variable
            tolx = 2.0*EPS*abs(bb) + 0.5*tol
            ! middle point
            xm = 0.5*(cc - bb)
            
            if (abs(xm) <= tolx .OR. abs(fa) < NEARZERO) then
                ! root has been found
                x = bb
                done = .TRUE.
                fx = f(x)
            else
                if (abs(ee) >= tolx .AND. abs(fa) > abs(fb)) then
                    ss = fb/fa ! make sure |ss| < 1
                    if (abs(aa - cc) < NEARZERO) then
                        ! linear interpolation
                        pp = 2.0*xm*ss
                        qq = 1.0 - ss
                    else
                        ! Inverse quadratic interpolation
                        qq = fa/fc
                        rr = fb/fc
                        pp = ss*(2.0*xm*qq*(qq-rr)-(bb-aa)*(rr-1.0))
                        qq = (qq - 1.0)*(rr - 1.0)*(ss - 1.0)
                    end if
                    
                    if (pp > NEARZERO) qq = -qq
                    pp = abs(pp)
                    if ((2.0*pp) < min(3.0*xm*qq-abs(tolx*qq), abs(ee*qq))) then
                        ! use linear or inverse quadratic interpolation
                        ee = dd
                        dd = pp/qq
                    else
                        ! use bisection
                        dd = xm
                        ee = dd
                    end if
                else
                    ! use bisection
                    dd = xm
                    ee = dd
                end if

                aa = bb
                fa = fb
                if (abs(dd) > tolx) then
                    bb = bb + dd
                else
                    ! Correction term too small, advance by at least tolx
                    if (xm > 0) then
                        bb = bb + abs(tolx)
                    else
                        bb = bb - abs(tolx)
                    end if
                end if
                fb = f(bb)
                itr = itr + 1
            end if
        end do
        
        if (itr >= MAX_ITERATIONS) error = 2
        niter = itr

    end subroutine Root

    
    
end module RootFinding
