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
!> \brief       Vector module
!! \details     This module provides the Vector data structure (similar to 
!!              C++ STL Vector) represented internally using an array of 
!!              buckets, which is again an array.
!! \author      Bharat Mahajan
!! \date        Created: 05/27/2020    
!
!##############################################################################
 
    
module Vectors

    use FMUTILBase
    
    implicit none
    
    private

    !> Default capacity of internal buckets
    integer, parameter :: DEFAULT_BKTCAP = 3

    !> Default number of buckets to create on vector capacity increase
    integer, parameter :: DEFAULT_NEWBKTSALLOCATED = 2

    !> Abstract vector element type that must be extended by user to
    !! define the type for the Vector elements
    type, abstract, public :: VecElem
    contains
        !> user must override this procedure to define the assignment operator
        !! for thir Vector element type
        procedure(assign_vecelem), deferred :: AssignVecElem
        !> Assignment operator for Vector Element type
        generic, public :: assignment(=) => AssignVecElem
    end type VecElem

    abstract interface
    !> Interface for the procedure invoked during assignment of VecElem
    subroutine assign_vecelem(lhs, rhs)
        import :: VecElem
        implicit none
        class(vecElem), intent(out) :: lhs
        class(VecElem), intent(in) :: rhs
    end subroutine assign_vecelem    
    end interface

    !> Invalid Vector element type that is returned in case of an exception
    type, public, extends(VecElem) :: InvalidVecElem
        private
        !> This internal flag will be set true when there is any error in
        !! element query functions
        logical :: Invalid = .TRUE.
    contains
        private
        procedure, public :: AssignVecElem => assign_invalidelem
    end type InvalidVecElem

    !> Internal Bucket sturcture. Each bucket can contain a number of elements.
    type, private :: Bkt
        private        
        !> Allocatable array to data in this bucket
        class(VecElem), dimension(:), allocatable :: BktData
        !> Flags indicating free slots in the bucket
        logical, dimension(:), allocatable :: SlotFree
    contains
        private
        !> Initializes the bucket struct
        procedure :: bktinit
        !> Clears the conrents of the bucket
        procedure :: bktclear
        !> Adds a new element to the bucket
        procedure :: add_bktelem
        !> Deletes an element from the bucket
        procedure :: del_bktelem
        !> Returns a pointer to the element
        procedure :: bktelem
        !> Returns a pointer to the slice of the bucket data
        procedure :: bktsliceptr
        !> Checks if the bucket is empty
        procedure :: is_bktempty
        !> Returns the current size of the bucket
        procedure :: bktsz
    end type Bkt

    !> Pointer to a single bucket
    type :: BktPtr
        type(Bkt), pointer :: pBkt => null()
    end type BktPtr

    !> Vector type
    type, public :: Vector
        
        private
        
        !> Status: 0-success, any negative value-exception condition
        integer, public :: status = 0

        !> Flag to indicate Vector storage has been allocated
        logical :: StorageAllocated = .FALSE.

        !> Number of new buckets to create on Vector capacity increase
        integer :: NewBktsToAllocate = DEFAULT_NEWBKTSALLOCATED
        
        !> Each bucket capacity
        integer :: BktCap = DEFAULT_BKTCAP

        !> Number of buckets that currently contain data
        integer :: UsedBkts = 0

        !> Number of buckets that are currently allocated
        integer :: AllocatedBkts = 0

        !> Mold element to be used for memory allocation
        class(VecElem), allocatable :: MoldElem

        !> Array of bucket pointers
        type(BktPtr), dimension(:), allocatable :: VecData
 
    contains
    
        private

        procedure :: AssignVector

        !> Assignment operator for the Vector type, this enables
        !! copying of RHS vector contents to LHS vector. Note that
        !! after this operation, LHS Vector will be an exact copy
        !! of the RHS vector including data and internal state.
        generic, public :: assignment(=) => AssignVector
        
        ! Internal procedures
        procedure :: create_newbkts
        procedure :: Index2Bkt

        !> Initialize the vector with different options and allocates memory
        procedure, public :: Init => initialize

        !> Returns size of the vector or number of elements stored
        procedure, public :: Size => vec_size

        !> Returns the current capacity of the vector
        procedure, public :: Capacity

        !> Returns number of non-empty internal buckets
        procedure, public :: NUsedBkts

        !> Reserve the requested storage minimum capacity by allocating 
        !! new buckets if needed
        procedure, public :: Reserve

        !> Shrink the capacity by deallocating unused buckets if needed.
        !! It tries to make Capacity as close to Size as possible
        procedure, public :: ShrinkToFit

        !> Push one element at the back of the vector
        procedure, public :: PushBack => push_back

        !> Pop one element from the back of the vector
        procedure, public :: PopBack => pop_back        
        
        !> Returns a pointer to the element with the given index (1-based)
        procedure, public :: ElemAt => elem_at

        !> Returns the element with the index-1
        procedure, public :: Front => elem_front

        !> Returns the element with the last index
        procedure, public :: Back => elem_back

        !> Returns an array containing vector elements within the 
        !! requested lower and upper bounds. This is inefficient as it 
        !! copies the requested contents in a new array and returns it
        procedure, public :: Slice => vec_slice

        !> Returns the pointer to the element array starting from the 
        !! user-specified lower bound. If the upper bound is beyond the 
        !! extent of the current data bucket, then the array is truncated
        !! at the the current bucket boundary. This is an efficient method
        !! as no copying of data is performed.
        procedure, public :: BktSlice => bkt_slice

        !> Inserts the element/elements at the given index and relocate 
        !! elements at the later indices
        procedure, public :: Insert => elem_insert

        !> Delete the element/elements at the given index and relocate 
        !! the elements at the later indices
        procedure, public :: Erase => elem_erase

        !> Clear the contents of the Vector but memory is not deallocated
        procedure, public :: Clear

        !> Frees up all the memory allocated
        final :: Destroy

    end type Vector


    
    contains


    
    !> Optional procedure to initialize Vector type and allocate internal
    !! storage. It can be used to modify the default settings for storage 
    !! allocation. If Init is not called first, Vector is initialized
    !! using the default settings and storage is allocated on the first call to
    !! push data in the Vector. This procedure will only change the number 
    !! of new buckets to create on Vector capacity increase if it is invoked 
    !! after the first call to push data in the Vector.
    subroutine initialize(me, NewBktsToAllocate, BktCap, MoldElem)
        implicit none
        
        class(vector), intent(inout) :: me
        !> Number of new buckets to allocate on Vector's capacity expansion  
        integer, intent(in), optional :: NewBktsToAllocate
        !> Each bucket capacity. Note that providing a value for the bucket capacity
        !! will have effect only if Vector is not already initialized.
        integer, intent(in), optional :: BktCap
        !> Element mold to use for allocating bucket's memory
        class(VecElem), intent(in), optional :: MoldElem

        ! Set the number of new buckets to allocate when last bucket is full
        if (present(NewBktsToAllocate)) me%NewBktsToAllocate = NewBktsToAllocate

        ! If Vector is already initialized, we are done
        if (me%StorageAllocated) return

        if (present(BktCap)) me%BktCap = BktCap

        ! if mold_elem is provided, then allocate initial buckets
        if (present(MoldElem)) then
            allocate(me%MoldElem, source=MoldElem)
            call me%create_newbkts(me%NewBktsToAllocate)
        end if
        ! If successful, start using the first bucket
        if (me%status == 0) then
            me%StorageAllocated = .TRUE.
            me%UsedBkts = 1
        end if
    end subroutine initialize




    !> Returns the total number of allocated elements in the Vector
    pure function vec_size(me)
        implicit none
        class(Vector), intent(in) :: me
        integer :: vec_size

        vec_size = 0
        ! if not allocated size is 0
        if ((.NOT. me%StorageAllocated) .OR. me%UsedBkts == 0) return

        ! N-1 full bkts and Nth partial filled bkts
        vec_size = (me%UsedBkts-1)*me%BktCap &
                    + me%VecData(me%UsedBkts)%pBkt%bktsz()
    end function vec_size



    !> Returns the total number of elements that can be saved in the Vector
    !! without any need for increasing the memory footprint
    pure function Capacity(me)
        implicit none
        class(Vector), intent(in) :: me
        integer :: Capacity

        Capacity = me%AllocatedBkts*me%BktCap
    end function Capacity


    !> Returns the total number of internal data buckets allocated
    !! by the Vector. Each bucket can contain multiple elements
    !! defined by its capacity
    pure function NUsedBkts(me)
        implicit none
        class(Vector), intent(in) :: me
        integer :: NUsedBkts

        NUsedBkts = me%UsedBkts
    end function NUsedBkts




    !> Increases the capcity of Vector to 'n' if the current capacity is smaller than 'n'
    subroutine Reserve(me, n)
        implicit none
        class(Vector), intent(inout) :: me
        !> Reserve space of the minimum 'n' elements
        integer, intent(in) :: n

        integer :: NumBkts

        real :: fracbkts
        
        ! required number of new bkts
        fracbkts = n
        NumBkts = ceiling(fracbkts/me%BktCap) - me%AllocatedBkts

        if (NumBkts > 0) then
            call me%create_newbkts(NumBkts)
        end if
    end subroutine Reserve




    !> Deallocate unused buckets to make Capacity as close to Size as possible
    subroutine ShrinkToFit(me)
        implicit none
        class(Vector), intent(inout) :: me

        type(BktPtr), dimension(:), allocatable :: tmp
        integer :: ctr

        ! deallocate if used bkts are less than allocated bkts
        if (me%UsedBkts < me%AllocatedBkts) then
            do ctr = me%Size()+1, me%AllocatedBkts
                if (associated(me%VecData(ctr)%pBkt)) deallocate(me%VecData(ctr)%pBkt)
                me%VecData(ctr)%pBkt => null()
            end do
            me%AllocatedBkts = me%UsedBkts
        end if

        if (me%AllocatedBkts < 1) then
            ! if all storage deallocated, then set the flag            
            me%StorageAllocated = .FALSE.
            if (allocated(me%VecData)) deallocate(me%VecData, stat=me%status)
        else
            ! shrink the bucket pointer array
            allocate(tmp(1:me%AllocatedBkts), stat=me%status)
            if (me%status == 0) then
                tmp = me%VecData(1:me%AllocatedBkts)
                call move_alloc(from=tmp, to=me%VecData)
            end if
        end if
    end subroutine ShrinkToFit
    


    !> Returns a pointer to the element with the requested index (1-based)
    !! Pointer will not be associated in case of an exception
    function elem_at(me, Index) result(elem)
        implicit none
        class(Vector), intent(inout) :: me
        !> Index of the requested element
        integer, intent(in) :: Index
        !> pointer to the requested element
        class(VecElem), pointer :: elem
        
        integer :: BktNum, Offset

        ! compute bucket number and offset
        call me%Index2Bkt(Index, BktNum, Offset)

        if (BktNum > 0 .AND. BktNum <= me%UsedBkts &
            .AND. Offset > 0 .AND. Offset <= me%BktCap) then
            elem => me%VecData(BktNum)%pBkt%bktelem(Offset)
        else
            ! incorrect index
            elem => null()
        end if
    end function elem_at
    

    !> Returns the element with the index=1
    function elem_front(me) result(elem)
        implicit none
        class(Vector), intent(inout) :: me
        class(VecElem), allocatable :: elem
        
        allocate(elem, source=me%ElemAt(1))
    end function elem_front



    !> Returns the last element in the Vector
    function elem_back(me) result(elem)
        implicit none
        class(Vector), intent(inout) :: me
        class(VecElem), allocatable :: elem

        integer :: Index
        Index = (me%UsedBkts-1)*me%BktCap &
                        + me%VecData(me%UsedBkts)%pBkt%bktsz()
        allocate(elem, source=me%ElemAt(Index))
    end function elem_back




    !> Returns the element array within the requested lower and upper 
    !! bounds. This is inefficient as it copies the requested slice of
    !! the Vector in an array and returns it. If bounds result in
    !! zero-length slice, an empty array of VecElem type is returned.
    function vec_slice(me, lower, upper, stride) result(arr)
        implicit none
        class(Vector), intent(inout) :: me

        !> Lower bound as m in MATLAB-like syntax Vector(m:n)
        integer, intent(in) :: lower
        !> Upper bound as n in MATLAB-like syntax Vector(m:n)
        !! + Can be a negative integer as '-|a|' where |a| < Vector size
        integer, intent(in) :: upper
        !> stride as in MATLAB-like syntax Vector(m:stride:n)
        !! + Default is 1
        integer, intent(in), optional :: stride
        !> Array of VecElem type containing the subsection of Vector
        class(VecElem), dimension(:), allocatable :: arr

        integer :: step, ctr, sz, vecsz
        integer, dimension(:), allocatable :: ElemIndices

        if (present(stride)) then
            step = stride
        else
            step = 1
        end if

        ! create indices of the requested elements
        ElemIndices = [(ctr, ctr = lower, upper, step)]

        ! allocate memory
        sz = size(ElemIndices)
        vecsz = me%Size()
        if (sz < 1 .OR. vecsz < 1 &
            .OR. ElemIndices(1) < 1 .OR. ElemIndices(1) > vecsz &
            .OR. ElemIndices(sz) < 1 .OR. ElemIndices(sz) > vecsz) then
            ! If Vector is empty or requested slice is 0 size then
            ! return 0-sized array
            allocate(InvalidVecElem:: arr(0), stat=me%status)
            return
        else
            allocate(arr(sz), mold=me%VecData(1)%pBkt%bktelem(1), &
                                                stat=me%status)
        end if

        ! fill the array with data to return
        do ctr = 1, sz
        block
            integer :: Bkt, Offset
            call me%Index2Bkt(ElemIndices(ctr), Bkt, Offset)
            arr(ctr) = me%VecData(Bkt)%pBkt%bktelem(Offset)
        end block
        end do
    end function vec_slice




    !> Returns a pointer to the Vector slice specified using the 
    !! user-specified lower and upper bounds. If the requested slice 
    !! is beyond the extent of the data bucket that contains the element
    !! corresponding to the lower bound, then the data array is truncated
    !! at the bucket boundary and the upper bound is updated to indicate the
    !! actual upper bound of data that can be referenced using the returned
    !! pointer. This is an efficient method as no copying of data is done.
    !! In case of an exception, the returned pointer will not be associated.
    function bkt_slice(me, lower, upper) result(ptr)
        implicit none
        class(Vector), intent(inout) :: me

        !> Lower bound as m in MATLAB-like syntax Vector(m:n)
        integer, intent(in) :: lower
        !> Upper bound as n in MATLAB-like syntax Vector(m:n)
        !! + Can be a negative integer as '-|a|' where |a| < Vector size
        !! + It is updated to return the index of the last element that
        !! can be referenced using the returned pointer if the Vector slice
        !! is not entirely contained in a single bucket.
        integer, intent(inout) :: upper
        !> Array of VecElem type containing the subsection of Vector
        class(VecElem), dimension(:), pointer :: ptr

        integer :: Bktl, Offsetl, Bktu, Offsetu, ub, vecsz, ctr
        logical :: BoundsValid

        ptr => null()

        ! check for bounds
        vecsz = me%Size()
        BoundsValid = .TRUE.
        if (lower < 1 .OR. lower > vecsz) BoundsValid = .FALSE.
        if ((upper > lower .AND. upper > vecsz) &
                .OR. upper < lower .AND. upper < 1) BoundsValid = .FALSE.

        if (BoundsValid) then
            call me%Index2Bkt(lower, Bktl, Offsetl)
            call me%Index2Bkt(upper, Bktu, Offsetu)
            ! check whether the both bounds lie in the same bucket 
            if (Bktl /= Bktu) then
                ! truncate the slice to bucket boundary
                if (upper < lower) then
                    ! slice is from lower bound to start of the bucket
                    upper = (Bktl-1)*me%BktCap + 1
                    ptr => me%VecData(Bktl)%pBkt%bktsliceptr(Offsetl,1)
                else
                    ! slice is from lower bound to end of the bucket
                    upper = Bktl*me%BktCap
                    ptr => me%VecData(Bktl)%pBkt%bktsliceptr(Offsetl,me%BktCap)
                end if
            else
                ! slice is from lower to upper bound
                ptr => me%VecData(Bktl)%pBkt%bktsliceptr(Offsetl,Offsetu)
            end if
        end if
    end function bkt_slice


    !> Function to add an element at the back of the vector
    function push_back(me, velem) result(Index)
        implicit none
        class(Vector), intent(inout) :: me
        !> Element to add to the vector
        class(VecElem), intent(in) :: velem
        !> 0 on error, otherwise returns the position of the element
        integer :: Index
        
        integer :: offset
        
        Index = 0
        if (.NOT. me%StorageAllocated) then
            ! allocate buckets for the first time
            allocate(me%MoldElem, source=velem)
            call me%create_newbkts(me%NewBktsToAllocate)
            if (me%status /= 0) return
            ! use the newly created first bucket
            me%UsedBkts = 1
        end if
        
        ! extract the index of the last occupied slot in bucket
        if (me%AllocatedBkts > 0 .AND. me%UsedBkts < 1) me%UsedBkts = 1
        offset = me%VecData(me%UsedBkts)%pBkt%bktsz() + 1
        
        ! if bucket is full then allocate more buckets
        if (offset > me%BktCap) then
            ! allocate new buckets
            call me%create_newbkts(me%NewBktsToAllocate)
            if (me%status /= 0) return
            ! use the newly created bucket
            me%UsedBkts = me%UsedBkts + 1
            offset = 1
        end if

        ! copy the elem at the back of the bucket
        call me%VecData(me%UsedBkts)%pBkt%add_bktelem(offset, velem)
        Index = offset + (me%UsedBkts-1)*me%BktCap
        
    end function push_back
    



    !> Function to extract the last element of the vector
    function pop_back(me) result(velem)
        implicit none
        class(vector), intent(inout) :: me
        class(VecElem), allocatable :: velem
        
        integer :: bktind
        
        ! get the last element
        bktind = me%vecdata(me%UsedBkts)%pBkt%bktsz()
        allocate(velem, source=me%VecData(me%UsedBkts)%pBkt%bktelem(bktind))

        ! delete this element
        call me%VecData(me%UsedBkts)%pBkt%del_bktelem(bktind)

        ! if this bucket is empty, then update used bucket counter
        if (me%vecdata(me%UsedBkts)%pBkt%is_bktempty()) then
            me%UsedBkts = me%UsedBkts - 1
        end if        
    end function pop_back

    



    !> Inserts the element/elements at the given index and relocate 
    !! existing elements at the later indices. Very inefficient way
    !! to add elements to Vector unless elements are inserted after
    !! the last element.
    subroutine elem_insert(me, pos, NewElems, count)
        implicit none
        class(Vector), intent(inout) :: me
        !> Position at which the new elements are inserted (1-based index)
        !! Must be >= 1 and <= vector size
        integer, intent(in) :: pos
        !> Array of New Elements to be inserted
        class(VecElem), dimension(:), intent(in) :: NewElems
        !> If provided, NewElems are repeated count times before inserted
        !! Total number of elements inserted=count*size(NewElems)
        !! Default value is 1. Must be >= 1
        integer, intent(in), optional :: count

        integer :: countval, reqdslots, emptyslots, newbkts, ctr, sz, vecsz
        integer :: Bkt0, Offset0, Bkt1, Offset1, ctr1, ctr2
        real :: tmp

        countval = 1
        if (present(count)) countval = count

        ! if empty array is passed, nothing to be done
        sz = size(NewElems) 
        vecsz = me%Size()
        if (sz < 1) return

        ! if vector is not already allocated, do it now
        if (.NOT. me%StorageAllocated) then
            ! allocate buckets for the first time
            allocate(me%MoldElem, source=NewElems(1))
            call me%create_newbkts(me%NewBktsToAllocate)
            if (me%status /= 0) return
            ! use the newly created first bucket
            me%UsedBkts = 1
        end if        
        
        ! check for parameters
        if (pos < 1 .OR. countval < 1 .OR. ((pos-vecsz) > 1)) then
            me%status = -1
            return
        end if

        ! Expand Vector for new elements
        reqdslots = countval*sz
        emptyslots = me%Capacity() - me%Size()
        tmp = reqdslots - emptyslots
        newbkts = ceiling(tmp/me%BktCap)
        
        if (newbkts > 0) then
            ! allocate new buckets
            call me%create_newbkts(newbkts)
        end if
                        
        ! First Bucket for the new elements
        call me%Index2Bkt(pos, Bkt0, Offset0)
        ! First Bucket for the moved elements
        call me%Index2Bkt(pos+countval*sz, Bkt1, Offset1)

        ! Move old element to the new empty locations
        reqdslots = me%Size() - pos + 1
        ctr1 = Bkt0
        ctr2 = Offset0
        do ctr = 1, reqdslots
            ! Move old element to the new empty location

            call me%VecData(Bkt1)%pBkt%add_bktelem(Offset1, &
                    me%VecData(Bkt0)%pBkt%bktelem(Offset0))

            call me%VecData(Bkt0)%pBkt%del_bktelem(Offset0)
            
            ! increment counters
            Offset0 = Offset0 + 1
            Offset1 = Offset1 + 1
            if (Offset0 > me%BktCap) then
                Offset0 = 1
                Bkt0 = Bkt0 + 1
            end if
            if (Offset1 > me%BktCap) then
                Offset1 = 1
                Bkt1 = Bkt1 + 1
            end if
        end do

        ! copy the new element in the old location
        Bkt0 = ctr1
        Offset0 = ctr2
        ctr1 = 1
        do ctr = 1, countval*sz
            ! copy the new element in the old location
            call me%VecData(Bkt0)%pBkt%add_bktelem(Offset0, NewElems(ctr1))
            ! increment counters
            ctr1 = ctr1 + 1
            if (ctr1 > sz) ctr1 = 1
            Offset0 = Offset0 + 1
            if (Offset0 > me%BktCap) then
                Offset0 = 1
                Bkt0 = Bkt0 + 1
            end if            
        end do
        
        ! update the buckets used
        me%UsedBkts = me%usedBkts + newbkts

    end subroutine elem_insert





    !> Delete the element/elements between and including lower and upper
    !! bounds. The elements are moved to fill up empty locations. Very
    !! inefficient way to delete elements from Vector.
    subroutine elem_erase(me, lower, upper)
        implicit none
        class(Vector), intent(inout) :: me

        integer, intent(In) :: lower !< Lower bound
        !> Upper bound. All the elements witin lower and upper bounds
        !! are deleted. If upper is omitted only a single element with
        !! index=lower is deleted.
        integer, intent(In), optional :: upper

        integer :: sz, ctr, upperval
        integer :: Bkt0, Offset0, Bkt1, Offset1

        ! parameter checks
        sz = me%Size()
        if (lower < 1 .OR. lower > sz .OR. sz < 1) return

        if (present(upper)) then
            if (lower > upper) return
        end if

        ! Location of the first element to be deleted
        call me%Index2Bkt(lower, Bkt0, Offset0)

        ! Location of the element just after the last element to be deleted
        if (present(upper)) then
            upperval = upper + 1
            call me%Index2Bkt(upperval, Bkt1, Offset1)
        else
            upperval = lower + 1
            Bkt1 = Bkt0
            Offset1 = Offset0 + 1
            if (Offset1 > me%BktCap) then
                Offset1 = 1
                Bkt1 = Bkt1 + 1
            end if
        end if

        ! shift the other elements one by one
        do ctr = 1, (sz - upperval + 1)
            ! shift
            call me%VecData(Bkt0)%pBkt%add_bktelem(Offset0, &
                            me%VecData(Bkt1)%pBkt%bktelem(Offset1))
            
            ! increment counters
            Offset0 = Offset0 + 1
            Offset1 = Offset1 + 1
            if (Offset0 > me%BktCap) then
                Offset0 = 1
                Bkt0 = Bkt0 + 1
            end if         
            if (Offset1 > me%BktCap) then
                Offset1 = 1
                Bkt1 = Bkt1 + 1
            end if    
        end do

        ! Clear the empty slots in the last partially filled bkt
        if (Offset0 /= 1) then
            do ctr = Offset0, me%BktCap
                call me%VecData(Bkt0)%pBkt%del_bktelem(ctr)
            end do
            Bkt0 = Bkt0 + 1
        end if
        ! clear the extra buckets starting from Bkt0+1
        do ctr = Bkt0, Bkt1
            call me%VecData(ctr)%pBkt%bktclear()            
        end do
        ! update number of used buckets
        me%UsedBkts = Bkt0 - 1

    end subroutine elem_erase




    !> Clear the contents of Vector but the bucket memory is not deallocated
    subroutine Clear(me)
        implicit none
        class(Vector), intent(inout) :: me

        integer :: ctr
        ! clear bucket slots
        do ctr = 1, me%UsedBkts
            call me%VecData(ctr)%pBkt%bktclear()
        end do
        ! used buckets are 0 now
        me%UsedBkts = 0
    end subroutine Clear

    


    !> Subroutine to allocate storage for a new bucket
    subroutine create_newbkts(me, NumBkts)
        implicit none
        class(Vector), intent(inout) :: me

        integer, intent(in) :: NumBkts

        type(BktPtr), dimension(:), allocatable :: tmp
        integer :: ctr

        ! allocate new bucket array
        allocate(tmp(me%AllocatedBkts+NumBkts), stat=me%status)
            
        if (me%status == 0) then
            ! if vector already has buckets
            if (me%StorageAllocated) then
                ! copy the previous bucket pointers
                tmp(1:me%AllocatedBkts) = me%VecData
            end if

            ! expand the bucket array
            call move_alloc(from=tmp, to=me%VecData)

            ! allocate storage for new buckets
            do ctr = 1, NumBkts
                allocate(me%VecData(me%AllocatedBkts+ctr)%pBkt, stat=me%status)
                call me%VecData(me%AllocatedBkts+ctr)%pbkt%bktinit(me%status, &
                                                me%BktCap, me%MoldElem)
                if (me%status /= 0) exit
            end do
            
            ! If the intended number of buckets are not allocated, init failed
            if (ctr > NumBkts) then
                me%AllocatedBkts = me%AllocatedBkts + ctr - 1
                me%StorageAllocated = .TRUE.
            else
                me%status = -1
            end if
        end if
    end subroutine create_newbkts



    !> Converts Vector index to bucket number and offset into the bucket
    pure subroutine Index2Bkt(me, Index, Bkt, BktOffset)
        implicit none
        class(Vector), intent(in) :: me

        integer, intent(in) :: Index        !< Given index
        integer, intent(out) :: Bkt         !< Bucket number
        integer, intent(out) :: BktOffset   !< Offset into a bucket

        real :: frac
        
        ! Bucket to which this index belongs
        frac = Index
        frac = frac/me%BktCap
        Bkt = ceiling(frac)

        ! Offset into the bucket
        BktOffset = Index - (Bkt-1)*me%BktCap
    end subroutine


    ! Do nothing for the invalid element assignement
    subroutine assign_invalidelem(lhs, rhs)
        implicit none
        class(InvalidVecElem), intent(out) :: lhs
        class(VecElem), intent(in) :: rhs
    end subroutine assign_invalidelem
    

    !> Subroutine for Vector assignment operator
    subroutine AssignVector(lhs, rhs)
        implicit none
        class(Vector), intent(out) :: lhs
        class(Vector), intent(in) :: rhs

        integer :: ctr

        ! we assign the contents by moving all the buckets of rhs to lhs

        ! allocate memory for buckets array
        allocate(lhs%VecData(size(rhs%VecData)))

        ! create buckets and mold elem
        allocate(lhs%MoldElem, source=rhs%MoldElem)
        do ctr = 1, rhs%AllocatedBkts
            allocate(lhs%VecData(ctr)%pBkt, source=rhs%VecData(ctr)%pBkt)
        end do
        
        ! copy Vector state
        lhs%status              = rhs%status
        lhs%StorageAllocated    = rhs%StorageAllocated
        lhs%NewBktsToAllocate   = rhs%NewBktsToAllocate
        lhs%BktCap              = rhs%BktCap
        lhs%UsedBkts            = rhs%UsedBkts
        lhs%AllocatedBkts       = rhs%AllocatedBkts
        
    end subroutine AssignVector    

    
    !> Destructor
    subroutine Destroy(me)
        implicit none
        type(Vector), intent(inout) :: me
    
        integer :: ctr
        
        ! free up memory held up by all the buckets
        do ctr = 1, size(me%VecData)
            if (associated(me%VecData(ctr)%pBkt)) deallocate(me%VecData(ctr)%pBkt)
            me%VecData(ctr)%pBkt => null()
        end do
    end subroutine Destroy


    !> Bucket Type Methods

    subroutine bktinit(me, status, cap, mold_elem)
        implicit none
        class(Bkt), intent(inout) :: me
        integer, intent(out) :: status
        integer, intent(in) :: cap
        class(VecElem), intent(in) :: mold_elem

        allocate(me%SlotFree(cap), stat=status)
        allocate(me%BktData(cap), stat=status, mold=mold_elem)        
        me%SlotFree = .TRUE.
    end subroutine bktinit



    subroutine add_bktelem(me, offset, elem)
        implicit none
        class(Bkt), intent(inout) :: me
        integer, intent(in) :: offset
        class(VecElem), intent(in) :: elem

        me%BktData(offset) = elem
        me%SlotFree(offset) = .FALSE.
    end subroutine add_bktelem



    subroutine del_bktelem(me, offset)
        implicit none
        class(Bkt), intent(inout) :: me
        integer, intent(in) :: offset

        me%SlotFree(offset) = .TRUE.
    end subroutine del_bktelem


    subroutine bktclear(me)
        implicit none
        class(Bkt), intent(inout) :: me

        me%SlotFree = .TRUE.
    end subroutine bktclear



    function bktelem(me, offset)
        implicit none
        class(Bkt), intent(in), target :: me
        integer, intent(in) :: offset
        class(VecElem), pointer :: bktelem

        if (.NOT. me%SlotFree(offset)) then
            bktelem => me%BktData(offset)
        else
            bktelem => null()
        end if
    end function bktelem



    function bktsliceptr(me, start_ind, end_ind)
        implicit none
        class(Bkt), intent(in), target :: me
        integer, intent(in) :: start_ind, end_ind
        class(VecElem), dimension(:), pointer :: bktsliceptr

        integer :: stride

        bktsliceptr => null()

        stride = 1
        if (start_ind > end_ind) stride = -1

        if (any(me%SlotFree(start_ind:end_ind:stride))) return
            
        bktsliceptr => me%BktData(start_ind:end_ind:stride)
    end function bktsliceptr



    pure function is_bktempty(me)
        implicit none
        class(Bkt), intent(in) :: me
        logical :: is_bktempty

        is_bktempty = .FALSE.
        if (all(me%SlotFree)) is_bktempty = .TRUE.
    end function is_bktempty



    pure function bktsz(me)
        implicit none
        class(Bkt), intent(in) :: me
        integer :: bktsz
        bktsz = count(.NOT. me%SlotFree)
    end function bktsz



end module Vectors


