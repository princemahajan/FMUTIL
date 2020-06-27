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
!> \brief       List module
!! \details     This module provides the list data structure (similar to 
!!              Python list) represented internally using an array.
!! \author      Bharat Mahajan
!! \date        Created: 05/27/2020    
!
!##############################################################################
 
    
module Lists

    use FMUTILBase
    
    implicit none
    
    private

    !> Default number of slots to create on list capacity increase
    integer, parameter :: DEFAULT_NEWSLOTSALLOCATED = 5

    !> List Item type
     type, private :: ListItem
        private
        !> Must be .TRUE. if data item exists
        logical :: Valid = .FALSE.
        !> Pointer to the data item
        class(*), pointer :: item
        contains
        private
        !> Copy the given data in item
        procedure :: add_listitem
        !> Deallocate the item
        procedure :: del_listitem
        !> Check for valid item
        procedure :: isvalid_listitem
     end type ListItem



    !> List type
    type, public :: List
        
        private
        
        !> Status: 0-success, any negative value indicates exception condition
        integer, public :: status = 0

        !> Number of slots that currently contain data
        integer :: UsedSlots = 0

        !> Number of slots that are currently allocated
        integer :: AllocatedSlots = 0

        !> Number of new slots to create on List capacity increase
        integer :: NewSlotsToAllocate = DEFAULT_NEWSLOTSALLOCATED

        !> Array of List items
        type(ListItem), dimension(:), allocatable :: ListData
 
    contains
    
        private

        procedure :: AssignList

        !> Assignment operator for the List type, this enables
        !! copying of RHS List items to LHS List. Note that
        !! after this operation, LHS List will be an exact copy
        !! of the RHS List including data and internal state.
        generic, public :: assignment(=) => AssignList

        ! procedure :: ConcatList

        ! !> Concatenate operator for the List type, this enables
        ! !! concatenationo of the two lists to create a new list 
        ! !! with the combined contents.
        ! generic, public :: assignment(//) => ConcatList

        ! Internal procedures
        procedure :: create_newslots        
        
        !> Returns size of the List or number of items stored
        procedure, public :: Size => list_size

        !> Push one item at the back of the List
        procedure, public :: PushBack => push_back

        !> Pop one item from the back of the List
        procedure, public :: PopBack => pop_back        
        
        !> Returns a pointer to the item with the given index (1-based)
        procedure, public :: Item => item_at

        !> Returns the item with the index-1
        procedure, public :: Front => item_front

        !> Returns the item with the last index
        procedure, public :: Back => item_back

        !> Inserts the item at the given index and relocate 
        !! items at the later indices
        procedure, public :: Insert => item_insert

        !> Delete the item at the given index and relocate 
        !! the item at the later indices
        procedure, public :: Erase => item_erase

        !> Deallocate unused List slots
        procedure, public :: ShrinkToFit

        !> Clear the contents of the List but memory is not deallocated
        procedure, public :: Clear

        !> Frees up all the memory allocated
        final :: Destroy

    end type List


contains




!> Returns the total number of items in the List
pure function list_size(me)
    implicit none
    class(List), intent(in) :: me
    integer :: list_size

    list_size = me%UsedSlots
end function list_size






!> Subroutine to append an element at the back of the list
function push_back(me, newitem) result(Index)
    implicit none
    class(List), intent(inout) :: me
    !> item to add to the list
    class(*), intent(in) :: newitem
    !> 0 on error, otherwise returns the position of the element
    integer :: Index
    
    Index = 0
    
    if (me%UsedSlots == me%AllocatedSlots) then
        ! allocate new slots
        call me%create_newslots(me%NewSlotsToAllocate)
        if (me%status /= 0) return
    end if
    
    ! copy the item at the back of the list
    call me%ListData(me%UsedSlots+1)%add_listitem(newitem)
    me%UsedSlots = me%UsedSlots + 1
    Index = me%UsedSlots
end function push_back




!> Function to extract the last item of List
function pop_back(me) result(item)
    implicit none
    class(List), intent(inout) :: me
    class(*), allocatable :: item
        
    ! get the last item
    allocate(item, source=me%Listdata(me%UsedSlots)%item, stat=me%status)

    ! delete this item
    call me%ListData(me%UsedSlots)%del_listitem()
    me%UsedSlots = me%UsedSlots - 1
end function pop_back


!> Returns a pointer to the item with the requested index (1-based)
!! Pointer will not be associated in case of an exception
function item_at(me, Index) result(item)
    implicit none
    class(List), intent(inout), target :: me
    !> Index of the requested item
    integer, intent(in) :: Index

    class(*), pointer :: item
        
    item => null()
    ! check if Index is valid
    if (Index < 1 .OR. Index > me%Size()) return
    ! If element is valid, return the pointer
    if (me%Listdata(Index)%isvalid_listitem()) item=>me%Listdata(Index)%item
end function item_at




!> Returns the item with the index=1
function item_front(me) result(item)
    implicit none
    class(List), intent(inout) :: me
    class(*), allocatable :: item
    class(*), pointer :: ptr
    ptr => me%Item(1)
    allocate(item, source=ptr)
end function item_front



!> Returns the last item in List
function item_back(me) result(item)
    implicit none
    class(List), intent(inout) :: me
    class(*), allocatable :: item
    class(*), pointer :: ptr
    ptr => me%Item(me%Size())
    allocate(item, source=ptr)
end function item_back


!> Inserts an item at the given position
subroutine item_insert(me, pos, NewItem)
    implicit none
    class(List), intent(inout) :: me
    integer, intent(in) :: pos
    class(*), intent(in) :: NewItem
    
    integer :: ctr

    ! parameter check
    if (pos < 1 .OR. pos > (me%Size()+1)) then
        me%status = -1
        return
    end if

    ! expand internal array if needed
    if (me%Size() == me%AllocatedSlots) then
        ! allocate new slots
        call me%create_newslots(me%NewSlotsToAllocate)
        if (me%status /= 0) return
    end if
    
    ! Move the old list item pointers
    do ctr = me%Size(), pos, -1
        call me%ListData(ctr+1)%add_listitem(me%ListData(ctr)%item)
    end do

    ! insert the new item
    call me%ListData(pos)%add_listitem(NewItem)
    me%UsedSlots = me%UsedSlots + 1
end subroutine item_insert




!> Erase an item at the given position
subroutine item_erase(me, pos)
    implicit none
    class(List), intent(inout) :: me
    integer, intent(in) :: pos
    
    integer :: ctr
    
    ! parameter check
    if (pos < 1 .OR. pos > me%Size()) then
        me%status = -1
        return
    end if
   
    ! delete the element
    call me%ListData(pos)%del_listitem()

    ! Move the other list item pointers
    do ctr = pos, me%Size()-1
        call me%ListData(ctr)%add_listitem(me%ListData(ctr+1)%item)
    end do
    me%UsedSlots = me%UsedSlots - 1
end subroutine item_erase





!> Subroutine to allocate storage for new slots
subroutine create_newslots(me, NumSlots)
    implicit none
    class(List), intent(inout) :: me
    !> Number of slots to create
    integer, intent(in) :: NumSlots

    type(ListItem), dimension(:), allocatable :: tmp
    integer :: ctr

    ! allocate new item array
    allocate(tmp(me%AllocatedSlots+NumSlots), stat=me%status)
        
    if (me%status == 0) then
        ! if List already has some slots
        if (me%UsedSlots /= 0) then
            ! copy the previous data pointers
            tmp(1:me%AllocatedSlots) = me%ListData
        end if
        ! expand the list array
        call move_alloc(from=tmp, to=me%ListData)
        me%AllocatedSlots = me%AllocatedSlots + NumSlots
    end if
end subroutine create_newslots




!> Subroutine for List assignment operator
subroutine AssignList(lhs, rhs)
    implicit none
    class(List), intent(out) :: lhs
    class(List), intent(in) :: rhs

    integer :: ctr

    ! we assign the contents by moving all the items of rhs to lhs

    ! allocate memory for items array
    allocate(lhs%ListData(rhs%AllocatedSlots))

    ! copy items
    do ctr = 1, rhs%AllocatedSlots
        call lhs%ListData(ctr)%add_listitem(rhs%ListData(ctr)%item)
    end do
    
    ! copy List state
    lhs%status               = rhs%status
    lhs%NewSlotsToAllocate   = rhs%NewSlotsToAllocate
    lhs%UsedSlots            = rhs%UsedSlots
    lhs%AllocatedSlots       = rhs%AllocatedSlots 
end subroutine AssignList    






!> Clear the contents of List
subroutine Clear(me)
    implicit none
    class(List), intent(inout) :: me

    integer :: ctr
    ! clear slots
    do ctr = 1, me%Size()
        call me%ListData(ctr)%del_listitem()
    end do
    ! used slots are 0 now
    me%UsedSlots = 0
end subroutine Clear


!> Free up unused allocated memory and make List capacity equal to its size
subroutine ShrinkToFit(me)
    implicit none
    class(List), intent(inout) :: me

    integer :: ctr
    type(ListItem), dimension(:), allocatable :: tmp

    if (me%Size() < me%AllocatedSlots) then
        ! destroy the slots
        do ctr = me%Size()+1, me%AllocatedSlots
            call me%ListData(ctr)%del_listitem()
        end do
        me%AllocatedSlots = me%Size()
    end if

    if (me%AllocatedSlots < 1) then
        if (allocated(me%ListData)) deallocate(me%ListData, stat=me%status)
    else
        ! shrink the list data pointer array
        allocate(tmp(1:me%AllocatedSlots), stat=me%status)
        if (me%status == 0) then
            tmp = me%ListData(1:me%AllocatedSlots)
            call move_alloc(from=tmp, to=me%ListData)
        end if
    end if
end subroutine ShrinkToFit




!> Destructor
subroutine Destroy(me)
    implicit none
    type(List), intent(inout) :: me

    integer :: ctr
    
    ! free up memory held up by all the slots
    do ctr = 1, me%Size()
        call me%ListData(ctr)%del_listitem()
    end do
end subroutine Destroy



! List Item procedures

elemental subroutine add_listitem(me, item)
    implicit none
    class(ListItem), intent(inout) :: me
    !> new item to be allocated
    class(*), intent(in) :: item

    integer :: status

    allocate(me%item, source=item, stat=status)
    if (status == 0) then
        me%Valid = .TRUE.
    else
        me%Valid = .False.
        if (associated(me%item)) deallocate(me%item)
        me%item => null()
    end if
end subroutine add_listitem



subroutine del_listitem(me)
    implicit none
    class(ListItem), intent(inout) :: me

    if (associated(me%item)) deallocate(me%item)
    me%item => null()
    me%Valid = .False.
end subroutine del_listitem



pure elemental function isvalid_listitem(me)
    implicit none
    class(ListItem), intent(in) :: me
    logical :: isvalid_listitem
    isvalid_listitem = me%Valid
end function isvalid_listitem

end module Lists
