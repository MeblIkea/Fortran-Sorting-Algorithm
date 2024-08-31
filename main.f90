module algos
  implicit none
  private

  public :: insertion

contains

  function insertion(a, v) result(babar)
    integer, dimension(:) :: a
    integer, dimension(size(a)+1) :: babar
    integer :: v, u

    do u=1,size(a)
      if (a(u) < v) then
        babar(u) = a(u)
      elseif (a(u) > v) then
        if (a(u-1) <= v) then
          babar(u) = v
          babar(u+1) = a(u)
        else
          babar(u+1) = a(u)
        end if
      end if
    end do
    if (a(u) < v) then
      babar(u+1) = v
    end if

  end function insertion

end module algos
 
 
program main
  use algos
  implicit none

  integer, dimension(5) :: t
  integer, dimension(size(t)+1) :: rep

  t = [1, 2, 4, 5, 6]

  rep = insertion(t, 4)

  print *, rep

end program main