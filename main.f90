module config
    implicit none

! Threads count
    integer, parameter :: thr = 4

! Number of points in 1 bin
    integer(8), parameter :: clust = 1d3

! Lower limit of random points count
    integer(8), parameter :: mfl = 1d0
! Upper limit of random points count
    integer(8), parameter :: mcl = 5d3
! Number of bins between mfl and mcl
    integer(8), parameter :: bins = 1d3

! Beginning of the segment
    real(8), parameter :: fl = 0d0
! End of the segment
    real(8), parameter :: cl = 1d0

contains

    pure real(8) function func(arg)
        real(8), intent(in) :: arg
        func = dcos(arg)
    end function

    pure real(8) function sol()
        sol = dsin(cl) - dsin(fl)
    end function

end module

program main
    use config
    implicit none
    abstract interface
        real(8) function fct(x)
        real(8), intent(in) :: x
        end function
    end interface

    procedure(fct), pointer :: ptr
    ptr => func
    call random_seed()

    call drawDist(ptr, fl, cl, mfl, mcl, bins, clust)
    call drawSolution(mfl, mcl, bins)

contains

    real(8) function integ(fptr, bt, tp, dots)

        integer(8), intent(in) :: dots
        real(8), intent(in) :: bt, tp
        procedure(fct), pointer, intent(in) :: fptr

        real(8) :: rnd
        integer(8) :: j

        !call random_seed()
        integ = 0
        !$omp parallel default(none) shared(dots, fptr, bt, tp) private(j, rnd) num_threads(thr)
            !$omp do schedule(static) reduction(+: integ)
            do j = 1, dots
                call random_number(rnd)
                rnd = bt + (tp-bt)*rnd
                integ = integ + fptr(rnd)
            end do
            !$omp end do
        !$omp end parallel

        integ = (tp-bt)*integ/dots

    end function integ

    subroutine drawDist(fptr, bt, tp, mbt, mtp, nbins, clust)
        integer(8), intent(in) :: mbt, mtp, nbins, clust
        real(8), intent(in) :: bt, tp
        procedure(fct), pointer, intent(in) :: fptr

        integer(8) :: j, i, stp

        stp = (mtp - mbt) / nbins
        open(unit = 1, file = "graphics/mkintegr.dat")
        do j = mbt, mtp, stp
            do i = 1, clust
                write (1, *) j, integ(fptr, bt, tp, j)
            end do
        end do
        close(1)
    end subroutine drawDist

    subroutine drawSolution(mbt, mtp, nbins)
        integer(8), intent(in) :: mbt, mtp, nbins
        integer(8) :: j, stp

        stp = (mtp - mbt) / nbins
        open(unit = 1, file = "graphics/solution.dat")
        do j = mbt, mtp, stp
            write (1, *) j, sol()
        end do
        close(1)
    end subroutine drawSolution

    subroutine drawDistAveraged(fptr, bt, tp, mbt, mtp, nbins, clust)
        integer(8), intent(in) :: mbt, mtp, nbins, clust
        real(8), intent(in) :: bt, tp
        procedure(fct), pointer, intent(in) :: fptr

        real(8) :: tmp
        integer(8) :: j, i, stp

        stp = (mtp - mbt) / nbins

        open(unit = 1, file = "graphics/mkintegr.dat")
        do j = mbt, mtp, stp
            tmp = 0
            do i = 1, clust
                tmp = tmp + integ(fptr, bt, tp, j)
            end do
            tmp = tmp / clust
            write (1, *) j, tmp
        end do
        close(1)
    end subroutine drawDistAveraged

end program main

