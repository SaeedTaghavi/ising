
subroutine print_spinArray(spinArray,size)
    implicit none
    integer::i,j
    integer::size,spinArray(size,size)
    ! print*,"spinArray:"
    do i=1,size
        do j=1,size
            if (spinArray(i,j)==1) then 
            ! write(*,'(i3$)')spinArray(i,j)
                write(*,'(a1$)')"+"
            else 
                write(*,'(a1$)')"-"
            end if
        end do
        write(*,*)
    end do
end subroutine print_spinArray

subroutine write_spinArray_snapshot_toFile(spinArray,size,mc_steps,temprature)
    implicit none
    integer::i,j
    integer::size,spinArray(size,size),mc_steps
    real*8::temprature
    character (len=90) :: filename
    write(filename,'( "spinArray.",I7,".",f6.4,".dat")')mc_steps,temprature
    ! open (92,file='spinArray.dat')
    open(92,file=filename)

    do i=1,size
        do j=1,size
            if (spinArray(i,j)==1) then 
            ! write(*,'(i3$)')spinArray(i,j)
                write(92,'(i2$)')1
            else 
                write(92,'(i2$)')0
            end if
        end do
        write(92,*)
    end do
    close(92)
end subroutine write_spinArray_snapshot_toFile
