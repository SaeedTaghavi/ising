
subroutine read_input(size,mc_steps,temprature_start,temprature_stop,dtemprature)
    implicit none
    integer::size,mc_steps
    real*8::dtemprature,temprature_start,temprature_stop
    print*,
    print*,"start READING INPUT"

    open(91,file='input.dat')
    read(91,*) 
    read(91,*)size
    read(91,*) 
    read(91,*)mc_steps
    read(91,*) 
    read(91,*)temprature_start,temprature_stop,dtemprature
    print*
    print*,"size:",size
    print*,"mc_steps:",mc_steps
    print*,"temprature_start,temprature_stop,dtemprature:",temprature_start,temprature_stop,dtemprature
    print*,"reading input FINISHED!"
    print*
    print*,"*******************"
    print*
    print*
end subroutine read_input


subroutine initialize(size,spinArray,key)
    implicit none
    integer::size,i,j,spinArray(size,size),key
    real*8::temp_random

    print*,"initializing with size:",size
    !key=0 hot initial configuration
    if (key==0) then 
        do i=1,size
            do j=1,size
                call random_number(temp_random)
                if (temp_random<0.5) then
                    spinArray(i,j)=1
                else
                    spinArray(i,j)=-1
                end if
            end do
        end do
    !key=1 cold initial configuration
    else if (key==1) then 
        spinArray=1
    else 
        print*,"you are using wrong key parameter!!!"
        print*,"use it like:    initialize(size,spinArray,key)"
        print*,"k=0: the system start with randomly aligned spins "
        print*,"k=1: the system start with all the spins aligned in one direction"
        print*,
        print*,
        stop
    end if
        

    print*,"initializing complete!"
    ! call print_spinArray(spinArray,size)
    print*,
    print*,
end subroutine initialize