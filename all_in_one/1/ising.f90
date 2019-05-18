program ising
implicit none
integer::size
integer::mc_steps,E,temp_int
integer,allocatable::spinArray(:,:)
real*8::temprature_start,temprature_stop,dtemprature,temprature
real*8::energy,energy2,Cv,variance_energy_per_particle,M,magnetization_per_particle

call read_input(size,mc_steps,temprature_start,temprature_stop,dtemprature)
allocate(spinArray(size,size))

temprature=temprature_stop
do while(temprature>temprature_start)
    ! print*,"temprature:",temprature
    call initialize(size,spinArray,1) 
    !0 hot initial configuration
    !1 cold initial configuration
    

    ! call write_spinArray_toFile(spinArray,size)

    ! call print_spinArray(spinArray,size)
    ! print*,E(spinArray,size)
    ! print*,"****************"
    ! call metropolis(temprature, mc_steps, size,spinArray)

    call metropolis(temprature, mc_steps, size,spinArray)
    temp_int=E(spinArray,size)
    energy=real(temp_int)/real(size*size)

    energy2= variance_energy_per_particle(spinArray,size)
    Cv=(energy2-(energy*energy))/temprature
    M=magnetization_per_particle(spinArray,size)
    write(1,*) temprature,energy,energy2,Cv,M
    temprature=temprature-dtemprature
end do
     
end program ising

function magnetization_per_particle(spinArray,size)
    implicit none
    integer::size,spinArray(size,size)
    real*8::magnetization_per_particle
    integer::i,j,magnetization
    do i=1,size
        do j=1,size
            magnetization=magnetization+spinArray(i,j)
        end do
    end do
    magnetization_per_particle=real(magnetization)/real(size*size)
end function magnetization_per_particle

function variance_energy_per_particle(spinArray,size)
    implicit none
    integer::size,spinArray(size,size)
    real*8::variance_energy_per_particle
    integer::i,j,temp_int,energy_for_particle,Eij

    do i=1,size
        do j=1,size
            Eij=energy_for_particle(size,spinArray,i,j)
            temp_int=temp_int+Eij*Eij
        end do
    end do
    variance_energy_per_particle=real(temp_int)/real(size*size)

end function variance_energy_per_particle


function energy_for_particle(size,spinArray,i,j)
    implicit none
    integer::size, spinArray(size,size),i,j
    integer::l,r,t,b,left,right,top,bottom
    integer::energy_for_particle
    l=left(i,j,spinArray,size)
    r=right(i,j,spinArray,size)
    t=top(i,j,spinArray,size)
    b=bottom(i,j,spinArray,size)
    energy_for_particle = -spinArray(i,j)*(l+r+t+b)
end function energy_for_particle

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

subroutine metropolis(temprature, mc_steps, size,spinArray)
    implicit none
    integer::size,mc_steps,spinArray(size,size)
    real*8::temprature,k
    integer::E,deltaE,dE
    integer::i,j,step
    real::temp_random
    k=1.0
    do step=1,mc_steps
        
        !choosing a random spin spot
        call random_number(temp_random)
        temp_random=temp_random*(size+1)
        i=int(temp_random)

        call random_number(temp_random)
        temp_random=temp_random*(size+1)
        j=int(temp_random)
        
        dE=deltaE (i,j,spinArray,size)
        call random_number(temp_random)

        if (dE<=0  .or. temp_random<exp(-dE/(k*temprature)))  then
            spinArray(i,j)=-spinArray(i,j)
            dE= E(spinArray,size)
        !    print*,"energy:",dE
        !    print*,"temprature:",temprature 
            ! call print_spinArray(spinArray,size)
        end if
        ! !printing energy is just for checking

        
        ! write(1,*)de
        ! print*,"*****************************"
        ! print*,
        ! pause
    end do  
end subroutine metropolis

function deltaE (i,j,spinArray,size)
    implicit none
    integer::size,i,j,spinArray(size,size)
    ! real*8::deltaE
    integer::deltaE
    integer::left,right,top,bottom
    integer::l,r,t,b
    l=left(i,j,spinArray,size)
    r=right(i,j,spinArray,size)
    t=top(i,j,spinArray,size)
    b=bottom(i,j,spinArray,size)
    deltaE = 2 * spinArray(i,j)*(l+r+t+b)
end function deltaE

function E (spinArray,size)
    implicit none
    integer::size,spinArray(size,size)
    integer::E,l,r,t,b
    integer::i,j,left,right,top,bottom
    
    E=0
    do i=1,size
        do j=1,size
            l=left(i,j,spinArray,size)
            r=right(i,j,spinArray,size)
            t=top(i,j,spinArray,size)
            b=bottom(i,j,spinArray,size)
            E = E - spinArray(i,j)*(l+r+t+b)
        end do
    end do
end function E

!left neighbour of position(i,j)
function left(i,j,spinArray,size)
    implicit none
    integer::size,i,j,spinArray(size,size),left
    
    if (i==1) then 
        left=spinArray(size,j)
    else
        left=spinArray(i-1,j)
    end if

end function left

!right neighbour of position(i,j)
function right(i,j,spinArray,size)
    implicit none
    integer::size,i,j,spinArray(size,size),right
    
    if (i==size) then 
        right=spinArray(1,j)
    else
        right=spinArray(i+1,j)
    end if

end function right

!top neighbour of position(i,j)
function top(i,j,spinArray,size)
    implicit none
    integer::size,i,j,spinArray(size,size),top
    
    if (j==1) then 
        top=spinArray(i,size)
    else
        top=spinArray(i,j-1)
    end if

end function top

!bottom neighbour of position(i,j)
function bottom(i,j,spinArray,size)
    implicit none
    integer::size,i,j,spinArray(size,size),bottom
    
    if (j==size) then 
        bottom=spinArray(i,1)
    else
        bottom=spinArray(i,j+1)
    end if

end function bottom

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

subroutine write_spinArray_toFile(spinArray,size)
    implicit none
    integer::i,j
    integer::size,spinArray(size,size)
    open (92,file='spinArray.dat')
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
end subroutine write_spinArray_toFile
