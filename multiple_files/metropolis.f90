
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
