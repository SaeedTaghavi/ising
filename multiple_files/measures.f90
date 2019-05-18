function heat_capacity(spinArray,size,temprature)
    implicit none
    integer::size,spinArray(size,size)
    real*8::heat_capacity,energy_per_particle,temprature,variance_energy_per_particle
    integer:: E
    energy_per_particle=real(E(spinArray,size))/real(size*size)
    heat_capacity=(variance_energy_per_particle(spinArray,size)-(energy_per_particle*energy_per_particle))/temprature
end function heat_capacity


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
