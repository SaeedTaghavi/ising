program ising
implicit none
integer::size
integer::mc_steps
integer,allocatable::spinArray(:,:)
real*8::temprature_start,temprature_stop,dtemprature,temprature
real*8::Cv,heat_capacity
real*8::M,magnetization_per_particle

call read_input(size,mc_steps,temprature_start,temprature_stop,dtemprature)
allocate(spinArray(size,size))

open (1,file='tempCvM.dat')

temprature=temprature_stop
do while(temprature>temprature_start)
    ! print*,"temprature:",temprature
    call initialize(size,spinArray,1) 
    !0 hot initial configuration
    !1 cold initial configuration

    call metropolis(temprature, mc_steps, size,spinArray)
    M=magnetization_per_particle(spinArray,size)
    Cv=heat_capacity(spinArray,size,temprature)
    write(1,*) temprature,Cv,M
    !call write_spinArray_snapshot_toFile(spinArray,size,mc_steps,temprature)
    temprature=temprature-dtemprature
end do
close(1)
     
end program ising
