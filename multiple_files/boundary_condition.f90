

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