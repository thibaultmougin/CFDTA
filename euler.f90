function F_1(rho, rhou , e,p) 
    double precision, intent(in) :: rho(Nx+2,Ny+2), rhou(2, Nx+2,Ny+2), e(Nx+2,Ny+2),p
    double precision :: F_1(2, Nx+2,Ny+2)
    F_1=rhou
end function

function F_2(rho, rhou, e, p) 
    double precision, intent(in) :: rho(Nx+2,Ny+2), rhou(2, Nx+2,Ny+2), e(Nx+2,Ny+2),p
    double precision :: F_2(2,2, Nx+2,Ny+2)

    double precision :: rhou1,rhou2
    integer :: i,j
    do i=1, Nx+2
        do j=1, Ny+2
            rhou1 = rhou(1, i,j)
            rhou2 = rhou(2,i,j)
            F_2(1,1,i,j)=(1/rho(i,j))*rhou1**2+p
            F_2(1,2,i,j)=(1/rho(i,j))*rhou1*rhou2
            F_2(2,1,i,j)=(1/rho(i,j))*rhou1*rhou2
            F_2(2,2,i,j)=(1/rho(i,j))*rhou2**2+p

        end do
    end do 
end function


function F_3(rho, rhou,e,p)
    double precision, intent(in) :: rho(Nx+2,Ny+2),rhou(2,Nx+2,Ny+2), e(Nx+2,Ny+2),p
    double precision :: F_3(2, Nx+2,Ny+2)
    
    integer :: i,j

    do i =1,Nx+2
        do j =1,Ny+2
            F_3(:,i,j)=(e(i,j)+p)*(1/rho(i,j))*rhou(:,i,j)
        end do 
    end do 


end function 
program euler 

    implicit none 

    double precision, parameter :: a = 2.
    double precision, parameter :: b = 1.         


    integer, parameter :: Nx = 100
    integer, parameter :: Ny = 100
    double precision, parameter :: T = 10
    integer, parameter :: Nt =1000
    double precision :: delta_t = T/Nt

    double precision :: rho(Nx+2,Ny+2), rhou(2, Nx+2,Ny+2), e(Nx+2,Ny+2)

    integer :: i,j

!Initialization
    do i=1, Nx+2
        do j=1, Ny+2
            rho(i,j) = 0.
            rhou(1, i,j) = 0.
            rhou(2, i,j) = 0.
            e(i,j) = 1.
            if (i<Nx/2) then 
                rho(i,j) = 1.
            end if 

        end do
    end do

end program euler