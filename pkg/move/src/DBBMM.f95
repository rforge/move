!   
!   BBMM
!
!   This file contains routines to build a DLL which is callable from
!   R to fit the Brownian Bridge Movement Model.
!
!   Created by Ryan Nielson.
!
!   To compile with gfortran:
!
!   gfortran -shared -o bbmm.dll bbmm.f95 
!

! ---------------------------------------------------------------------------
subroutine dBBMM(nLocs, gridSize, timeDiff, tTotal, X, Y, BMvar, LocationError, gridX, gridY, timeStep, probability)
!
! Brownian Bridge Movement Model (does not include motion variance)
!
! Ryan Nielson
!

implicit none

! Input variables
integer :: nLocs, gridSize
double precision, dimension(nLocs) :: timeDiff
double precision :: tTotal         
double precision, dimension(nLocs) :: X       
double precision, dimension(nLocs) :: Y       
double precision, dimension(nLocs) :: BMvar          
double precision, dimension(nLocs) :: LocationError  
double precision, dimension(gridSize) :: gridX
double precision, dimension(gridSize) :: gridY
double precision :: timeStep       

!character (len=180) MSG
! Local variables
double precision :: tm, alpha, muX, muY, sigma2
double precision, dimension(gridSize) :: int, theta, ZTZ
integer :: i               

! Output variables
double precision, dimension(gridSize) :: probability
int = 0.0
alpha = 0.0
muX = 0.0
muY = 0.0
sigma2 = 0.0

do i = 1, nLocs-1
    theta = 0.0                            
    tm = 0.0
    ZTZ = 0.0
    do while(tm <= timeDiff(i))
        call rchkusr()
        alpha = tm / timeDiff(i)
        muX = X(i) + alpha*(X(i+1) - X(i))
        muY = Y(i) + alpha*(Y(i+1) - Y(i))
        sigma2 = timeDiff(i)*alpha*(1-alpha)*BMvar(i) + &
                 ((1-alpha)**2)*(LocationError(i)**2) + &
                 (alpha**2)*(LocationError(i+1)**2)
        ZTZ = (gridX - muX)**2 + (gridY - muY)**2
        theta = (1/(2*3.14*sigma2))*exp(-ZTZ/(2*sigma2)) 
!        call rwarn(sum(theta))
!        format('nmax from workspace =',i8)
!        write(msg,901) sum(theta) 
       ! write(msg,901) sum(theta) 
        !901 format('nmax from workspace =',1pe11.3)
!        print(theta)
!                  call dblepr("X was", 5, sum(theta), 1)
        int = int + theta
        tm = tm + timeStep
    end do
end do

!Scaling probabilities so they sum to 1.0
probability = int/tTotal
probability = probability/sum(probability)

end subroutine
