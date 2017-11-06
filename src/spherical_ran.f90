program main
  implicit none
  integer,parameter :: Nran = 10
  real(8) :: rvec(3)
  integer :: ir
  real(8) :: x,y,z,xx,yy,zz,rr
  
  x = 0d0
  y = 0d0
  z = 0d0
  xx = 0d0
  yy = 0d0
  zz = 0d0
  rr = 0d0

  do ir = 1,Nran
     call random_number(rvec)
  end do

  do ir = 1,Nran
     call spherical_ran(rvec)
     x = x + rvec(1)
     y = y + rvec(2)
     z = z + rvec(3)
     xx = xx + rvec(1)**2
     yy = yy + rvec(2)**2
     zz = zz + rvec(3)**2
     rr = rr + rvec(1)**2 + rvec(2)**2 + rvec(3)**2

  end do
  x = x/Nran
  y = y/Nran
  z = z/Nran
  xx = xx/Nran
  yy = yy/Nran
  zz = zz/Nran
  rr = rr/Nran

  write(*,"(A,2x,I7)")"# of random numbers",Nran
  write(*,"(A,2x,es26.16e3)")"x ",x
  write(*,"(A,2x,es26.16e3)")"y ",y
  write(*,"(A,2x,es26.16e3)")"z ",z
  write(*,"(A,2x,es26.16e3)")"xx",xx
  write(*,"(A,2x,es26.16e3)")"yy",yy
  write(*,"(A,2x,es26.16e3)")"zz",zz
  write(*,"(A,2x,es26.16e3)")"rr",rr


end program main

subroutine spherical_ran(rvec)
  implicit none
  real(8), intent(out) :: rvec(3)
  real(8), parameter :: pi = 4d0*atan(1d0)
  real(8)  :: x,y,z,r,phi

  call random_number(z); z = z*2d0-1d0
  call random_number(phi); phi = phi*2d0*pi

  r = sqrt(1d0-z**2)
  x = r*cos(phi)
  y = r*sin(phi)

  rvec(1) = x
  rvec(2) = y
  rvec(3) = z

end subroutine spherical_ran
