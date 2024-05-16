      subroutine brdfgrid(mu,np,rm,rp,brdfdat,angmu,angphi,
     s           brdfint)
      integer mu,np
      real rp(np),brdfint(-mu:mu,np),rm(-mu:mu)
     s    ,angmu(10),angphi(13),brdfdat(10,13)
      real brdftemp(10,13)
      real gaussmu,gaussphi,y
      integer j,k
      do j=1,np
        do k=1,mu
          brdfint(k,j)=0.
        end do
      end do
      call splie2(angphi,brdfdat,10,13,brdftemp)
      do j=1,np
        do k=1,mu
            gaussmu=rm(k)
            gaussphi=rp(j)
            call splin2(angmu,angphi,brdfdat,brdftemp,10,13,
     s      gaussmu,gaussphi,y)
            brdfint(k,j)=y
        end do
      end do
      return
      end
