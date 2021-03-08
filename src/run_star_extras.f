! ***********************************************************************
!
!   Copyright (C) 2011  Bill Paxton
!
!   this file is part of mesa.
!
!   mesa is free software; you can redistribute it and/or modify
!   it under the terms of the gnu general library public license as published
!   by the free software foundation; either version 2 of the license, or
!   (at your option) any later version.
!
!   mesa is distributed in the hope that it will be useful, 
!   but without any warranty; without even the implied warranty of
!   merchantability or fitness for a particular purpose.  see the
!   gnu library general public license for more details.
!
!   you should have received a copy of the gnu library general public license
!   along with this software; if not, write to the free software
!   foundation, inc., 59 temple place, suite 330, boston, ma 02111-1307 usa
!
! ***********************************************************************
 
      module run_star_extras

      use star_lib
      use star_def
      use const_def
      
      implicit none

      integer :: time0, time1, clock_rate
      
      ! these routines are called by the standard run_star check_model
      contains
      
      
      subroutine extras_controls(id, ierr)
         integer, intent(in) :: id
         integer, intent(out) :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
         s% extras_startup => extras_startup
         s% extras_check_model => extras_check_model
         s% extras_finish_step => extras_finish_step
         s% extras_after_evolve => extras_after_evolve
         s% how_many_extra_history_columns => how_many_extra_history_columns
         s% data_for_extra_history_columns => data_for_extra_history_columns
         s% how_many_extra_profile_columns => how_many_extra_profile_columns
         s% data_for_extra_profile_columns => data_for_extra_profile_columns  
         s% other_D_mix => my_other_D_mix11
      end subroutine extras_controls
      
      
      integer function extras_startup(id, restart, ierr)
         integer, intent(in) :: id
         logical, intent(in) :: restart
         integer, intent(out) :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
         extras_startup = 0
         call system_clock(time0,clock_rate)
         if (.not. restart) then
            call alloc_extra_info(s)
         else ! it is a restart
            call unpack_extra_info(s)
         end if
      end function extras_startup
      
      
      subroutine extras_after_evolve(id, id_extra, ierr)
         integer, intent(in) :: id, id_extra
         integer, intent(out) :: ierr
         type (star_info), pointer :: s
         real(dp) :: dt
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
         call system_clock(time1,clock_rate)
         dt = dble(time1 - time0) / clock_rate / 60
         write(*,'(/,a50,f12.2,99i10/)') 'runtime (minutes), retries, backups, steps', &
            dt, s% num_retries, s% num_backups, s% model_number
         ierr = 0
      end subroutine extras_after_evolve
      

      ! returns either keep_going, retry, backup, or terminate.
      integer function extras_check_model(id, id_extra)
         integer, intent(in) :: id, id_extra
         integer :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
         extras_check_model = keep_going         
      end function extras_check_model


      integer function how_many_extra_history_columns(id, id_extra)
         integer, intent(in) :: id, id_extra
         integer :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
         how_many_extra_history_columns = 0
      end function how_many_extra_history_columns
      
      
      subroutine data_for_extra_history_columns(id, id_extra, n, names, vals, ierr)
         integer, intent(in) :: id, id_extra, n
         character (len=maxlen_history_column_name) :: names(n)
         real(dp) :: vals(n)
         integer, intent(out) :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
      end subroutine data_for_extra_history_columns

      
      integer function how_many_extra_profile_columns(id, id_extra)
         use star_def, only: star_info
         integer, intent(in) :: id, id_extra
         integer :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
         how_many_extra_profile_columns = 0
      end function how_many_extra_profile_columns
      
      
      subroutine data_for_extra_profile_columns(id, id_extra, n, nz, names, vals, ierr)
         use star_def, only: star_info, maxlen_profile_column_name
         use const_def, only: dp
         integer, intent(in) :: id, id_extra, n, nz
         character (len=maxlen_profile_column_name) :: names(n)
         real(dp) :: vals(nz,n)
         integer, intent(out) :: ierr
         type (star_info), pointer :: s
         integer :: k
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
      end subroutine data_for_extra_profile_columns
      

      ! returns either keep_going or terminate.
      integer function extras_finish_step(id, id_extra)
         integer, intent(in) :: id, id_extra
         integer :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
         extras_finish_step = keep_going
         call store_extra_info(s)
      end function extras_finish_step
      
      
      ! routines for saving and restoring extra data so can do restarts
         
         ! put these defs at the top and delete from the following routines
         !integer, parameter :: extra_info_alloc = 1
         !integer, parameter :: extra_info_get = 2
         !integer, parameter :: extra_info_put = 3
      
      
      subroutine alloc_extra_info(s)
         integer, parameter :: extra_info_alloc = 1
         type (star_info), pointer :: s
         call move_extra_info(s,extra_info_alloc)
      end subroutine alloc_extra_info
      
      
      subroutine unpack_extra_info(s)
         integer, parameter :: extra_info_get = 2
         type (star_info), pointer :: s
         call move_extra_info(s,extra_info_get)
      end subroutine unpack_extra_info
      
      
      subroutine store_extra_info(s)
         integer, parameter :: extra_info_put = 3
         type (star_info), pointer :: s
         call move_extra_info(s,extra_info_put)
      end subroutine store_extra_info
      
      
      subroutine move_extra_info(s,op)
         integer, parameter :: extra_info_alloc = 1
         integer, parameter :: extra_info_get = 2
         integer, parameter :: extra_info_put = 3
         type (star_info), pointer :: s
         integer, intent(in) :: op
         
         integer :: i, j, num_ints, num_dbls, ierr
         
         i = 0
         ! call move_int or move_flg    
         num_ints = i
         
         i = 0
         ! call move_dbl       
         
         num_dbls = i
         
         if (op /= extra_info_alloc) return
         if (num_ints == 0 .and. num_dbls == 0) return
         
         ierr = 0
         call star_alloc_extras(s% id, num_ints, num_dbls, ierr)
         if (ierr /= 0) then
            write(*,*) 'failed in star_alloc_extras'
            write(*,*) 'alloc_extras num_ints', num_ints
            write(*,*) 'alloc_extras num_dbls', num_dbls
            stop 1
         end if
         
         contains
         
         subroutine move_dbl(dbl)
            real(dp) :: dbl
            i = i+1
            select case (op)
            case (extra_info_get)
               dbl = s% extra_work(i)
            case (extra_info_put)
               s% extra_work(i) = dbl
            end select
         end subroutine move_dbl
         
         subroutine move_int(int)
            integer :: int
            i = i+1
            select case (op)
            case (extra_info_get)
               int = s% extra_iwork(i)
            case (extra_info_put)
               s% extra_iwork(i) = int
            end select
         end subroutine move_int
         
         subroutine move_flg(flg)
            logical :: flg
            i = i+1
            select case (op)
            case (extra_info_get)
               flg = (s% extra_iwork(i) /= 0)
            case (extra_info_put)
               if (flg) then
                  s% extra_iwork(i) = 1
               else
                  s% extra_iwork(i) = 0
               end if
            end select
         end subroutine move_flg
      
      end subroutine move_extra_info

      subroutine my_other_D_mix11(id, ierr)
         use num_lib, only: two_piece_linear_coeffs
         integer, intent(in) :: id
         integer, intent(out) :: ierr

         type (star_info), pointer :: s

         real(dp) :: dq0, dq_outer, rho_0, T_0, D_He_0
         real(dp) :: x, x0, x1, x2, a0, a1, a2
         integer :: i, loc, n_conv_bdy, nz
         logical :: dbg

         include 'formats'

         dbg = .false.

         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) then
            return
         endif

         if (s% x_ctrl(1) <= 0) return

         ! get reference density (rho_0), temperature (T_0), and atomic diffusion
         ! coefficient for He (D_He_0) at a depth where M_* - M_r = outer mixed mass.
         dq0 = s% x_ctrl(2)*msun/s% mstar
         dq_outer = 0.d0
         nz = s% nz
         do i = 2, nz
            dq_outer = dq_outer + s% dq(i-1)
            if (dq_outer >= dq0) then
               x0 = s% dq(i-1)/2
               x1 = s% dq(i-1) + s% dq(i)/2
               x2 = s% dq(i-1) + s% dq(i) + s% dq(i+1)/2
               x = s% dq(i-1) - (dq_outer - dq0)
               call two_piece_linear_coeffs(x, x0, x1, x2, a0, a1, a2, ierr)
               if (ierr /= 0) return
               rho_0 = exp(a0*s% lnd(i-1) + a1*s% lnd(i) + a2*s% lnd(i+1))
               T_0 = exp(a0*s% lnT(i-1) + a1*s% lnT(i) + a2*s% lnT(i+1))
               D_He_0 = 3.3d-15 * T_0**2.5/&
                        (4*rho_0*log(1+(1.125d-16*T_0**3/rho_0)))
               exit
             endif
         enddo

         n_conv_bdy = s% num_conv_boundaries
         do i=1, n_conv_bdy

            loc = s% conv_bdy_loc(i)
            if (loc < 1 .or. loc > nz) then
               write(*,*) 'bad s% conv_bdy_loc(i)', i, s% conv_bdy_loc(i)
               ierr = -1
               return
            end if

            if (s% top_conv_bdy(i)) cycle


            call do_turbulent_diff11(ierr)
            if (ierr /= 0) return

            if (dbg) write(*,*)

         end do


         contains


         subroutine do_turbulent_diff11(ierr)
            integer, intent(out) :: ierr

            real(dp) :: alfa, beta, rho_face, Hp, D, cdc, vc, P_face, &
               r_face, q_face
            integer :: k
            logical :: dbg
            include 'formats'

            dbg = .false. ! r_edge < 0.7*Rsun .and. r_edge > 0.6*Rsun

            ierr = 0

            do k=loc, nz

               alfa = s% dq(k-1)/(s% dq(k-1) + s% dq(k))
               beta = 1 - alfa
               rho_face = alfa*s% rho(k) + beta*s% rho(k-1)
               D = s% x_ctrl(1)*D_He_0*(rho_0/rho_face)**s% x_integer_ctrl(1)

               if (D < s% D_mix_ov_limit) exit

               P_face = alfa*s% P(k) + beta*s% P(k-1)
               r_face = s% r(k)
               q_face = s% q(k)
               cdc = (pi4*s% r(k)*s% r(k)*rho_face)**2*D ! gm^2/sec
               Hp = P_face/(rho_face*s% cgrav(k)*(s% M_center + s% xmstar*q_face)/(r_face*r_face))
               vc = 3*D/(s% alpha_mlt(k)*Hp)

               if (s% mixing_type(k) == convective_mixing) then
                  ! leave it convective type, but change mixing coeff
                  s% cdc(k) = cdc
                  s% D_mix(k) = D
                  s% conv_vel(k) = vc
               else if (s% mixing_type(k) == no_mixing) then ! change to overshooting
                  s% cdc(k) = cdc
                  s% D_mix(k) = D
                  s% conv_vel(k) = vc
                  s% mixing_type(k) = overshoot_mixing
               else if (s% mixing_type(k) == overshoot_mixing) then
                  ! have encountered overshooting from another cz; add effects
                  s% cdc(k) = s% cdc(k) + cdc
                  s% D_mix(k) = s% D_mix(k) + D
                  s% conv_vel(k) = s% conv_vel(k) + vc
               else ! have run into a different mixing region
                  exit
               end if

            end do

         end subroutine do_turbulent_diff11

      end subroutine my_other_D_mix11


      end module run_star_extras
