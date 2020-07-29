!--- J. Campbell, June 25th, 2008.
!--- Ivan Pogrebnyak, July 2020

!--- Generic C interface for ntuple output for MCFM
!--- In particular, allowing easy interface with any ROOT version

      subroutine bookfill(tag,p,wt) ! This routine is called by nplotter
        implicit none
        include 'types.f'
        include 'mxpart.f'
        include 'maxwt.f'

        integer  :: tag
        real(dp) :: p(mxpart,4)
        real(dp) :: wt

        ! TODO: something is not intitialized here

        if (.not.skipnt) then
          if (tag == 1) then
            call mcfm_ntuple_book
          else if (tag == 2) then
            call mcfm_ntuple_fill(p,wt)
          endif
        endif
      end subroutine

      subroutine NTfinalize
        ! This routine is called at the end of the program's execution;
        ! it should finalize the output and close opened files, if necessary
        use iso_c_binding
        implicit none

        real(c_double) pfill(105)
        type(c_ptr) :: ntuple
        common/ntuple_interface/ntuple,pfill

        write(*,*) " * * * finalizing"

        call close_ntuple(ntuple)
      end subroutine

      subroutine mcfm_ntuple_book
        use iso_c_binding
        implicit none
        include 'types.f'
        include 'npart.f'
        include 'mxdim.f'
        include 'scale.f'
        include 'facscale.f'
        include 'PDFerrors.f'
        include 'kpart.f'

        ! Extra definitions to facilitate dummy call to lowint
        real(dp):: scale_store, facscale_store
        real(dp):: dummy, wgt, r(mxdim), lowint
        integer:: i, imaxmom, ipdf
        common/iarray/imaxmom,ipdf
        integer:: lenocc
        character(len=255) :: runname
        common/runname/runname

        ! assume at most 10 final state particles and 60 PDF sets
        real(c_double) pfill(105)
        type(c_ptr) :: ntuple
        common/ntuple_interface/ntuple,pfill
        type(c_ptr) :: open_ntuple
        character(len=16) :: branch_name

        logical:: first
        data first/.true./
        save first

        ! Need to ascertain the correct size for momenta n-tuples when this routine
        ! is called for the first time, achieved via a dummy call to lowint
        if (first) then
          do i = 1, mxdim
            r(i) = 0.5_dp
          end do
          ! Be careful that dynamic scale choices aren't ruined
          ! (in versions 5.1 and before, this occured when calling lowint)
          scale_store = scale
          facscale_store = facscale
          dummy = lowint(r,wgt)
          scale = scale_store
          facscale = facscale_store

          imaxmom = npart
          if ((kpart == kreal).or.(kpart == ktota).or.(kpart == ktodk)) then
            imaxmom = imaxmom+1
          endif

          first = .false.
        endif

        ! determine if we need space in array to store PDF weights (ipdf)
        ipdf = 0
        if (PDFerrors) then
          ipdf = maxPDFsets
        endif

        ! open ntuple file
        call load_ntuple_lib("./ntuples.so"//C_NULL_CHAR)
        ntuple = open_ntuple(runname(1:lenocc(runname))//".root"//C_NULL_CHAR)

        ! create ntuple branches
        do i = 0, imaxmom-1
          write(branch_name,"(A2I1A)") "px", i+3, C_NULL_CHAR
          call add_ntuple_branch_double(ntuple,branch_name,pfill(i*4+1))
          write(branch_name,"(A2I1A)") "py", i+3, C_NULL_CHAR
          call add_ntuple_branch_double(ntuple,branch_name,pfill(i*4+2))
          write(branch_name,"(A2I1A)") "pz", i+3, C_NULL_CHAR
          call add_ntuple_branch_double(ntuple,branch_name,pfill(i*4+3))
          write(branch_name,"(A2I1A)") "E_", i+3, C_NULL_CHAR
          call add_ntuple_branch_double(ntuple,branch_name,pfill(i*4+4))
        end do

        call add_ntuple_branch_double(
     &    ntuple, 'wt_ALL'//C_NULL_CHAR, pfill(imaxmom*4+1))
        call add_ntuple_branch_double(
     &    ntuple, 'wt_gg' //C_NULL_CHAR, pfill(imaxmom*4+2))
        call add_ntuple_branch_double(
     &    ntuple, 'wt_gq' //C_NULL_CHAR, pfill(imaxmom*4+3))
        call add_ntuple_branch_double(
     &    ntuple, 'wt_qq' //C_NULL_CHAR, pfill(imaxmom*4+4))
        call add_ntuple_branch_double(
     &    ntuple, 'wt_qqb'//C_NULL_CHAR, pfill(imaxmom*4+5))

        do i = 1, ipdf
          write(branch_name,"(A3I2A)") "PDF", i, C_NULL_CHAR
          call add_ntuple_branch_double(
     &      ntuple, branch_name, pfill(imaxmom*4+5+i))
        end do
      end subroutine

      subroutine mcfm_ntuple_fill(p,wt)
        use iso_c_binding
        implicit none
        include 'types.f'
        include 'mxpart.f'
        include 'wts_bypart.f'
        include 'PDFerrors.f'

        real(dp):: p(mxpart,4)
        real(dp):: wt

        ! Extra common block to carry the information about maximum momenta entries
        integer:: i, imaxmom, ipdf
        common/iarray/imaxmom, ipdf

        ! assume at most 10 final state particles and 60 PDF sets
        real(c_double) pfill(105)
        type(c_ptr) :: ntuple
        common/ntuple_interface/ntuple,pfill

        ! If the event weight is zero, don't bother to add the n-tuple
        if (wt == 0._dp) return

        do i = 0, imaxmom-1
          pfill(4*i+1) = p(i+3,1)
          pfill(4*i+2) = p(i+3,2)
          pfill(4*i+3) = p(i+3,3)
          pfill(4*i+4) = p(i+3,4)
        end do

        ! set up single precision variables for the event weights
        pfill(imaxmom*4+1) = wt
        pfill(imaxmom*4+2) = wt_gg
        pfill(imaxmom*4+3) = wt_gq
        pfill(imaxmom*4+4) = wt_qq
        pfill(imaxmom*4+5) = wt_qqb

        ! include PDF errors if necessary
        if (PDFerrors) then
          do i = 1, ipdf
            pfill(imaxmom*4+5+i) = wt*PDFwgt(i)/PDFwgt(0)
          end do
        endif

        call fill_ntuple(ntuple) ! add ntuple entry
      end subroutine

! ===================================================================
      ! dummy routines, as in dsw_dummy

      subroutine dswhbook(n,titlex,dx,xmin,xmax)
        implicit none
        include 'types.f'

        integer:: n
        character titlex*8
        real(dp)::dx,xmin,xmax

        call dsw_error
      end subroutine

      subroutine dswhfill(n,var,wgt)
        implicit none
        include 'types.f'

        integer:: n
        real(dp)::var,wgt

        call dsw_error
      end subroutine

      subroutine dsw_error
        implicit none

        write(6,*) 'This version of MCFM has not been compiled for'
        write(6,*) 'ROOT output only; DSW-style histograms are not'
        write(6,*) 'available.'
        write(6,*)

        stop
      end subroutine

