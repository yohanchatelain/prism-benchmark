!Copyright 2017 R. CARPENTIER, P. CORDE, F. JEZEQUEL, J.-L. LAMOTTE
!
!This file is part of CADNA.
!
!    CADNA is free software: you can redistribute it and/or modify it
!    under the terms of the GNU Lesser General Public License as
!    published by the Free Software Foundation, either version 3 of the
!    License, or (at your option) any later version.
!
!    CADNA is distributed in the hope that it will be useful, but WITHOUT
!    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
!    or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General
!    Public License for more details.
!
!    You should have received a copy of the GNU Lesser General Public
!    License along with CADNA. If not, see
!    <http://www.gnu.org/licenses/>.
!



! Module de surcharge de l'op�rateur unaire "-"




module cadna_cmplx_opp
  use cadna_types
  use cadna_opp
  implicit none




  interface operator(-)
    module procedure opp_complex_single_st
  end interface operator(-)

  interface operator(-)
    module procedure opp_complex_double_st
  end interface operator(-)


contains



  elemental function opp_complex_single_st(a)
    type(complex_single_st), intent(in) :: a
    type(complex_single_st) opp_complex_single_st

    opp_complex_single_st%x = -a%x
    opp_complex_single_st%y = -a%y
  end function opp_complex_single_st

  elemental function opp_complex_double_st(a)
    type(complex_double_st), intent(in) :: a
    type(complex_double_st) opp_complex_double_st

    opp_complex_double_st%x = -a%x
    opp_complex_double_st%y = -a%y
  end function opp_complex_double_st




end module cadna_cmplx_opp

