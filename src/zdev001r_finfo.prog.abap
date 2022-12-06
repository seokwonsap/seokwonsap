***********************************************************************
* Program ID  : ZDEV001R0001
* Title       : [CM] 펑션모듈 파라미터 정보 다운로드
* Created By  : Jeon Seokwon
* Created On  : 2022-12-01
* Frequency   : N/A
* Category    : TEST
* Description : Function Module Parameter Info Excel Download Program
************************************************************************
* Change History.
*-----------------------------------------------------------------------
* Logic will be Appended.
*-----------------------------------------------------------------------

************************************************************************
* Mod. # |Date        |Developer |Description(Reason)
************************************************************************
*        | 2022-12-01 | ZDEV001  | inital coding
************************************************************************

REPORT ZDEV001R_FINFO.

INCLUDE ZDEV001R001_FINFO_TOP.
*INCLUDE ZDEV001R001_TOP.
INCLUDE ZDEV001R001_FINFO_SCR.
*INCLUDE ZDEV001R001_SCR.
INCLUDE ZDEV001R001_FINFO_CLS.
*INCLUDE ZDEV001R001_CLS.
INCLUDE ZDEV001R001_FINFO_O01.
*INCLUDE ZDEV001R001_O01.
INCLUDE ZDEV001R001_FINFO_I01.
*INCLUDE ZDEV001R001_I01.
INCLUDE ZDEV001R001_FINFO_F01.
*INCLUDE ZDEV001R001_F01.

LOAD-OF-PROGRAM.

INITIALIZATION.
  GV_TITLE = |Function Module Parameters|.

AT SELECTION-SCREEN OUTPUT.

AT SELECTION-SCREEN.

START-OF-SELECTION.
  PERFORM GET_DATA.

END-OF-SELECTION.
  CALL SCREEN '0100'.
