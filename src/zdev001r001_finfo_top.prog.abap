*&---------------------------------------------------------------------*
*&  Include           ZDEV001R001_TOP
*&---------------------------------------------------------------------*


"SELECT-OPTIONS 데이터--------------------------------------------------
DATA: GV_FNAME  TYPE FUPARAREF-FUNCNAME,
      GV_PTYPE  TYPE FUPARAREF-PARAMTYPE,
      GV_FLANGU TYPE SY-LANGU,
      GV_PLANGU TYPE SY-LANGU.
"-----------------------------------------------------------------------


"ALV용 데이터-----------------------------------------------------------
TYPES : BEGIN OF TY_FUNCTION.
TYPES : FUNCNAME    TYPE FUPARAREF-FUNCNAME,   "펑션모듈 명
        PSORT       TYPE I,                    "파라미터 타입 정렬순서
        PPOSITION   TYPE FUPARAREF-PPOSITION,  "파라미터      정렬순서
        POSITION    TYPE DD03L-POSITION,       "테이블        정렬순서
        PARAMTYPE   TYPE FUPARAREF-PARAMTYPE,  "파라미터 타입
        PARAMTYPE_T TYPE CHAR20,               "파라미터 타입 텍스트
        PARAMETER   TYPE FUPARAREF-PARAMETER,  "파라미터
        STRUCTURE   TYPE FUPARAREF-STRUCTURE,  "필드명
        DATATYPE    TYPE DD04L-DATATYPE,       "데이터 타입
        LENG        TYPE DD04L-LENG,           "길이
        DECIMALS    TYPE DD04L-DECIMALS,       "소수점
        DEFAULTVAL  TYPE FUPARAREF-DEFAULTVAL, "기본값
        OPTIONAL    TYPE FUPARAREF-OPTIONAL,   "옵션여부
        TYPE        TYPE CHAR15,               "TYPE/TYPE REF TO/LIKE
        STEXT       TYPE FUNCT-STEXT,          "파라미터 내역
        "---DECORATION---
        COLOR       TYPE LVC_T_SCOL.           "색상 표기
TYPES:  END   OF TY_FUNCTION,
TT_FUNCTION TYPE STANDARD TABLE OF TY_FUNCTION.
DATA : GT_FUNCTION TYPE TT_FUNCTION.
"-----------------------------------------------------------------------


"ALV Create-------------------------------------------------------------

DATA : GV_TITLE TYPE STRING. "ALV 타이틀
DATA : GV_OK TYPE SY-UCOMM.

DATA : GO_CON         TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
       GO_ALV         TYPE REF TO CL_GUI_ALV_GRID.
DATA : GO_CON_TOP     TYPE REF TO CL_GUI_CONTAINER,
       GO_CON_BOT     TYPE REF TO CL_GUI_CONTAINER,
       GO_SPLIT       TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
       GO_DYNDOC_ID   TYPE REF TO CL_DD_DOCUMENT,
       GO_HTML_CNTRL  TYPE REF TO CL_GUI_HTML_VIEWER.

DATA : GT_FCAT TYPE LVC_T_FCAT,
       GS_LAYO TYPE LVC_S_LAYO,
       GS_DISV TYPE DISVARIANT,
       GT_EXCL TYPE UI_FUNCTIONS,
       GT_SORT TYPE LVC_T_SORT.
"-----------------------------------------------------------------------
