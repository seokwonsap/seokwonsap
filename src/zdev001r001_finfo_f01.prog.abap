*&---------------------------------------------------------------------*
*&  Include           ZDEV001R001_F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
FORM GET_DATA.

  CLEAR : GT_FUNCTION.

  SELECT A~FUNCNAME,    "Function
         CASE PARAMTYPE "Sort Number For Parameter Type
           WHEN 'I' THEN 1
           WHEN 'E' THEN 2
           WHEN 'C' THEN 3
           WHEN 'T' THEN 4
         END AS PSORT,
         A~PARAMTYPE,   "Parameter Type(Import, Export, Changing, Table)
         A~PPOSITION,   "Parameter Position
         A~PARAMETER,   "Parameter
         A~STRUCTURE,   "Associated Type
         A~DEFAULTVAL,  "Default Value
         CASE PARAMTYPE "Optional
           WHEN 'E'    THEN '-'        "Export인 경우 '-'로 표시
           ELSE ( CASE OPTIONAL        "이 외에는 ■ 또는 공백
                    WHEN 'X' THEN '■'
                    ELSE ' '
                  END )
         END AS OPTIONAL,
         A~TYPE,         "TYPE/LIKE
         A~REF_CLASS     "TYPE_REF_TO/LIKE
    FROM            FUPARAREF   AS A
    WHERE A~FUNCNAME  IN @S_FNAME AND
          A~PARAMTYPE IN @S_PTYPE AND
          A~R3STATE   EQ 'A'
    INTO TABLE @DATA(LT_FINFO).

  IF LT_FINFO IS NOT INITIAL.
    "정렬
    SORT LT_FINFO BY FUNCNAME  ASCENDING  "Function Name
                     PSORT     ASCENDING  "Parameter Type Sort
                     PPOSITION ASCENDING. "Parameter Name

    "텍스트 데이터
    SELECT FUNCNAME,
           PARAMETER,
           SPRAS,
           KIND,
           VERSION,
           STEXT
      FROM FUNCT
      FOR ALL ENTRIES IN @LT_FINFO
      WHERE SPRAS     IN @S_FLANGU           AND
            FUNCNAME  EQ @LT_FINFO-FUNCNAME  AND
            PARAMETER EQ @LT_FINFO-PARAMETER AND
            KIND      EQ 'P'
      INTO TABLE @DATA(LT_FINFO_TEXT).
    SORT LT_FINFO_TEXT BY FUNCNAME  ASCENDING
                          PARAMETER ASCENDING
                          SPRAS     ASCENDING.

    TYPES : BEGIN OF TY_STR.
      TYPES : STRUCTURE TYPE FUPARAREF-STRUCTURE.
    TYPES : END   OF TY_STR.
    DATA : LT_STR TYPE TABLE OF TY_STR.

   LOOP AT LT_FINFO ASSIGNING FIELD-SYMBOL(<FINFO>)
                    GROUP BY ( STRUCTURE = <FINFO>-STRUCTURE )
                    ASCENDING
                    ASSIGNING FIELD-SYMBOL(<FINFO_G>).
     LOOP AT GROUP <FINFO_G> ASSIGNING FIELD-SYMBOL(<FINFO_L>).
       IF <FINFO_L>-STRUCTURE CS '-'.
         SPLIT <FINFO_L>-STRUCTURE AT '-' INTO DATA(LV_TABLE)
                                               DATA(LV_FIELD).
         APPEND LV_TABLE TO LT_STR.
       ELSE.
         APPEND <FINFO_L>-STRUCTURE TO LT_STR.
       ENDIF.
     ENDLOOP.
   ENDLOOP.
   SORT LT_STR BY STRUCTURE ASCENDING.
   DELETE ADJACENT DUPLICATES FROM LT_STR COMPARING STRUCTURE.

    IF LT_STR IS NOT INITIAL.
      "Table & Structure: DD03L -> LT_DD03L
      SELECT A~TABNAME,   "KEY: Table
             A~FIELDNAME, "KEY: Component
             A~AS4LOCAL,  "KEY:
             A~AS4VERS,   "KEY:
             A~POSITION,  "KEY: Sort
             A~ROLLNAME,  "Component Type
             A~PRECFIELD, "Structure Name
             A~DATATYPE,  "DataType
             A~LENG,      "Length
             A~DECIMALS,  "Decimal
             A~COMPTYPE   "Component Type
        FROM            DD03L AS A
        FOR ALL ENTRIES IN @LT_STR
        WHERE A~TABNAME    EQ @LT_STR-STRUCTURE+0(30) AND
              A~AS4LOCAL   EQ 'A' "Active
              "A~COMPTYPE NE 'S'
        INTO TABLE @DATA(LT_DD03L).
      SORT LT_DD03L BY TABNAME   ASCENDING
                       FIELDNAME ASCENDING.

      IF LT_DD03L IS NOT INITIAL.
        SELECT ROLLNAME,
               DDLANGUAGE,
               AS4LOCAL,
               AS4VERS,
               DDTEXT
          FROM DD04T
          FOR ALL ENTRIES IN @LT_DD03L
          WHERE ROLLNAME   EQ @LT_DD03L-ROLLNAME AND
                DDLANGUAGE IN @S_PLANGU          AND
                AS4LOCAL   EQ @LT_DD03L-AS4LOCAL AND
                AS4VERS    EQ @LT_DD03L-AS4VERS
          INTO TABLE @DATA(LT_DD04T).
        SORT LT_DD04T BY ROLLNAME    ASCENDING
                         AS4LOCAL    ASCENDING  "Active
                         AS4VERS     DESCENDING
                         DDLANGUAGE  ASCENDING.

        SELECT TABNAME,
               FIELDNAME,
               AS4LOCAL,
               DDLANGUAGE,
               DDTEXT
          FROM DD03T
          FOR ALL ENTRIES IN @LT_DD03L
          WHERE TABNAME    EQ @LT_DD03L-TABNAME AND
                DDLANGUAGE IN @S_PLANGU          AND
                AS4LOCAL   EQ @LT_DD03L-AS4LOCAL AND
                FIELDNAME  EQ @LT_DD03L-FIELDNAME
          INTO TABLE @DATA(LT_DD03T).
        SORT LT_DD03T BY TABNAME    ASCENDING
                         FIELDNAME  ASCENDING
                         AS4LOCAL   ASCENDING
                         DDLANGUAGE ASCENDING.
      ENDIF.

      "Data Element: DD04L -> LT_DD04L
      SELECT A~ROLLNAME, "KEY
             A~AS4LOCAL, "KEY
             A~AS4VERS,  "KEY
             A~DATATYPE,
             A~LENG,
             A~DECIMALS
        FROM       DD04L AS A
        FOR ALL ENTRIES IN @LT_STR
        WHERE A~ROLLNAME   EQ @LT_STR-STRUCTURE+0(30) AND
              A~AS4LOCAL   EQ 'A' "Active
        INTO TABLE @DATA(LT_DD04L).
      SORT LT_DD04L BY ROLLNAME ASCENDING.

      "LT_DD04T_OF 는 거의 안쓰임
      IF LT_DD04T IS NOT INITIAL.
        SELECT ROLLNAME,
               AS4LOCAL,
               AS4VERS,
               DDLANGUAGE,
               DDTEXT
          FROM DD04T
          FOR ALL ENTRIES IN @LT_DD04T
          WHERE ROLLNAME   EQ @LT_DD04T-ROLLNAME AND
                DDLANGUAGE IN @S_PLANGU          AND
                AS4LOCAL   EQ @LT_DD04T-AS4LOCAL AND
                AS4VERS    EQ @LT_DD04T-AS4VERS
          INTO TABLE @DATA(LT_DD04T_OF). "Only Field
          SORT LT_DD04T_OF BY ROLLNAME   ASCENDING
                              AS4LOCAL   ASCENDING  "A만 들어옴
                              AS4VERS    DESCENDING "버전은 최신버전
                              DDLANGUAGE ASCENDING.
      ENDIF.
    ENDIF.

    DATA : LS_FUNCTION LIKE LINE OF GT_FUNCTION.
    DATA : LS_COLOR TYPE LVC_S_SCOL.
    LOOP AT LT_FINFO ASSIGNING <FINFO>.

      CLEAR : LS_FUNCTION.

      LS_FUNCTION-FUNCNAME   = <FINFO>-FUNCNAME.
      LS_FUNCTION-PARAMTYPE  = <FINFO>-PARAMTYPE.
      LS_FUNCTION-PARAMETER  = <FINFO>-PARAMETER.
      LS_FUNCTION-STRUCTURE  = <FINFO>-STRUCTURE.
      LS_FUNCTION-DEFAULTVAL = <FINFO>-DEFAULTVAL.
      LS_FUNCTION-OPTIONAL   = <FINFO>-OPTIONAL.
      LS_FUNCTION-PSORT      = <FINFO>-PSORT.
      LS_FUNCTION-PPOSITION  = <FINFO>-PPOSITION.

      "Function Module에 입력된 파라미터 텍스트를 먼저 입력
      READ TABLE LT_FINFO_TEXT ASSIGNING FIELD-SYMBOL(<FINFO_TEXT>)
                               WITH KEY FUNCNAME  = <FINFO>-FUNCNAME
                                        PARAMETER = <FINFO>-PARAMETER
                               BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        LS_FUNCTION-STEXT = <FINFO_TEXT>-STEXT.
      ELSE.
        "없다면 Field 기준으로 입력
        READ TABLE LT_DD04T_OF ASSIGNING FIELD-SYMBOL(<DD04T_OF>)
                               WITH KEY ROLLNAME = <FINFO>-STRUCTURE
                                        "AS4LOCAL = 'A' "Active
                               BINARY SEARCH.
        IF SY-SUBRC EQ 0.
          LS_FUNCTION-STEXT = <DD04T_OF>-DDTEXT.
        ENDIF.
      ENDIF.

      IF <FINFO>-TYPE IS NOT INITIAL.
        LS_FUNCTION-TYPE = |TYPE|.
      ELSE.
        IF <FINFO>-REF_CLASS IS NOT INITIAL.
          LS_FUNCTION-TYPE = |TYPE REF TO|.
        ELSE.
          LS_FUNCTION-TYPE = |LIKE|.
        ENDIF.
      ENDIF.

      CASE LS_FUNCTION-PARAMTYPE.
        WHEN 'I'.
          LS_FUNCTION-PARAMTYPE_T = 'Import'.
        WHEN 'E'.
          LS_FUNCTION-PARAMTYPE_T = 'Export'.
        WHEN 'C'.
          LS_FUNCTION-PARAMTYPE_T = 'Changing'.
        WHEN 'T'.
          LS_FUNCTION-PARAMTYPE_T = 'Tables'.
      ENDCASE.


      " MARA-MATNR 처럼 '-' 포함 시
      IF LS_FUNCTION-STRUCTURE CS '-'.
        CLEAR : LV_TABLE,
                LV_FIELD.
        SPLIT LS_FUNCTION-STRUCTURE AT '-' INTO LV_TABLE
                                                LV_FIELD.
        READ TABLE LT_DD03L ASSIGNING FIELD-SYMBOL(<DD03L>)
                            WITH KEY TABNAME   = LV_TABLE
                                     FIELDNAME = LV_FIELD
                            BINARY SEARCH.
        IF SY-SUBRC EQ 0.
          LS_FUNCTION-DATATYPE  = <DD03L>-DATATYPE.
          LS_FUNCTION-LENG      = <DD03L>-LENG.
          LS_FUNCTION-DECIMALS  = <DD03L>-DECIMALS.
          LS_FUNCTION-POSITION  = <DD03L>-POSITION.
        ENDIF.
        APPEND LS_FUNCTION TO GT_FUNCTION.
      " '-' 미포함시
      ELSE.
        "데이터 엘리먼트 검색
        READ TABLE LT_DD04L ASSIGNING FIELD-SYMBOL(<DD04L>)
                            WITH KEY ROLLNAME = <FINFO>-STRUCTURE
                            BINARY SEARCH.
        IF SY-SUBRC EQ 0.
          LS_FUNCTION-DATATYPE = <DD04L>-DATATYPE.
          LS_FUNCTION-LENG     = <DD04L>-LENG.
          LS_FUNCTION-DECIMALS = <DD04L>-DECIMALS.
        ENDIF.
        IF LS_FUNCTION-DATATYPE IS INITIAL.
          LS_COLOR-COLOR-COL = 3.
          LS_COLOR-COLOR-INT = 1.
          LS_COLOR-COLOR-INV = 0.
          LS_COLOR-FNAME     = 'STRUCTURE'.
          INSERT LS_COLOR INTO TABLE LS_FUNCTION-COLOR.
          LS_COLOR-FNAME     = 'PARAMETER'.
          INSERT LS_COLOR INTO TABLE LS_FUNCTION-COLOR.
        ENDIF.
        APPEND LS_FUNCTION TO GT_FUNCTION.
        "테이블 & 스트럭처 검색
        READ TABLE LT_DD03L ASSIGNING FIELD-SYMBOL(<DD03L_CHECK>)
                            WITH KEY TABNAME = <FINFO>-STRUCTURE
                            BINARY SEARCH.
        IF SY-SUBRC EQ 0.
          DATA(LV_TABIX) = SY-TABIX.
          CLEAR : LS_FUNCTION-OPTIONAL,
                  LS_FUNCTION-COLOR.
          LOOP AT LT_DD03L ASSIGNING <DD03L>
                           FROM LV_TABIX
                           WHERE TABNAME EQ <DD03L_CHECK>-TABNAME.
            CLEAR : LS_FUNCTION-PARAMETER,
                    LS_FUNCTION-STRUCTURE,
                    LS_FUNCTION-DATATYPE,
                    LS_FUNCTION-LENG,
                    LS_FUNCTION-DECIMALS,
                    LS_FUNCTION-STEXT.

            LS_FUNCTION-PARAMETER = <DD03L>-FIELDNAME.
            LS_FUNCTION-STRUCTURE = <DD03L>-ROLLNAME.
            LS_FUNCTION-DATATYPE  = <DD03L>-DATATYPE.
            LS_FUNCTION-LENG      = <DD03L>-LENG.
            LS_FUNCTION-DECIMALS  = <DD03L>-DECIMALS.
            LS_FUNCTION-POSITION  = <DD03L>-POSITION.
            IF <DD03L>-ROLLNAME IS NOT INITIAL.
              READ TABLE LT_DD04T ASSIGNING FIELD-SYMBOL(<DD04T>)
                                  WITH KEY ROLLNAME = <DD03L>-ROLLNAME
                                           AS4LOCAL = <DD03L>-AS4LOCAL
                                           AS4VERS  = <DD03L>-AS4VERS
                                  BINARY SEARCH.
              IF SY-SUBRC EQ 0.
                LS_FUNCTION-STEXT     = <DD04T>-DDTEXT.
              ENDIF.
            ELSE.
              READ TABLE LT_DD03T ASSIGNING FIELD-SYMBOL(<DD03T>)
                                  WITH KEY TABNAME   = <DD03L>-TABNAME
                                           FIELDNAME = <DD03L>-FIELDNAME
                                           AS4LOCAL  = <DD03L>-AS4LOCAL
                                  BINARY SEARCH.
              IF SY-SUBRC EQ 0.
                LS_FUNCTION-STEXT     = <DD03T>-DDTEXT.
              ENDIF.
            ENDIF.
            ".Include 제외 시
            IF <DD03L>-COMPTYPE EQ 'S'.
              IF P_NOINC IS NOT INITIAL.
                CONTINUE.
              ELSE.
                LS_FUNCTION-STRUCTURE = <DD03L>-PRECFIELD.
              ENDIF.
            ENDIF.
            APPEND LS_FUNCTION TO GT_FUNCTION.
          ENDLOOP.
        ENDIF. " READ LT_DD03L SUBRC EQ 0
      ENDIF. "END CS '-'
    ENDLOOP.

  ENDIF.

  SORT GT_FUNCTION BY FUNCNAME  ASCENDING  "펑션명
                      PSORT     ASCENDING  "I/E/C/T 정렬
                      PPOSITION ASCENDING  "파라미터 입력 순서 정렬
                      POSITION  ASCENDING. "테이블 필드 입력 순서 정렬

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  START_ALV  OUTPUT
*&---------------------------------------------------------------------*
MODULE START_ALV OUTPUT.

  IF GO_CON IS INITIAL.
    PERFORM SET_ALV.
    PERFORM SET_LAYO.
    PERFORM SET_DISV.
    PERFORM SET_FCAT.
    PERFORM SET_EXCL.
    PERFORM SET_SORT.
    PERFORM SET_EVNT.
    PERFORM DISP_ALV.
  ELSE.
    PERFORM REFRESH_ALV.
  ENDIF.



ENDMODULE.


*&---------------------------------------------------------------------*
*& Form SET_ALV
*&---------------------------------------------------------------------*
FORM SET_ALV .
  CREATE OBJECT GO_CON
    EXPORTING
      CONTAINER_NAME              = 'CON'
      REPID                       = SY-REPID
      DYNNR                       = SY-DYNNR
    EXCEPTIONS
      CNTL_ERROR                  = 1
      CNTL_SYSTEM_ERROR           = 2
      CREATE_ERROR                = 3
      LIFETIME_ERROR              = 4
      LIFETIME_DYNPRO_DYNPRO_LINK = 5
      OTHERS                      = 6.
  IF SY-SUBRC EQ 0.

    CREATE OBJECT GO_SPLIT
      EXPORTING
        PARENT            = GO_CON
        ROWS              = 2
        COLUMNS           = 1
      EXCEPTIONS
        CNTL_ERROR        = 1
        CNTL_SYSTEM_ERROR = 2
        OTHERS            = 3.

    GO_SPLIT->GET_CONTAINER(
      EXPORTING
        ROW       = 1
        COLUMN    = 1
      RECEIVING
        CONTAINER = GO_CON_TOP ).

    GO_SPLIT->GET_CONTAINER(
      EXPORTING
        ROW       = 2
        COLUMN    = 1
      RECEIVING
        CONTAINER = GO_CON_BOT ).

    GO_SPLIT->SET_ROW_HEIGHT(
      EXPORTING
        ID                = 1
        HEIGHT            = 5 ).

    CREATE OBJECT GO_ALV
      EXPORTING
        I_PARENT          = GO_CON_BOT
      EXCEPTIONS
        ERROR_CNTL_CREATE = 1
        ERROR_CNTL_INIT   = 2
        ERROR_CNTL_LINK   = 3
        ERROR_DP_CREATE   = 4
        OTHERS            = 5.

* Create TOP-Document
    CREATE OBJECT GO_DYNDOC_ID
      EXPORTING
        STYLE = 'ALV_GRID'.
  ENDIF.

ENDFORM.



*&---------------------------------------------------------------------*
*& Form SET_LAYO
*&---------------------------------------------------------------------*
FORM SET_LAYO.

  CLEAR: GS_LAYO.

  GS_LAYO-CTAB_FNAME = 'COLOR'.
  GS_LAYO-SEL_MODE   = 'D'.
  GS_LAYO-CWIDTH_OPT = 'A'.
  GS_LAYO-COL_OPT    = ABAP_TRUE.
  GS_LAYO-SMALLTITLE = ABAP_TRUE.
  GS_LAYO-NO_ROWINS  = ABAP_TRUE.
  GS_LAYO-NO_ROWMOVE = ABAP_TRUE.
  GS_LAYO-EDIT       = SPACE.
  GS_LAYO-EDIT_MODE  = SPACE.

ENDFORM.



*&---------------------------------------------------------------------*
*& Form SET_DISV
*&---------------------------------------------------------------------*
FORM SET_DISV .
  CLEAR: GS_DISV.
  GS_DISV-REPORT   = SY-REPID.
  GS_DISV-USERNAME = SY-UNAME.
  GS_DISV-HANDLE   = '0001'.
ENDFORM.



*&---------------------------------------------------------------------*
*& Form SET_FCAT
*&---------------------------------------------------------------------*
FORM SET_FCAT .

  CLEAR : GT_FCAT.

  GT_FCAT = LCL_FCAT=>LVC_FCAT_FROM_INTERNAL_TABLE( IT_TABLE = GT_FUNCTION ).

  LOOP AT GT_FCAT ASSIGNING FIELD-SYMBOL(<FCAT>).
    <FCAT>-JUST           = 'C'.
    <FCAT>-NO_ZERO        = 'X'.
    <FCAT>-F4AVAILABL     = ' '.
    CASE <FCAT>-FIELDNAME.
      WHEN 'FUNCNAME   '.
        <FCAT>-SCRTEXT_S  =
        <FCAT>-SCRTEXT_M  =
        <FCAT>-SCRTEXT_L  =
        <FCAT>-SELDDICTXT =
        <FCAT>-COLTEXT    = |Function Module|.
      WHEN 'PSORT    '.
        <FCAT>-NO_OUT     = 'X'.
        <FCAT>-SCRTEXT_S  =
        <FCAT>-SCRTEXT_M  =
        <FCAT>-SCRTEXT_L  =
        <FCAT>-SELDDICTXT =
        <FCAT>-COLTEXT    = |파라미터 타입 정렬|.
      WHEN 'PPOSITION'.
        <FCAT>-NO_OUT     = 'X'.
        <FCAT>-SCRTEXT_S  =
        <FCAT>-SCRTEXT_M  =
        <FCAT>-SCRTEXT_L  =
        <FCAT>-SELDDICTXT =
        <FCAT>-COLTEXT    = |파라미터 정렬|.
      WHEN 'POSITION '.
        <FCAT>-NO_OUT     = 'X'.
        <FCAT>-SCRTEXT_S  =
        <FCAT>-SCRTEXT_M  =
        <FCAT>-SCRTEXT_L  =
        <FCAT>-SELDDICTXT =
        <FCAT>-COLTEXT    = |테이블 정렬|.
      WHEN 'PARAMTYPE  '.
        <FCAT>-NO_OUT     = 'X'.
      WHEN 'PARAMTYPE_T'.
        <FCAT>-SCRTEXT_S  =
        <FCAT>-SCRTEXT_M  =
        <FCAT>-SCRTEXT_L  =
        <FCAT>-SELDDICTXT =
        <FCAT>-COLTEXT    = |항목|.
      WHEN 'PARAMETER  '.
        <FCAT>-JUST       = 'L'.
        <FCAT>-SCRTEXT_S  =
        <FCAT>-SCRTEXT_M  =
        <FCAT>-SCRTEXT_L  =
        <FCAT>-SELDDICTXT =
        <FCAT>-COLTEXT    = |Parameter Name|.
      WHEN 'STRUCTURE  '.
        <FCAT>-JUST       = 'L'.
        <FCAT>-SCRTEXT_S  =
        <FCAT>-SCRTEXT_M  =
        <FCAT>-SCRTEXT_L  =
        <FCAT>-SELDDICTXT =
        <FCAT>-COLTEXT    = |Associated Type|.
      WHEN 'DATATYPE   '.
        <FCAT>-JUST       = 'L'.
        <FCAT>-SCRTEXT_S  =
        <FCAT>-SCRTEXT_M  =
        <FCAT>-SCRTEXT_L  =
        <FCAT>-SELDDICTXT =
        <FCAT>-COLTEXT    = |Data Type|.
      WHEN 'LENG       '.
        <FCAT>-JUST       = 'R'.
        <FCAT>-SCRTEXT_S  =
        <FCAT>-SCRTEXT_M  =
        <FCAT>-SCRTEXT_L  =
        <FCAT>-SELDDICTXT =
        <FCAT>-COLTEXT    = |Length|.
      WHEN 'DECIMALS   '.
        <FCAT>-JUST       = 'R'.
        <FCAT>-SCRTEXT_S  =
        <FCAT>-SCRTEXT_M  =
        <FCAT>-SCRTEXT_L  =
        <FCAT>-SELDDICTXT =
        <FCAT>-COLTEXT    = |Decimal Places|.
      WHEN 'DEFAULTVAL '.
        <FCAT>-JUST       = 'L'.
        <FCAT>-SCRTEXT_S  =
        <FCAT>-SCRTEXT_M  =
        <FCAT>-SCRTEXT_L  =
        <FCAT>-SELDDICTXT =
        <FCAT>-COLTEXT    = |Default value|.
      WHEN 'OPTIONAL   '.
        <FCAT>-SCRTEXT_S  =
        <FCAT>-SCRTEXT_M  =
        <FCAT>-SCRTEXT_L  =
        <FCAT>-SELDDICTXT =
        <FCAT>-COLTEXT    = |Optional|.
      WHEN 'TYPE       '.
        <FCAT>-NO_OUT     = 'X'.
        <FCAT>-SCRTEXT_S  =
        <FCAT>-SCRTEXT_M  =
        <FCAT>-SCRTEXT_L  =
        <FCAT>-SELDDICTXT =
        <FCAT>-COLTEXT    = |TYPE/LIKE|.
      WHEN 'STEXT      '.
        <FCAT>-JUST       = 'L'.
        <FCAT>-SCRTEXT_S  =
        <FCAT>-SCRTEXT_M  =
        <FCAT>-SCRTEXT_L  =
        <FCAT>-SELDDICTXT =
        <FCAT>-COLTEXT    = |Short text|.
      WHEN 'COLOR      '.
        <FCAT>-TECH       = 'X'.
    ENDCASE.
  ENDLOOP.

ENDFORM.



*&---------------------------------------------------------------------*
*& Form SET_EXCL
*&---------------------------------------------------------------------*
FORM SET_EXCL .

  DATA : LS_EXCL TYPE UI_FUNC.

  CLEAR : GT_EXCL.

  LS_EXCL = CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW.
  APPEND LS_EXCL TO GT_EXCL.

  LS_EXCL = CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW.
  APPEND LS_EXCL TO GT_EXCL.

  LS_EXCL = CL_GUI_ALV_GRID=>MC_FC_REFRESH.
  APPEND LS_EXCL TO GT_EXCL.

  LS_EXCL = CL_GUI_ALV_GRID=>MC_FC_SUM.
  APPEND LS_EXCL TO GT_EXCL.

  LS_EXCL = CL_GUI_ALV_GRID=>MC_FC_COUNT.
  APPEND LS_EXCL TO GT_EXCL.

  LS_EXCL = CL_GUI_ALV_GRID=>MC_FC_AVERAGE.
  APPEND LS_EXCL TO GT_EXCL.

  LS_EXCL = CL_GUI_ALV_GRID=>MC_FC_MAXIMUM.
  APPEND LS_EXCL TO GT_EXCL.

  LS_EXCL = CL_GUI_ALV_GRID=>MC_FC_MINIMUM.
  APPEND LS_EXCL TO GT_EXCL.

  LS_EXCL = CL_GUI_ALV_GRID=>MC_FC_SUBTOT.
  APPEND LS_EXCL TO GT_EXCL.

  LS_EXCL = CL_GUI_ALV_GRID=>MC_FC_AUF.
  APPEND LS_EXCL TO GT_EXCL.

  LS_EXCL = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE.
  APPEND LS_EXCL TO GT_EXCL.

  LS_EXCL = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW.
  APPEND LS_EXCL TO GT_EXCL.

  LS_EXCL = CL_GUI_ALV_GRID=>MC_MB_VIEW.
  APPEND LS_EXCL TO GT_EXCL.

  LS_EXCL = CL_GUI_ALV_GRID=>MC_FC_VIEWS.
  APPEND LS_EXCL TO GT_EXCL.

  LS_EXCL = CL_GUI_ALV_GRID=>MC_FC_VIEW_CRYSTAL.
  APPEND LS_EXCL TO GT_EXCL.

  LS_EXCL = CL_GUI_ALV_GRID=>MC_FC_VIEW_LOTUS.
  APPEND LS_EXCL TO GT_EXCL.

  LS_EXCL = CL_GUI_ALV_GRID=>MC_FC_SEND.
  APPEND LS_EXCL TO GT_EXCL.

  LS_EXCL = CL_GUI_ALV_GRID=>MC_FC_CALL_ABC.
  APPEND LS_EXCL TO GT_EXCL.

  LS_EXCL = CL_GUI_ALV_GRID=>MC_FC_CALL_XINT.
  APPEND LS_EXCL TO GT_EXCL.

  LS_EXCL = CL_GUI_ALV_GRID=>MC_FC_EXPCRDESIG.
  APPEND LS_EXCL TO GT_EXCL.

  LS_EXCL = CL_GUI_ALV_GRID=>MC_FC_EXPCRTEMPL.
  APPEND LS_EXCL TO GT_EXCL.

  LS_EXCL = CL_GUI_ALV_GRID=>MC_MB_PASTE.
  APPEND LS_EXCL TO GT_EXCL.

  LS_EXCL = CL_GUI_ALV_GRID=>MC_FC_GRAPH.
  APPEND LS_EXCL TO GT_EXCL.

  LS_EXCL = CL_GUI_ALV_GRID=>MC_FC_INFO.
  APPEND LS_EXCL TO GT_EXCL.

  LS_EXCL = CL_GUI_ALV_GRID=>MC_FC_F4.
  APPEND LS_EXCL TO GT_EXCL.

  LS_EXCL = CL_GUI_ALV_GRID=>MC_FC_CALL_REPORT.
  APPEND LS_EXCL TO GT_EXCL.

  LS_EXCL = CL_GUI_ALV_GRID=>MC_FC_CHECK.
  APPEND LS_EXCL TO GT_EXCL.

  LS_EXCL = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
  APPEND LS_EXCL TO GT_EXCL.

  LS_EXCL = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY.
  APPEND LS_EXCL TO GT_EXCL.

  LS_EXCL = CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO.
  APPEND LS_EXCL TO GT_EXCL.

  LS_EXCL = CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW.
  APPEND LS_EXCL TO GT_EXCL.

  LS_EXCL = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW.
  APPEND LS_EXCL TO GT_EXCL.

  LS_EXCL = CL_GUI_ALV_GRID=>MC_FC_REPREP.
  APPEND LS_EXCL TO GT_EXCL.

  LS_EXCL = CL_GUI_ALV_GRID=>MC_FC_CALL_CHAIN.
  APPEND LS_EXCL TO GT_EXCL.

  LS_EXCL = CL_GUI_ALV_GRID=>MC_FC_CALL_MORE.
  APPEND LS_EXCL TO GT_EXCL.

  LS_EXCL = CL_GUI_ALV_GRID=>MC_FC_CALL_MASTER_DATA.
  APPEND LS_EXCL TO GT_EXCL.

*  LS_EXCL = CL_GUI_ALV_GRID=>MC_FC_PRINT_PREV.
*  APPEND LS_EXCL TO GT_EXCL.

*  LS_EXCL = CL_GUI_ALV_GRID=>MC_FC_VIEW_GRID.
*  APPEND LS_EXCL TO GT_EXCL.

*  LS_EXCL = CL_GUI_ALV_GRID=>MC_FC_VIEW_EXCEL.
*  APPEND LS_EXCL TO GT_EXCL.

*  LS_EXCL = CL_GUI_ALV_GRID=>MC_FC_LOAD_VARIANT.
*  APPEND LS_EXCL TO GT_EXCL.

*  LS_EXCL = CL_GUI_ALV_GRID=>MC_FC_CURRENT_VARIANT.
*  APPEND LS_EXCL TO GT_EXCL.

*  LS_EXCL = CL_GUI_ALV_GRID=>MC_FC_MAINTAIN_VARIANT.
*  APPEND LS_EXCL TO GT_EXCL.

*  LS_EXCL = CL_GUI_ALV_GRID=>MC_FC_SAVE_VARIANT.
*  APPEND LS_EXCL TO GT_EXCL.

*  LS_EXCL = CL_GUI_ALV_GRID=>MC_FC_SELECT_ALL.
*  APPEND LS_EXCL TO GT_EXCL.
*
*  LS_EXCL = CL_GUI_ALV_GRID=>MC_FC_DESELECT_ALL.
*  APPEND LS_EXCL TO GT_EXCL.

ENDFORM.



*&---------------------------------------------------------------------*
*& Form SET_SORT
*&---------------------------------------------------------------------*
FORM SET_SORT .

  DATA : LS_SORT TYPE LVC_S_SORT,
         LV_SPOS TYPE SLIS_SPOS.

  CLEAR : GT_SORT.

*  "Function Module
*  CLEAR LS_SORT.
*  LV_SPOS           = LV_SPOS + 1.
*  LS_SORT-SPOS      = LV_SPOS.
*  LS_SORT-FIELDNAME = 'FUNCNAME'.
*  LS_SORT-UP        = 'X'.
*  LS_SORT-COMP      = 'X'.
*  APPEND LS_SORT TO GT_SORT.
*
*  "항목
*  CLEAR LS_SORT.
*  LV_SPOS           = LV_SPOS + 1.
*  LS_SORT-SPOS      = LV_SPOS.
*  LS_SORT-FIELDNAME = 'PARAMTYPE_T'.
*  LS_SORT-UP        = 'X'.
*  LS_SORT-COMP      = 'X'.
*  APPEND LS_SORT TO GT_SORT.
*
*  "Parameter Name
*  CLEAR LS_SORT.
*  LV_SPOS           = LV_SPOS + 1.
*  LS_SORT-SPOS      = LV_SPOS.
*  LS_SORT-FIELDNAME = 'BTEXT'.
*  LS_SORT-UP        = 'X'.
*  LS_SORT-COMP      = 'X'.
*  APPEND LS_SORT TO GT_SORT.
*
*  "거래키 내역
*  CLEAR LS_SORT.
*  LV_SPOS           = LV_SPOS + 1.
*  LS_SORT-SPOS      = LV_SPOS.
*  LS_SORT-FIELDNAME = 'KTOSL'.
*  LS_SORT-UP        = 'X'.
*  LS_SORT-COMP      = 'X'.
*  APPEND LS_SORT TO GT_SORT.
*
*  "평가클래스
*  CLEAR LS_SORT.
*  LV_SPOS           = LV_SPOS + 1.
*  LS_SORT-SPOS      = LV_SPOS.
*  LS_SORT-FIELDNAME = 'BKLAS'.
*  LS_SORT-UP        = 'X'.
*  LS_SORT-COMP      = 'X'.
*  APPEND LS_SORT TO GT_SORT.
*
*  "계정수정
*  CLEAR LS_SORT.
*  LV_SPOS           = LV_SPOS + 1.
*  LS_SORT-SPOS      = LV_SPOS.
*  LS_SORT-FIELDNAME = 'KOMOK'.
*  LS_SORT-UP        = 'X'.
*  LS_SORT-COMP      = 'X'.
*  APPEND LS_SORT TO GT_SORT.

ENDFORM.



*&---------------------------------------------------------------------*
*& Form SET_EVNT
*&---------------------------------------------------------------------*
FORM SET_EVNT .

  CREATE OBJECT GR_EVENT_HANDLER.

  SET HANDLER GR_EVENT_HANDLER->HANDLE_TOP_OF_PAGE        FOR GO_ALV.

ENDFORM.



*&---------------------------------------------------------------------*
*& Form DISP_ALV
*&---------------------------------------------------------------------*
FORM DISP_ALV .

  CALL METHOD GO_ALV->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_VARIANT                    = GS_DISV
      I_SAVE                        = 'A'
      IS_LAYOUT                     = GS_LAYO
      IT_TOOLBAR_EXCLUDING          = GT_EXCL
    CHANGING
      IT_OUTTAB                     = GT_FUNCTION
      IT_FIELDCATALOG               = GT_FCAT
      "IT_SORT                       = GT_SORT
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      OTHERS                        = 4.

  CALL METHOD GO_DYNDOC_ID->INITIALIZE_DOCUMENT
    EXPORTING
      BACKGROUND_COLOR = CL_DD_AREA=>COL_TEXTAREA.

  CALL METHOD GO_ALV->LIST_PROCESSING_EVENTS
    EXPORTING
      I_EVENT_NAME = 'TOP_OF_PAGE'
      I_DYNDOC_ID  = GO_DYNDOC_ID.

ENDFORM.



*&---------------------------------------------------------------------*
*& Form REFRESH_ALV
*&---------------------------------------------------------------------*
FORM REFRESH_ALV .

  " { DEFINITION
  DATA : LS_SCROLL     TYPE LVC_S_STBL,
         LT_INDEX_ROWS TYPE LVC_T_ROW.
  LS_SCROLL-ROW = 'X'.
  LS_SCROLL-COL = 'X'.
  " }

  CALL METHOD GO_ALV->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = LT_INDEX_ROWS.

  CALL METHOD GO_ALV->SET_FRONTEND_LAYOUT
    EXPORTING
      IS_LAYOUT = GS_LAYO.

  CALL METHOD GO_ALV->SET_FRONTEND_FIELDCATALOG
    EXPORTING
      IT_FIELDCATALOG = GT_FCAT.

  CALL METHOD GO_ALV->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE      = LS_SCROLL
      I_SOFT_REFRESH = ''
    EXCEPTIONS
      FINISHED       = 1
      OTHERS         = 2.

  CALL METHOD GO_ALV->SET_SELECTED_ROWS
    EXPORTING
      IT_INDEX_ROWS = LT_INDEX_ROWS.


  CALL METHOD CL_GUI_CFW=>FLUSH.

  PERFORM TOP_OF_PAGE.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM TOP_OF_PAGE.

  DATA : LT_FENTS        TYPE LVC_T_FIDX.
  DATA : LV_DL_TEXT(255) TYPE C.  "Text
  DATA : LV_COUNT_CHAR(5).

  CLEAR : LV_DL_TEXT.


  CALL METHOD GO_DYNDOC_ID->INITIALIZE_DOCUMENT
    EXPORTING
      BACKGROUND_COLOR = CL_DD_AREA=>COL_TEXTAREA.

  LV_DL_TEXT = ' 조회결과   :'.

  CALL METHOD GO_DYNDOC_ID->ADD_GAP.

  CALL METHOD GO_DYNDOC_ID->ADD_TEXT
    EXPORTING
      TEXT         = LV_DL_TEXT
      SAP_COLOR    = SPACE
      SAP_FONTSIZE = CL_DD_AREA=>MEDIUM
      SAP_EMPHASIS = CL_DD_AREA=>STRONG.
*      sap_emphasis = cl_dd_area=>heading
*      sap_color    = cl_dd_area=>list_heading. "list_heading_int.

  CLEAR : LV_DL_TEXT.

  DESCRIBE TABLE GT_FUNCTION LINES DATA(LV_LINES).
  DATA : LV_STRING(255) TYPE C.
  LV_STRING = LV_LINES.

  CALL METHOD GO_DYNDOC_ID->ADD_TEXT
    EXPORTING
      TEXT         = LV_STRING
      SAP_EMPHASIS = CL_DD_AREA=>HEADING
      SAP_COLOR    = CL_DD_AREA=>LIST_NEGATIVE_INV.

  CALL METHOD GO_DYNDOC_ID->NEW_LINE.

  PERFORM SET_TOP_OF_PAGE.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SET_TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM SET_TOP_OF_PAGE .
* Creating html control
  IF GO_HTML_CNTRL IS INITIAL.
    CREATE OBJECT GO_HTML_CNTRL
      EXPORTING
        PARENT = GO_CON_TOP.
  ENDIF.
  CALL METHOD GO_DYNDOC_ID->MERGE_DOCUMENT.
  GO_DYNDOC_ID->HTML_CONTROL = GO_HTML_CNTRL.

* Display document
  CALL METHOD GO_DYNDOC_ID->DISPLAY_DOCUMENT
    EXPORTING
      REUSE_CONTROL      = 'X'
      PARENT             = GO_CON_TOP
    EXCEPTIONS
      HTML_DISPLAY_ERROR = 1.
  IF SY-SUBRC NE 0.
    MESSAGE 'Error in displaying top-of-page' TYPE 'S'.
  ENDIF.
ENDFORM.
