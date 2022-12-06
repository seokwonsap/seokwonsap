*&---------------------------------------------------------------------*
*& Include          ZDEV001R001_CLS
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------
*       CLASS LCL_FCAT DEFINITION
*----------------------------------------------------------------------
CLASS LCL_FCAT DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS LVC_FCAT_FROM_INTERNAL_TABLE
      IMPORTING
                IT_TABLE       TYPE ANY TABLE
      RETURNING VALUE(RT_FCAT) TYPE LVC_T_FCAT.

    CLASS-METHODS SLIS_FCAT_FROM_INTERNAL_TABLE
      IMPORTING
                IT_TABLE       TYPE ANY TABLE
      RETURNING VALUE(RT_FCAT) TYPE SLIS_T_FIELDCAT_ALV.
ENDCLASS.

*----------------------------------------------------------------------
*       CLASS LCL_FCAT IMPLEMENTATION
*----------------------------------------------------------------------
CLASS LCL_FCAT IMPLEMENTATION.
  METHOD LVC_FCAT_FROM_INTERNAL_TABLE.
    DATA : TABLE TYPE REF TO DATA.
    CREATE DATA TABLE LIKE IT_TABLE.
    ASSIGN TABLE->* TO FIELD-SYMBOL(<TABLE>).
    TRY.
        CL_SALV_TABLE=>FACTORY( IMPORTING
                                  R_SALV_TABLE   = DATA(SALV_TABLE)
                                CHANGING
                                  T_TABLE        = <TABLE>  ).
        RT_FCAT = CL_SALV_CONTROLLER_METADATA=>GET_LVC_FIELDCATALOG(
            R_COLUMNS      = SALV_TABLE->GET_COLUMNS( )      " ALV Filter
            R_AGGREGATIONS = SALV_TABLE->GET_AGGREGATIONS( ) " ALV Aggregations
    ).
      CATCH CX_ROOT.
    ENDTRY.
  ENDMETHOD.

  METHOD SLIS_FCAT_FROM_INTERNAL_TABLE.
    DATA : TABLE TYPE REF TO DATA.
    CREATE DATA TABLE LIKE IT_TABLE.
    ASSIGN TABLE->* TO FIELD-SYMBOL(<TABLE>).
    TRY.
        CL_SALV_TABLE=>FACTORY( IMPORTING
                                  R_SALV_TABLE   = DATA(SALV_TABLE)
                                CHANGING
                                  T_TABLE        = <TABLE>  ).
        RT_FCAT = CL_SALV_CONTROLLER_METADATA=>GET_SLIS_FIELDCATALOG(
            R_COLUMNS      = SALV_TABLE->GET_COLUMNS( )      " ALV Filter
            R_AGGREGATIONS = SALV_TABLE->GET_AGGREGATIONS( ) " ALV Aggregations
    ).
      CATCH CX_ROOT.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------
*       CLASS LCL_EVENT_HANDLER DEFINITION
*----------------------------------------------------------------------
CLASS LCL_EVENT_HANDLER DEFINITION.

  PUBLIC SECTION.

    METHODS HANDLE_TOP_OF_PAGE FOR EVENT TOP_OF_PAGE
                                      OF CL_GUI_ALV_GRID
      IMPORTING E_DYNDOC_ID.

ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

*----------------------------------------------------------------------
*       CLASS LCL_EVENT_HANDLER IMPLEMENTATION
*----------------------------------------------------------------------
CLASS LCL_EVENT_HANDLER IMPLEMENTATION.

  METHOD HANDLE_TOP_OF_PAGE.
    PERFORM TOP_OF_PAGE.
  ENDMETHOD.

ENDCLASS.

DATA : GR_EVENT_HANDLER TYPE REF TO LCL_EVENT_HANDLER.
