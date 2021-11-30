*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_/VTPR/T_PARAM
*   generation date: 27.02.2016 at 11:12:36 by user KMOTA
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_/VTPR/T_PARAM      .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
