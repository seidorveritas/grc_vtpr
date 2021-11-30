*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: /VTPR/T_PARAM...................................*
DATA:  BEGIN OF STATUS_/VTPR/T_PARAM                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/VTPR/T_PARAM                 .
CONTROLS: TCTRL_/VTPR/T_PARAM
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: */VTPR/T_PARAM                 .
TABLES: /VTPR/T_PARAM                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
