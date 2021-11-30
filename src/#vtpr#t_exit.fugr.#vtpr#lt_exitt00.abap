*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: /VTPR/T_EXIT....................................*
DATA:  BEGIN OF STATUS_/VTPR/T_EXIT                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/VTPR/T_EXIT                  .
CONTROLS: TCTRL_/VTPR/T_EXIT
            TYPE TABLEVIEW USING SCREEN '0002'.
*.........table declarations:.................................*
TABLES: */VTPR/T_EXIT                  .
TABLES: /VTPR/T_EXIT                   .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
