*&---------------------------------------------------------------------*
*&  Include  /vtpr/adiciona_objs_pacote_i02
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK q1 WITH FRAME TITLE text-001.

PARAMETER:
p_sobj  TYPE c RADIOBUTTON GROUP r1 DEFAULT 'X' USER-COMMAND selecao,
p_sdevc TYPE c RADIOBUTTON GROUP r1.

SELECT-OPTIONS:
s_devc                            FOR tadir-devclass MODIF ID pkg,
s_object                          FOR tadir-object MODIF ID obj,
s_objnam                          FOR tadir-obj_name MODIF ID obj.
PARAMETER:
p_nblock TYPE c AS CHECKBOX, "Não bloquear objetos
p_indevc TYPE c AS CHECKBOX, "Incluir entrada do pacote?
p_prefix TYPE string DEFAULT '/VT'.
SELECTION-SCREEN END OF BLOCK q1.

*SELECTION-SCREEN FUNCTION KEY 1.
*=======================================================================
* TABs
*=======================================================================
SELECTION-SCREEN BEGIN OF TABBED BLOCK  tab_selecao FOR 15 LINES.
SELECTION-SCREEN TAB (23) oo        USER-COMMAND oo         DEFAULT SCREEN 9100.
SELECTION-SCREEN TAB (23) resource  USER-COMMAND resource   DEFAULT SCREEN 9200.
SELECTION-SCREEN END OF BLOCK tab_selecao.

SELECTION-SCREEN BEGIN OF SCREEN 9100 AS SUBSCREEN.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-002.
PARAMETER:
    p_ooint TYPE c AS CHECKBOX DEFAULT'X',
    p_oosup TYPE c AS CHECKBOX DEFAULT 'X',
    p_oosub TYPE c AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN END OF SCREEN 9100.

SELECTION-SCREEN BEGIN OF SCREEN 9200 AS SUBSCREEN.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-003.
PARAMETERS:
  p_incssf TYPE c AS CHECKBOX USER-COMMAND ssf. "Incluir smartforms/smartstyles/sapscript?
SELECT-OPTIONS:
  p_namssf FOR tadir-obj_name MODIF ID ssf.

PARAMETERS:
  p_web    TYPE c AS CHECKBOX.
PARAMETERS:
  p_images TYPE c AS CHECKBOX  USER-COMMAND img. "Incluir imagens?
SELECT-OPTIONS:
  s_images FOR stxbitmaps-tdname MODIF ID img.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN END OF SCREEN 9200.

INITIALIZATION.
  oo       = 'ABAP Objetos'.
  resource = 'Recursos'.

AT SELECTION-SCREEN OUTPUT.

  PERFORM trata_exibicao_campos.

AT SELECTION-SCREEN.
  PERFORM valida_selecao.
