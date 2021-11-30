class /VTPR/_CL_UTILS definition
  public
  final
  create public .

*"* public components of class /VTPR/_CL_UTILS
*"* do not include other source files here!!!
public section.

  class-methods SHOW_MSGS
    importing
      !I_MSGS type /VTPR/S002T .
  class-methods CHECK_FIELD_OBLIGATORY
    importing
      !P_FIELD type ANY
    exporting
      !P_MSG type /VTPR/S002 .
  class-methods GET_FIELDCAT
    importing
      !P_DATA type ANY
      !P_EXC_FIELDS type ALFIELDNAMES optional
    returning
      value(P_FIELDCAT) type LVC_T_FCAT
    exceptions
      NO_FIELD_CATALOG .
  class-methods READ_XLS_TO_IT_BY_SHEET
    importing
      !P_FILE type STRING
      !P_EXC_TAB type RSAR_T_TABNAMES optional
      !P_TAB_PREFIX type TABNAME optional
    returning
      value(P_FINAL) type /VTPR/S001T
    exceptions
      ERROR_OPEN_DOCUMENT .
  class-methods READ_EXCEL_BY_SHEET
    importing
      !P_FILE type STRING
    returning
      value(P_FINAL) type /VTPR/S001T
    exceptions
      ERROR_OPEN_DOCUMENT .
  class-methods GET_MATRIZ
    importing
      !P_BUKRS type BUKRS
    returning
      value(P_BRANCH) type J_1BBRANC_ .
  PROTECTED SECTION.
*"* protected components of class ZVTAX_CL_MESSAGES
*"* do not include other source files here!!!
  PRIVATE SECTION.
*"* private components of class ZVTAX_CL_UTILS
*"* do not include other source files here!!!
ENDCLASS.



CLASS /VTPR/_CL_UTILS IMPLEMENTATION.


  METHOD CHECK_FIELD_OBLIGATORY.

    INCLUDE /vtpr/_cl_utils_i02.


  ENDMETHOD.


  METHOD GET_FIELDCAT.

    INCLUDE /vtpr/_cl_utils_i03.

  ENDMETHOD.


  METHOD GET_MATRIZ.

    DATA: lv_prog  TYPE /vtpr/t_exit-prog,
          lv_form  TYPE /vtpr/t_exit-form,
          lv_bukrs TYPE bukrs.

    CONSTANTS: c_repid TYPE sy-repid VALUE 'ZVTAX_EXIT_MATRIZ_FILIAL'.

    lv_bukrs = p_bukrs.

    CALL FUNCTION '/VTPR/_READ_EXIT'
      EXPORTING
        i_bukrs  = space
        i_branch = space
        i_repid  = c_repid
        i_seq    = '001'
      IMPORTING
        e_prog   = lv_prog
        e_form   = lv_form.

    IF lv_prog IS NOT INITIAL AND lv_form IS NOT INITIAL.
      PERFORM (lv_form) IN PROGRAM (lv_prog)
                        CHANGING lv_bukrs
                                 p_branch
                                 IF FOUND.
    ENDIF.

    IF p_branch IS INITIAL.
      CALL FUNCTION 'J_1BREAD_CGC_COMPANY'
        EXPORTING
          bukrs  = p_bukrs
        IMPORTING
          branch = p_branch.

      IF sy-subrc IS NOT INITIAL.
        CLEAR p_branch.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD READ_EXCEL_BY_SHEET.

    CONSTANTS:
    c_csv_vtpr_arq   TYPE string VALUE 'vtpr_temp.csv'.

    DATA :
      lv_worksheets  TYPE ole2_object,
      lv_activesheet TYPE ole2_object,
      lv_application TYPE ole2_object,
      lv_workbook    TYPE ole2_object,
      lv_saved       TYPE ole2_object,
      lv_save        TYPE ole2_object,
      lv_tabix       TYPE i,
      lv_path        TYPE string,
      lv_name        TYPE string,
      lv_csv_file    TYPE string,
      lv_rc          TYPE i,
      ld_final       TYPE REF TO data.

    FIELD-SYMBOLS: <fs_table> TYPE STANDARD TABLE,
                   <fs_wa>    LIKE LINE OF p_final.

    TRY.
        cl_bcs_utilities=>split_path(
            EXPORTING
                iv_path = p_file
            IMPORTING
                ev_path = lv_path
                ev_name = lv_name ).
      CATCH cx_root.
        RAISE error_open_document.
    ENDTRY.

    CONCATENATE lv_path c_csv_vtpr_arq INTO lv_csv_file.

    CLEAR: lv_tabix.

    "Cria uma instância para o Excel
    CREATE OBJECT lv_application 'EXCEL.APPLICATION'.

    "Cria uma instância para as pastas de trabalho do excel
    GET PROPERTY OF lv_application 'WORKBOOKS' = lv_workbook.

    "Abre o arquivo
    CALL METHOD OF lv_workbook 'Open'
      EXPORTING
        #1 = p_file. "Caminho do arquivo passado por parâmetros

    IF sy-subrc NE 0.

      RAISE error_open_document.

    ENDIF.

    DO.

      CLEAR: ld_final.

      UNASSIGN: <fs_table>,
                <fs_wa>.

      CREATE DATA ld_final LIKE LINE OF p_final.
      ASSIGN ld_final->* TO <fs_wa>.

      CLEAR: <fs_wa>.

      ADD 1 TO lv_tabix.

      GET PROPERTY OF lv_application 'Sheets' = lv_activesheet
        EXPORTING
            #1 = lv_tabix.

      GET PROPERTY OF lv_activesheet 'Name' = <fs_wa>-tabname.

      "Seleciona a aba
      CALL METHOD OF lv_application 'Worksheets' = lv_worksheets
        EXPORTING
          #1 = lv_tabix.

      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      "Coloca a aba selecionada como ativa
      CALL METHOD OF lv_worksheets 'Activate'.

      "Busca a pasta de trabalho ativa
      GET PROPERTY OF lv_application 'ActiveWorkbook' = lv_workbook.

      "Salva uma cópia em CSV
      CALL METHOD OF lv_workbook 'SaveAs'
        EXPORTING
          #1 = lv_csv_file
          #2 = 6.
*
*      "Salva o novo arquivo CSV para garantir
      CALL METHOD OF lv_workbook 'Save' = lv_save.

      CREATE DATA <fs_wa>-data TYPE STANDARD TABLE OF string.
*
      ASSIGN <fs_wa>-data->* TO <fs_table>.
*
      CHECK <fs_table> IS ASSIGNED.

      cl_gui_frontend_services=>gui_upload(
        EXPORTING
          filename                = lv_csv_file
*          filetype                = 'ASC'
          has_field_separator     = 'X'
*          header_length           = 0
*          read_by_line            = 'X'
*          dat_mode                = SPACE
*          codepage                =
*          ignore_cerr             = ABAP_TRUE
*          replacement             = '#'
*          virus_scan_profile      =
*        IMPORTING
*          filelength              =
*          header                  =
        CHANGING
          data_tab                = <fs_table>
*          isscanperformed         = SPACE
        EXCEPTIONS
          file_open_error         = 1
          file_read_error         = 2
          no_batch                = 3
          gui_refuse_filetransfer = 4
          invalid_type            = 5
          no_authority            = 6
          unknown_error           = 7
          bad_data_format         = 8
          header_not_allowed      = 9
          separator_not_allowed   = 10
          header_too_long         = 11
          unknown_dp_error        = 12
          access_denied           = 13
          dp_out_of_memory        = 14
          disk_full               = 15
          dp_timeout              = 16
          not_supported_by_gui    = 17
          error_no_gui            = 18
          OTHERS                  = 19
      ).
      IF sy-subrc <> 0.
*       MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      APPEND <fs_wa> TO p_final.

    ENDDO.

    "Para finalizar o Excel mostrar mensagem,
    "seta a pasta de trabalho como 'salva'
    SET PROPERTY OF lv_workbook 'Saved' = 1.

    "Fecha a pasta de trabalho
    CALL METHOD OF lv_workbook 'Close'.

    "Finaliza o Excel
    CALL METHOD OF lv_application 'Quit'.

    FREE OBJECT:
      lv_worksheets,
      lv_workbook,
      lv_application,
      lv_saved,
      lv_save.

    FREE:
      lv_worksheets,
      lv_workbook,
      lv_application,
      lv_saved,
      lv_save.

  ENDMETHOD.


  METHOD READ_XLS_TO_IT_BY_SHEET.

    INCLUDE /vtpr/_cl_utils_i04.

  ENDMETHOD.


  METHOD SHOW_MSGS.

    INCLUDE /vtpr/_cl_utils_i01.


  ENDMETHOD.
ENDCLASS.
