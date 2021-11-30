*&---------------------------------------------------------------------*
*&  Include           /VTPR/_CL_UTILS_I04
*&---------------------------------------------------------------------*
CONSTANTS: BEGIN OF c_types,
    packed              VALUE 'P',
    float               VALUE 'F',
    dats                VALUE 'D',
  END OF c_types.

FIELD-SYMBOLS: <fs_table>         TYPE STANDARD TABLE,
               <fs_db_table>      TYPE STANDARD TABLE,
               <fs_wa>            TYPE ANY,
               <fs_field>         TYPE ANY,
               <fs_db_wa>         TYPE ANY,
               <fs_fld_db>        TYPE ANY,
               <fs_fld_aux>       TYPE ANY.

FIELD-SYMBOLS: <fs_it_excel>      TYPE STANDARD TABLE.


DATA: v_document_url              TYPE c LENGTH 256,
      dyn_table                   TYPE REF TO data,
      dyn_line                    TYPE REF TO data,
      l_data                      TYPE REF TO data,
      it_fcat                     TYPE lvc_t_fcat,
      wa_fcat                     TYPE lvc_s_fcat,
      l_tab                       TYPE tabname,
      l_resto                     TYPE string,
      l_file                      TYPE rlgrap-filename,
      l_str                       TYPE string,
      l_float                     TYPE f,
      l_decimals                  TYPE i,
      l_valor                     TYPE char30,
      l_saida                     TYPE char30.

DATA: oref_container              TYPE REF TO cl_gui_custom_container,
      iref_control                TYPE REF TO i_oi_container_control,
      iref_document               TYPE REF TO i_oi_document_proxy,
      iref_spreadsheet            TYPE REF TO i_oi_spreadsheet,
      iref_error                  TYPE REF TO i_oi_error.

DATA: i_sheets                    TYPE soi_sheets_table,
      wa_sheets                   TYPE soi_sheets,
      l_lin                       TYPE i,
      l_col                       TYPE i,
      l_mandt                     TYPE i.

CLEAR: l_lin,
       l_mandt.

REFRESH: p_final.

FREE: iref_control,
      oref_container,
      iref_document,
      iref_error,
      iref_spreadsheet.

CALL METHOD c_oi_container_control_creator=>get_container_control
  IMPORTING
    control = iref_control
    error   = iref_error.
IF iref_error->has_failed = 'X'.
  CALL METHOD iref_error->raise_message
    EXPORTING
      type = 'E'.
ENDIF.

CREATE OBJECT oref_container
  EXPORTING
    container_name              = 'CONT'
  EXCEPTIONS
    cntl_error                  = 1
    cntl_system_error           = 2
    create_error                = 3
    lifetime_error              = 4
    lifetime_dynpro_dynpro_link = 5
    OTHERS                      = 6.

IF sy-subrc <> 0.
  RAISE error_open_document.
ENDIF.

CALL METHOD iref_control->init_control
  EXPORTING
    inplace_enabled      = 'X'
    r3_application_name  = 'EXCEL CONTAINER'
    parent               = oref_container
  IMPORTING
    error                = iref_error
  EXCEPTIONS
    javabeannotsupported = 1
    OTHERS               = 2.

IF iref_error->has_failed = 'X'.
  CALL METHOD iref_error->raise_message
    EXPORTING
      type = 'E'.
ENDIF.

CALL METHOD iref_control->get_document_proxy
  EXPORTING
    document_type  = soi_doctype_excel_sheet
  IMPORTING
    document_proxy = iref_document
    error          = iref_error.

IF iref_error->has_failed = 'X'.
  CALL METHOD iref_error->raise_message
    EXPORTING
      type = 'E'.
ENDIF.

TRY .
    CONCATENATE 'FILE://' p_file INTO v_document_url.
    l_file = p_file.

    "Abre documento
    CALL METHOD iref_document->open_document
      EXPORTING
        document_title = 'Excel'
        document_url   = v_document_url
        open_inplace   = 'X'
        open_readonly  = 'X'
      IMPORTING
        error          = iref_error.

    IF iref_error->has_failed = 'X'.
      CALL METHOD iref_error->raise_message
        EXPORTING
          type = 'I'.
      EXIT.
    ENDIF.
  CATCH cx_root.
    RAISE error_open_document.
ENDTRY.

"Referência do gerenciador da abas
CALL METHOD iref_document->get_spreadsheet_interface
  EXPORTING
    no_flush        = ' '
  IMPORTING
    error           = iref_error
    sheet_interface = iref_spreadsheet.

IF iref_error->has_failed = 'X'.
  CALL METHOD iref_error->raise_message
    EXPORTING
      type = 'I'.
  EXIT.
ENDIF.

"Retorna todas as abas do documento
CALL METHOD iref_spreadsheet->get_sheets
  EXPORTING
    no_flush = ' '
  IMPORTING
    sheets   = i_sheets
    error    = iref_error.

IF iref_error->has_failed = 'X'.
  CALL METHOD iref_error->raise_message
    EXPORTING
      type = 'I'.
  EXIT.
ENDIF.

"Fecha documento
CALL METHOD iref_document->close_document
  IMPORTING
    error = iref_error.

IF iref_error->has_failed = 'X'.
  CALL METHOD iref_error->raise_message
    EXPORTING
      type = 'I'.
  EXIT.
ENDIF.

"Libera memória utiliazada
CALL METHOD iref_document->release_document
  IMPORTING
    error = iref_error.

IF iref_error->has_failed = 'X'.
  CALL METHOD iref_error->raise_message
    EXPORTING
      type = 'I'.
  EXIT.
ENDIF.

IF p_exc_tab[] IS NOT INITIAL.
  LOOP AT p_exc_tab INTO l_tab.
    DELETE i_sheets WHERE sheet_name = l_tab.
  ENDLOOP.
ENDIF.

LOOP AT i_sheets INTO wa_sheets.

  "Lê a aba especificada
  CALL FUNCTION '/VTPR/F001_01'
    EXPORTING
      i_arquivo    = p_file
      i_nome_aba   = wa_sheets-sheet_name
    IMPORTING
      e_table      = l_data
    EXCEPTIONS
      invalid_file = 1.

  "Referência da tabela lida do excel
  ASSIGN l_data->* TO <fs_it_excel>.

  REFRESH: it_fcat.
  CLEAR: l_tab,
         l_resto.

  l_tab = wa_sheets-sheet_name.
  SPLIT l_tab AT '-' INTO l_tab
                          l_resto.

  IF l_resto IS INITIAL.
    l_tab = wa_sheets-sheet_name.
  ENDIF.

  "Se a tabela possui prefixo.
  IF p_tab_prefix IS NOT INITIAL.
    CONCATENATE p_tab_prefix wa_sheets-sheet_name INTO l_tab.
    CONDENSE l_tab NO-GAPS.
  ENDIF.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = l_tab
    CHANGING
      ct_fieldcat            = it_fcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CHECK it_fcat[] IS NOT INITIAL.
  SORT it_fcat BY row_pos col_pos.

  READ TABLE it_fcat TRANSPORTING NO FIELDS WITH KEY fieldname = 'MANDT'.
  IF sy-subrc = 0.
    l_mandt = 1.
  ENDIF.

  "Cria tabela interna a partir do catalogo de campos
  CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING
      it_fieldcatalog = it_fcat
    IMPORTING
      ep_table        = dyn_table.

  ASSIGN dyn_table->* TO <fs_db_table>.

  CHECK <fs_db_table> IS ASSIGNED.

  "Cria uma linha da tabela interna criada
  CREATE DATA dyn_line LIKE LINE OF <fs_db_table>.
  ASSIGN dyn_line->* TO <fs_db_wa>.

  CHECK <fs_db_wa> IS ASSIGNED.

  "Loop na tabela lida do excel.
  LOOP AT <fs_it_excel> ASSIGNING <fs_wa>.

    "Pega o valor da Linha
    ASSIGN ('<FS_WA>-ROW') TO <fs_field>.
    CHECK <fs_field> IS ASSIGNED.

*      "Pula a linha de cabeçalho
*      IF <fs_field> = 1 AND i_arq_field_header IS NOT INITIAL.
*        CONTINUE.
*      ENDIF.

    "Guarda a linha atual, se for a primeira iteração
    IF l_lin IS INITIAL.
      l_lin = <fs_field>.
    ENDIF.

    "Se a linha do registro for diferente da linha atual guardada, o registro mudou
    IF l_lin <> <fs_field>.
      APPEND <fs_db_wa> TO <fs_table>.
      CLEAR: <fs_db_wa>.
      l_lin = <fs_field>.
    ENDIF.

    "Coluna
    ASSIGN ('<FS_WA>-COL') TO <fs_field>.
    CHECK <fs_field> IS ASSIGNED.

*      IF i_arq_field_header IS NOT INITIAL.
*        READ TABLE <fs_t_data> ASSIGNING <fs_wa_aux> WITH KEY ('ROW') = 1 ('COL') = <fs_field>  BINARY SEARCH.
*        IF sy-subrc <> 0.
*          CONTINUE.
*        ENDIF.
*
*        "Nome do campo da tabela do dicionario
*        CONCATENATE '<FS_WA_AUX>-' c_value INTO l_str.
*        ASSIGN (l_str) TO <fs_fld_aux>.
*      ELSE.
    l_col = <fs_field> + l_mandt.
    SORT it_fcat BY row_pos col_pos.
    READ TABLE it_fcat INTO wa_fcat WITH KEY row_pos = 0 col_pos = l_col BINARY SEARCH.
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.

    "Nome do campo da tabela do dicionario
    ASSIGN ('WA_FCAT-FIELDNAME') TO <fs_fld_aux>.

*      ENDIF.

    "Nome do campo na estrutura
    CONCATENATE '<FS_DB_WA>-' <fs_fld_aux> INTO l_str.
    ASSIGN (l_str) TO <fs_fld_db>.

    IF <fs_fld_db> IS NOT ASSIGNED.
      CONTINUE.
    ENDIF.

    "Valor do campo vindo do excel
    ASSIGN ('<FS_WA>-VALUE') TO <fs_field>.
    CHECK <fs_field> IS ASSIGNED.

    SORT it_fcat BY fieldname.
    READ TABLE it_fcat INTO wa_fcat WITH KEY fieldname = <fs_fld_aux> BINARY SEARCH.

    CHECK sy-subrc = 0.

*      PERFORM f_move_valor USING it_fcat <fs_fld_aux> <fs_field> CHANGING <fs_fld_db>.

    l_valor = <fs_field>.

    CASE wa_fcat-inttype.
      WHEN c_types-packed OR c_types-float.
        CALL FUNCTION 'C14DG_CHAR_NUMBER_CONVERSION'
          EXPORTING
            i_string                   = l_valor
          IMPORTING
            e_float                    = l_float
            e_dec                      = <fs_fld_db>
            e_decimals                 = l_decimals
          EXCEPTIONS
            wrong_characters           = 1
            first_character_wrong      = 2
            arithmetic_sign            = 3
            multiple_decimal_separator = 4
            thousandsep_in_decimal     = 5
            thousand_separator         = 6
            number_too_big             = 7
            OTHERS                     = 8.

        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
      WHEN c_types-dats.
        CALL FUNCTION 'CONVERT_DATE_TO_INTERN_FORMAT'
          EXPORTING
            datum = l_valor
            dtype = wa_fcat-datatype
          IMPORTING
            idate = <fs_fld_db>.
      WHEN OTHERS.
        <fs_fld_db> = l_valor.
    ENDCASE.


  ENDLOOP.

ENDLOOP.
