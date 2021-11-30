*&---------------------------------------------------------------------*
*&  Include  /vtpr/adiciona_objs_pacote_i01
*&---------------------------------------------------------------------*

"----------------------------------------------------------------
" Classe de definição da seleção
"----------------------------------------------------------------
*----------------------------------------------------------------------*
*       CLASS lcl_select DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_select DEFINITION.

  PUBLIC SECTION.

    DATA: v_package  TYPE tadir-devclass,
          s_objnames TYPE RANGE OF tadir-obj_name.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.                    "lcl_select DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_select IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_select IMPLEMENTATION.

ENDCLASS.                    "lcl_select IMPLEMENTATION

"----------------------------------------------------------------
" Classe de modelo de dados
"----------------------------------------------------------------
*----------------------------------------------------------------------*
*       CLASS lcl_data DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_data DEFINITION.

  PUBLIC SECTION.

    TYPE-POOLS: trwbo.

    DATA: o_sel  TYPE REF TO lcl_select,
          t_data TYPE mctadir_tab.

    METHODS:
      constructor IMPORTING io_sel TYPE REF TO lcl_select OPTIONAL,

      select_data.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.                    "lcl_data DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_data IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_data IMPLEMENTATION.
  METHOD constructor.

    IF io_sel IS BOUND.
      me->o_sel = io_sel.
    ELSE.
      CREATE OBJECT me->o_sel.
    ENDIF.

  ENDMETHOD.                    "constructor

  METHOD select_data.
    SELECT *
      FROM tadir
      INTO CORRESPONDING FIELDS OF TABLE t_data
      WHERE obj_name IN me->o_sel->s_objnames[]
        AND devclass = me->o_sel->v_package.

    IF sy-subrc <> 0.
      REFRESH: t_data.
    ENDIF.

  ENDMETHOD.                    "select_data

ENDCLASS.                    "lcl_data IMPLEMENTATION

"----------------------------------------------------------------
" Classe para a geração da request
"----------------------------------------------------------------
*----------------------------------------------------------------------*
*       CLASS lcl_request DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_request DEFINITION .

  PUBLIC SECTION.
*----------------------------------------------------------------------
* Constantes
*----------------------------------------------------------------------
    CONSTANTS: c_prefix_devc    TYPE tadir-devclass VALUE '/VT',
               c_complete_obj   TYPE tadir-pgmid    VALUE 'R3TR',
               c_dominio        TYPE tadir-object   VALUE 'DOMA',
               c_elemento       TYPE tadir-object   VALUE 'DTEL',
               c_tabela         TYPE tadir-object   VALUE 'TABL',
               c_categ_tabela   TYPE tadir-object   VALUE 'TTYP',
               c_ajuda_pesquisa TYPE tadir-object   VALUE 'SHLP',
               c_pacote         TYPE tadir-object   VALUE 'DEVC',
               c_grupo_funcao   TYPE tadir-object   VALUE 'FUGR'.

    DATA: o_data TYPE REF TO lcl_data.

    METHODS:
      constructor           IMPORTING io_data TYPE REF TO lcl_data,
      add_objs_to_request.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES: yts_tadir TYPE SORTED TABLE OF tadir
                          WITH UNIQUE KEY pgmid object obj_name.


    METHODS:
      get_relacionados  IMPORTING VALUE(pw_tadir) TYPE tadir
                        CHANGING  pt_tadir        TYPE yts_tadir,

      get_info_dominio IMPORTING VALUE(pw_tadir) TYPE tadir
                       CHANGING  pt_tadir        TYPE yts_tadir,

      get_info_elemento IMPORTING VALUE(pw_tadir) TYPE tadir
                        CHANGING  pt_tadir        TYPE yts_tadir,

      get_info_tabela IMPORTING VALUE(pw_tadir) TYPE tadir
                      CHANGING  pt_tadir        TYPE yts_tadir,

      get_info_ctabela IMPORTING VALUE(pw_tadir) TYPE tadir
                       CHANGING  pt_tadir        TYPE yts_tadir,

      get_info_ajuda_pesquisa IMPORTING VALUE(pw_tadir) TYPE tadir
                              CHANGING  pt_tadir        TYPE yts_tadir,

      get_info_grupo_funcao IMPORTING VALUE(pw_tadir) TYPE tadir
                            CHANGING  pt_tadir        TYPE yts_tadir,

      object_verified IMPORTING VALUE(pw_tadir)  TYPE tadir
                      RETURNING VALUE(pv_result) TYPE xfeld,

      check_obj_exist IMPORTING VALUE(pw_tadir)  TYPE tadir
                                pt_tadir         TYPE yts_tadir
                      RETURNING VALUE(pv_result) TYPE xfeld,

      check_own_package IMPORTING VALUE(pw_tadir)  TYPE tadir
                        RETURNING VALUE(pv_result) TYPE xfeld,

      add_obj_to_table IMPORTING VALUE(pw_tadir)  TYPE tadir
                       EXPORTING pt_tadir         TYPE yts_tadir
                                 VALUE(pv_result) TYPE xfeld,

      select_tadir IMPORTING pgmid           TYPE tadir-pgmid
                             object          TYPE tadir-object OPTIONAL
                             obj_name        TYPE tadir-obj_name
                   RETURNING VALUE(pt_tadir) TYPE yts_tadir,

      get_obj_relacionados IMPORTING VALUE(obj_name) TYPE tadir-obj_name
                           CHANGING  pt_tadir        TYPE yts_tadir.



ENDCLASS.                    "lcl_request DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_request IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_request IMPLEMENTATION.

  METHOD constructor.

    me->o_data = io_data.
    me->o_data->select_data( ).
    SORT me->o_data->t_data BY object obj_name.

  ENDMETHOD.                    "constructor

  METHOD add_objs_to_request.

    FIELD-SYMBOLS: <fs_tadir>   TYPE tadir.
    DATA: lt_tadir      TYPE yts_tadir,
          lt_objects    TYPE tr_objects,
          lw_object     LIKE LINE OF lt_objects,
          lw_es_request TYPE trwbo_request_header,
          lv_msg        TYPE string,
          lv_total      TYPE i,
          lv_atual      TYPE i.

    lv_total = lines( me->o_data->t_data ).

    LOOP AT me->o_data->t_data ASSIGNING <fs_tadir>.

      CONCATENATE 'Gerando relações:' <fs_tadir>-object <fs_tadir>-obj_name INTO lv_msg SEPARATED BY space.

      lv_atual = sy-tabix.

      cl_progress_indicator=>progress_indicate(
        EXPORTING
            i_text               = lv_msg
            i_processed          = lv_atual    " Number of Objects Already Processed
            i_total              = lv_total    " Total Number of Objects to Be Processed
            i_output_immediately = 'X').

      WAIT UP TO '0.1' SECONDS.

      get_relacionados(
       EXPORTING
           pw_tadir = <fs_tadir>
       CHANGING
           pt_tadir = lt_tadir ).

    ENDLOOP.

    LOOP AT lt_tadir ASSIGNING <fs_tadir>.

      CLEAR: lw_object.

      lw_object-pgmid     = <fs_tadir>-pgmid.
      lw_object-object    = <fs_tadir>-object.
      lw_object-obj_name  = <fs_tadir>-obj_name.

      APPEND lw_object TO lt_objects.

    ENDLOOP.

    CALL FUNCTION 'TR_REQUEST_CHOICE'
      EXPORTING
        iv_suppress_dialog   = ' '
*       iv_request_types     =
*       iv_cli_dep           = SPACE
*       iv_request           = SPACE
        it_e071              = lt_objects
*       it_e071k             =
        iv_lock_objects      = ' '
*       iv_title             =
*       iv_start_column      =
*       iv_start_row         =
*       iv_with_error_log    = 'X'
*       iv_no_owner_check    = SPACE
*       iv_foreign_request   = '   '
*       it_e071k_str         =
*       it_obj_entries       =
      IMPORTING
        es_request           = lw_es_request
      EXCEPTIONS
        invalid_request      = 1
        invalid_request_type = 2
        user_not_owner       = 3
        no_objects_appended  = 4
        enqueue_error        = 5
        cancelled_by_user    = 6
        recursive_call       = 7.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.                    "add_objs_to_request

  METHOD get_relacionados.

    CHECK pw_tadir-obj_name IS NOT INITIAL.

    CHECK check_obj_exist( pw_tadir = pw_tadir pt_tadir = pt_tadir ) IS INITIAL.

    IF check_own_package( pw_tadir = pw_tadir ) IS NOT INITIAL.
      add_obj_to_table( EXPORTING pw_tadir = pw_tadir IMPORTING pt_tadir = pt_tadir ).
    ELSE.
      CHECK object_verified( pw_tadir = pw_tadir ) IS NOT INITIAL.
      add_obj_to_table( EXPORTING pw_tadir = pw_tadir IMPORTING pt_tadir = pt_tadir ).
    ENDIF.

    "Tornar essa chamada de método dinâmica
    CASE pw_tadir-object.
      WHEN c_dominio.
        get_info_dominio( EXPORTING pw_tadir = pw_tadir CHANGING pt_tadir = pt_tadir ).
      WHEN c_elemento.
        get_info_elemento( EXPORTING pw_tadir = pw_tadir CHANGING pt_tadir = pt_tadir ).
      WHEN c_tabela.
        get_info_tabela( EXPORTING pw_tadir = pw_tadir CHANGING pt_tadir = pt_tadir ).
      WHEN c_categ_tabela.
        get_info_ctabela( EXPORTING pw_tadir = pw_tadir CHANGING pt_tadir = pt_tadir ).
      WHEN c_ajuda_pesquisa.
        get_info_ajuda_pesquisa( EXPORTING pw_tadir = pw_tadir CHANGING pt_tadir = pt_tadir ).
      WHEN c_grupo_funcao.
        get_info_grupo_funcao( EXPORTING pw_tadir = pw_tadir CHANGING pt_tadir = pt_tadir ).
    ENDCASE.

  ENDMETHOD.                    "get_relacionados

  METHOD get_info_dominio.

    DATA: lw_info      TYPE dd01v,
          lw_tadir     TYPE tadir,
          lv_doma_name TYPE ddobjname.

    lv_doma_name = pw_tadir-obj_name.

    CALL FUNCTION 'DDIF_DOMA_GET'
      EXPORTING
        name          = lv_doma_name
*       state         = 'A'
*       langu         = ' '
      IMPORTING
*       gotstate      =
        dd01v_wa      = lw_info
*      TABLES
*       dd07v_tab     =
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CHECK lw_info-entitytab IS NOT INITIAL.

    lw_tadir-obj_name = lw_info-entitytab.

    get_obj_relacionados(
        EXPORTING
            obj_name = lw_tadir-obj_name
        CHANGING
            pt_tadir = pt_tadir ).

  ENDMETHOD.                    "get_info_dominio

  METHOD get_info_elemento.

    DATA: lw_info  TYPE dd04v,
          lw_tadir TYPE tadir,
          lv_name  TYPE ddobjname.

    lv_name = pw_tadir-obj_name.

    CALL FUNCTION 'DDIF_DTEL_GET'
      EXPORTING
        name          = lv_name
*       state         = 'A'
*       langu         = ' '
      IMPORTING
*       gotstate      =
        dd04v_wa      = lw_info
*       tpara_wa      =
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CHECK lw_info-domname IS NOT INITIAL.

    lw_tadir-obj_name = lw_info-domname.

    get_obj_relacionados(
        EXPORTING
            obj_name = lw_tadir-obj_name
        CHANGING
            pt_tadir = pt_tadir ).

  ENDMETHOD.                    "get_info_elemento

  METHOD get_info_tabela.

    DATA: p_dd02v  TYPE dd02v,
          pt_dd03p TYPE dd03ttyp,
          pt_dd05m TYPE dd05mttyp,
          pt_dd08v TYPE dd08vttyp,
          pt_dd12v TYPE STANDARD TABLE OF dd12v,
          pt_dd17v TYPE dd17vtab,
          pt_dd35v TYPE dd35vttyp,
          pt_dd36m TYPE dd36mttyp,
          lv_name  TYPE ddobjname,
          lw_tadir TYPE tadir.

    FIELD-SYMBOLS: <fs_field> TYPE dd03p,
                   <fs_shlp>  TYPE dd35v.

    lv_name = pw_tadir-obj_name.

    CALL FUNCTION 'DDIF_TABL_GET'
      EXPORTING
        name          = lv_name
*       state         = 'A'
*       langu         = ' '
      IMPORTING
*       gotstate      =
        dd02v_wa      = p_dd02v
*       dd09l_wa      =
      TABLES
        dd03p_tab     = pt_dd03p
        dd05m_tab     = pt_dd05m
        dd08v_tab     = pt_dd08v
        dd12v_tab     = pt_dd12v
        dd17v_tab     = pt_dd17v
        dd35v_tab     = pt_dd35v
        dd36m_tab     = pt_dd36m
      EXCEPTIONS
        illegal_input = 1.
    IF sy-subrc <> 0.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      EXIT.
    ENDIF.

    "Loop dos campos
    LOOP AT pt_dd03p ASSIGNING <fs_field>.

      "Elemento de dados utilizado
      CLEAR: lw_tadir.
      lw_tadir-obj_name = <fs_field>-rollname.

      get_obj_relacionados(
          EXPORTING
              obj_name = lw_tadir-obj_name
          CHANGING
              pt_tadir = pt_tadir ).

      "Tabela de verificação
      CLEAR: lw_tadir.
      lw_tadir-obj_name = <fs_field>-checktable.

      get_obj_relacionados(
          EXPORTING
              obj_name = lw_tadir-obj_name
          CHANGING
              pt_tadir = pt_tadir ).

      "Tabela de entidade
      CLEAR: lw_tadir.
      lw_tadir-obj_name = <fs_field>-entitytab.

      get_obj_relacionados(
          EXPORTING
              obj_name = lw_tadir-obj_name
          CHANGING
              pt_tadir = pt_tadir ).
    ENDLOOP.

    LOOP AT pt_dd35v ASSIGNING <fs_shlp>.
      "Search help
      CLEAR: lw_tadir.
      lw_tadir-obj_name = <fs_field>-shlpname.

      get_obj_relacionados(
          EXPORTING
              obj_name = lw_tadir-obj_name
          CHANGING
              pt_tadir = pt_tadir ).
    ENDLOOP.

  ENDMETHOD.                    "get_info_tabela

  METHOD get_info_ctabela.

    DATA: lw_info  TYPE dd40v,
          lw_tadir TYPE tadir,
          lv_name  TYPE string.

    lv_name = pw_tadir-obj_name.

    CALL FUNCTION 'DDIF_TTYP_GET'
      EXPORTING
        name          = lv_name
*       state         = 'A'
*       langu         = sy-langu
      IMPORTING
*       GOTSTATE      =
        dd40v_wa      = lw_info
*   TABLES
*       DD42V_TAB     =
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CLEAR: lw_tadir.
    lw_tadir-obj_name = lw_info-rowtype.
    get_obj_relacionados(
        EXPORTING
            obj_name = lw_tadir-obj_name
        CHANGING
            pt_tadir = pt_tadir ).

  ENDMETHOD.                    "get_info_ctabela

  METHOD get_info_ajuda_pesquisa.

    DATA: lw_info   TYPE dd30v,
          lt_fields TYPE rsdg_t_dd32p,
          lw_tadir  TYPE tadir,
          lv_name   TYPE ddobjname.

    FIELD-SYMBOLS: <fs_field> TYPE dd32p.

    lv_name = pw_tadir-obj_name.

    CALL FUNCTION 'DDIF_SHLP_GET'
      EXPORTING
        name          = lv_name
*       state         = 'A'
*       langu         = ' '
      IMPORTING
*       gotstate      =
        dd30v_wa      = lw_info
      TABLES
*       dd31v_tab     =
        dd32p_tab     = lt_fields
*       dd33v_tab     =
      EXCEPTIONS
        illegal_input = 1.

    IF sy-subrc <> 0.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      EXIT.
    ENDIF.

    "Tabela de seleção do search help
    CLEAR: lw_tadir.
    lw_tadir-obj_name = lw_info-selmethod.
    get_obj_relacionados(
        EXPORTING
            obj_name = lw_tadir-obj_name
        CHANGING
            pt_tadir = pt_tadir ).

    "Loop dos campos do search help
    LOOP AT lt_fields ASSIGNING <fs_field>.

      "Elemento de dados utilizado
      CLEAR: lw_tadir.
      lw_tadir-obj_name = <fs_field>-rollname.

      get_obj_relacionados(
          EXPORTING
              obj_name = lw_tadir-obj_name
          CHANGING
              pt_tadir = pt_tadir ).

    ENDLOOP.

  ENDMETHOD.                    "get_info_ajuda_pesquisa

  METHOD get_info_grupo_funcao.

    DATA: lw_info               TYPE dd30v,
          lt_functions          TYPE STANDARD TABLE OF rs38l_incl,
          lw_function           TYPE rs38l_incl,
          lw_tadir              TYPE tadir,
          lv_grupo              TYPE tlibg-area,
          l_global              TYPE rs38l-global,
          l_remote              TYPE rs38l-remote,
          l_utask               TYPE rs38l-utask,
          l_exception_classes   TYPE enlfdir-exten3,
          lt_exception_list     TYPE STANDARD TABLE OF rsexc,
          lw_exception_list     TYPE rsexc,
          lt_export_parameter   TYPE STANDARD TABLE OF rsexp,
          lw_export_parameter   TYPE rsexp,
          lt_import_parameter   TYPE STANDARD TABLE OF rsimp,
          lw_import_parameter   TYPE rsimp,
          lt_changing_parameter TYPE STANDARD TABLE OF rscha,
          lw_changing_parameter TYPE rscha,
          lt_tables_parameter   TYPE STANDARD TABLE OF rstbl,
          lw_tables_parameter   TYPE rstbl,
          lt_p_docu             TYPE STANDARD TABLE OF rsfdo,
          lt_enha_exp_parameter TYPE STANDARD TABLE OF rsexp,
          lt_enha_imp_parameter TYPE STANDARD TABLE OF rsimp,
          lt_enha_cha_parameter TYPE STANDARD TABLE OF rscha,
          lt_enha_tbl_parameter TYPE STANDARD TABLE OF rstbl,
          lt_enha_docu          TYPE STANDARD TABLE OF rsfdo.

    lv_grupo              = pw_tadir-obj_name.

    "Retorna todas as funções do grupo de funções
    CALL FUNCTION 'RS_FUNCTION_POOL_CONTENTS'
      EXPORTING
        function_pool           = lv_grupo
      TABLES
        functab                 = lt_functions
      EXCEPTIONS
        function_pool_not_found = 1.

    IF sy-subrc <> 0.
      EXIT.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    "Loop nas funções
    LOOP AT lt_functions INTO lw_function.

      "Carrega dados e parâmetros da função
      CALL FUNCTION 'FUNCTION_IMPORT_INTERFACE'
        EXPORTING
          funcname           = lw_function-funcname
        IMPORTING
          global_flag        = l_global
          remote_call        = l_remote
          update_task        = l_utask
          exception_classes  = l_exception_classes
        TABLES
          exception_list     = lt_exception_list
          export_parameter   = lt_export_parameter
          import_parameter   = lt_import_parameter
          changing_parameter = lt_changing_parameter
          tables_parameter   = lt_tables_parameter
          p_docu             = lt_p_docu
          enha_exp_parameter = lt_enha_exp_parameter
          enha_imp_parameter = lt_enha_imp_parameter
          enha_cha_parameter = lt_enha_cha_parameter
          enha_tbl_parameter = lt_enha_tbl_parameter
          enha_docu          = lt_enha_docu
        EXCEPTIONS
          error_message      = 1
          function_not_found = 2
          invalid_name       = 3.

      IF sy-subrc <> 0.
        CONTINUE.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      "Atualiza parâmetros de importação
      LOOP AT lt_import_parameter INTO lw_import_parameter.

        CLEAR: lw_tadir.
        lw_tadir-obj_name   = lw_import_parameter-typ.

        get_obj_relacionados(
            EXPORTING
                obj_name = lw_tadir-obj_name
            CHANGING
                pt_tadir = pt_tadir ).

      ENDLOOP.

      "Atualiza parâmetros de exportação
      LOOP AT lt_export_parameter INTO lw_export_parameter.

        CLEAR: lw_tadir.
        lw_tadir-obj_name   = lw_export_parameter-typ.

        get_obj_relacionados(
            EXPORTING
                obj_name = lw_tadir-obj_name
            CHANGING
                pt_tadir = pt_tadir ).

      ENDLOOP.

      "Atualiza parâmetros changing
      LOOP AT lt_changing_parameter INTO lw_changing_parameter.

        CLEAR: lw_tadir.
        lw_tadir-obj_name   = lw_changing_parameter-typ.

        get_obj_relacionados(
            EXPORTING
                obj_name = lw_tadir-obj_name
            CHANGING
                pt_tadir = pt_tadir ).

      ENDLOOP.

      "Atualiza parâmetros tables
      LOOP AT lt_tables_parameter INTO lw_tables_parameter.

        CLEAR: lw_tadir.
        lw_tadir-obj_name   = lw_tables_parameter-typ.

        get_obj_relacionados(
            EXPORTING
                obj_name = lw_tadir-obj_name
            CHANGING
                pt_tadir = pt_tadir ).

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.                    "get_info_grupo_funcao

  METHOD object_verified.

    CLEAR: pv_result.

    "Verifica se é um pacote de produtos
    IF pw_tadir-devclass(3) <> c_prefix_devc.
      EXIT.
    ENDIF.

    "Verifica se o pacote do objeto verificado é diferente
    "do pacote que foi selecionado
    IF pw_tadir-devclass = me->o_data->o_sel->v_package.
      EXIT.
    ENDIF.

    pv_result = 'X'.

  ENDMETHOD.                    "object_verified

  METHOD check_obj_exist.

    CLEAR: pv_result.

    READ TABLE pt_tadir TRANSPORTING NO FIELDS WITH KEY pgmid       = pw_tadir-pgmid
                                                        object      = pw_tadir-object
                                                        obj_name    = pw_tadir-obj_name BINARY SEARCH.

    CHECK sy-subrc = 0.

    pv_result = 'X'.

  ENDMETHOD.                    "check_obj_exist

  METHOD check_own_package.

    "Verifica se o pacote do objeto verificado é diferente
    "do pacote que foi selecionado
    IF pw_tadir-devclass <> me->o_data->o_sel->v_package.
      EXIT.
    ENDIF.

    pv_result = 'X'.

  ENDMETHOD.                    "check_own_package

  METHOD add_obj_to_table.

    DATA: lw_tadir TYPE tadir,
          lt_tadir TYPE yts_tadir.

    "Info do pacote adicionado
    lw_tadir-pgmid = c_complete_obj.
    lw_tadir-object = c_pacote.
    lw_tadir-obj_name = pw_tadir-devclass.
    IF check_obj_exist( pw_tadir = lw_tadir pt_tadir = pt_tadir ) IS INITIAL.
      select_tadir(
        EXPORTING
            pgmid    = c_complete_obj
            object   = lw_tadir-object
            obj_name = lw_tadir-obj_name
        RECEIVING
            pt_tadir = lt_tadir ).

      LOOP AT lt_tadir INTO lw_tadir.
        INSERT lw_tadir INTO TABLE pt_tadir.
      ENDLOOP.

    ENDIF.

    CHECK check_obj_exist( pw_tadir = pw_tadir pt_tadir = pt_tadir ) IS INITIAL.

    INSERT pw_tadir INTO TABLE pt_tadir.

    pv_result = 'X'.

  ENDMETHOD.                    "add_obj_to_table

  METHOD select_tadir.

    IF object IS INITIAL.
      SELECT *
        FROM tadir
        INTO TABLE pt_tadir
        WHERE pgmid = pgmid
          AND obj_name = obj_name.

      IF sy-subrc <> 0.
        CLEAR: pt_tadir.
      ENDIF.
    ELSE.
      SELECT *
        FROM tadir
        INTO TABLE pt_tadir
        WHERE pgmid = pgmid
          AND object = object
          AND obj_name = obj_name.

      IF sy-subrc <> 0.
        CLEAR: pt_tadir.
      ENDIF.
    ENDIF.

*    SELECT *
*      FROM tadir
*      INTO pw_tadir
*      WHERE pgmid = pgmid
*        AND obj_name = obj_name.
*
*      EXIT.
*
*    ENDSELECT.
*
*    IF sy-subrc <> 0.
*      CLEAR: pw_tadir.
*    ENDIF.

  ENDMETHOD.                    "select_tadir

  METHOD get_obj_relacionados.

    DATA: lw_tadir TYPE tadir,
          lt_tadir TYPE yts_tadir.

    CLEAR: lw_tadir.

    select_tadir(
    EXPORTING
      pgmid    = c_complete_obj
*        object   = c_tabela
      obj_name = obj_name
    RECEIVING
      pt_tadir = lt_tadir
  ).

    CHECK sy-subrc = 0.

    LOOP AT lt_tadir INTO lw_tadir.

      get_relacionados(
        EXPORTING
          pw_tadir = lw_tadir
        CHANGING
          pt_tadir = pt_tadir
      ).
    ENDLOOP.

  ENDMETHOD.                    "get_obj_relacionados

ENDCLASS.                    "lcl_request IMPLEMENTATION

"----------------------------------------------------------------------
" Tipos
"----------------------------------------------------------------------
TYPE-POOLS: trwbo.

"----------------------------------------------------------------
" Tables
"----------------------------------------------------------------
TABLES: stxbitmaps,
        tadir.

"----------------------------------------------------------------
" Declarações Globais
"----------------------------------------------------------------
DATA: gv_okcode       TYPE sy-ucomm,
      gt_tadir        TYPE scts_tadir,
      gt_e071k        TYPE tr_keys,
      go_dependencies TYPE REF TO /vtpr/cl_check_dependencies,
      go_request      TYPE REF TO lcl_request,
      go_alv          TYPE REF TO cl_gui_alv_grid.
