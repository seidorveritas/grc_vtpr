*&---------------------------------------------------------------------*
*&  Include  /vtpr/adiciona_objs_pacote_f01
*&---------------------------------------------------------------------*

"-----------------------------------------------------------------------
" Form rotina_principal
"-----------------------------------------------------------------------
FORM rotina_principal.

  PERFORM inicializa_globais.

  PERFORM seleciona_objetos CHANGING gt_tadir.

  PERFORM checa_dependencias USING gt_tadir.

  PERFORM seleciona_imagens CHANGING gt_tadir
                                     gt_e071k.

  IF gt_tadir[] IS INITIAL.
    MESSAGE s002(/vtpr/main) DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  CALL SCREEN 9000.

ENDFORM. " rotina_principal

"-----------------------------------------------------------------------
" Form inicializa_globais
"-----------------------------------------------------------------------
FORM inicializa_globais.

  DATA: lx_root TYPE REF TO cx_root,
        lv_msg  TYPE string.

  REFRESH: gt_tadir.

  FREE: go_dependencies.

  TRY.
      CREATE OBJECT go_dependencies
        EXPORTING
          iv_prefix = p_prefix.

      go_dependencies->set_include_interface( p_ooint ).
      go_dependencies->set_include_superclass( p_oosup ).
      go_dependencies->set_include_subclass( p_oosub ).

    CATCH cx_root INTO lx_root.
      lv_msg = lx_root->get_text( ).
      MESSAGE lv_msg TYPE 'E'.
  ENDTRY.

ENDFORM. " inicializa_globais

"-----------------------------------------------------------------------
" Form seleciona_objetos
"-----------------------------------------------------------------------
FORM seleciona_objetos CHANGING pt_tadir TYPE scts_tadir.

  PERFORM selecionar_objs_tadir CHANGING pt_tadir.

  PERFORM selecionar_formularios CHANGING pt_tadir.

  PERFORM selecionar_obj_mime CHANGING pt_tadir.

  PERFORM verifica_nome_objs CHANGING pt_tadir.

ENDFORM. " seleciona_objetos

"-----------------------------------------------------------------------
" Form CHECA_DEPENDENCIAS
"-----------------------------------------------------------------------
FORM checa_dependencias USING pt_tadir TYPE scts_tadir.

  FIELD-SYMBOLS: <fs> LIKE LINE OF s_object.

  DATA: lt_range TYPE /VTPR/t_RANGE,
        lw_range TYPE /VTPR/S_RANGE.

  LOOP AT s_object ASSIGNING <fs>.

    MOVE-CORRESPONDING <fs> TO lw_range.
    APPEND lw_range TO lt_range.

  ENDLOOP.

  go_dependencies->check_dependencies(
    EXPORTING
        ir_object   = lt_range
        it_objs     = pt_tadir
        iv_add_devc = p_indevc
    IMPORTING
        et_dependencies = gt_tadir ).

ENDFORM. " CHECA_DEPENDENCIAS

"-----------------------------------------------------------------------
" Form PREPARA_9000
"-----------------------------------------------------------------------
FORM prepara_9000.

  DATA: lt_fieldcat_parc TYPE lvc_t_fcat,
        lw_layout        TYPE lvc_s_layo,
        lw_variant       TYPE disvariant,
        lw_stbl          TYPE lvc_s_stbl,
        lt_exclude       TYPE ui_functions,
        lt_fcat          TYPE lvc_t_fcat,
        lw_tadir         TYPE tadir.

  DATA: lo_cont  TYPE REF TO cl_gui_docking_container.

  IF go_alv IS INITIAL.

    CREATE OBJECT lo_cont
      EXPORTING
        extension = 2000.

    PERFORM f_elimina_botoes CHANGING lt_exclude.

    CREATE OBJECT go_alv
      EXPORTING
        i_parent = lo_cont.

    CLEAR lw_layout.
    lw_layout-zebra      = 'X'.
    lw_layout-cwidth_opt = 'X'.
    lw_layout-sel_mode   = 'A'.
*    lw_layout-edit_MODE  = 'X'.

    CLEAR lw_variant.
    lw_variant-report  = sy-cprog.

    lt_fcat = /vtpr/_cl_utils=>get_fieldcat( lw_tadir ).

    CALL METHOD go_alv->set_table_for_first_display
      EXPORTING
        is_layout            = lw_layout
*       i_structure_name     = 'TADIR'
        is_variant           = lw_variant
        i_save               = 'X'
*       i_default            =
        it_toolbar_excluding = lt_exclude
      CHANGING
*       it_sort              = t_sort
        it_outtab            = gt_tadir
        it_fieldcatalog      = lt_fcat.

  ELSE.

    CALL METHOD go_alv->refresh_table_display
      EXPORTING
        i_soft_refresh = 'X'
      EXCEPTIONS
        finished       = 1.

  ENDIF.

ENDFORM. " PREPARA_9000

"-----------------------------------------------------------------------
" Form salvar
"-----------------------------------------------------------------------
FORM salvar.

  DATA :  lt_index_rows TYPE lvc_t_row,
          lw_index_rows TYPE lvc_s_row,
          lt_tadir      TYPE scts_tadir,
          lw_tadir      TYPE tadir,
          lv_lock       TYPE c VALUE 'X'.

  CALL METHOD go_alv->get_selected_rows
    IMPORTING
      et_index_rows = lt_index_rows.

  LOOP AT lt_index_rows INTO lw_index_rows.
    READ TABLE gt_tadir INTO lw_tadir INDEX lw_index_rows-index.

    CHECK sy-subrc = 0.

    APPEND lw_tadir TO lt_tadir.

  ENDLOOP.

  IF p_nblock IS NOT INITIAL.
    CLEAR: lv_lock.
  ENDIF.

  go_dependencies->add_objs_to_request(
    iv_lock_objs = lv_lock
    it_objs = lt_tadir
    it_keys = gt_e071k
    ).

  IF sy-subrc = 0.
    MESSAGE s006(/vtpr/main).
  ENDIF.

ENDFORM. " salvar

"-----------------------------------------------------------------------
" Form trata_exibicao_campos
"-----------------------------------------------------------------------
FORM trata_exibicao_campos.

  LOOP AT SCREEN.

    CASE screen-group1.
      WHEN 'PKG'.
        IF p_sdevc IS INITIAL.
          screen-invisible = 1.
          screen-active    = 0.
        ENDIF.
        screen-required = 2.
      WHEN 'OBJ'.
        IF p_sobj IS INITIAL.
          screen-invisible = 1.
          screen-active    = 0.
        ENDIF.
        screen-required = 2.
      WHEN 'SSF'.
        IF p_incssf IS INITIAL.
          screen-invisible = 1.
          screen-active = 0.
        ELSE.
          screen-invisible = 0.
          screen-active = 1.
        ENDIF.
        screen-required = 2.
      WHEN 'IMG'.
        IF p_images IS INITIAL.
          screen-invisible = 1.
          screen-active = 0.
        ELSE.
          screen-invisible = 0.
          screen-active = 1.
        ENDIF.
        screen-required = 2.
      WHEN OTHERS.
        screen-invisible = 0.
    ENDCASE.
    MODIFY SCREEN.
  ENDLOOP.

ENDFORM. " trata_exibicao_campos

"-----------------------------------------------------------------------
" Form valida_selecao
"-----------------------------------------------------------------------
FORM valida_selecao.

  CHECK sy-ucomm <> 'SELECAO'.

  CASE 'X'.
    WHEN p_sdevc.
      IF s_devc[] IS INITIAL.
        MESSAGE e003(/vtpr/main).
      ENDIF.
    WHEN p_sobj.
      IF s_objnam[] IS INITIAL.
        MESSAGE e003(/vtpr/main).
      ENDIF.
  ENDCASE.

ENDFORM. " valida_selecao

"-----------------------------------------------------------------------
" Form set_status_9000
"-----------------------------------------------------------------------
FORM set_status_9000.

  DATA: lv_lines TYPE i.

  lv_lines = lines( gt_tadir ).

  SET PF-STATUS 'S9000'.
  SET TITLEBAR 'T9000' WITH lv_lines.

ENDFORM. " set_status_9000

"TR_CHECK_OBJECT_LOCK

"-----------------------------------------------------------------------
" Form verifica_nome
"-----------------------------------------------------------------------
FORM verifica_nome_objs CHANGING pt_tadir TYPE scts_tadir.

  DATA: lt_tadir TYPE scts_tadir,
        lw_tadir TYPE tadir.

  CHECK pt_tadir[] IS NOT INITIAL.

  lt_tadir[] = pt_tadir[].
  REFRESH: pt_tadir.

  LOOP AT lt_tadir INTO lw_tadir.

    CHECK lw_tadir-obj_name IN go_dependencies->t_prefix[].

    APPEND lw_tadir TO pt_tadir.

  ENDLOOP.

ENDFORM. " verifica_nome

"-----------------------------------------------------------------------
" Form selecionar_objs_tadir
"-----------------------------------------------------------------------
FORM selecionar_objs_tadir CHANGING pt_tadir TYPE scts_tadir.

  CASE 'X'.
    WHEN p_sdevc.
      SELECT *
        FROM tadir
        INTO TABLE pt_tadir
        WHERE devclass IN s_devc[].
    WHEN p_sobj.
      SELECT *
        FROM tadir
        INTO TABLE pt_tadir
        WHERE object   IN s_object[]
          AND obj_name IN s_objnam[].
  ENDCASE.

  IF sy-subrc <> 0.
    REFRESH: pt_tadir.
  ENDIF.

ENDFORM. " selecionar_objs_tadir

"-----------------------------------------------------------------------
" Form selecionar_formularios
"-----------------------------------------------------------------------
FORM selecionar_formularios CHANGING pt_tadir TYPE scts_tadir.

  DATA: lt_object TYPE /VTPR/t_RANGE,
        lw_object TYPE /VTPR/S_RANGE,
        lt_tadir  TYPE scts_tadir.

  CHECK p_incssf IS NOT INITIAL.

  lw_object-sign    = 'I'.
  lw_object-option  = 'EQ'.
  lw_object-low     = /vtpr/cl_check_dependencies=>object_smartforms.
  APPEND lw_object TO lt_object.

  lw_object-low     = /vtpr/cl_check_dependencies=>object_smartstyles.
  APPEND lw_object TO lt_object.

  lw_object-low     = /vtpr/cl_check_dependencies=>object_sapscript.
  APPEND lw_object TO lt_object.

  IF p_namssf[] IS INITIAL.
    p_namssf-sign    = 'I'.
    p_namssf-option  = 'CP'.
    CONCATENATE go_dependencies->v_prefix '*' INTO p_namssf-low.
    APPEND p_namssf.
  ENDIF.

  SELECT *
    FROM tadir
    INTO TABLE lt_tadir
    WHERE object    IN lt_object[]
      AND obj_name  IN p_namssf[].

  IF sy-subrc <> 0.
    REFRESH: lt_tadir.
  ENDIF.

  APPEND LINES OF lt_tadir TO pt_tadir.

ENDFORM. " selecionar_formularios

*&---------------------------------------------------------------------*
*&      Form  F_ELIMINA_BOTOES
*&---------------------------------------------------------------------*
FORM f_elimina_botoes  CHANGING pt_exclude TYPE ui_functions.

  REFRESH pt_exclude.
*  APPEND cl_gui_alv_grid=>mc_fc_views          TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_graph          TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_info           TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_call_abc       TO pt_exclude.
*  APPEND cl_gui_alv_grid=>mc_fc_to_office      TO pt_exclude.
*  APPEND cl_gui_alv_grid=>mc_fc_send           TO pt_exclude.
*  APPEND cl_gui_alv_grid=>mc_fc_html           TO pt_exclude.
*  APPEND cl_gui_alv_grid=>mc_mb_sum            TO pt_exclude.
*  APPEND cl_gui_alv_grid=>mc_fc_check          TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_append_row TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row   TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_undo       TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy       TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_cut        TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_paste      TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_paste_new_row TO pt_exclude.
*  APPEND cl_gui_alv_grid=>mc_fc_url_copy_to_clipboard TO pt_exclude.

ENDFORM.                    " F_ELIMINA_BOTOES

"-----------------------------------------------------------------------
" Form selecionar_obj_mime
"-----------------------------------------------------------------------
FORM selecionar_obj_mime CHANGING pt_tadir TYPE scts_tadir.

  DATA: lt_object TYPE /VTPR/t_RANGE,
        lw_object TYPE /VTPR/S_RANGE,
        lt_tadir  TYPE scts_tadir.

  CHECK p_web IS NOT INITIAL.

  lw_object-sign    = 'I'.
  lw_object-option  = 'EQ'.
  lw_object-low     = /vtpr/cl_check_dependencies=>object_mime_file.
  APPEND lw_object TO lt_object.

  lw_object-low     = /vtpr/cl_check_dependencies=>object_html_file.
  APPEND lw_object TO lt_object.

  IF p_namssf[] IS INITIAL.
    p_namssf-sign    = 'I'.
    p_namssf-option  = 'CP'.
    CONCATENATE go_dependencies->v_prefix '*' INTO p_namssf-low.
    APPEND p_namssf.
  ENDIF.

  SELECT *
    FROM tadir
    INTO TABLE lt_tadir
    WHERE object    IN lt_object[]
      AND obj_name  IN p_namssf[].

  IF sy-subrc <> 0.
    REFRESH: lt_tadir.
  ENDIF.

  APPEND LINES OF lt_tadir TO pt_tadir.

ENDFORM. " selecionar_obj_mime

"-----------------------------------------------------------------------
" Form seleciona_imagens
"-----------------------------------------------------------------------
FORM seleciona_imagens CHANGING pt_tadir    TYPE scts_tadir
                                pt_keys     TYPE tr_keys.

  IF p_images IS INITIAL.
    RETURN.
  ENDIF.

  DATA: lt_tadir TYPE scts_tadir,
        lt_range TYPE /VTPR/t_RANGE,
        lw_range TYPE /VTPR/S_RANGE.

  IF s_images[] IS INITIAL.
    s_images-sign   = 'I'.
    s_images-option = 'CP'.
    CONCATENATE p_prefix '*' INTO s_images-low.
    APPEND s_images.
  ENDIF.

  LOOP AT s_images.
    MOVE-CORRESPONDING s_images TO lw_range.
    APPEND lw_range TO lt_range.
  ENDLOOP.

  /vtpr/cl_check_dependencies=>get_images_se78(
    EXPORTING
      ir_name  = lt_range
    IMPORTING
      et_tadir = lt_tadir
      et_e071k = pt_keys
  ).

  APPEND LINES OF lt_tadir TO pt_tadir.

  FREE: lt_tadir,
        lt_range.

ENDFORM. " seleciona_imagens
