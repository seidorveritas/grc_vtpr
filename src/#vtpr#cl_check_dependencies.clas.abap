class /VTPR/CL_CHECK_DEPENDENCIES definition
  public
  create public .

public section.
  type-pools ABAP .
  type-pools SEOC .
  type-pools SEOP .
  type-pools TRWBO .

  types:
        "----------------------------------------------------------------------
        " Tipos
        "----------------------------------------------------------------------
    yts_tadir TYPE SORTED TABLE OF tadir WITH UNIQUE KEY pgmid object obj_name .

    "----------------------------------------------------------------------
    " Constantes
    "----------------------------------------------------------------------
  constants PREFIX_FUNCTIONGROUP type TADIR-OBJ_NAME value 'SAPL'. "#EC NOTEXT
  constants PREFIX_LOGICALDB type TADIR-OBJ_NAME value 'SAPDB'. "#EC NOTEXT
  constants OBJECT_DOMAIN type TADIR-OBJECT value 'DOMA'. "#EC NOTEXT
  constants OBJECT_DATAELEMENT type TADIR-OBJECT value 'DTEL'. "#EC NOTEXT
  constants OBJECT_ENHACEMENT_SPOT type TADIR-OBJECT value 'ENHS'. "#EC NOTEXT
  constants OBJECT_TABLE type TADIR-OBJECT value 'TABL'. "#EC NOTEXT
  constants OBJECT_TYPETABLE type TADIR-OBJECT value 'TTYP'. "#EC NOTEXT
  constants OBJECT_SEARCH_HELP type TADIR-OBJECT value 'SHLP'. "#EC NOTEXT
  constants OBJECT_PACKAGE type TADIR-OBJECT value 'DEVC'. "#EC NOTEXT
  constants OBJECT_PROGRAM type TADIR-OBJECT value 'PROG'. "#EC NOTEXT
  constants OBJECT_CLASS type TADIR-OBJECT value 'CLAS'. "#EC NOTEXT
  constants OBJECT_MESSAGECLASS type TADIR-OBJECT value 'MSAG'. "#EC NOTEXT
  constants OBJECT_INTERFACE type TADIR-OBJECT value 'INTF'. "#EC NOTEXT
  constants OBJECT_FUNCTIONGROUP type TADIR-OBJECT value 'FUGR'. "#EC NOTEXT
  constants OBJECT_TRANSACTION type TADIR-OBJECT value 'TRAN'. "#EC NOTEXT
  constants OBJECT_SMARTFORMS type TADIR-OBJECT value 'SSFO'. "#EC NOTEXT
  constants OBJECT_SMARTSTYLES type TADIR-OBJECT value 'SSST'. "#EC NOTEXT
  constants OBJECT_BDS_LOGIC_INFO type TADIR-OBJECT value 'SBXL'. "#EC NOTEXT
  constants OBJECT_BDS_PHYSICAL_OBJ type TADIR-OBJECT value 'SBXP'. "#EC NOTEXT
  constants OBJECT_SAPSCRIPT type TADIR-OBJECT value 'FORM'. "#EC NOTEXT
  constants OBJECT_LOGICALDB type TADIR-OBJECT value 'LDBA'. "#EC NOTEXT
  constants OBJECT_TABLE_CONTENT type TADIR-OBJECT value 'TABU'. "#EC NOTEXT
  constants OBJECT_UPDATEVIEW type TADIR-OBJECT value 'TOBJ'. "#EC NOTEXT
  constants OBJECT_VIEW type TADIR-OBJECT value 'VIEW'. "#EC NOTEXT
  constants OBJECT_HTML_FILE type TADIR-OBJECT value 'W3TM'. "#EC NOTEXT
  constants OBJECT_MIME_FILE type TADIR-OBJECT value 'W3MI'. "#EC NOTEXT
  constants PGMID_COMPLETE type TADIR-PGMID value 'R3TR'. "#EC NOTEXT
  constants BITMAP_GRAPHICS type STXBITMAPS-TDOBJECT value 'GRAPHICS'. "#EC NOTEXT
    "----------------------------------------------------------------------
    " Declarações
    "----------------------------------------------------------------------
  data V_PREFIX type STRING read-only .
  data T_TADIR type YTS_TADIR read-only .
  data T_PREFIX type /VTPR/RANGE_T read-only .

    "----------------------------------------------------------------------
    " Métodos
    "----------------------------------------------------------------------
  methods CONSTRUCTOR
    importing
      !IV_PREFIX type STRING default '/VT' .
  methods SELECT_TADIR
  final
    importing
      !IV_PGMID type STRING default 'R3TR'
      !IV_OBJECT type STRING optional
      !IV_OBJ_NAME type STRING
    returning
      value(RT_TADIR) type SCTS_TADIR .
  methods CHECK_DEPENDENCIES
    importing
      !IR_OBJECT type /VTPR/RANGE_T
      !IT_OBJS type SCTS_TADIR
      !IV_ADD_DEVC type C default 'X'
    exporting
      !ET_DEPENDENCIES type SCTS_TADIR .
  methods CLEAR_DEPENDENCIES .
  methods SET_INCLUDE_SUPERCLASS
    importing
      !IV_INCLUDE type ABAP_BOOL .
  methods SET_INCLUDE_SUBCLASS
    importing
      !IV_INCLUDE type ABAP_BOOL .
  methods SET_INCLUDE_INTERFACE
    importing
      !IV_INCLUDE type ABAP_BOOL .
  class-methods ADD_OBJS_TO_REQUEST
    importing
      !IV_LOCK_OBJS type C default 'X'
      !IT_OBJS type SCTS_TADIR
      !IT_KEYS type TR_KEYS optional
    returning
      value(RW_REQUEST) type TRWBO_REQUEST_HEADER
    raising
      /VTAX/CX_EXCEPTION .
  methods GET_NOTATIONS_USED_BY_REPORT
    importing
      !IV_PROGNAME type ANY
      !IV_GET_STANDARD type C default ' '
    exporting
      !ET_FUNCTIONS type /VTPR/TFTIT_T
      !ET_INCLUDES type /VTPR/D010INC_T .
  class-methods GET_IMAGES_SE78
    importing
      !IR_NAME type /VTPR/RANGE_T
    exporting
      !ET_TADIR type SCTS_TADIR
      !ET_E071K type TR_KEYS .
  class-methods SELECT_TADIR_BY_RANGE_OBJ_NAME
    importing
      !IR_NAME type /VTPR/RANGE_T
      !IV_PGMID type STRING default 'R3TR'
    returning
      value(RT_TADIR) type SCTS_TADIR .
protected section.

    "----------------------------------------------------------------------
    " Declarações
    "----------------------------------------------------------------------
  data V_ADD_DEVC type C .
  data:
    t_object_type  TYPE STANDARD TABLE OF ko100 .
  data T_REF_CLASS type /VTPR/RANGE_T .
  data T_TYPE_MSGID type /VTPR/RANGE_T .

    "----------------------------------------------------------------------
    " Métodos
    "----------------------------------------------------------------------
  methods DEPENDENCIES_BY_NAME
    importing
      !IV_OBJ_NAME type ANY .
  methods DEPENDENCIES_BY_NOTATIONS
    importing
      !IT_FUNCTIONS type /VTPR/TFTIT_T
      !IT_INCLUDES type /VTPR/D010INC_T .
  methods GET_DEPENDENCIES
    importing
      !IW_TADIR type TADIR .
  methods OBJECT_IS_VALID
    importing
      !IW_OBJ type TADIR
    returning
      value(RV_VALIDO) type CHAR1 .
  methods CHECK_OBJ_NAME
    importing
      !IV_OBJ_NAME type ANY
    returning
      value(RV_VALIDO) type CHAR1 .
  methods SELECT_TYPES_REFERENCES
    importing
      !IW_OBJ type TADIR
    exporting
      !ET_D010TAB type /VTPR/D010TAB_T
      !ET_D010INC type /VTPR/D010INC_T .
  methods GET_ALL_CLASS_INCLUDES
    importing
      !CLASS_NAME type SEOCLSNAME
    returning
      value(RESULT) type SEOINCL_T
    exceptions
      NO_SUCH_CLASS .
  methods DOMA
    importing
      !IW_OBJ type TADIR .
  methods DTEL
    importing
      !IW_OBJ type TADIR .
  methods TABL
    importing
      !IW_OBJ type TADIR .
  methods TTYP
    importing
      !IW_OBJ type TADIR .
  methods SHLP
    importing
      !IW_OBJ type TADIR .
  methods DEVC
    importing
      !IW_OBJ type TADIR .
  methods PROG
    importing
      !IW_OBJ type TADIR .
  methods CLAS
    importing
      !IW_OBJ type TADIR .
  methods INTF
    importing
      !IW_OBJ type TADIR .
  methods FUGR
    importing
      !IW_OBJ type TADIR .
  methods LDBA
    importing
      !IW_OBJ type TADIR .
  methods TRAN
    importing
      !IW_OBJ type TADIR .
  methods ENHS
    importing
      !IW_OBJ type TADIR .
  methods VIEW
    importing
      !IW_OBJ type TADIR .
  methods GET_VIEWMAINTENANCE
    importing
      !IV_OBJECT type TADIR-OBJECT
      !IV_OBJ_NAME type TADIR-OBJ_NAME .
  methods GET_TCODE_SM30
    importing
      !IW_OBJ type TADIR .
  methods GET_CLASS_RELATIONS
    importing
      !IV_OBJ_NAME type CSEQUENCE .
  methods GET_TYPE_OBJECT
    importing
      !IV_NAME type ANY
    returning
      value(R_RESULT) type STRING .
  methods CHECK_OO_DEPENDENCIES
    importing
      !IV_OBJ_NAME type CSEQUENCE .
  methods ADD_METHODS_DEPENDENCIES
    importing
      !IO_OBJ type ref to CL_ABAP_OBJECTDESCR .
  methods ADD_INTERFACES_DEPENDENCIES
    importing
      !IO_OBJ type ref to CL_ABAP_OBJECTDESCR .
  methods ADD_ATTRIBUTES_DEPENDENCIES
    importing
      !IO_OBJ type ref to CL_ABAP_OBJECTDESCR .
  methods ADD_EVENTS_DEPENDENCIES
    importing
      !IO_OBJ type ref to CL_ABAP_OBJECTDESCR .
private section.

  data V_ATUAL type I .
  data V_TOTAL type I .
  data V_SUPERCLASS type ABAP_BOOL .
  data V_SUBCLASS type ABAP_BOOL .
  data V_INTERFACE type ABAP_BOOL .

  methods ADD_NUMBER_OBJECTS_TOTAL
    importing
      !IV_NUMBER type I .
  methods UPDATE_INDICADOR
    importing
      !IW_TADIR type TADIR .
ENDCLASS.



CLASS /VTPR/CL_CHECK_DEPENDENCIES IMPLEMENTATION.


METHOD add_attributes_dependencies.

    DATA: lw_tadir      TYPE tadir,
          lw_attribute  TYPE seocmpkey,
          lw_properties TYPE vseoattrib.

    FIELD-SYMBOLS: <fs_attr> LIKE LINE OF io_obj->attributes.

    LOOP AT io_obj->attributes ASSIGNING <fs_attr>.

      IF <fs_attr>-is_inherited EQ abap_true.
        CONTINUE.
      ENDIF.

      IF check_obj_name( <fs_attr>-name ) NE abap_true.
        CONTINUE.
      ENDIF.

      lw_attribute-clsname = io_obj->get_relative_name( ).
      lw_attribute-cmpname = <fs_attr>-name.

      CALL FUNCTION 'SEO_ATTRIBUTE_GET'
        EXPORTING
          attkey    = lw_attribute
        IMPORTING
          attribute = lw_properties.

      CLEAR: lw_tadir.
      lw_tadir-object     = get_type_object( lw_properties-type ).
      IF lw_tadir-object IS INITIAL.
        dependencies_by_name( lw_properties-type ).
      ELSE.
        lw_tadir-pgmid      = pgmid_complete.
        lw_tadir-obj_name   = lw_properties-type.
        get_dependencies( iw_tadir = lw_tadir ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


METHOD add_events_dependencies.

    "Não implementado

  ENDMETHOD.


METHOD add_interfaces_dependencies.

    DATA: lw_tadir TYPE tadir.

    FIELD-SYMBOLS: <fs_intf> LIKE LINE OF io_obj->interfaces.

    LOOP AT io_obj->interfaces ASSIGNING <fs_intf>.

      IF <fs_intf>-is_inherited EQ abap_true.
        CONTINUE.
      ENDIF.

      IF check_obj_name( <fs_intf>-name ) NE abap_true.
        CONTINUE.
      ENDIF.

      CLEAR: lw_tadir.
      lw_tadir-pgmid      = pgmid_complete.
      lw_tadir-object     = object_interface.
      lw_tadir-obj_name   = <fs_intf>-name.
      get_dependencies( iw_tadir = lw_tadir ).

    ENDLOOP.

  ENDMETHOD.


METHOD add_methods_dependencies.

    DATA: lw_param      TYPE seoscokey,
          lw_param_info TYPE vseoparam,
          lw_tadir      TYPE tadir.

    FIELD-SYMBOLS: <fs_method> LIKE LINE OF io_obj->methods,
                   <fs_param>  LIKE LINE OF <fs_method>-parameters.

    LOOP AT io_obj->methods ASSIGNING <fs_method>.

      IF <fs_method>-is_inherited EQ abap_true OR
         <fs_method>-is_redefined EQ abap_true.
        CONTINUE.
      ENDIF.

      IF <fs_method>-alias_for IS NOT INITIAL.
        CONTINUE.
      ENDIF.

      LOOP AT <fs_method>-parameters ASSIGNING <fs_param>.

        lw_param-clsname = io_obj->get_relative_name( ).
        lw_param-cmpname = <fs_method>-name.
        lw_param-sconame = <fs_param>-name.

        CALL FUNCTION 'SEO_PARAMETER_GET'
          EXPORTING
            parkey       = lw_param
            version      = seoc_version_active
          IMPORTING
            parameter    = lw_param_info
          EXCEPTIONS
            not_existing = 1
            deleted      = 2
            is_exception = 3
            OTHERS       = 4.

        IF sy-subrc <> 0.
          CONTINUE.
*         MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        IF check_obj_name( lw_param_info-type ) NE abap_true.
          CONTINUE.
        ENDIF.

        CLEAR: lw_tadir.
        lw_tadir-pgmid      = pgmid_complete.
        lw_tadir-obj_name   = lw_param_info-type.
        lw_tadir-object     = get_type_object( lw_param_info-type ).

        IF lw_tadir-object IS INITIAL.
          dependencies_by_name( lw_param_info-type ).
        ELSE.
          get_dependencies( iw_tadir = lw_tadir ).
        ENDIF.

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


METHOD add_number_objects_total.
    ADD iv_number TO me->v_total.
  ENDMETHOD.


METHOD add_objs_to_request.

    FIELD-SYMBOLS: <fs_obj> TYPE e071,
                   <fs_key> TYPE e071k.

    DATA: lt_objects TYPE tr_objects,
          lw_object  LIKE LINE OF lt_objects,
          lw_tadir   TYPE tadir,
          lv_lock    TYPE trparflag VALUE 'X'.

    CHECK it_objs[] IS NOT INITIAL.

    lv_lock = iv_lock_objs.

    LOOP AT it_objs INTO lw_tadir.

      CLEAR: lw_object.
      MOVE-CORRESPONDING lw_tadir TO lw_object.

      IF lw_object-object = object_table_content.
        lw_object-objfunc = 'K'.
      ENDIF.

      APPEND lw_object TO lt_objects.
    ENDLOOP.

    CALL FUNCTION 'TR_REQUEST_CHOICE'
      EXPORTING
        iv_suppress_dialog   = ' '
*       iv_request_types     =
*       iv_cli_dep           = SPACE
*       iv_request           = SPACE
        it_e071              = lt_objects
        it_e071k             = it_keys
        iv_lock_objects      = lv_lock
*       iv_title             =
*       iv_start_column      =
*       iv_start_row         =
*       iv_with_error_log    = 'X'
*       iv_no_owner_check    = SPACE
*       iv_foreign_request   = '   '
*       it_e071k_str         =
*       it_obj_entries       =
      IMPORTING
        es_request           = rw_request
      EXCEPTIONS
        invalid_request      = 1
        invalid_request_type = 2
        user_not_owner       = 3
        no_objects_appended  = 4
        enqueue_error        = 5
        cancelled_by_user    = 6
        recursive_call       = 7.

    IF sy-subrc <> 0.
      CLEAR: rw_request.
      RAISE EXCEPTION TYPE /vtax/cx_exception.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.


METHOD check_dependencies.

    FIELD-SYMBOLS: <fs_tadir>   TYPE tadir.

    DATA: lt_tadir TYPE scts_tadir.

    CHECK it_objs[] IS NOT INITIAL.

    clear_dependencies( ).

    me->v_add_devc      = iv_add_devc.

    CALL FUNCTION 'TR_OBJECT_TABLE'
      TABLES
        wt_object_text = me->t_object_type.

    DELETE me->t_object_type WHERE pgmid  <> pgmid_complete OR
                                   object NOT IN ir_object.

    SORT me->t_object_type BY object.

    lt_tadir[] = it_objs[].
    SORT lt_tadir BY object obj_name.
    DELETE ADJACENT DUPLICATES FROM lt_tadir COMPARING object obj_name.

    me->v_total = lines( lt_tadir ).

    LOOP AT lt_tadir ASSIGNING <fs_tadir>.

      get_dependencies(
          iw_tadir = <fs_tadir>
      ).

    ENDLOOP.

    et_dependencies[] = me->t_tadir[].

  ENDMETHOD.


METHOD check_obj_name.

    DATA: lw_range TYPE /vtpr/s_range.

    LOOP AT t_prefix INTO lw_range.

      CHECK iv_obj_name CP lw_range-low.

      rv_valido = 'X'.

      EXIT.

    ENDLOOP.

  ENDMETHOD.


METHOD check_oo_dependencies.

    DATA: lo_typedescr TYPE REF TO cl_abap_typedescr,
          lo_obj       TYPE REF TO cl_abap_objectdescr.

    cl_abap_intfdescr=>describe_by_name(
      EXPORTING
        p_name         = iv_obj_name
      RECEIVING
        p_descr_ref    = lo_typedescr
      EXCEPTIONS
        type_not_found = 1
        OTHERS         = 2
    ).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    lo_obj ?= lo_typedescr.

    add_interfaces_dependencies( io_obj = lo_obj ).
    add_attributes_dependencies( lo_obj ).
    add_methods_dependencies( lo_obj ).
    add_events_dependencies( lo_obj ).

  ENDMETHOD.


METHOD clas.

    DATA:
      lt_d010inc    TYPE /vtpr/d010inc_t,
      lw_d010inc    TYPE d010inc,
      lt_d010tab    TYPE /vtpr/d010tab_t,
      lw_d010tab    TYPE d010tab,
      lt_functions  TYPE /vtpr/tftit_t,
      lv_class_name TYPE seoclsname,
      lt_includes   TYPE seoincl_t,
      lw_include    TYPE programm,
      lv_lines      TYPE i.

    CHECK iw_obj-object = object_class.

    check_oo_dependencies( iw_obj-obj_name ).

    select_types_references(
      EXPORTING
        iw_obj = iw_obj
      IMPORTING
        et_d010tab  = lt_d010tab
        et_d010inc  = lt_d010inc
    ).

    lv_class_name = iw_obj-obj_name.

    me->get_all_class_includes(
      EXPORTING
        class_name    = lv_class_name
      RECEIVING
        result        = lt_includes
      EXCEPTIONS
        no_such_class = 1
    ).
    IF sy-subrc <> 0.
      FREE: lt_includes.
    ENDIF.

    lv_lines = lines( lt_d010tab )  +
               lines( lt_d010inc )  +
               lines( lt_includes ).

    add_number_objects_total( lv_lines ).

    LOOP AT lt_d010tab INTO lw_d010tab.
      dependencies_by_name( iv_obj_name = lw_d010tab-tabname ).
    ENDLOOP.

    get_class_relations( iw_obj-obj_name ).

    LOOP AT lt_d010inc INTO lw_d010inc.
      get_class_relations( lw_d010inc-include ).
*      dependencies_by_name( iv_obj_name = lw_d010inc-include ).
    ENDLOOP.

    LOOP AT lt_includes INTO lw_include.

      get_notations_used_by_report(
        EXPORTING
            iv_progname     = lw_include
        IMPORTING
            et_functions = lt_functions
            et_includes  = lt_d010inc
     ).

      lv_lines = lines( lt_functions )  +
                 lines( lt_d010inc ).

      add_number_objects_total( lv_lines ).

      dependencies_by_notations( it_functions = lt_functions
                                 it_includes  = lt_d010inc ).

    ENDLOOP.

  ENDMETHOD.


METHOD clear_dependencies.
    REFRESH: t_tadir.

    CLEAR: me->v_atual,
           me->v_total.
  ENDMETHOD.


METHOD constructor.

    DATA: lw_range TYPE /vtpr/s_range.

    me->v_prefix = iv_prefix.

    "Range de prefixo dos objetos
    CLEAR: lw_range.
    lw_range-sign      = 'I'.
    lw_range-option    = 'CP'.
    CONCATENATE iv_prefix '*' INTO lw_range-low.
    APPEND lw_range TO t_prefix.

    "Prefixo para referências de classes
    CLEAR: lw_range.
    lw_range-sign      = 'I'.
    lw_range-option    = 'CP'.
    CONCATENATE iv_prefix '*=CU' INTO lw_range-low.
    APPEND lw_range TO t_ref_class.

    CLEAR: lw_range.
    lw_range-sign      = 'I'.
    lw_range-option    = 'CP'.
    CONCATENATE iv_prefix '*=IU' INTO lw_range-low.
    APPEND lw_range TO t_ref_class.

    "Tipo de Classe de Mensagem na tabela CROSS
    CLEAR: lw_range.
    lw_range-sign      = 'I'.
    lw_range-option    = 'EQ'.
    lw_range-low       = '3'.
    APPEND lw_range TO t_type_msgid.

    CLEAR: lw_range.
    lw_range-sign      = 'I'.
    lw_range-option    = 'EQ'.
    lw_range-low       = 'N'.
    APPEND lw_range TO t_type_msgid.

  ENDMETHOD.


METHOD dependencies_by_name.

    DATA: lw_tadir    TYPE tadir,
          lt_tadir    TYPE scts_tadir,
          lv_obj_name TYPE string,
          lv_valido   TYPE char1.

    CHECK iv_obj_name IS NOT INITIAL.

    lv_valido = check_obj_name( iv_obj_name ).

    CHECK lv_valido IS NOT INITIAL.

    lv_obj_name = iv_obj_name.

    select_tadir(
      EXPORTING
*        iv_pgmid    = iv_pgmid
*        iv_object   = iv_object
        iv_obj_name = lv_obj_name
      RECEIVING
        rt_tadir    = lt_tadir
    ).

    LOOP AT lt_tadir INTO lw_tadir.

      get_dependencies( iw_tadir = lw_tadir ).

    ENDLOOP.

  ENDMETHOD.


METHOD dependencies_by_notations.

    DATA: lw_tadir      TYPE tadir,
          lw_function   TYPE tftit,
          lt_tfdir      TYPE STANDARD TABLE OF tfdir,
          lw_tfdir      TYPE tfdir,
          lt_group      TYPE STANDARD TABLE OF tfdir,
          lt_tftit      TYPE STANDARD TABLE OF tftit,
          lt_funct      TYPE STANDARD TABLE OF funct,
          lt_enlfdir    TYPE STANDARD TABLE OF enlfdir,
          lt_trdir      TYPE STANDARD TABLE OF trdir,
          lt_sfupararef TYPE STANDARD TABLE OF sfupararef,
          lt_uincl      TYPE STANDARD TABLE OF abaptxt255,
          lw_include    TYPE d010inc.

    LOOP AT it_functions INTO lw_function.
      CALL FUNCTION 'FUNC_GET_OBJECT'
        EXPORTING
          funcname           = lw_function-funcname
*         r3state            = r3state
        TABLES
          ptfdir             = lt_tfdir
          ptftit             = lt_tftit
          pfunct             = lt_funct
          penlfdir           = lt_enlfdir
          ptrdir             = lt_trdir
          pfupararef         = lt_sfupararef
          uincl              = lt_uincl
*         vsmodisrc          = vsmodisrc
*         vsmodilog          = vsmodilog
        EXCEPTIONS
          function_not_exist = 1
          version_not_found  = 2.

      IF sy-subrc <> 0.
        REFRESH: lt_tfdir.
      ENDIF.

      APPEND LINES OF lt_tfdir TO lt_group.

    ENDLOOP.

    SORT lt_group BY pname.
    DELETE ADJACENT DUPLICATES FROM lt_group COMPARING pname.
    LOOP AT lt_group INTO lw_tfdir.

      "Grupo de função
      CLEAR: lw_tadir.
      lw_tadir-pgmid      = pgmid_complete.
      lw_tadir-object     = object_functiongroup.
      lw_tadir-obj_name   = lw_tfdir-pname.
      get_dependencies( iw_tadir = lw_tadir ).

    ENDLOOP.

    LOOP AT it_includes INTO lw_include.

      CLEAR: lw_tadir.
      lw_tadir-pgmid      = pgmid_complete.
      lw_tadir-object     = object_program.
      lw_tadir-obj_name   = lw_include-include.

      get_dependencies( lw_tadir ).

    ENDLOOP.

  ENDMETHOD.


METHOD devc.

  ENDMETHOD.


METHOD doma.

    CHECK iw_obj-object = object_domain.

    DATA: lw_info      TYPE dd01v,
          lw_tadir     TYPE tadir,
          lv_doma_name TYPE ddobjname.

    lv_doma_name = iw_obj-obj_name.

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

    lw_tadir-pgmid      = pgmid_complete.
    lw_tadir-object     = object_table.
    lw_tadir-obj_name   = lw_info-entitytab.

    get_dependencies( iw_tadir = lw_tadir ).

  ENDMETHOD.


METHOD dtel.

    CHECK iw_obj-object = object_dataelement.

    DATA: lw_info  TYPE dd04v,
          lw_tadir TYPE tadir,
          lv_name  TYPE ddobjname.

    lv_name = iw_obj-obj_name.

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

    IF lw_info-domname IS NOT INITIAL.
      lw_tadir-pgmid      = pgmid_complete.
      lw_tadir-object     = object_domain.
      lw_tadir-obj_name   = lw_info-domname.
      add_number_objects_total( 1 ).

      get_dependencies( iw_tadir = lw_tadir ).

    ENDIF.

    IF lw_info-shlpname IS NOT INITIAL.
      lw_tadir-pgmid      = pgmid_complete.
      lw_tadir-object     = object_search_help.
      lw_tadir-obj_name   = lw_info-shlpname.
      add_number_objects_total( 1 ).

      get_dependencies( iw_tadir = lw_tadir ).
    ENDIF.

    IF lw_info-entitytab IS NOT INITIAL.

      lw_tadir-pgmid      = pgmid_complete.
      lw_tadir-object     = object_table.
      lw_tadir-obj_name   = lw_info-entitytab.
      add_number_objects_total( 1 ).

      get_dependencies( iw_tadir = lw_tadir ).
    ENDIF.

  ENDMETHOD.


METHOD enhs.

    FIELD-SYMBOLS: <fs> TYPE enhspotobj.

    DATA: lw_tadir           TYPE tadir,
          lt_interfaces_badi TYPE STANDARD TABLE OF enhspotobj,
          lv_lines           TYPE i.

    SELECT *
        FROM enhspotobj
        INTO TABLE lt_interfaces_badi
        WHERE enhspot = iw_obj-obj_name
          AND version = 'A'. "Versão ativa

    CHECK sy-subrc = 0.

    lv_lines = lines( lt_interfaces_badi ).
    add_number_objects_total( lv_lines ).

    LOOP AT lt_interfaces_badi ASSIGNING <fs>.

      CLEAR: lw_tadir.
      lw_tadir-pgmid    = <fs>-pgmid.
      lw_tadir-object   = <fs>-main_type.
      lw_tadir-obj_name = <fs>-main_name.
      get_dependencies( iw_tadir = lw_tadir ).

    ENDLOOP.

  ENDMETHOD.


METHOD fugr.

    DATA: lv_group_name TYPE rs38l-area,
          lv_namespace  TYPE rs38l-namespace,
          lt_d010inc    TYPE /vtpr/d010inc_t,
          lw_d010inc    TYPE d010inc,
          lt_d010tab    TYPE /vtpr/d010tab_t,
          lw_d010tab    TYPE d010tab,
          lw_tadir      TYPE tadir,
          lt_includes   TYPE STANDARD TABLE OF rs38l_incl,
          lw_include    TYPE rs38l_incl,
          lt_functions  TYPE /vtpr/tftit_t,
          lv_lines      TYPE i.

    CHECK iw_obj-object = object_functiongroup.

    CLEAR: lw_tadir.
    lw_tadir = iw_obj.

    FIND prefix_functiongroup IN lw_tadir-obj_name.

    IF sy-subrc <> 0.
      lv_group_name = lw_tadir-obj_name.

      CALL FUNCTION 'FUNCTION_INCLUDE_SPLIT'
        EXPORTING
*         program                      = program
*         suppress_select              = suppress_select
          complete_area                = lv_group_name
        IMPORTING
          namespace                    = lv_namespace
*         function_not_exists          = function_not_exists
          group                        = lv_group_name
*         funcname                     = funcname
*         include_number               = include_number
*         no_function_include          = no_function_include
*         no_function_module           = no_function_module
*         suffix                       = suffix
*         reserved_name                = reserved_name
*         too_many_delimiters          = too_many_delimiters
*         reserved_for_exits           = reserved_for_exits
*         hidden_name                  = hidden_name
*        CHANGING
*         include                      = include
        EXCEPTIONS
          include_not_exists           = 1
          group_not_exists             = 2
          no_selections                = 3
          no_function_include          = 4
          no_function_pool             = 5
          delimiter_wrong_position     = 6
          no_customer_function_group   = 7
          no_customer_function_include = 8
          reserved_name_customer       = 9
          namespace_too_long           = 10
          area_length_error            = 11.

      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
      CONCATENATE lv_namespace prefix_functiongroup lv_group_name INTO lw_tadir-obj_name.
    ENDIF.

    select_types_references(
      EXPORTING
        iw_obj = lw_tadir
      IMPORTING
        et_d010tab  = lt_d010tab
        et_d010inc  = lt_d010inc
    ).

    lv_lines = lines( lt_d010tab )  +
               lines( lt_d010inc ).

    add_number_objects_total( lv_lines ).

    LOOP AT lt_d010tab INTO lw_d010tab.
      dependencies_by_name( iv_obj_name = lw_d010tab-tabname ).
    ENDLOOP.

    LOOP AT lt_d010inc INTO lw_d010inc.
      dependencies_by_name( iv_obj_name = lw_d010inc-include ).
    ENDLOOP.

    lv_group_name = iw_obj-obj_name.

    REPLACE ALL OCCURRENCES OF prefix_functiongroup IN lv_group_name WITH ''.

    CALL FUNCTION 'RS_FUNCTION_POOL_CONTENTS'
      EXPORTING
        function_pool           = lv_group_name
      TABLES
        functab                 = lt_includes
      EXCEPTIONS
        function_pool_not_found = 1.

    IF sy-subrc <> 0.
      REFRESH: lt_includes.
    ENDIF.

    LOOP AT lt_includes INTO lw_include.

      get_notations_used_by_report(
        EXPORTING
            iv_progname     = lw_include-include
        IMPORTING
            et_functions = lt_functions
            et_includes  = lt_d010inc
     ).

      lv_lines = lines( lt_functions )  +
                 lines( lt_d010inc ).

      add_number_objects_total( lv_lines ).

      dependencies_by_notations( it_functions = lt_functions
                                 it_includes  = lt_d010inc ).

    ENDLOOP.

  ENDMETHOD.


METHOD get_all_class_includes.
    DATA: clskey          TYPE seoclskey,
          class_info      TYPE vseoclass,
          method_includes TYPE seop_methods_w_include,
          wa_result       TYPE programm.
    " call function module SEO_CLASS_GET to retrieve info which include names are available for a class
    clskey-clsname = class_name.
    CALL FUNCTION 'SEO_CLASS_GET'
      EXPORTING
        clskey       = clskey
      IMPORTING
        class        = class_info
      EXCEPTIONS
        not_existing = 1
        deleted      = 2
        is_interface = 3
        model_only   = 4
        OTHERS       = 5.
    IF sy-subrc <> 0.
      " error -> just return an empty list
      RAISE no_such_class.
      RETURN.
    ENDIF.
    " Local Classes (new or orld) = if VSEOCLASS-CLSCCINCL = 'X' -> CCDEF, CCMAC, CCIMP; if blank -> CL
    IF class_info-clsccincl = 'X'.
      "CCDEF
      wa_result = cl_oo_classname_service=>get_ccdef_name( class_name ).
      INSERT wa_result INTO TABLE result.
      "CCMAC
      wa_result = cl_oo_classname_service=>get_ccmac_name( class_name ).
      INSERT wa_result INTO TABLE result.
      "CCIMP
      wa_result = cl_oo_classname_service=>get_ccimp_name( class_name ).
      INSERT wa_result INTO TABLE result.
    ELSEIF class_info-clsccincl = ''.
      "CL
      wa_result = cl_oo_classname_service=>get_cl_name( class_name ).
      INSERT wa_result INTO TABLE result.
    ENDIF.


    " Abap Unit = if VSEOCLASS-WITH_UNIT_TEST -> CCAU
    IF class_info-with_unit_tests = 'X'.
      wa_result = cl_oo_classname_service=>get_ccau_name( class_name ).
      INSERT wa_result INTO TABLE result.
    ENDIF.

*  " Package Section = if VSEOCLASS-WITHIN_PACKAGE -> CA
*  if class_info-within_package = 'X'.
*    wa_result = cl_oo_classname_service=>get_paksec_name( class_name ).
*    insert wa_result into table result.
*  endif.

    " following include name are always relevant
    " CU = public section
    " CO = protected section
    " CI = private section
    " CP = class pool
    " CT = used internally to store compiler optimizations??
    " CS = complete source - currently not used!
    wa_result = cl_oo_classname_service=>get_pubsec_name( class_name ).
    INSERT wa_result INTO TABLE result.
    wa_result = cl_oo_classname_service=>get_prosec_name( class_name ).
    INSERT wa_result INTO TABLE result.
    wa_result = cl_oo_classname_service=>get_prisec_name( class_name ).
    INSERT wa_result INTO TABLE result.
    wa_result = cl_oo_classname_service=>get_classpool_name( class_name ).
    INSERT wa_result INTO TABLE result.
*    wa_result = cl_oo_classname_service=>get_ct_name( class_name ).
*    INSERT wa_result INTO TABLE result.
*    wa_result = cl_oo_classname_service=>get_cs_name( class_name ).
*    INSERT wa_result INTO TABLE result.

    " get all method include names
    cl_oo_classname_service=>get_all_method_includes( EXPORTING  clsname            = class_name
                                                      RECEIVING  result             = method_includes
                                                      EXCEPTIONS class_not_existing = 1 ).
    IF ( sy-subrc = 1 ).
      RAISE no_such_class.
    ENDIF.
    FIELD-SYMBOLS: <method_include> TYPE seop_method_w_include.
    LOOP AT method_includes ASSIGNING <method_include>.
      INSERT <method_include>-incname INTO TABLE result.
    ENDLOOP.
  ENDMETHOD.


METHOD get_class_relations.

    DATA: lv_class_name      TYPE seoclsname,
          lo_class_relations TYPE REF TO cl_oo_class_relations,
          lw_tadir           TYPE tadir,
          lv_lines           TYPE i.

    lv_class_name = iv_obj_name.

    CREATE OBJECT lo_class_relations
      EXPORTING
        clsname      = lv_class_name
*       w_superclasses  =
*       w_subclasses =
*       w_references =
*       w_redefinitions =
*       w_eventhandler  =
*       w_implementings =
      EXCEPTIONS
        is_interface = 1
        not_existing = 2
        OTHERS       = 3.

    CASE sy-subrc.
      WHEN 0.
        lw_tadir-pgmid      = pgmid_complete.
        lw_tadir-object     = object_class.
        lw_tadir-obj_name   = lv_class_name.
        get_dependencies( iw_tadir = lw_tadir ).
      WHEN 1.
        lw_tadir-pgmid      = pgmid_complete.
        lw_tadir-object     = object_interface.
        lw_tadir-obj_name   = lv_class_name.
        get_dependencies( iw_tadir = lw_tadir ).
      WHEN 2.
        lw_tadir-pgmid      = pgmid_complete.
        lw_tadir-object     = get_type_object( lv_class_name ).
        lw_tadir-obj_name   = lv_class_name.
        get_dependencies( iw_tadir = lw_tadir ).
        RETURN.
      WHEN OTHERS.
        RETURN.
    ENDCASE.

    lv_lines = lines( lo_class_relations->if_implementings ) +
               lines( lo_class_relations->superclasses )     +
               lines( lo_class_relations->subclasses ).

    add_number_objects_total( lv_lines ).

    FIELD-SYMBOLS: <fs_interface>  LIKE LINE OF lo_class_relations->if_implementings,
                   <fs_superclass> LIKE LINE OF lo_class_relations->superclasses,
                   <fs_subclass>   LIKE LINE OF lo_class_relations->subclasses.

    IF me->v_interface IS NOT INITIAL.
      LOOP AT lo_class_relations->if_implementings ASSIGNING <fs_interface>.

        lw_tadir-pgmid      = pgmid_complete.
        lw_tadir-object     = object_interface.
        lw_tadir-obj_name   = <fs_interface>-refclsname.

        get_dependencies( iw_tadir = lw_tadir ).
      ENDLOOP.
    ENDIF.

    IF me->v_superclass IS NOT INITIAL.
      LOOP AT lo_class_relations->superclasses ASSIGNING <fs_superclass>.

        lw_tadir-pgmid      = pgmid_complete.
        lw_tadir-object     = object_class.
        lw_tadir-obj_name   = <fs_superclass>-refclsname.

        get_dependencies( iw_tadir = lw_tadir ).
      ENDLOOP.
    ENDIF.

    IF me->v_subclass IS NOT INITIAL.
      LOOP AT lo_class_relations->subclasses ASSIGNING <fs_subclass>.

        lw_tadir-pgmid      = pgmid_complete.
        lw_tadir-object     = object_class.
        lw_tadir-obj_name   = <fs_subclass>-clsname.

        get_dependencies( iw_tadir = lw_tadir ).
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


METHOD get_dependencies.

    DATA: lv_valido TYPE c.

    CHECK iw_tadir IS NOT INITIAL AND iw_tadir-obj_name IS NOT INITIAL.

    lv_valido = object_is_valid( iw_tadir ).

    IF lv_valido IS INITIAL.
      ADD 1 TO me->v_atual.
*      update_indicador( iw_tadir ).
      RETURN.
    ENDIF.

    ADD: 1 TO me->v_atual.
*         1 TO me->v_total.
    update_indicador( iw_tadir ).

    TRY.
        CALL METHOD (iw_tadir-object)
          EXPORTING
            iw_obj = iw_tadir.
      CATCH cx_root.
        EXIT.
    ENDTRY.

  ENDMETHOD.


METHOD get_images_se78.

    FIELD-SYMBOLS: <fs_image> TYPE stxbitmaps.

    DATA: lt_images  TYPE STANDARD TABLE OF stxbitmaps,
          lt_bds     TYPE STANDARD TABLE OF bdsphio3,
          lw_bds     TYPE bdsphio3,
          lv_loio_id TYPE string,
          lw_tadir   TYPE tadir,
          lt_data    TYPE /vtpr/s_tabkeyt,
          lw_data    TYPE /vtpr/s_tabkey,
          lw_e071    TYPE e071,
          lt_e071    TYPE tr_objects.

    IF ir_name[] IS INITIAL.
      RETURN.
    ENDIF.

    SELECT *
      FROM stxbitmaps
      INTO TABLE lt_images
      WHERE tdobject = bitmap_graphics
        AND tdname   IN ir_name[].

    IF sy-subrc <> 0.
      FREE: lt_images.
      RETURN.
    ENDIF.

    LOOP AT lt_images ASSIGNING <fs_image>.

      SPLIT <fs_image>-docid AT space
        INTO lw_bds-lo_class
             lv_loio_id.

      CONDENSE lv_loio_id.
      lw_bds-loio_id = lv_loio_id.

      APPEND lw_bds TO lt_bds.

    ENDLOOP.

    SELECT *
        FROM bdsphio3
        INTO TABLE lt_bds
        FOR ALL ENTRIES IN lt_bds
            WHERE lo_class = lt_bds-lo_class
              AND loio_id  = lt_bds-loio_id.

    IF sy-subrc <> 0.
      FREE: lt_bds.
    ENDIF.

    CLEAR: lw_data.
    lw_data-tabname = 'STXBITMAPS'.
    GET REFERENCE OF lt_images INTO lw_data-data.
    APPEND lw_data TO lt_data.

    /vtpr/cl_utilities=>get_entries_table_content(
      EXPORTING
        it_data    = lt_data
      IMPORTING
        et_e071    = lt_e071
        et_e071k   = et_e071k
    ).

    LOOP AT lt_e071 INTO lw_e071.
      MOVE-CORRESPONDING lw_e071 TO lw_tadir.
      APPEND lw_tadir TO et_tadir.
    ENDLOOP.

    LOOP AT lt_bds INTO lw_bds.

      CLEAR: lw_tadir.
      lw_tadir-pgmid      = pgmid_complete.
      lw_tadir-object     = object_bds_logic_info.
      lw_tadir-obj_name   = lw_bds-loio_id.
      APPEND lw_tadir TO et_tadir.

      CLEAR: lw_tadir.
      lw_tadir-pgmid      = pgmid_complete.
      lw_tadir-object     = object_bds_physical_obj.
      lw_tadir-obj_name   = lw_bds-phio_id.
      APPEND lw_tadir TO et_tadir.

    ENDLOOP.

  ENDMETHOD.


METHOD get_notations_used_by_report.

    DATA: lt_code       TYPE STANDARD TABLE OF char255,
          lw_line_code  TYPE char255,
          lt_results    TYPE match_result_tab,
          lw_results    TYPE match_result,
          lv_line(100),
          lv_line1(100),
          lv_i          TYPE i,
          lv_idx        TYPE i,
          lw_function   TYPE tftit,
          lv_valido     TYPE c,
          lw_include    TYPE d010inc.

    CHECK iv_progname IS NOT INITIAL.

    READ REPORT iv_progname INTO lt_code.

    IF sy-subrc <> 0.
      REFRESH: et_functions.
      EXIT.
    ENDIF.

    FIND ALL OCCURRENCES OF REGEX 'CALL FUNCTION'
      IN TABLE lt_code
      RESPECTING CASE
      RESULTS lt_results.

    LOOP AT lt_results INTO lw_results.
      CLEAR: lv_line1, lv_idx.

      READ TABLE lt_code INTO lw_line_code INDEX lw_results-line.
      CHECK sy-subrc = 0.
      SPLIT lw_line_code AT 'CALL FUNCTION ' INTO lw_line_code lv_line.
      lv_i = strlen( lv_line ).
      lv_i = lv_i - 1.

      DO lv_i TIMES.
        lv_idx = lv_idx + 1.
        IF lv_idx <> lv_i.
          CONCATENATE lv_line1 lv_line+lv_idx(1) INTO lv_line1.
        ENDIF.
      ENDDO.

      IF iv_get_standard IS INITIAL.
        lv_valido = check_obj_name( lv_line1 ).

        CHECK lv_valido IS NOT INITIAL.
      ENDIF.

      lw_function-funcname = lv_line1.

      TRANSLATE lw_function TO UPPER CASE.

      APPEND lw_function TO et_functions.

    ENDLOOP.

    SORT et_functions BY funcname.
    DELETE ADJACENT DUPLICATES FROM et_functions COMPARING funcname.

    IF et_functions[] IS NOT INITIAL.

      SELECT *
          FROM tftit
          INTO TABLE et_functions
          FOR ALL ENTRIES IN et_functions
          WHERE funcname = et_functions-funcname.

      IF sy-subrc <> 0.
        REFRESH: et_functions.
      ENDIF.
    ENDIF.

    FIND ALL OCCURRENCES OF REGEX 'INCLUDE'
      IN TABLE lt_code
      RESPECTING CASE
      RESULTS lt_results.

    LOOP AT lt_results INTO lw_results.

      CLEAR: lw_include.

      READ TABLE lt_code INTO lw_line_code INDEX lw_results-line.

      CHECK sy-subrc = 0.
      lw_include-master = iv_progname.
      SPLIT lw_line_code AT 'INCLUDE ' INTO lw_line_code lw_include-include.
      REPLACE ALL OCCURRENCES OF '.' IN lw_include-include WITH ''.

      TRANSLATE lw_include-master  TO UPPER CASE.
      TRANSLATE lw_include-include TO UPPER CASE.

      APPEND lw_include TO et_includes.

    ENDLOOP.

  ENDMETHOD.


METHOD get_tcode_sm30.

    DATA: lt_range TYPE /vtpr/range_t,
          lw_range TYPE /vtpr/s_range,
          lw_tadir TYPE tadir,
          lt_tstcp TYPE STANDARD TABLE OF tstcp,
          lw_tstcp TYPE tstcp,
          lv_lines TYPE i.

    "Get transações
    REFRESH: lt_range.
    CLEAR: lw_range.
    lw_range-sign   = 'I'.
    lw_range-option = 'CP'.
    CONCATENATE '*' iw_obj-obj_name '*' INTO lw_range-low.

    APPEND lw_range TO lt_range.

    SELECT *
        FROM tstcp
        INTO TABLE lt_tstcp
        WHERE param IN lt_range[].

    IF sy-subrc <> 0.
      REFRESH: lt_tstcp.
    ENDIF.

    lv_lines = lines( lt_tstcp ).
    add_number_objects_total( lv_lines ).

    LOOP AT lt_tstcp INTO lw_tstcp.
      CLEAR: lw_tadir.
      lw_tadir-pgmid      = pgmid_complete.
      lw_tadir-object     = object_transaction.
      lw_tadir-obj_name   = lw_tstcp-tcode.
      get_dependencies( iw_tadir = lw_tadir ).
    ENDLOOP.

  ENDMETHOD.


METHOD get_type_object.

    DATA: lo_type  TYPE REF TO cl_abap_typedescr,
          lv_name  TYPE ddobjname,
          lw_dd02v TYPE dd02v,
          lw_dd09v TYPE dd09v.

    cl_abap_typedescr=>describe_by_name(
      EXPORTING
        p_name         = iv_name
      RECEIVING
        p_descr_ref    = lo_type
      EXCEPTIONS
        type_not_found = 1
        OTHERS         = 2
    ).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CASE lo_type->kind.
      WHEN cl_abap_typedescr=>kind_class.
        r_result     = object_class.
      WHEN cl_abap_typedescr=>kind_intf.
        r_result     = object_interface.
      WHEN cl_abap_typedescr=>kind_struct OR cl_abap_typedescr=>kind_table.

        lv_name = iv_name.

        CALL FUNCTION 'DDIF_TABL_GET'
          EXPORTING
            name          = lv_name
*           state         = 'A'
*           langu         = ' '
          IMPORTING
*           gotstate      =
            dd02v_wa      = lw_dd02v
            dd09l_wa      = lw_dd09v
*          TABLES
*           dd03p_tab     =
*           dd05m_tab     =
*           dd08v_tab     =
*           dd12v_tab     =
*           dd17v_tab     =
*           dd35v_tab     =
*           dd36m_tab     =
          EXCEPTIONS
            illegal_input = 1
            OTHERS        = 2.
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.

        IF lw_dd02v IS INITIAL.
          r_result = object_typetable.
        ELSE.
          r_result = object_table.
        ENDIF.

    ENDCASE.

  ENDMETHOD.


METHOD get_viewmaintenance.

    DATA: lw_tadir TYPE tadir,
          lw_tvdir TYPE tvdir,
          lv_type  TYPE c.

    "Get Sm30
    SELECT SINGLE *
        FROM tvdir
        INTO lw_tvdir
        WHERE tabname = iv_obj_name.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    "Visão de atualização.
    CLEAR: lw_tadir.
    lw_tadir-pgmid      = pgmid_complete.
    lw_tadir-object     = object_updateview.

    CASE iv_object.
      WHEN object_table.
        lv_type = 'S'.
      WHEN object_view.
        lv_type = 'V'.
    ENDCASE.

    CONCATENATE iv_obj_name lv_type INTO lw_tadir-obj_name.
    get_dependencies( iw_tadir = lw_tadir ).

    "Grupo de função
    CLEAR: lw_tadir.
    lw_tadir-pgmid      = pgmid_complete.
    lw_tadir-object     = object_functiongroup.
    lw_tadir-obj_name   = lw_tvdir-area.
    get_dependencies( iw_tadir = lw_tadir ).

  ENDMETHOD.


METHOD intf.

    TYPES:
      BEGIN OF ty_badi,
        badi_name      TYPE badi_main-badi_name,
        interface_name TYPE badi_main-interface_name,
        enhspotname    TYPE badi_spot-enhspotname,
      END OF ty_badi.

    DATA:
      lt_d010inc   TYPE /vtpr/d010inc_t,
      lw_d010inc   TYPE d010inc,
      lt_d010tab   TYPE /vtpr/d010tab_t,
      lw_d010tab   TYPE d010tab,
      lt_badi_main TYPE STANDARD TABLE OF ty_badi,
      lw_badi_main TYPE ty_badi,
      lw_tadir     TYPE tadir,
      lv_lines     TYPE i.

    CHECK iw_obj-object = object_interface.

    check_oo_dependencies( iw_obj-obj_name ).

    select_types_references(
      EXPORTING
        iw_obj = iw_obj
      IMPORTING
        et_d010tab  = lt_d010tab
        et_d010inc  = lt_d010inc
    ).

    SELECT spot~badi_name
           badi~interface_name
           spot~enhspotname
        FROM badi_main AS badi INNER JOIN badi_spot AS spot
        ON badi~badi_name = spot~badi_name
        INTO TABLE lt_badi_main
        WHERE interface_name = iw_obj-obj_name.

    IF sy-subrc <> 0.
      FREE: lt_badi_main.
    ENDIF.

    lv_lines = lines( lt_d010tab ) +
               lines( lt_d010inc ) +
               lines( lt_badi_main ).

    add_number_objects_total( lv_lines ).

    LOOP AT lt_d010tab INTO lw_d010tab.
      dependencies_by_name( iv_obj_name = lw_d010tab-tabname ).
    ENDLOOP.

    LOOP AT lt_d010inc INTO lw_d010inc.
      get_class_relations( lw_d010inc-include ).
    ENDLOOP.

    LOOP AT lt_badi_main INTO lw_badi_main.

      CLEAR: lw_tadir.

      lw_tadir-pgmid    = pgmid_complete.
      lw_tadir-object   = object_enhacement_spot.
      lw_tadir-obj_name = lw_badi_main-enhspotname.

      get_dependencies( iw_tadir = lw_tadir ).

    ENDLOOP.

  ENDMETHOD.


METHOD ldba.

    DATA: lv_ldb_name  TYPE rs38l-area,
          lv_namespace TYPE rs38l-namespace,
          lt_d010inc   TYPE /vtpr/d010inc_t,
          lw_d010inc   TYPE d010inc,
          lt_d010tab   TYPE /vtpr/d010tab_t,
          lw_d010tab   TYPE d010tab,
          lw_tadir     TYPE tadir,
          lt_includes  TYPE STANDARD TABLE OF rs38l_incl,
          lw_include   TYPE rs38l_incl,
          lt_functions TYPE /vtpr/tftit_t,
          lv_include   TYPE char255,
          lv_lines     TYPE i.

    CHECK iw_obj-object = object_logicaldb.

    CLEAR: lw_tadir.
    lw_tadir = iw_obj.

    FIND prefix_logicaldb IN lw_tadir-obj_name.

    IF sy-subrc <> 0.
      lv_ldb_name = lw_tadir-obj_name.

      CALL FUNCTION 'FUNCTION_INCLUDE_SPLIT'
        EXPORTING
*         program                      = program
*         suppress_select              = suppress_select
          complete_area                = lv_ldb_name
        IMPORTING
          namespace                    = lv_namespace
*         function_not_exists          = function_not_exists
          group                        = lv_ldb_name
*         funcname                     = funcname
*         include_number               = include_number
*         no_function_include          = no_function_include
*         no_function_module           = no_function_module
*         suffix                       = suffix
*         reserved_name                = reserved_name
*         too_many_delimiters          = too_many_delimiters
*         reserved_for_exits           = reserved_for_exits
*         hidden_name                  = hidden_name
*        CHANGING
*         include                      = include
        EXCEPTIONS
          include_not_exists           = 1
          group_not_exists             = 2
          no_selections                = 3
          no_function_include          = 4
          no_function_pool             = 5
          delimiter_wrong_position     = 6
          no_customer_function_group   = 7
          no_customer_function_include = 8
          reserved_name_customer       = 9
          namespace_too_long           = 10
          area_length_error            = 11.

      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
      CONCATENATE lv_namespace prefix_logicaldb lv_ldb_name INTO lw_tadir-obj_name.
    ENDIF.

    lw_tadir-object = object_program.

    select_types_references(
        EXPORTING
            iw_obj = lw_tadir
        IMPORTING
            et_d010tab  = lt_d010tab
            et_d010inc  = lt_d010inc
    ).

    lv_lines = lines( lt_d010tab )  +
               lines( lt_d010inc ).

    add_number_objects_total( lv_lines ).

    LOOP AT lt_d010tab INTO lw_d010tab.
      dependencies_by_name( iv_obj_name = lw_d010tab-tabname ).
    ENDLOOP.

    LOOP AT lt_d010inc INTO lw_d010inc.
      dependencies_by_name( iv_obj_name = lw_d010inc-include ).
    ENDLOOP.

    CALL FUNCTION 'GET_INCLUDETAB'
      EXPORTING
        progname = lw_tadir-obj_name
      TABLES
        incltab  = lt_includes.

    lv_lines = lines( lt_includes ).
    add_number_objects_total( lv_lines ).

    "Todos os includes
    LOOP AT lt_includes INTO lv_include.

      CLEAR: lw_tadir.
      lw_tadir-pgmid      = pgmid_complete.
      lw_tadir-object     = object_program.
      lw_tadir-obj_name   = lv_include.
      get_dependencies( iw_tadir = lw_tadir ).

    ENDLOOP.

  ENDMETHOD.


METHOD object_is_valid.

    DATA: lw_tadir    TYPE tadir,
          lv_valido   TYPE char1,
          lv_obj_name TYPE tadir-obj_name,
          lw_e071     TYPE e071,
          lv_exist    TYPE strl_pari-flag.

    CHECK iw_obj-pgmid  IS NOT INITIAL AND
          iw_obj-object IS NOT INITIAL AND
          iw_obj-obj_name IS NOT INITIAL.

    READ TABLE me->t_object_type TRANSPORTING NO FIELDS
        WITH KEY object = iw_obj-object BINARY SEARCH.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    lv_obj_name = iw_obj-obj_name.
    CASE iw_obj-object.
      WHEN object_functiongroup.
        REPLACE ALL OCCURRENCES OF prefix_functiongroup IN lv_obj_name WITH ''.
      WHEN object_logicaldb.
        REPLACE ALL OCCURRENCES OF prefix_logicaldb IN lv_obj_name WITH ''.
    ENDCASE.

    lv_valido = check_obj_name( lv_obj_name ).

    CHECK lv_valido IS NOT INITIAL.

    READ TABLE t_tadir TRANSPORTING NO FIELDS WITH KEY pgmid    = iw_obj-pgmid
                                                       object   = iw_obj-object
                                                       obj_name = lv_obj_name BINARY SEARCH.

    IF sy-subrc = 0.
      RETURN.
    ENDIF.

    TRANSLATE lv_obj_name TO UPPER CASE.

    SELECT SINGLE *
      FROM tadir
      INTO lw_tadir
      WHERE pgmid       = iw_obj-pgmid
        AND object      = iw_obj-object
        AND obj_name    = lv_obj_name
        AND delflag     = space.

    IF sy-subrc <> 0.
      CLEAR: rv_valido.
      RETURN.
    ENDIF.

    lw_e071-pgmid       = iw_obj-pgmid.
    lw_e071-object      = iw_obj-object.
    lw_e071-obj_name    = lv_obj_name.

    CALL FUNCTION 'TR_CHECK_EXIST'
      EXPORTING
        iv_pgmid             = lw_e071-pgmid    " Key field E071-PGMID
        iv_object            = lw_e071-object    " Key field E071-OBJECT
        iv_obj_name          = lw_e071-obj_name    " Key field E071-OBJ_NAME
*       is_tadir             = SPACE    " Assigned TADIR key if known
      IMPORTING
        e_exist              = lv_exist    " Return value of object existence
      EXCEPTIONS
        tr_no_check_function = 1
        OTHERS               = 2.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF lv_exist IS INITIAL.
      RETURN.
    ENDIF.

    CASE lv_exist.
      WHEN 'X' OR 'N'. "Existe ou está inativo
      WHEN OTHERS.
        RETURN.
    ENDCASE.

    INSERT lw_tadir INTO TABLE t_tadir.

    CHECK sy-subrc = 0.

    rv_valido = abap_true.

    CHECK me->v_add_devc IS NOT INITIAL.

    READ TABLE t_tadir TRANSPORTING NO FIELDS WITH KEY pgmid    = pgmid_complete
                                                       object   = object_package
                                                       obj_name = lw_tadir-devclass BINARY SEARCH.

    CHECK sy-subrc <> 0.

    SELECT SINGLE *
        FROM tadir
        INTO lw_tadir
        WHERE pgmid       = pgmid_complete
          AND object      = object_package
          AND obj_name    = lw_tadir-devclass.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    INSERT lw_tadir INTO TABLE t_tadir.

  ENDMETHOD.


METHOD prog.

    DATA:
      lt_tstc         TYPE STANDARD TABLE OF tstc,
      lw_tstc         TYPE tstc,
      lt_d010inc      TYPE /vtpr/d010inc_t,
      lt_d010inc_rep  TYPE /vtpr/d010inc_t,
      lw_d010inc      TYPE d010inc,
      lt_d010tab      TYPE /vtpr/d010tab_t,
      lw_d010tab      TYPE d010tab,
      lw_tadir        TYPE tadir,
      lt_includes     TYPE STANDARD TABLE OF char255,
      lv_include      TYPE char255,
      lt_functions    TYPE /vtpr/tftit_t,
      lt_cross        TYPE STANDARD TABLE OF cross,
      lw_cross        TYPE cross,
      lv_program_name TYPE swyprog-prognam,
      lw_trdir        TYPE trdir,
      lv_lines        TYPE i.

    CHECK iw_obj-object = object_program.

    lv_program_name = iw_obj-obj_name.

    CALL FUNCTION 'SWY_PROGRAM_EXISTS'
      EXPORTING
        program            = lv_program_name
      IMPORTING
        trdir_entry        = lw_trdir
      EXCEPTIONS
        program_not_exists = 1.

    IF sy-subrc <> 0.
      EXIT.
    ENDIF.

    select_types_references(
    EXPORTING
        iw_obj = iw_obj
    IMPORTING
        et_d010tab  = lt_d010tab
        et_d010inc  = lt_d010inc ).

    get_notations_used_by_report(
      EXPORTING
          iv_progname     = iw_obj-obj_name
      IMPORTING
          et_functions = lt_functions
          et_includes  = lt_d010inc_rep  ).

    CALL FUNCTION 'GET_INCLUDETAB'
      EXPORTING
        progname = iw_obj-obj_name
      TABLES
        incltab  = lt_includes.

    "Seleciona referências de classe de mensagem dos includes
    SELECT *
        FROM cross
        INTO TABLE lt_cross
        WHERE type      IN t_type_msgid[]
          AND name      IN t_prefix[]
          AND include   = iw_obj-obj_name.

    IF sy-subrc <> 0.
      REFRESH: lt_cross.
    ENDIF.

    SELECT *
        FROM tstc
        INTO TABLE lt_tstc
        WHERE pgmna = iw_obj-obj_name.

    IF sy-subrc <> 0.
      REFRESH: lt_tstc.
    ENDIF.

    lv_lines = lines( lt_d010tab )      +
               lines( lt_d010inc )      +
               lines( lt_functions )    +
               lines( lt_d010inc_rep )  +
               lines( lt_includes )     +
               lines( lt_cross )        +
               lines( lt_tstc ).

    add_number_objects_total( lv_lines ).

    LOOP AT lt_tstc INTO lw_tstc.
      CLEAR: lw_tadir.
      lw_tadir-pgmid      = pgmid_complete.
      lw_tadir-object     = object_transaction.
      lw_tadir-obj_name   = lw_tstc-tcode.
      get_dependencies( iw_tadir = lw_tadir ).
    ENDLOOP.



    "Verifica a Existência de banco de dados lógico
    IF lw_trdir-ldbname IS NOT INITIAL.
      lw_tadir-pgmid      = pgmid_complete.
      lw_tadir-object     = object_logicaldb.
      lw_tadir-obj_name   = lw_trdir-ldbname.
      get_dependencies( iw_tadir = lw_tadir ).
    ENDIF.


*    select_types_references(
*        EXPORTING
*            iw_obj = iw_obj
*        IMPORTING
*            et_d010tab  = lt_d010tab
*            et_d010inc  = lt_d010inc ).

    "Seleciona referências de dicionário de dados
    LOOP AT lt_d010tab INTO lw_d010tab.
      dependencies_by_name( iv_obj_name = lw_d010tab-tabname ).
    ENDLOOP.

    "Seleciona referências de classes
    LOOP AT lt_d010inc INTO lw_d010inc.
      dependencies_by_name( iv_obj_name = lw_d010inc-include ).
    ENDLOOP.

*    get_notations_used_by_report(
*      EXPORTING
*          iv_progname     = iw_obj-obj_name
*      IMPORTING
*          et_functions = lt_functions
*          et_includes  = lt_d010inc  ).

    dependencies_by_notations( it_functions = lt_functions
                               it_includes  = lt_d010inc_rep ).

*    CALL FUNCTION 'GET_INCLUDETAB'
*      EXPORTING
*        progname = iw_obj-obj_name
*      TABLES
*        incltab  = lt_includes.

    "Todos os includes
    LOOP AT lt_includes INTO lv_include.

      CLEAR: lw_tadir.
      lw_tadir-pgmid      = pgmid_complete.
      lw_tadir-object     = object_program.
      lw_tadir-obj_name   = lv_include.
      get_dependencies( iw_tadir = lw_tadir ).

    ENDLOOP.

*    "Seleciona referências de classe de mensagem dos includes
*    SELECT *
*        FROM cross
*        INTO TABLE lt_cross
*        WHERE type      IN t_type_msgid[]
*          AND name      IN t_prefix[]
*          AND include   = iw_obj-obj_name.
*
*    IF sy-subrc <> 0.
*      REFRESH: lt_cross.
*    ENDIF.

    SORT lt_cross BY name.
    DELETE ADJACENT DUPLICATES FROM lt_cross COMPARING name.
    LOOP AT lt_cross INTO lw_cross.

      CLEAR: lw_tadir.
      lw_tadir-pgmid      = pgmid_complete.
      lw_tadir-object     = object_messageclass.
      SPLIT lw_cross-name AT space INTO lw_tadir-obj_name lv_include.
      get_dependencies( iw_tadir = lw_tadir ).

    ENDLOOP.

  ENDMETHOD.


METHOD select_tadir.

    DATA: lw_es_spot TYPE badi_spot,
          lw_tadir   TYPE tadir.

    FIND 'BADI' IN iv_obj_name.

    IF sy-subrc = 0.

      SELECT SINGLE *
        FROM badi_spot
        INTO lw_es_spot
            WHERE badi_name = iv_obj_name.

      IF sy-subrc = 0.
        SELECT SINGLE *
            FROM tadir
            INTO lw_tadir
            WHERE pgmid = pgmid_complete
              AND object = object_enhacement_spot
              AND obj_name = lw_es_spot-enhspotname.

        IF sy-subrc = 0.
          APPEND lw_tadir TO rt_tadir.
          RETURN.
        ENDIF.

      ENDIF.

    ENDIF.

    IF iv_object IS INITIAL.
      SELECT *
        FROM tadir
        INTO TABLE rt_tadir
        WHERE pgmid = iv_pgmid
          AND obj_name = iv_obj_name.

      IF sy-subrc <> 0.
        REFRESH: rt_tadir.
      ENDIF.
    ELSE.
      SELECT SINGLE *
        FROM tadir
        INTO  lw_tadir
        WHERE pgmid = iv_pgmid
          AND object = iv_object
          AND obj_name = iv_obj_name.

      IF sy-subrc <> 0.
        REFRESH: rt_tadir.
      ELSE.
        APPEND lw_tadir TO rt_tadir.
      ENDIF.
    ENDIF.

  ENDMETHOD.


METHOD select_tadir_by_range_obj_name.

    SELECT *
      FROM tadir
      INTO TABLE rt_tadir
      WHERE pgmid = iv_pgmid
        AND obj_name IN ir_name.

  ENDMETHOD.


METHOD select_types_references.

    FIELD-SYMBOLS: <fs_include> TYPE d010inc.

    DATA: lv_valido     TYPE char1,
          lt_obj_name   TYPE /vtpr/range_t,
          lw_obj_name   TYPE /vtpr/s_range,
          lr_classes    TYPE /vtpr/range_t,
          lr_subclasses TYPE /vtpr/range_t,
          lv_resto      TYPE string,
          lt_subclasses TYPE /vtpr/d010inc_t,
          lw_obj        TYPE d010inc,
          lv_obj_name   TYPE tadir-obj_name.

    lv_obj_name = iw_obj-obj_name.
    CASE iw_obj-object.
      WHEN object_functiongroup.
        REPLACE ALL OCCURRENCES OF prefix_functiongroup IN lv_obj_name WITH ''.
      WHEN object_logicaldb.
        REPLACE ALL OCCURRENCES OF prefix_logicaldb IN lv_obj_name WITH ''.
    ENDCASE.

    lv_valido = check_obj_name( lv_obj_name ).

    CHECK lv_valido IS NOT INITIAL.

    CLEAR: lw_obj_name.
    lw_obj_name-sign = 'I'.

    CASE iw_obj-object.
      WHEN object_class OR object_interface.
        "Referências públicas
        lw_obj_name-option  = 'CP'.
        CONCATENATE iw_obj-obj_name '*CU' INTO lw_obj_name-low.
        APPEND lw_obj_name TO lr_classes.

*        "Referências de subclasses
*        lw_obj_name-option  = 'CP'.
*        lw_obj_name-low = '*=CP'.
*        APPEND lw_obj_name TO lr_subclasses.

        "Referências do nome do objeto
        lw_obj_name-option  = 'CP'.
        CONCATENATE iw_obj-obj_name '*' INTO lw_obj_name-low.

      WHEN OTHERS.
        lw_obj_name-option  = 'EQ'.
        lw_obj_name-low     = iw_obj-obj_name.
    ENDCASE.

    APPEND lw_obj_name TO lt_obj_name.

    "Seleciona referências utilizadas
    SELECT *
        FROM d010tab
        INTO TABLE et_d010tab
        WHERE master    IN lt_obj_name[]
          AND tabname   IN t_prefix[].

    IF sy-subrc <> 0.
      REFRESH: et_d010tab.
    ENDIF.

    "Seleciona as referências de classes utilizadas
    SELECT *
        FROM d010inc
        INTO TABLE et_d010inc
        WHERE master    IN lt_obj_name[].
*          AND include   IN t_ref_class[].

    IF sy-subrc <> 0.
      REFRESH: et_d010inc.
    ENDIF.

    SORT et_d010tab BY tabname.
    DELETE ADJACENT DUPLICATES FROM et_d010tab COMPARING tabname.

    SORT et_d010inc BY include.
    DELETE ADJACENT DUPLICATES FROM et_d010inc COMPARING include.

    LOOP AT et_d010inc ASSIGNING <fs_include>.

      SPLIT <fs_include>-include AT '=' INTO <fs_include>-include
                                           lv_resto.

    ENDLOOP.

    DELETE ADJACENT DUPLICATES FROM et_d010inc COMPARING include.

  ENDMETHOD.


METHOD set_include_interface.
    me->v_interface = iv_include.
  ENDMETHOD.


METHOD set_include_subclass.
    me->v_subclass = iv_include.
  ENDMETHOD.


METHOD set_include_superclass.
    me->v_superclass = iv_include.
  ENDMETHOD.


METHOD shlp.

    DATA: lw_info       TYPE dd30v,
          lt_shlps_aux  TYPE STANDARD TABLE OF dd31v,
          lt_fields     TYPE rsdg_t_dd32p,
          lw_tadir      TYPE tadir,
          lv_name       TYPE ddobjname,
          lv_funcname   TYPE rs38l-name,
          lt_tfdir      TYPE STANDARD TABLE OF tfdir,
          lw_tfdir      TYPE tfdir,
          lt_tftit      TYPE STANDARD TABLE OF tftit,
          lt_funct      TYPE STANDARD TABLE OF funct,
          lt_enlfdir    TYPE STANDARD TABLE OF enlfdir,
          lt_trdir      TYPE STANDARD TABLE OF trdir,
          lt_sfupararef TYPE STANDARD TABLE OF sfupararef,
          lt_uincl      TYPE STANDARD TABLE OF abaptxt255,
          lv_valido     TYPE char1,
          lv_lines      TYPE i.

    FIELD-SYMBOLS: <fs_field> TYPE dd32p,
                   <fs_shlp>  LIKE LINE OF lt_shlps_aux.

    CHECK iw_obj-object = object_search_help.

    lv_name = iw_obj-obj_name.

    CALL FUNCTION 'DD_SHLP_GET'
      EXPORTING
*       get_state     = 'M    '
*       langu         = SY-LANGU
*       prid          =
        shlp_name     = lv_name
*       withtext      = ' '
*       add_typeinfo  = 'X'
*       tracelevel    =
      IMPORTING
        dd30v_wa_a    = lw_info
*       dd30v_wa_n    =
*       got_state     =
      TABLES
        dd31v_tab_a   = lt_shlps_aux
*       dd31v_tab_n   =
        dd32p_tab_a   = lt_fields
*       dd32p_tab_n   =
*       dd33v_tab_a   =
*       dd33v_tab_n   =
      EXCEPTIONS
        illegal_value = 1
        op_failure    = 2
        OTHERS        = 3.
    IF sy-subrc <> 0.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      EXIT.
    ENDIF.

*    CALL FUNCTION 'DDIF_SHLP_GET'
*      EXPORTING
*        name          = lv_name
**       state         = 'A'
**       langu         = ' '
*      IMPORTING
**       gotstate      =
*        dd30v_wa      = lw_info
*      TABLES
**       dd31v_tab     =
*        dd32p_tab     = lt_fields
**       dd33v_tab     =
*      EXCEPTIONS
*        illegal_input = 1.
*
*    IF sy-subrc <> 0.
**     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
**                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*      EXIT.
*    ENDIF.

    lv_lines = lines( lt_fields ) + lines( lt_shlps_aux ) + 2.
    add_number_objects_total( lv_lines ).

    "Tabela de seleção do search help
    CLEAR: lw_tadir.
    lw_tadir-pgmid      = pgmid_complete.
    lw_tadir-object     = object_table.
    lw_tadir-obj_name   = lw_info-selmethod.
    get_dependencies( iw_tadir = lw_tadir ).

    "Visão de seleção do search help
    CLEAR: lw_tadir.
    lw_tadir-pgmid      = pgmid_complete.
    lw_tadir-object     = object_view.
    lw_tadir-obj_name   = lw_info-selmethod.
    get_dependencies( iw_tadir = lw_tadir ).

    "Loop dos campos do search help
    LOOP AT lt_fields ASSIGNING <fs_field>.

      "Elemento de dados utilizado
      CLEAR: lw_tadir.
      lw_tadir-pgmid      = pgmid_complete.
      lw_tadir-object     = object_dataelement.
      lw_tadir-obj_name   = <fs_field>-rollname.
      get_dependencies( iw_tadir = lw_tadir ).

    ENDLOOP.

    LOOP AT lt_shlps_aux ASSIGNING <fs_shlp>.

      "Subsearch helps que compõe o search help
      CLEAR: lw_tadir.
      lw_tadir-pgmid      = pgmid_complete.
      lw_tadir-object     = object_search_help.
      lw_tadir-obj_name   = <fs_shlp>-subshlp.
      get_dependencies( iw_tadir = lw_tadir ).

    ENDLOOP.

    CHECK lw_info-selmexit IS NOT INITIAL.

    lv_valido = check_obj_name( lw_info-selmexit ).

    CHECK lv_valido IS NOT INITIAL.

    lv_funcname = lw_info-selmexit.

    CALL FUNCTION 'FUNC_GET_OBJECT'
      EXPORTING
        funcname           = lv_funcname
*       r3state            = r3state
      TABLES
        ptfdir             = lt_tfdir
        ptftit             = lt_tftit
        pfunct             = lt_funct
        penlfdir           = lt_enlfdir
        ptrdir             = lt_trdir
        pfupararef         = lt_sfupararef
        uincl              = lt_uincl
*       vsmodisrc          = vsmodisrc
*       vsmodilog          = vsmodilog
      EXCEPTIONS
        function_not_exist = 1
        version_not_found  = 2.

    IF sy-subrc <> 0.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      REFRESH: lt_tfdir.
    ENDIF.

    lv_lines = lines( lt_tfdir ).
    add_number_objects_total( lv_lines ).

    LOOP AT lt_tfdir INTO lw_tfdir.

      "Grupo de função
      CLEAR: lw_tadir.
      lw_tadir-pgmid      = pgmid_complete.
      lw_tadir-object     = object_functiongroup.
      lw_tadir-obj_name   = lw_tfdir-pname.
      get_dependencies( iw_tadir = lw_tadir ).

    ENDLOOP.

  ENDMETHOD.


METHOD tabl.

    FIELD-SYMBOLS: <fs_dd26i> TYPE dd26i.

    DATA: p_dd02v  TYPE dd02v,
          pt_dd03p TYPE dd03ttyp,
          pt_dd05m TYPE dd05mttyp,
          pt_dd08v TYPE dd08vttyp,
          pt_dd12v TYPE STANDARD TABLE OF dd12v,
          pt_dd17v TYPE /vtpr/dd17v_t,
          pt_dd35v TYPE dd35vttyp,
          pt_dd36m TYPE dd36mttyp,
          lv_name  TYPE ddobjname,
          lw_tadir TYPE tadir,
          lt_dd26i TYPE STANDARD TABLE OF dd26i,
          lv_lines TYPE i.

    FIELD-SYMBOLS: <fs_field> TYPE dd03p,
                   <fs_shlp>  TYPE dd35v.

    CHECK iw_obj-object = object_table.

    lv_name = iw_obj-obj_name.

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

    "Get Visões que utilizam a tabela
    SELECT *
      FROM dd26i
      INTO TABLE lt_dd26i
      WHERE tabname = iw_obj-obj_name.

    IF sy-subrc <> 0.
      FREE: lt_dd26i.
    ENDIF.

    lv_lines = lines( pt_dd03p ).
    lv_lines = lv_lines * 4.
    lv_lines = lv_lines + lines( pt_dd35v ).
    lv_lines = lv_lines + lines( lt_dd26i ).
    add_number_objects_total( lv_lines ).

    "Loop dos campos
    LOOP AT pt_dd03p ASSIGNING <fs_field>.

      IF <fs_field>-fieldname = '.INCLUDE'.
        "Estrutura utilizada
        CLEAR: lw_tadir.
        lw_tadir-pgmid      = pgmid_complete.
        lw_tadir-object     = object_table.
        lw_tadir-obj_name   = <fs_field>-precfield.
        get_dependencies( iw_tadir = lw_tadir ).
      ELSEIF <fs_field>-datatype = 'STRU'.
        "Estrutura utilizada
        CLEAR: lw_tadir.
        lw_tadir-pgmid      = pgmid_complete.
        lw_tadir-object     = object_table.
        lw_tadir-obj_name   = <fs_field>-rollname.
        get_dependencies( iw_tadir = lw_tadir ).
      ELSEIF <fs_field>-datatype = 'TTYP'.
        "Estrutura utilizada
        CLEAR: lw_tadir.
        lw_tadir-pgmid      = pgmid_complete.
        lw_tadir-object     = object_typetable.
        lw_tadir-obj_name   = <fs_field>-rollname.
        get_dependencies( iw_tadir = lw_tadir ).
      ELSE.
        "Elemento de dados utilizado
        CLEAR: lw_tadir.
        lw_tadir-pgmid      = pgmid_complete.
        lw_tadir-object     = object_dataelement.
        lw_tadir-obj_name   = <fs_field>-rollname.
        get_dependencies( iw_tadir = lw_tadir ).
      ENDIF.

      "Tabela de verificação
      CLEAR: lw_tadir.
      lw_tadir-pgmid      = pgmid_complete.
      lw_tadir-object     = object_table.
      lw_tadir-obj_name   = <fs_field>-checktable.
      get_dependencies( iw_tadir = lw_tadir ).

      "Tabela de entidade
      CLEAR: lw_tadir.
      lw_tadir-pgmid      = pgmid_complete.
      lw_tadir-object     = object_table.
      lw_tadir-obj_name   = <fs_field>-entitytab.
      get_dependencies( iw_tadir = lw_tadir ).

      "Search help
      CLEAR: lw_tadir.
      lw_tadir-pgmid      = pgmid_complete.
      lw_tadir-object     = object_search_help.
      lw_tadir-obj_name   = <fs_field>-shlpname.
      get_dependencies( iw_tadir = lw_tadir ).

    ENDLOOP.

    "Search help
    LOOP AT pt_dd35v ASSIGNING <fs_shlp>.
      CLEAR: lw_tadir.
      lw_tadir-pgmid      = pgmid_complete.
      lw_tadir-object     = object_search_help.
      lw_tadir-obj_name   = <fs_shlp>-shlpname.
      get_dependencies( iw_tadir = lw_tadir ).
    ENDLOOP.

    SORT lt_dd26i BY viewname.
    DELETE ADJACENT DUPLICATES FROM lt_dd26i COMPARING viewname.

    LOOP AT lt_dd26i ASSIGNING <fs_dd26i>.
      CLEAR: lw_tadir.
      lw_tadir-pgmid      = pgmid_complete.
      lw_tadir-object     = object_view.
      lw_tadir-obj_name   = <fs_dd26i>-viewname.
      get_dependencies( iw_tadir = lw_tadir ).
    ENDLOOP.

    get_tcode_sm30( iw_obj ).

    get_viewmaintenance( iv_object   = iw_obj-object
                         iv_obj_name = iw_obj-obj_name ).

  ENDMETHOD.


METHOD tran.

    DATA: lw_tadir     TYPE tadir,
          lv_tcode     TYPE tstc-tcode,
          lv_tcode_aux TYPE tstcp-tcode,
          lt_tstc      TYPE STANDARD TABLE OF tstc,
          lw_tstc      TYPE tstc,
          lv_lines     TYPE i.

    CHECK iw_obj-object = object_transaction.

    lv_tcode = iw_obj-obj_name.

    CALL FUNCTION 'RS_TRANSACTION_SINGLE_GET'
      EXPORTING
        parameter_tcode = lv_tcode
      IMPORTING
        tcode           = lv_tcode_aux.

    IF lv_tcode_aux IS NOT INITIAL.
      CLEAR: lw_tadir.
      lw_tadir-pgmid      = pgmid_complete.
      lw_tadir-object     = object_transaction.
      lw_tadir-obj_name   = lv_tcode_aux.
      get_dependencies( iw_tadir = lw_tadir ).
    ENDIF.

*    PRGN_GET_ORIGINAL_TRANSACTION

    CALL FUNCTION 'RPY_TRANSACTION_READ'
      EXPORTING
        transaction      = lv_tcode
*       program          = program
*       dynpro           = dynpro
*       transaction_type = transaction_type
      TABLES
        tcodes           = lt_tstc
*       gui_attributes   = gui_attributes
      EXCEPTIONS
        permission_error = 1
        cancelled        = 2
        not_found        = 3
        object_not_found = 4.

    IF sy-subrc <> 0.
      EXIT.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    lv_lines = lines( lt_tstc ).
    add_number_objects_total( lv_lines ).

    LOOP AT lt_tstc INTO lw_tstc.

      CHECK lw_tstc-pgmna IS NOT INITIAL.

      CLEAR: lw_tadir.
      lw_tadir-pgmid      = pgmid_complete.
      lw_tadir-object     = object_program.
      lw_tadir-obj_name   = lw_tstc-pgmna.
      get_dependencies( iw_tadir = lw_tadir ).

    ENDLOOP.

  ENDMETHOD.


METHOD ttyp.

    CHECK iw_obj-object = object_typetable.

    DATA: lw_info  TYPE dd40v,
          lw_tadir TYPE tadir,
          lv_name  TYPE ddobjname.

    lv_name = iw_obj-obj_name.

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

    IF sy-subrc <> 0 OR lw_info-rowtype IS INITIAL.
      RETURN.
    ENDIF.

    CLEAR: lw_tadir.
    lw_tadir-pgmid      = pgmid_complete.
    lw_tadir-obj_name   = lw_info-rowtype.
    lw_tadir-object = get_type_object( lw_info-rowtype ).

    get_dependencies( iw_tadir = lw_tadir ).

  ENDMETHOD.


METHOD update_indicador.

    DATA: lv_msg  TYPE string,
          lv_msg1 TYPE balm-msgv1,
          lv_msg2 TYPE balm-msgv2,
          lv_msg3 TYPE balm-msgv3,
          lv_msg4 TYPE balm-msgv4,
          lt_in   TYPE STANDARD TABLE OF ko105,
          lw_in   TYPE ko105,
          lt_text TYPE STANDARD TABLE OF ko100,
          lw_text TYPE ko100.

    lw_in-pgmid = pgmid_complete.
    lw_in-object = iw_tadir-object.
    APPEND lw_in TO lt_in.

    CALL FUNCTION 'TRINT_OBJECT_TABLE'
      TABLES
        tt_types_in  = lt_in
        tt_types_out = lt_text.

    SORT lt_text BY object.

    READ TABLE lt_text INTO lw_text WITH KEY object = iw_tadir-object BINARY SEARCH.
    IF sy-subrc <> 0.
      CLEAR: lw_text.
    ENDIF.

    lv_msg1 = lw_text-text.
    lv_msg2 = iw_tadir-obj_name.

    CALL FUNCTION 'MESSAGE_PREPARE'
      EXPORTING
*       language               = language    " Language in which the message is read
        msg_id                 = '/VTPR/MAIN'    " Message ID
        msg_no                 = '001'    " Message number
        msg_var1               = lv_msg1    " Message variable 1
        msg_var2               = lv_msg2    " Message variable 2
        msg_var3               = lv_msg3    " Message variable 3
        msg_var4               = lv_msg4    " Message variable 4
      IMPORTING
        msg_text               = lv_msg    " Message text
      EXCEPTIONS
        function_not_completed = 1
        message_not_found      = 2.

    IF sy-subrc <> 0.
      lv_msg = '***'.
    ENDIF.

    cl_progress_indicator=>progress_indicate(
      i_text               = lv_msg
      i_processed          = me->v_atual    " Number of Objects Already Processed
      i_total              = me->v_total    " Total Number of Objects to Be Processed
      i_output_immediately = 'X').

    WAIT UP TO '0.0003' SECONDS.

  ENDMETHOD.


METHOD view.

    FIELD-SYMBOLS: <dd26v> TYPE dd26v.

    DATA: lv_viewname TYPE ddobjname,
          lt_dd26v    TYPE STANDARD TABLE OF dd26v,
          lv_lines    TYPE i.

    get_viewmaintenance( iv_object   = iw_obj-object
                         iv_obj_name = iw_obj-obj_name ).

    get_tcode_sm30( iw_obj ).

    lv_viewname = iw_obj-obj_name.

    CALL FUNCTION 'DDIF_VIEW_GET'
      EXPORTING
        name          = lv_viewname
        state         = 'A'
*       langu         = langu
*      IMPORTING
*       gotstate      = gotstate
*       dd25v_wa      = dd25v_wa
*       dd09l_wa      = dd09l_wa
      TABLES
        dd26v_tab     = lt_dd26v
*       dd27p_tab     = dd27p_tab
*       dd28j_tab     = dd28j_tab
*       dd28v_tab     = dd28v_tab
      EXCEPTIONS
        illegal_input = 1.

    IF sy-subrc <> 0.
      EXIT.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    lv_lines = lines( lt_dd26v ).
    add_number_objects_total( lv_lines ).

    LOOP AT lt_dd26v ASSIGNING <dd26v>.
      dependencies_by_name( iv_obj_name = <dd26v>-tabname ).
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
