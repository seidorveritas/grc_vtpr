*&---------------------------------------------------------------------*
*&  Include           /VTPR/_CL_UTILS_I03
*&---------------------------------------------------------------------*
DATA: lobj_stdesc TYPE REF TO cl_abap_structdescr,
        lt_fields   TYPE cl_abap_structdescr=>included_view,
        lw_fields   TYPE LINE OF cl_abap_structdescr=>included_view,
        lw_desc     TYPE x030l,
        lw_fldcat   TYPE LINE OF lvc_t_fcat,
        lv_stname   TYPE dd02l-tabname,
        wa_field    LIKE LINE OF p_exc_fields.

REFRESH: p_fieldcat.

TRY.
    lobj_stdesc ?= cl_abap_structdescr=>describe_by_data( p_data ).
  CATCH cx_root.
    RAISE no_field_catalog.
ENDTRY.
* If it is DDIC structure, determine field catalog using ALV FM
IF lobj_stdesc->is_ddic_type( ) IS NOT INITIAL.
  lv_stname = lobj_stdesc->get_relative_name( ).
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_buffer_active        = space
      i_structure_name       = lv_stname
      i_bypassing_buffer     = 'X'
    CHANGING
      ct_fieldcat            = p_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    REFRESH: p_fieldcat.
    RAISE no_field_catalog.
  ENDIF.

  LOOP AT p_fieldcat INTO lw_fldcat.
    READ TABLE p_exc_fields INTO wa_field WITH KEY table_line = lw_fldcat-fieldname.
    IF sy-subrc = 0.
      DELETE p_fieldcat.
    ENDIF.
  ENDLOOP.

  RETURN.
ENDIF.
* Get structure fields
lt_fields = lobj_stdesc->get_included_view( ).
* Build field catalog
LOOP AT lt_fields INTO lw_fields.
  CLEAR: lw_fldcat,
  lw_desc.
  lw_fldcat-col_pos   = sy-tabix.
  lw_fldcat-fieldname = lw_fields-name.
  IF lw_fields-type->is_ddic_type( ) IS NOT INITIAL.
    lw_desc            = lw_fields-type->get_ddic_header( ).
    lw_fldcat-rollname = lw_desc-tabname.
    CASE lw_desc-tabtype.
      WHEN 'A' OR 'T' OR 'L' OR	'P' OR 'C' OR 'V' OR 'J'. "Tipos de tabela
        lw_fldcat-no_out = 'X'.
    ENDCASE.
  ELSE.
    lw_fldcat-inttype  = lw_fields-type->type_kind.
    lw_fldcat-intlen   = lw_fields-type->length.
    lw_fldcat-decimals = lw_fields-type->decimals.
  ENDIF.
  APPEND lw_fldcat TO p_fieldcat.
ENDLOOP.
IF p_fieldcat[] IS INITIAL.
  RAISE no_field_catalog.
ENDIF.

LOOP AT p_fieldcat INTO lw_fldcat.
  READ TABLE p_exc_fields INTO wa_field WITH KEY table_line = lw_fldcat-fieldname.
  IF sy-subrc = 0.
    DELETE p_fieldcat.
  ENDIF.
ENDLOOP.
