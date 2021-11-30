*&---------------------------------------------------------------------*
*&  Include           /VTPR/_CL_UTILS_I02
*&---------------------------------------------------------------------*
DATA: lo_type                   TYPE REF TO cl_abap_typedescr,
        lo_elem                   TYPE REF TO cl_abap_elemdescr,
        l_dfies                   TYPE dfies,
        l_ddic_type               TYPE abap_bool.

  CHECK p_field IS INITIAL.

  CLEAR: p_msg.

  CALL METHOD cl_abap_elemdescr=>describe_by_data
    EXPORTING
      p_data      = p_field
    RECEIVING
      p_descr_ref = lo_type.

  CALL METHOD lo_type->is_ddic_type
    RECEIVING
      p_abap_bool = l_ddic_type.

  IF l_ddic_type IS INITIAL.
    p_msg-tp_msg = 'E'.
    MOVE text-m01 TO p_msg-msg.
    EXIT.
  ENDIF.

  lo_elem ?= lo_type.

  CALL METHOD lo_elem->get_ddic_field
    EXPORTING
      p_langu      = sy-langu
    RECEIVING
      p_flddescr   = l_dfies
    EXCEPTIONS
      not_found    = 1
      no_ddic_type = 2
      OTHERS       = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  p_msg-tp_msg = 'E'.
  CONCATENATE text-m01 l_dfies-scrtext_m INTO p_msg-msg SEPARATED BY space.
