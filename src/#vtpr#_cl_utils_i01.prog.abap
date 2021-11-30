*&---------------------------------------------------------------------*
*&  Include           /VTPR/_CL_UTILS_I01
*&---------------------------------------------------------------------*
  CONSTANTS: BEGIN OF c_tipo_msg,
    critical            TYPE icon-name VALUE 'ICON_MESSAGE_CRITICAL_SMALL',
    error               TYPE icon-name VALUE 'ICON_MESSAGE_ERROR_SMALL',
    info                TYPE icon-name VALUE 'ICON_MESSAGE_INFORMATION_SMALL',
    question            TYPE icon-name VALUE 'ICON_MESSAGE_QUESTION_SMALL',
    warning             TYPE icon-name VALUE 'ICON_MESSAGE_WARNING_SMALL',
  END OF c_tipo_msg.

  TYPES: BEGIN OF y_msg,
    status                 TYPE icon-id,
    msg                    TYPE /VTPR/s002-msg,
  END OF y_msg.

  DATA: wa_msg                    TYPE /VTPR/S002,
        it_icons                  TYPE TABLE OF icon,
        wa_icon                   TYPE icon,
        it_final                  TYPE TABLE OF y_msg,
        wa_final                  TYPE y_msg,
        gr_table                  TYPE REF TO cl_salv_table.

  SELECT id name
  FROM icon
  INTO CORRESPONDING FIELDS OF TABLE it_icons
  WHERE name = c_tipo_msg-critical
  OR name = c_tipo_msg-error
  OR name = c_tipo_msg-info
  OR name = c_tipo_msg-question
  OR name = c_tipo_msg-warning.

  CHECK sy-subrc = 0.
  SORT it_icons BY name.

  LOOP AT i_msgs INTO wa_msg.

    CLEAR: wa_final.

    wa_final-msg = wa_msg-msg.

    CASE wa_msg-tp_msg.
      WHEN 'E'.
        READ TABLE it_icons INTO wa_icon WITH KEY name = c_tipo_msg-error BINARY SEARCH.
      WHEN 'I'.
        READ TABLE it_icons INTO wa_icon WITH KEY name = c_tipo_msg-info BINARY SEARCH.
      WHEN 'W'.
        READ TABLE it_icons INTO wa_icon WITH KEY name = c_tipo_msg-warning BINARY SEARCH.
      WHEN 'Q'.
        READ TABLE it_icons INTO wa_icon WITH KEY name = c_tipo_msg-question BINARY SEARCH.
      WHEN 'C'.
        READ TABLE it_icons INTO wa_icon WITH KEY name = c_tipo_msg-critical BINARY SEARCH.
    ENDCASE.

    CHECK sy-subrc = 0.

    wa_final-status = wa_icon-id.

    APPEND wa_final TO it_final.

  ENDLOOP.

  CHECK it_final[] IS NOT INITIAL.

  TRY.
      cl_salv_table=>factory(
      IMPORTING
        r_salv_table = gr_table
      CHANGING
        t_table      = it_final[] ).
    CATCH cx_salv_msg.                                  "#EC NO_HANDLER
  ENDTRY.

  gr_table->set_screen_popup(
  start_column = 30
  end_column   = 150
  start_line   = 1
  end_line     = 10 ).

  gr_table->display( ).
