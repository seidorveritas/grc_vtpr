FUNCTION /vtpr/_read_exit.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_BUKRS) TYPE  BUKRS OPTIONAL
*"     REFERENCE(I_BRANCH) TYPE  J_1BBRANC_ OPTIONAL
*"     REFERENCE(I_REPID) TYPE  SY-REPID
*"     REFERENCE(I_SEQ) TYPE  /VTPR/E_SEQUENCIAL
*"  EXPORTING
*"     REFERENCE(E_PROG) TYPE  /VTPR/E_PROGRAMA
*"     REFERENCE(E_FORM) TYPE  /VTPR/E_FORM
*"----------------------------------------------------------------------

  DATA: lv_id_exit TYPE /vtpr/t_exit-id_exit.

* Montagem do ID da EXIT
  CONCATENATE i_repid '_' i_seq INTO lv_id_exit.
  CONDENSE lv_id_exit NO-GAPS.

* Obter dados tabela de EXIT
  SELECT SINGLE prog form
    FROM /vtpr/t_exit
    INTO (e_prog, e_form)
   WHERE bukrs   = i_bukrs
     AND branch  = i_branch
     AND id_exit = lv_id_exit
     AND ativo   = 'X'.

ENDFUNCTION.
