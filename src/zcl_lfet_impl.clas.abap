CLASS zcl_lfet_impl DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_lfet_risikoanalyse .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA b01_rechtl_rahmenbedingung TYPE zif_lfet_values=>_bedingung .
    DATA b02_ressourcen_abhaengigkeit TYPE zif_lfet_values=>_bedingung .
    DATA b03_projektumfang TYPE zif_lfet_values=>_bedingung .
    DATA a01_risikoanalyse TYPE zif_lfet_values=>_aktion .
    DATA values TYPE REF TO zif_lfet_values .
ENDCLASS.



CLASS zcl_lfet_impl IMPLEMENTATION.

  METHOD zif_lfet_risikoanalyse~do_risikoanalyse.
    values->set_aktionsergebnis(
      aktion = zif_lfet_risikoanalyse~c_aktionen-a01_risikoanalyse
      wert   = input ).
  ENDMETHOD.

  METHOD zif_lfet_risikoanalyse~get_value_object.
    object = values.
  ENDMETHOD.

  METHOD zif_lfet_risikoanalyse~is_projektumfang.
    IF values->get_bedingungswert( zif_lfet_risikoanalyse~c_bedingungen-b03_projektumfang ) = input.
      result = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD zif_lfet_risikoanalyse~is_rechtl_rahmenbedingungen.
    IF values->get_bedingungswert( zif_lfet_risikoanalyse~c_bedingungen-b01_rechtl_rahmenbedingung ) = input.
      result = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD zif_lfet_risikoanalyse~is_ressourcen_abhaengigkeit.
    IF values->get_bedingungswert( zif_lfet_risikoanalyse~c_bedingungen-b02_ressourcen_abhaengigkeit ) = input.
      result = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD zif_lfet_risikoanalyse~set_value_object.
    values = object.
  ENDMETHOD.

ENDCLASS.
