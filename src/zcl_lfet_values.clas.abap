CLASS zcl_lfet_values DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_lfet_values.

   PROTECTED SECTION.

    DATA bedingungswerte TYPE zif_lfet_values=>_bedingungswerte.
    DATA aktionsergebnisse TYPE zif_lfet_values=>_aktionsergebnisse.
    DATA trace_info TYPE zif_lfet_values=>_trace .
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_lfet_values IMPLEMENTATION.


  METHOD zif_lfet_values~dotrace.

    trace_info = VALUE #(
      name            = trace_info-name
      used_rule       = iv_rule
      number_of_rules = trace_info-number_of_rules
      version         = trace_info-version ).

  ENDMETHOD.


  METHOD zif_lfet_values~get_aktionsergebnis.

    IF line_exists( aktionsergebnisse[ aktion = aktion ] ).
      wert = aktionsergebnisse[ aktion = aktion ]-wert.
    ELSE.
      wert = zif_lfet_values=>c_wert_ungueltig.
    ENDIF.

  ENDMETHOD.


  METHOD zif_lfet_values~get_bedingungswert.

    IF line_exists( bedingungswerte[ bedingung = bedingung ] ).
      wert = bedingungswerte[ bedingung = bedingung ]-wert.
    ELSE.
      wert = zif_lfet_values=>c_wert_ungueltig.
    ENDIF.

  ENDMETHOD.


  METHOD zif_lfet_values~get_trace.

    trace = trace_info.

  ENDMETHOD.


  METHOD zif_lfet_values~inittrace.

    trace_info-name            = name.
    trace_info-number_of_rules = number_of_rules.
    trace_info-version         = version.

  ENDMETHOD.


  METHOD zif_lfet_values~set_aktionsergebnis.

    IF line_exists( aktionsergebnisse[ aktion = aktion ] ).
      aktionsergebnisse[ aktion = aktion ]-wert = wert.
    ELSE.
      INSERT VALUE #(  aktion = aktion wert = wert ) INTO TABLE aktionsergebnisse.
    ENDIF.

  ENDMETHOD.


  METHOD zif_lfet_values~set_bedingungswert.

    IF line_exists( bedingungswerte[ bedingung = bedingung ] ).
      bedingungswerte[ bedingung = bedingung ]-wert = wert.
    ELSE.
      INSERT VALUE #(  bedingung = bedingung wert = wert ) INTO TABLE bedingungswerte.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
