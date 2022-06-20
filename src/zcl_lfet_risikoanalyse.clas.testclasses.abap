CLASS ltcl_simple DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    "Die Logik zur Entscheidungstabelle
    DATA et    TYPE REF TO zif_lfet_risikoanalyse.
    "Die Bedingungswerte
    DATA werte TYPE REF TO zif_lfet_values.

    METHODS setup.
    METHODS test_et
      IMPORTING
        b01  TYPE clike
        b02  TYPE clike
        b03  TYPE clike
        a01  TYPE clike
        rule TYPE clike.

    METHODS r01 FOR TESTING.
    METHODS r02 FOR TESTING.
    METHODS r03 FOR TESTING.
    METHODS r04 FOR TESTING.
    METHODS r05 FOR TESTING.
    METHODS r06 FOR TESTING.
    METHODS r07 FOR TESTING.
ENDCLASS.

CLASS ltcl_simple IMPLEMENTATION.


  METHOD r01.
    test_et(
      b01  = zif_lfet_risikoanalyse=>c_rechtl_rahmenbedingungen-keine
      b02  = zif_lfet_risikoanalyse=>c_ressourcenabhaengigkeit-int1
      b03  = zif_lfet_risikoanalyse=>c_projektumfang-mittel
      a01  = zif_lfet_risikoanalyse=>c_risikoanalyse-nicht_notwendig
      rule = '1' ).
  ENDMETHOD.

  METHOD r02.
    test_et(
      b01  = zif_lfet_risikoanalyse=>c_rechtl_rahmenbedingungen-keine
      b02  = zif_lfet_risikoanalyse=>c_ressourcenabhaengigkeit-int2plus
      b03  = zif_lfet_risikoanalyse=>c_projektumfang-klein
      a01  = zif_lfet_risikoanalyse=>c_risikoanalyse-nicht_notwendig
      rule = '2' ).
  ENDMETHOD.

  METHOD r03.
    test_et(
      b01  = zif_lfet_risikoanalyse=>c_rechtl_rahmenbedingungen-keine
      b02  = zif_lfet_risikoanalyse=>c_ressourcenabhaengigkeit-int2plus
      b03  = zif_lfet_risikoanalyse=>c_projektumfang-mittel
      a01  = zif_lfet_risikoanalyse=>c_risikoanalyse-empfohlen
      rule = '3' ).
  ENDMETHOD.

  METHOD r04.
    test_et(
    b01  = zif_lfet_risikoanalyse=>c_rechtl_rahmenbedingungen-keine
    b02  = zif_lfet_risikoanalyse=>c_ressourcenabhaengigkeit-int2plus
    b03  = zif_lfet_risikoanalyse=>c_projektumfang-gross
    a01  = zif_lfet_risikoanalyse=>c_risikoanalyse-empfohlen
    rule = '4' ).
  ENDMETHOD.

  METHOD r05.
    test_et(
      b01  = zif_lfet_risikoanalyse=>c_rechtl_rahmenbedingungen-keine
      b02  = zif_lfet_risikoanalyse=>c_ressourcenabhaengigkeit-ext1plus
      b03  = zif_lfet_risikoanalyse=>c_projektumfang-mittel
      a01  = zif_lfet_risikoanalyse=>c_risikoanalyse-empfohlen
      rule = '5' ).
  ENDMETHOD.

  METHOD r06.
    test_et(
      b01  = zif_lfet_risikoanalyse=>c_rechtl_rahmenbedingungen-unklar
      b02  = zif_lfet_risikoanalyse=>c_ressourcenabhaengigkeit-int1
      b03  = zif_lfet_risikoanalyse=>c_projektumfang-mittel
      a01  = zif_lfet_risikoanalyse=>c_risikoanalyse-empfohlen
      rule = '6' ).
  ENDMETHOD.

  METHOD r07.
    test_et(
      b01  = zif_lfet_risikoanalyse=>c_rechtl_rahmenbedingungen-vorhanden
      b02  = zif_lfet_risikoanalyse=>c_ressourcenabhaengigkeit-int1
      b03  = zif_lfet_risikoanalyse=>c_projektumfang-klein
      a01  = zif_lfet_risikoanalyse=>c_risikoanalyse-notwendig
      rule = '7' ).
  ENDMETHOD.

  METHOD setup.

    "Objekt zur Speicherung der Bedingungswerte und Aktionsergebnisse erzeugen
    werte = NEW zcl_lfet_values( ).
    "Entscheidungstabellenobjekt erzeugen
    et = NEW zcl_lfet_impl( ).
    "Setzen des Werteobjektes
    et->set_value_object( werte ).
  ENDMETHOD.

  METHOD test_et.

    "Setzen der Bedingungswerte
    werte->set_bedingungswert(
      bedingung = zif_lfet_risikoanalyse=>c_bedingungen-b01_rechtl_rahmenbedingung
      wert      = b01 ).
    werte->set_bedingungswert(
      bedingung = zif_lfet_risikoanalyse=>c_bedingungen-b02_ressourcen_abhaengigkeit
      wert      = b02 ). "egal
    werte->set_bedingungswert(
      bedingung = zif_lfet_risikoanalyse=>c_bedingungen-b03_projektumfang
      wert      = b03 ).

    "Risikoermittlung durchführen
    zcl_lfet_risikoanalyse=>execute( et ).

    "Ernmittlung des Ergebnisses für die Aktion A01
    cl_abap_unit_assert=>assert_equals(
        act = werte->get_aktionsergebnis( zif_lfet_risikoanalyse=>c_aktionen-a01_risikoanalyse )
        exp = a01 ).
    "Ermittlung der verwendeten Regel
    cl_abap_unit_assert=>assert_equals(
        act = werte->get_trace( )-used_rule
        exp = rule  ).

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_late DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    "Die Logik zur Entscheidungstabelle
    DATA et    TYPE REF TO zif_lfet_risikoanalyse.
    "Die Bedingungswerte
    DATA werte TYPE REF TO zif_lfet_values.

    METHODS setup.
    METHODS r03 FOR TESTING.
ENDCLASS.

CLASS ltcl_late IMPLEMENTATION.

  METHOD r03.

    "Setzen der Bedingungswerte
    werte->set_bedingungswert(
      bedingung = zif_lfet_risikoanalyse=>c_bedingungen-b01_rechtl_rahmenbedingung
      wert      = zif_lfet_risikoanalyse=>c_rechtl_rahmenbedingungen-keine ).
    werte->set_bedingungswert(
      bedingung = zif_lfet_risikoanalyse=>c_bedingungen-b02_ressourcen_abhaengigkeit
      wert      = zif_lfet_risikoanalyse=>c_ressourcenabhaengigkeit-int2plus ).

    "Risikoermittlung durchführen
    zcl_lfet_risikoanalyse=>execute( et ).

    "Ernmittlung des Ergebnisses für die Aktion A01
    cl_abap_unit_assert=>assert_equals(
        act = werte->get_aktionsergebnis( zif_lfet_risikoanalyse=>c_aktionen-a01_risikoanalyse )
        exp = zif_lfet_risikoanalyse=>c_risikoanalyse-empfohlen ).
    "Ermittlung der verwendeten Regel
    cl_abap_unit_assert=>assert_equals(
        act = werte->get_trace( )-used_rule
        exp = '3'  ).

  ENDMETHOD.

  METHOD setup.

    "Objekt zur Speicherung der Bedingungswerte und Aktionsergebnisse erzeugen
    werte ?= NEW zcl_lfet_values_late( ).

    "Entscheidungstabellenobjekt erzeugen
    et = NEW zcl_lfet_impl( ).
    "Setzen des Werteobjektes
    et->set_value_object( werte ).
  ENDMETHOD.

ENDCLASS.
