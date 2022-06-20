REPORT zzlfet_demo02.


"!<p class="shorttext" lang="DE">Interface zur Verwaltung der Bedingungswerte</p>
"! Dies ist ein zentrales Interface und wird von allen Entscheidungstabellen verwendet.
INTERFACE zif_lfet_values.

  TYPES _bedingung TYPE string.
  TYPES _wert TYPE string.
  TYPES _aktion TYPE string.

  TYPES: BEGIN OF _trace,
           name            TYPE string,
           used_rule       TYPE string,
           number_of_rules TYPE string,
           version         TYPE string,
         END OF _trace.

  TYPES: BEGIN OF _bedingungswert,
           bedingung TYPE _bedingung,
           wert      TYPE _wert,
         END OF _bedingungswert,
         _bedingungswerte TYPE SORTED TABLE OF _bedingungswert WITH UNIQUE KEY bedingung.

  TYPES: BEGIN OF _aktionsergebnis,
           aktion TYPE _aktion,
           wert   TYPE _wert,
         END OF _aktionsergebnis,
         _aktionsergebnisse TYPE SORTED TABLE OF _aktionsergebnis WITH UNIQUE KEY aktion.

  CONSTANTS: c_wert_ungueltig TYPE _wert VALUE '###'.

  "! Initialisierung des Trace mit Name der ET, Anzahl der Regeln und verwendeter Version
  METHODS inittrace
    IMPORTING
      name            TYPE csequence
      number_of_rules TYPE csequence
      version         TYPE csequence.


  "! Dokumentation der verwendeten Regel
  METHODS dotrace
    IMPORTING
      iv_rule TYPE csequence.

  "! Rückgabe der verwendeten Regel und Versionsnummer
  METHODS get_trace
    RETURNING
      VALUE(trace) TYPE _trace.

  "Setzt den Bedingungswert für eine Bedingung
  METHODS set_bedingungswert
    IMPORTING
      bedingung TYPE _bedingung
      wert      TYPE any.

  "! Liefert den Bedingungswert zu einer Bedingung zurück
  "! @parameter bedingung | Bedingung (Benutzung der Konstante c_bedingung)
  "! @parameter wert | aktuell zugeordneter Wert zur Bedingung
  METHODS get_bedingungswert
    IMPORTING
      bedingung   TYPE _bedingung
    RETURNING
      VALUE(wert) TYPE _wert.

  "! Setzt das Ergebnis zu einer Aktion aus der Ermittlung der Entscheidungstabelle
  "! @parameter aktion | Aktion (Benutzung der Konstante c_aktion)
  "! @parameter wert | aktuell zuzuordnender Wert zur Aktion
  METHODS set_aktionsergebnis
    IMPORTING
      aktion TYPE _aktion
      wert   TYPE any.

  "! Liefert das Ergebnis zu einer Aktion zurück
  "! @parameter aktion | Aktion (Benutzung der Konstante c_aktion)
  "! @parameter wert | aktuell zugeordneter Wert zur Aktion
  METHODS get_aktionsergebnis
    IMPORTING
      aktion      TYPE _aktion
    RETURNING
      VALUE(wert) TYPE _wert.

ENDINTERFACE.

"!<p class="shorttext" lang="DE">Generiertes Interface für ET "Risikoermittlung"</p>
INTERFACE zif_risikoanalyse.


  CONSTANTS: BEGIN OF c_bedingungen,
               b01_rechtl_rahmenbedingung   TYPE zif_lfet_values=>_bedingung VALUE 'B01',
               b02_ressourcen_abhaengigkeit TYPE zif_lfet_values=>_bedingung VALUE 'B02',
               b03_projektumfang            TYPE zif_lfet_values=>_bedingung VALUE 'B03',
             END  OF c_bedingungen.

  CONSTANTS: BEGIN OF c_aktionen,
               a01_risikoanalyse TYPE zif_lfet_values=>_aktion VALUE 'A01',
             END OF c_aktionen.


  CONSTANTS: BEGIN OF c_risikoanalyse,
               "! Eine Risikoanalyse wird empfohlen, ist jedoch nicht zwingend notwendig
               empfohlen       TYPE string VALUE 'empf',
               "! Eine Risikoanalyse ist nicht notwendig
               nicht_notwendig TYPE string VALUE 'keine',
               "! Eine Risikoanalyse ist notwendig
               notwendig       TYPE string VALUE 'notw',
             END OF c_risikoanalyse.

  CONSTANTS: BEGIN OF c_rechtl_rahmenbedingungen,
               "! Es sind keine rechtlichen Rahmenbedingungen zu berücksichtigen
               keine     TYPE string VALUE 'nv' ##NO_TEXT,
               "! Es ist unklar/ nicht geklärt, ob rechtliche Rahmenbedingungen vorhanden sind
               unklar    TYPE string VALUE 'unkl' ##NO_TEXT,
               "! Es existieren rechtliche Rahmenbedinungen, die zu berücksichtigen sind
               vorhanden TYPE string VALUE 'vorh' ##NO_TEXT,
             END OF c_rechtl_rahmenbedingungen.

  CONSTANTS: BEGIN OF c_projektumfang,
               klein  TYPE string VALUE '<=15',
               mittel TYPE string VALUE ']15 : 60]',
               gross  TYPE string VALUE '>60',
             END OF c_projektumfang.

  CONSTANTS: BEGIN OF c_ressourcenabhaengigkeit,
               "! Genau eine interne Ressource; keine externe Ressource
               int1     TYPE string VALUE 'int1',
               "! Mindestens zwei interne Ressourcen; keine externe Ressource
               int2plus TYPE string VALUE 'int2+',
               "! Mindestens eine externe Ressource
               ext1plus TYPE string VALUE 'ext1+',
             END OF c_ressourcenabhaengigkeit.


  "!<p class="shorttext" lang="DE">B01: Rechtliche Rahmenbedingungen?</p>
  "! @parameter input | Bedingung
  "! <ul>
  "! <li>B01/01: nv   - nicht vorhanden</li>
  "! <li>B01/02: unkl - unklar</li>
  "! <li>B01/03: vorh - vorhanden</li></ul>
  "! @parameter result | Rückgabewert
  METHODS is_rechtl_rahmenbedingungen
    IMPORTING
      input         TYPE clike
    RETURNING
      VALUE(result) TYPE bapigsbool.


  "!<p class="shorttext" lang="DE">B02: Ressourcenabhängigkeit?</p>
  "! @parameter input | Bedingung
  "! <ul>
  "! <li>B01/01: int1 - Genau eine interne Ressource; keine externe Ressource</li>
  "! <li>B01/02: int2+ - Mindestens zwei interne Ressourcen; keine externe Ressource</li>
  "! <li>B01/03: ext1+ - Mindestens eine externe Ressource</li></ul>
  "! @parameter result | Rückgabewert
  METHODS is_ressourcen_abhaengigkeit
    IMPORTING
      input         TYPE clike
    RETURNING
      VALUE(result) TYPE bapigsbool.


  "!<p class="shorttext" lang="DE">B03: Projektumfang (PT)?</p>
  "! @parameter input | Bedingung
  "! <ul>
  "! <li>B01/01: B03/01: &lt;=15 - Bis 15 PT</li>
  "! <li>B01/02: B03/02: ] 15 : 60 ] - Bis 60 PT</li>
  "! <li>B01/03: B03/03: &gt; 60 - Ab 61 PT</li></ul>
  "! @parameter result | Rückgabewert
  METHODS is_projektumfang
    IMPORTING
      input         TYPE string
    RETURNING
      VALUE(result) TYPE bapigsbool.

  "!<p class="shorttext" lang="DE">A01: Risikoanalyse</p>
  "! @parameter input | Bedingung
  "! <ul>
  "! <li>A01/01: keine - keine Risikoanalyse</li>
  "! <li>A01/02: empf - Risikoanalyse empfohlen</li>
  "! <li>A01/03: notw - Risikoanalyse notwendig</li></ul>
  "! @parameter result | Rückgabewert
  METHODS do_risikoanalyse
    IMPORTING
      input TYPE string.

  "! setzt das Werteobjekt, das die aktuellen Bedingungen enthält
  METHODS set_value_object
    IMPORTING
      object TYPE REF TO zif_lfet_values.
  "! Liefert das Werteobjekt mit den aktuellen Bedingungen zurück
  METHODS get_value_object
    RETURNING
      VALUE(object) TYPE REF TO zif_lfet_values.

ENDINTERFACE.

CLASS lcl_lfet_values DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_lfet_values.

  PROTECTED SECTION.
    DATA bedingungswerte TYPE zif_lfet_values=>_bedingungswerte.
    DATA aktionsergebnisse TYPE zif_lfet_values=>_aktionsergebnisse.
    DATA trace_info TYPE zif_lfet_values=>_trace.

ENDCLASS.

"!<p class="shorttext" lang="DE">Konkrete Klasse zur Verwaltung der Bedingungswerte</p>
"! Dies ist ein zentrales Objekt und wird von allen Entscheidungstabellen verwendet.
"! Wenn eine abweichende, bzw. spätere Werteermittlung erfolgen soll, dann kann eine
"! Ableitung dieser Klasse verwendet werden, bei der die Methoden zur Ermittlung der
"! Werte erst erfolgt, wenn der Aufruf aus der Entscheidungstabelle erfolgt (Performance).
CLASS lcl_lfet_values IMPLEMENTATION.

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

  METHOD zif_lfet_values~get_trace.
    trace = trace_info.
  ENDMETHOD.

  METHOD zif_lfet_values~inittrace.
    trace_info-name            = name.
    trace_info-number_of_rules = number_of_rules.
    trace_info-version         = version.
  ENDMETHOD.

  METHOD zif_lfet_values~dotrace.
    trace_info = VALUE #(
      name            = trace_info-name
      used_rule       = iv_rule
      number_of_rules = trace_info-number_of_rules
      version         = trace_info-version ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_lfet_values_late DEFINITION INHERITING FROM lcl_lfet_values.
  PUBLIC SECTION.
    METHODS zif_lfet_values~get_bedingungswert REDEFINITION.
  PRIVATE SECTION.
    "!<p class="shorttext" lang="DE">komplizierte und langwierige Berechnung des Projektumfangs</p>
    METHODS get_projektumfang
      RETURNING
        VALUE(wert) TYPE zif_lfet_values=>_wert.
ENDCLASS.

CLASS lcl_lfet_values_late IMPLEMENTATION.

  METHOD zif_lfet_values~get_bedingungswert.
    CASE bedingung.
      WHEN zif_risikoanalyse=>c_bedingungen-b03_projektumfang.
        wert = get_projektumfang( ).
      WHEN OTHERS.
        wert = super->zif_lfet_values~get_bedingungswert( bedingung ).
    ENDCASE.
  ENDMETHOD.

  METHOD get_projektumfang.

    "Aufwändige Ermittlung des Projektumnfangs, der nur durchgeführt wird,
    "wenn die Entscheidungstabelle auch wirklich danach fragt.

    wert = zif_risikoanalyse=>c_projektumfang-mittel.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_impl DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_risikoanalyse.


  PRIVATE SECTION.

    DATA b01_rechtl_rahmenbedingung TYPE zif_lfet_values=>_bedingung.
    DATA b02_ressourcen_abhaengigkeit TYPE zif_lfet_values=>_bedingung.
    DATA b03_projektumfang TYPE zif_lfet_values=>_bedingung.

    DATA a01_risikoanalyse TYPE zif_lfet_values=>_aktion.

    DATA values TYPE REF TO zif_lfet_values.

ENDCLASS.

CLASS lcl_impl IMPLEMENTATION.

  METHOD zif_risikoanalyse~do_risikoanalyse.
    values->set_aktionsergebnis(
      aktion = zif_risikoanalyse~c_aktionen-a01_risikoanalyse
      wert   = input ).
  ENDMETHOD.


  METHOD zif_risikoanalyse~is_projektumfang.
    IF values->get_bedingungswert( zif_risikoanalyse~c_bedingungen-b03_projektumfang ) = input.
      result = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD zif_risikoanalyse~is_rechtl_rahmenbedingungen.
    IF values->get_bedingungswert( zif_risikoanalyse~c_bedingungen-b01_rechtl_rahmenbedingung ) = input.
      result = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD zif_risikoanalyse~is_ressourcen_abhaengigkeit.
    IF values->get_bedingungswert( zif_risikoanalyse~c_bedingungen-b02_ressourcen_abhaengigkeit ) = input.
      result = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD zif_risikoanalyse~set_value_object.
    values = object.
  ENDMETHOD.

  METHOD zif_risikoanalyse~get_value_object.
    object = values.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_risikoanalyse DEFINITION
  CREATE PROTECTED.

  PUBLIC SECTION.

    CLASS-METHODS execute
      IMPORTING
        !io_if TYPE REF TO zif_risikoanalyse.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_risikoanalyse IMPLEMENTATION.

  METHOD execute.

* Prolog Standard <----

    io_if->get_value_object( )->inittrace(
      name            = 'Risikoanalyse'
      number_of_rules = '25'
      version         = '20220613.181835' ).

    IF ( io_if->is_rechtl_rahmenbedingungen( io_if->c_rechtl_rahmenbedingungen-keine ) ).
      IF ( io_if->is_ressourcen_abhaengigkeit(  io_if->c_ressourcenabhaengigkeit-int1 ) ).
        " Rule R01 ---->
        io_if->get_value_object( )->dotrace( '1' ).
        io_if->do_risikoanalyse( io_if->c_risikoanalyse-nicht_notwendig ).
        " Rule R01 <----
      ELSEIF ( io_if->is_ressourcen_abhaengigkeit( io_if->c_ressourcenabhaengigkeit-int2plus ) ).
        IF ( io_if->is_projektumfang( io_if->c_projektumfang-klein ) ).
          "Rule R02 ---->
          io_if->get_value_object( )->dotrace( '2' ).
          io_if->do_risikoanalyse( io_if->c_risikoanalyse-nicht_notwendig ).
        ELSEIF ( io_if->is_projektumfang( io_if->c_projektumfang-mittel ) ).
          "Rule R03 ---->
          io_if->get_value_object( )->dotrace( '3' ).
          io_if->do_risikoanalyse( io_if->c_risikoanalyse-empfohlen ).
        ELSE.
          "Rule R04 ---->
          io_if->get_value_object( )->dotrace( '4' ).
          io_if->do_risikoanalyse( io_if->c_risikoanalyse-empfohlen ).
        ENDIF.
      ELSE.
        "Rule R05 ---->
        io_if->get_value_object( )->dotrace( '5' ).
        io_if->do_risikoanalyse( io_if->c_risikoanalyse-empfohlen ).

      ENDIF.
    ELSEIF ( io_if->is_rechtl_rahmenbedingungen( io_if->c_rechtl_rahmenbedingungen-unklar ) ).
      "Rule R06 ---->
      io_if->get_value_object( )->dotrace( '6' ).
      io_if->do_risikoanalyse( io_if->c_risikoanalyse-empfohlen ).
    ELSE.
      "Rule R07 ---->
      io_if->get_value_object( )->dotrace( '7' ).
      io_if->do_risikoanalyse( io_if->c_risikoanalyse-notwendig ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.


CLASS ltcl_simple DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    "Die Logik zur Entscheidungstabelle
    DATA et    TYPE REF TO zif_risikoanalyse.
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
      b01  = zif_risikoanalyse=>c_rechtl_rahmenbedingungen-keine
      b02  = zif_risikoanalyse=>c_ressourcenabhaengigkeit-int1
      b03  = zif_risikoanalyse=>c_projektumfang-mittel
      a01  = zif_risikoanalyse=>c_risikoanalyse-nicht_notwendig
      rule = '1' ).
  ENDMETHOD.

  METHOD r02.
    test_et(
      b01  = zif_risikoanalyse=>c_rechtl_rahmenbedingungen-keine
      b02  = zif_risikoanalyse=>c_ressourcenabhaengigkeit-int2plus
      b03  = zif_risikoanalyse=>c_projektumfang-klein
      a01  = zif_risikoanalyse=>c_risikoanalyse-nicht_notwendig
      rule = '2' ).
  ENDMETHOD.

  METHOD r03.
    test_et(
      b01  = zif_risikoanalyse=>c_rechtl_rahmenbedingungen-keine
      b02  = zif_risikoanalyse=>c_ressourcenabhaengigkeit-int2plus
      b03  = zif_risikoanalyse=>c_projektumfang-mittel
      a01  = zif_risikoanalyse=>c_risikoanalyse-empfohlen
      rule = '3' ).
  ENDMETHOD.

  METHOD r04.
    test_et(
    b01  = zif_risikoanalyse=>c_rechtl_rahmenbedingungen-keine
    b02  = zif_risikoanalyse=>c_ressourcenabhaengigkeit-int2plus
    b03  = zif_risikoanalyse=>c_projektumfang-gross
    a01  = zif_risikoanalyse=>c_risikoanalyse-empfohlen
    rule = '4' ).
  ENDMETHOD.

  METHOD r05.
    test_et(
      b01  = zif_risikoanalyse=>c_rechtl_rahmenbedingungen-keine
      b02  = zif_risikoanalyse=>c_ressourcenabhaengigkeit-ext1plus
      b03  = zif_risikoanalyse=>c_projektumfang-mittel
      a01  = zif_risikoanalyse=>c_risikoanalyse-empfohlen
      rule = '5' ).
  ENDMETHOD.

  METHOD r06.
    test_et(
      b01  = zif_risikoanalyse=>c_rechtl_rahmenbedingungen-unklar
      b02  = zif_risikoanalyse=>c_ressourcenabhaengigkeit-int1
      b03  = zif_risikoanalyse=>c_projektumfang-mittel
      a01  = zif_risikoanalyse=>c_risikoanalyse-empfohlen
      rule = '6' ).
  ENDMETHOD.

  METHOD r07.
    test_et(
      b01  = zif_risikoanalyse=>c_rechtl_rahmenbedingungen-vorhanden
      b02  = zif_risikoanalyse=>c_ressourcenabhaengigkeit-int1
      b03  = zif_risikoanalyse=>c_projektumfang-klein
      a01  = zif_risikoanalyse=>c_risikoanalyse-notwendig
      rule = '7' ).
  ENDMETHOD.

  METHOD setup.

    "Objekt zur Speicherung der Bedingungswerte und Aktionsergebnisse erzeugen
    werte = NEW lcl_lfet_values( ).
    "Entscheidungstabellenobjekt erzeugen
    et = NEW lcl_impl( ).
    "Setzen des Werteobjektes
    et->set_value_object( werte ).
  ENDMETHOD.

  METHOD test_et.

    "Setzen der Bedingungswerte
    werte->set_bedingungswert(
      bedingung = zif_risikoanalyse=>c_bedingungen-b01_rechtl_rahmenbedingung
      wert      = b01 ).
    werte->set_bedingungswert(
      bedingung = zif_risikoanalyse=>c_bedingungen-b02_ressourcen_abhaengigkeit
      wert      = b02 ). "egal
    werte->set_bedingungswert(
      bedingung = zif_risikoanalyse=>c_bedingungen-b03_projektumfang
      wert      = b03 ).

    "Risikoermittlung durchführen
    lcl_risikoanalyse=>execute( et ).

    "Ernmittlung des Ergebnisses für die Aktion A01
    cl_abap_unit_assert=>assert_equals(
        act = werte->get_aktionsergebnis( zif_risikoanalyse=>c_aktionen-a01_risikoanalyse )
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
    DATA et    TYPE REF TO zif_risikoanalyse.
    "Die Bedingungswerte
    DATA werte TYPE REF TO zif_lfet_values.

    METHODS setup.
    METHODS r03 FOR TESTING.
ENDCLASS.

CLASS ltcl_late IMPLEMENTATION.

  METHOD r03.

    "Setzen der Bedingungswerte
    werte->set_bedingungswert(
      bedingung = zif_risikoanalyse=>c_bedingungen-b01_rechtl_rahmenbedingung
      wert      = zif_risikoanalyse=>c_rechtl_rahmenbedingungen-keine ).
    werte->set_bedingungswert(
      bedingung = zif_risikoanalyse=>c_bedingungen-b02_ressourcen_abhaengigkeit
      wert      = zif_risikoanalyse=>c_ressourcenabhaengigkeit-int2plus ).

    "Risikoermittlung durchführen
    lcl_risikoanalyse=>execute( et ).

    "Ernmittlung des Ergebnisses für die Aktion A01
    cl_abap_unit_assert=>assert_equals(
        act = werte->get_aktionsergebnis( zif_risikoanalyse=>c_aktionen-a01_risikoanalyse )
        exp = zif_risikoanalyse=>c_risikoanalyse-empfohlen ).
    "Ermittlung der verwendeten Regel
    cl_abap_unit_assert=>assert_equals(
        act = werte->get_trace( )-used_rule
        exp = '3'  ).

  ENDMETHOD.

  METHOD setup.

    "Objekt zur Speicherung der Bedingungswerte und Aktionsergebnisse erzeugen
    werte ?= NEW lcl_lfet_values_late( ).

    "Entscheidungstabellenobjekt erzeugen
    et = NEW lcl_impl( ).
    "Setzen des Werteobjektes
    et->set_value_object( werte ).
  ENDMETHOD.


ENDCLASS.


"GUI zur Eingabe der Bedingungswerte
PARAMETERS p_legal TYPE char10 LOWER CASE DEFAULT 'vorh'.
PARAMETERS p_rssrc TYPE char10 LOWER CASE DEFAULT 'int1'.
PARAMETERS p_psize TYPE char10 LOWER CASE DEFAULT '<=15'.

"GUI zur Anzeige des ermittelten Aktionsergebnisses und der verwendeten Regel
PARAMETERS p_result TYPE char20 LOWER CASE MODIF ID dsp.
PARAMETERS p_rule   TYPE char20 LOWER CASE MODIF ID dsp.

"Die Logik zur Entscheidungstabelle
DATA et    TYPE REF TO zif_risikoanalyse.
"Die Bedingungswerte
DATA werte TYPE REF TO zif_lfet_values.

INITIALIZATION.
  "Objekt zur Speicherung der Bedingungswerte und Aktionsergebnisse erzeugen
  werte = NEW lcl_lfet_values( ).
  "Entscheidungstabellenobjekt erzeugen
  et = NEW lcl_impl( ).
  "Setzen des Werteobjektes
  et->set_value_object( werte ).

AT SELECTION-SCREEN OUTPUT.
  "GUI: Ergebnisfelder auf "nicht eingabebereit" setzen
  LOOP AT SCREEN.
    IF screen-group1 = 'DSP'.
      screen-input = '0'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

  "Aktion bei "ENTER"

AT SELECTION-SCREEN.

  "Seten der Bedingungswerte
  werte->set_bedingungswert(
    bedingung = zif_risikoanalyse=>c_bedingungen-b01_rechtl_rahmenbedingung
    wert = p_legal ).
  werte->set_bedingungswert(
     bedingung = zif_risikoanalyse=>c_bedingungen-b02_ressourcen_abhaengigkeit
     wert = p_rssrc ).
  werte->set_bedingungswert(
    bedingung = zif_risikoanalyse=>c_bedingungen-b03_projektumfang
    wert = p_psize ).

  "Risikoermittlung durchführen
  lcl_risikoanalyse=>execute( et ).

  "Ernmittlung des Ergebnisses für die Aktion A01
  p_result = werte->get_aktionsergebnis( zif_risikoanalyse=>c_aktionen-a01_risikoanalyse ).
  "Ermittlung der verwendeten Regel
  p_rule   = werte->get_trace( )-used_rule.
