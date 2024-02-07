REPORT zzlfet_demo04.


"GUI zur Eingabe der Bedingungswerte
PARAMETERS p_legal TYPE char10 LOWER CASE DEFAULT 'vorh'.
PARAMETERS p_rssrc TYPE char10 LOWER CASE DEFAULT 'int1'.
PARAMETERS p_psize TYPE char10 LOWER CASE DEFAULT '<=15'.

"GUI zur Anzeige des ermittelten Aktionsergebnisses und der verwendeten Regel
PARAMETERS p_result TYPE char20 LOWER CASE MODIF ID dsp.
PARAMETERS p_rule   TYPE char20 LOWER CASE MODIF ID dsp.

"Die Logik zur Entscheidungstabelle
DATA et    TYPE REF TO zif_lfet_risikoanalyse.
"Die Bedingungswerte
DATA werte TYPE REF TO zif_lfet_values.

INITIALIZATION.
  "Objekt zur Speicherung der Bedingungswerte und Aktionsergebnisse erzeugen
  werte = NEW zcl_lfet_values( ).
  "Entscheidungstabellenobjekt erzeugen
  et = NEW zcl_lfet_impl( ).
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

  "Setzen der Bedingungswerte
  werte->set_bedingungswert(
    bedingung = zif_lfet_risikoanalyse=>c_bedingungen-b01_rechtl_rahmenbedingung
    wert = p_legal ).
  werte->set_bedingungswert(
     bedingung = zif_lfet_risikoanalyse=>c_bedingungen-b02_ressourcen_abhaengigkeit
     wert = p_rssrc ).
  werte->set_bedingungswert(
    bedingung = zif_lfet_risikoanalyse=>c_bedingungen-b03_projektumfang
    wert = p_psize ).

  "Risikoermittlung durchführen
  zcl_lfet_risikoanalyse=>execute( et ).

  "Ernmittlung des Ergebnisses für die Aktion A01
  p_result = werte->get_aktionsergebnis( zif_lfet_risikoanalyse=>c_aktionen-a01_risikoanalyse ).
  "Ermittlung der verwendeten Regel
  p_rule   = werte->get_trace( )-used_rule.
