CLASS zcl_lfet_values_late DEFINITION
  PUBLIC
  INHERITING FROM zcl_lfet_values
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS zif_lfet_values~get_bedingungswert
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS get_projektumfang
      RETURNING
        VALUE(wert) TYPE zif_lfet_values=>_wert .
ENDCLASS.



CLASS zcl_lfet_values_late IMPLEMENTATION.


  METHOD get_projektumfang.

    "Aufwändige Ermittlung des Projektumnfangs, die nur durchgeführt wird,
    "wenn die Entscheidungstabelle auch wirklich danach fragt.
    wert = zif_lfet_risikoanalyse=>c_projektumfang-mittel.

  ENDMETHOD.


  METHOD zif_lfet_values~get_bedingungswert.

    CASE bedingung.
      WHEN zif_lfet_risikoanalyse=>c_bedingungen-b03_projektumfang.
        wert = get_projektumfang( ).
      WHEN OTHERS.
        wert = super->zif_lfet_values~get_bedingungswert( bedingung ).
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
