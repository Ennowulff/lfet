INTERFACE zif_lfet_risikoanalyse
  PUBLIC .


  CONSTANTS:
    BEGIN OF c_bedingungen,
      b01_rechtl_rahmenbedingung   TYPE zif_lfet_values=>_bedingung VALUE 'B01',
      b02_ressourcen_abhaengigkeit TYPE zif_lfet_values=>_bedingung VALUE 'B02',
      b03_projektumfang            TYPE zif_lfet_values=>_bedingung VALUE 'B03',
    END  OF c_bedingungen .
  CONSTANTS:
    BEGIN OF c_aktionen,
      a01_risikoanalyse TYPE zif_lfet_values=>_aktion VALUE 'A01',
    END OF c_aktionen .
  CONSTANTS:
    BEGIN OF c_risikoanalyse,
      "! Eine Risikoanalyse wird empfohlen, ist jedoch nicht zwingend notwendig
      empfohlen       TYPE string VALUE 'empf',
      "! Eine Risikoanalyse ist nicht notwendig
      nicht_notwendig TYPE string VALUE 'keine',
      "! Eine Risikoanalyse ist notwendig
      notwendig       TYPE string VALUE 'notw',
    END OF c_risikoanalyse .
  CONSTANTS:
    BEGIN OF c_rechtl_rahmenbedingungen,
      "! Es sind keine rechtlichen Rahmenbedingungen zu berücksichtigen
      keine     TYPE string VALUE 'nv' ##NO_TEXT,
      "! Es ist unklar/ nicht geklärt, ob rechtliche Rahmenbedingungen vorhanden sind
      unklar    TYPE string VALUE 'unkl' ##NO_TEXT,
      "! Es existieren rechtliche Rahmenbedinungen, die zu berücksichtigen sind
      vorhanden TYPE string VALUE 'vorh' ##NO_TEXT,
    END OF c_rechtl_rahmenbedingungen .
  CONSTANTS:
    BEGIN OF c_projektumfang,
      klein  TYPE string VALUE '<=15',
      mittel TYPE string VALUE ']15 : 60]',
      gross  TYPE string VALUE '>60',
    END OF c_projektumfang .
  CONSTANTS:
    BEGIN OF c_ressourcenabhaengigkeit,
      "! Genau eine interne Ressource; keine externe Ressource
      int1     TYPE string VALUE 'int1',
      "! Mindestens zwei interne Ressourcen; keine externe Ressource
      int2plus TYPE string VALUE 'int2+',
      "! Mindestens eine externe Ressource
      ext1plus TYPE string VALUE 'ext1+',
    END OF c_ressourcenabhaengigkeit .

  METHODS is_rechtl_rahmenbedingungen
    IMPORTING
      !input        TYPE clike
    RETURNING
      VALUE(result) TYPE bapigsbool .
  METHODS is_ressourcen_abhaengigkeit
    IMPORTING
      !input        TYPE clike
    RETURNING
      VALUE(result) TYPE bapigsbool .
  METHODS is_projektumfang
    IMPORTING
      !input        TYPE string
    RETURNING
      VALUE(result) TYPE bapigsbool .
  METHODS do_risikoanalyse
    IMPORTING
      !input TYPE string .
  METHODS set_value_object
    IMPORTING
      !object TYPE REF TO zif_lfet_values .
  METHODS get_value_object
    RETURNING
      VALUE(object) TYPE REF TO zif_lfet_values .
ENDINTERFACE.
