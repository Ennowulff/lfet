CLASS zcl_lfet_risikoanalyse DEFINITION
  PUBLIC
  CREATE PROTECTED .

  PUBLIC SECTION.

    CLASS-METHODS execute
      IMPORTING
        !io_if TYPE REF TO zif_lfet_risikoanalyse .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_lfet_risikoanalyse IMPLEMENTATION.


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
