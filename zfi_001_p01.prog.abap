*---------------------------------------------------------------------*&
*& Report ZFI_001_P01
*&----------------------------------------------------------------------
*& Created By       : Muhammet Fatih Karagüzel
*& Analyst          : Gökhan Salih Akbıyık
*& Created Date     : 08.11.2020
*& Title            :
*& FS-TS Number     : FI001
*&----------------------------------------------------------------------
*& Description      :
*  Developer : Hasan YILDIRIM - 19.DEC.2005  [ ALFA YAZILIM ]
*&----------------------------------------------------------------------
REPORT zfi_001_p01.

INCLUDE zfi_001_p01_i01.


AT SELECTION-SCREEN OUTPUT.
  PERFORM at_selection_screen_output.


INITIALIZATION.
  PERFORM initialization                         .

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM list_f4_for_variant USING p_vari.

AT SELECTION-SCREEN.
  PERFORM at_selection_screen.

START-OF-SELECTION.
  PERFORM get_datas.

*-------------------------  END-OF-SELECTION --------------------------*
END-OF-SELECTION                                 .
  PERFORM display_alv                            .
*&---------------------------------------------------------------------*
*& Form at_selection_screen_output
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM at_selection_screen_output .
  zbc_000_cl01=>get_selections_from_program( IMPORTING es_range = go_main->gs_range ) .
  go_main->zbc_000_if01~at_selection_screen_output(
*      CHANGING
*        cs_sscrfields =                  " Seçim Ekranındaki Alanlar
  ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form initialization
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM initialization .
  go_main = zfi_001_cl01=>get_instance( ).
  go_main->initialization(
    CHANGING
      cv_variant =  p_vari       " Düzen
  ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form list_f4_for_variant
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_VARI
*&---------------------------------------------------------------------*
FORM list_f4_for_variant  USING p_vari.
  go_main->list_f4_for_variant(
    CHANGING
      cv_variant = p_vari               " Düzen
  ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_datas
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_datas .
  go_main->zbc_000_if01~start_of_selection( ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form display_alv
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM display_alv .
  IF go_main->mt_out IS INITIAL.
    MESSAGE s022(zbc).
    RETURN.
  ENDIF.

  CALL FUNCTION 'ZBC_000_FM01'
    EXPORTING
      io_screen = go_main.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form at_selection_Screen
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM at_selection_screen .

  go_main->zbc_000_if01~at_selection_screen( sy-ucomm ).

ENDFORM.
