*&---------------------------------------------------------------------*
*& Include          ZFI_001_P01_I01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Hareket Listesi
*&---------------------------------------------------------------------*
***
***
*-tables---------------------------------------------------------------*

TABLES: bsid, knc1, kna1, t001, tbslt, sadr,  knb1 , t074 , lfb1 ,
        bseg , usr02,zfi_001_t01.

*-type-pools-----------------------------------------------------------*
TYPE-POOLS: slis, kkblo.

*-data.
DATA: g_selfield TYPE kkblo_selfield.

DATA go_main TYPE REF TO zfi_001_cl01.

DATA: bdate       LIKE sy-datum,     "tarih
      yumsav(15)  TYPE p DECIMALS 2, "bakiye
      yumsav2(15) TYPE p DECIMALS 2. "sağlam pb bakiye

*-email screeni acılınca silinecek.
DATA: p_mail(1).

*** added by gsumengen req num 7100002417 req by moz 24.06.2015.
*     DATA: ls_line TYPE ZLDG_LINE_NUM .
*-selection-screen-----------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-t01.
  PARAMETER p_rb1 RADIOBUTTON GROUP one DEFAULT 'X' USER-COMMAND a1.
  PARAMETER p_rb2 RADIOBUTTON GROUP one.
  PARAMETER p_rb3 RADIOBUTTON GROUP one.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-t02.
  SELECT-OPTIONS : s_bukrs FOR bsid-bukrs OBLIGATORY MEMORY ID buk DEFAULT ZFI_000_CL02=>mc_bukrs-hayat_kimya. "AŞ
  SELECT-OPTIONS:  s_hkont  FOR bsid-hkont MODIF ID ana,
                   s_kunnr  FOR bseg-kunnr MODIF ID mus,
                   s_lifnr  FOR bseg-lifnr MODIF ID sat,
                   s_budat  FOR bsid-budat NO-EXTENSION OBLIGATORY, "AŞ
*                   s_gsber  FOR bsid-gsber,
                   s_blart  FOR bsid-blart,
                   s_prctr  FOR bsid-prctr,
*                   s_matnr  FOR bseg-matnr,
                   s_umskz  FOR bsid-umskz MATCHCODE OBJECT H_T074U.
  PARAMETERS:
    p_rldnr TYPE acdoca-rldnr AS LISTBOX VISIBLE LENGTH 20 DEFAULT '0L' OBLIGATORY MODIF ID ANA.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-t03.
  PARAMETER p_rb4 RADIOBUTTON GROUP one1 USER-COMMAND a2.
  PARAMETER p_rb5 RADIOBUTTON GROUP one1.
SELECTION-SCREEN END OF BLOCK b4.

SELECTION-SCREEN BEGIN OF BLOCK block2 WITH FRAME TITLE TEXT-t04.
  PARAMETERS: p_vari LIKE disvariant-variant,
              c_cust AS CHECKBOX MODIF ID ust.
SELECTION-SCREEN END OF BLOCK block2.
