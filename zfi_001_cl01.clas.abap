class ZFI_001_CL01 definition
  public
  final
  create public .

public section.

  interfaces ZBC_000_IF01 .

  class-data MO_SINGLETON type ref to ZFI_001_CL01 .
  data GS_RANGE type ZFI_001_S01 .
  data MV_REPID like SYST-REPID .
  data GS_USR02 type USR02 .
  data MT_DEVIR type ZFI_001_TT01 .
  data MT_ITAB type ZFI_001_TT02 .
  data MT_OUT type ZFI_001_TT03 .
  data MT_BAKIYE type ZFI_001_TT04 .
  data MT_BAKIYE2 type ZFI_001_TT04 .
  data MT_HKONT type ZFI_001_TT05 .
  data MT_WAERS type ZFI_001_TT06 .
  data MT_WAERS2 type ZFI_001_TT06 .
  data MT_SORT type SLIS_T_SORTINFO_ALV .
  data MT_BUK type ZFI_001_TT07 .
  data MT_LIF type ZFI_001_TT08 .
  data MT_HKON type ZFI_001_TT09 .
  data MS_VARIANT type DISVARIANT .
  data MV_VARIANT_SAVE type CHAR01 .
  data MV_VARIANT_EXIT type CHAR01 .
  data MS_XVARIANT type DISVARIANT .

  methods GET_ACDOCA .
  methods GET_ACDOCA_DEVIR
    importing
      !IV_KEYDT type DATUM .
  methods GET_DATA_UP .
  methods GET_DATA_BP .
  methods GET_NAME .
  methods CONSTRUCTOR .
  methods INITIALIZATION
    changing
      !CV_VARIANT type DISVARIANT-VARIANT .
  class-methods GET_INSTANCE
    returning
      value(RO_RESULT) type ref to ZFI_001_CL01 .
  methods LIST_F4_FOR_VARIANT
    changing
      value(CV_VARIANT) type DISVARIANT-VARIANT optional .
  methods AUTHORITY_CHECK .
  methods ANAHESAP .
  methods ANAHESAP_BP .
  methods DEVIR_ANAHESAP_NEW .
  methods ANA_HASAP_GET_NEW .
  methods ANA_HASAP_GET_BP .
  methods SORT_ITAB .
  methods SORT_ITAB_BP .
  methods FILL_TOUT .
  methods FILL_TOUT_BP .
  methods GET_MATERIAL
    importing
      !BUKRS type BUKRS
      !BELNR type BELNR_D
      !GJAHR type GJAHR
      !BUZEI type BUZEI .
  methods GET_BASLIK_BILGILERI
    importing
      value(BUKRS) type BUKRS
      value(BELNR) type BELNR_D
      value(GJAHR) type GJAHR
    changing
      !STBLG type STBLG
      !XREF1_HD type XREF1_HD
      !BKTXT type BKTXT .
  methods NET_VADE_TARIHI
    importing
      !ZFBDT type DZFBDT
      !ZBD1T type DZBD1T
      !ZBD2T type DZBD2T
      !ZBD3T type DZBD3T
      !SHKZG type SHKZG
      !REBZG type REBZG
      !KOART type KOART
      !BLDAT type BLDAT
    changing
      !FAEDT type FAEDT_FPOS .
  methods LIST_OUT .
  methods SEND_MAIL .
  methods LIST_FILL_FIELDCAT
    importing
      value(P_TABNAME) type SLIS_TABNAME
    changing
      !T_FIELDCAT type LVC_T_FCAT .
  methods LIST_SET_ATTRIBUTE
    importing
      !P_TABNAME type SLIS_TABNAME
      !P_FIELDNAMES type DSTRING
      !P_ATTRIBUTES type DSTRING
      !P_VALUE type DSTRING
    changing
      !T_FIELDCAT type SLIS_T_FIELDCAT_ALV .
  methods LIST_MERGE_FIELDCAT
    importing
      !P_TABNAME type SLIS_TABNAME
    changing
      !T_FIELDCAT type LVC_T_FCAT .
  methods MUSTERI_HESAP .
  methods MUSTERI_HESAP_BP .
  methods DEVIR_MUSTERI_NEW .
  methods DEVIR_MUSTERI_BP .
  methods MUSTERI_HASAP_GET_NEW .
  methods MUSTERI_HASAP_GET_BP .
  methods FILL_TOUT_WITH_ODK
    importing
      !KOART type CHAR1 .
  methods GET_MUTABAKAT_HESAP_NO
    importing
      !HKONT type HKONT
      !BUKRS type BUKRS
      !KTOPL type CHAR3
      !KOART type KOART
      !UMSKZ type UMSKZ
    changing
      !C_HKONT type HKONT .
  methods SORT_WAERS_ODK
    importing
      !HKONT type HKONT
      !BUKRS type BUKRS
      !WAERS type WAERS
      !UMSKZ type UMSKZ .
  methods SATICI_HESAP .
  methods SATICI_HESAP_BP .
  methods DEVIR_SATICI_NEW .
  methods DEVIR_SATICI_BP .
  methods SATICI_HASAP_GET_NEW .
  methods SATICI_HASAP_GET_BP .
  methods CHECK_AUTHORIZATION .
protected section.
private section.
ENDCLASS.



CLASS ZFI_001_CL01 IMPLEMENTATION.


  METHOD list_merge_fieldcat.
    DATA: cdmbtrs(20)   TYPE c,
        cdmbtrh(20)   TYPE c,
        ctotals_d(25) TYPE c,
        ctotalh_d(25) TYPE c.
    data ls_out like line of mt_out.
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
     EXPORTING
       I_BUFFER_ACTIVE              = abap_true
       I_STRUCTURE_NAME             = zfi_000_cl02=>MC_I_STRUCTURE_NAME_ZFI001S04
       I_CLIENT_NEVER_DISPLAY       = abap_true
*       I_BYPASSING_BUFFER           =
       I_INTERNAL_TABNAME           = p_tabname
      CHANGING
        ct_fieldcat                  = T_FIELDCAT
     EXCEPTIONS
       INCONSISTENT_INTERFACE       = 1
       PROGRAM_ERROR                = 2
       OTHERS                       = 3
              .
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.


     read table mt_out into ls_out index 1.
     SELECT SINGLE * FROM t001 into @data(ls_t001) WHERE bukrs = @ls_out-bukrs.

  CONCATENATE TEXT-008 ls_t001-waers
                                  INTO cdmbtrs
                                  SEPARATED BY space.

  CONCATENATE TEXT-009 ls_t001-waers
                                  INTO cdmbtrh
                                  SEPARATED BY space.

  CONCATENATE TEXT-010 ls_t001-waers
                                  INTO ctotals_d
                                  SEPARATED BY space.

  CONCATENATE TEXT-011 ls_t001-waers
                                  INTO ctotalh_d
                                  SEPARATED BY space.

    LOOP AT T_FIELDCAT REFERENCE INTO DATA(lr_fieldcat).
      CASE lr_fieldcat->fieldname.
        WHEN zfi_000_cl02=>MC_FIELDNAME_SHKZG.
           lr_fieldcat->no_out = abap_true.
        WHEN zfi_000_cl02=>MC_FIELDNAME_WRBTR.
           lr_fieldcat->no_out = abap_true.
           lr_fieldcat->cfieldname = zfi_000_cl02=>MC_CFIELDNAME_WAERS_BLG.
        WHEN zfi_000_cl02=>MC_FIELDNAME_UMSKZ.
           lr_fieldcat->no_out = abap_true.
        WHEN zfi_000_cl02=>MC_FIELDNAME_GJAHR.
           lr_fieldcat->no_out = abap_true.
        WHEN zfi_000_cl02=>MC_FIELDNAME_ZUONR.
           lr_fieldcat->no_out = abap_true.
        WHEN zfi_000_cl02=>MC_FIELDNAME_BUDAT.
           lr_fieldcat->no_out = abap_true.
        WHEN zfi_000_cl02=>MC_FIELDNAME_GSBER.
           lr_fieldcat->no_out = abap_true.
        WHEN zfi_000_cl02=>MC_FIELDNAME_BLART.
           lr_fieldcat->no_out = abap_true.
        WHEN zfi_000_cl02=>MC_FIELDNAME_MATNR.
           lr_fieldcat->no_out = abap_true.
        WHEN zfi_000_cl02=>MC_FIELDNAME_WAERS.
           lr_fieldcat->no_out = abap_true.
        WHEN zfi_000_cl02=>MC_FIELDNAME_WAERS_BLG.
          lr_fieldcat->no_out = abap_true.
        WHEN zfi_000_cl02=>MC_FIELDNAME_TOTALS.
          lr_fieldcat->tech = abap_true.
          lr_fieldcat->ref_field = space.
          lr_fieldcat->ref_table = space.
          lr_fieldcat->scrtext_l = space.
          lr_fieldcat->scrtext_m = space.
          lr_fieldcat->scrtext_l = TEXT-001.
          lr_fieldcat->reptext = TEXT-001.
          lr_fieldcat->no_out = abap_true.
        WHEN zfi_000_cl02=>MC_FIELDNAME_TOTALH.
          lr_fieldcat->tech = abap_true.
          lr_fieldcat->ref_field = space.
          lr_fieldcat->ref_table = space.
          lr_fieldcat->scrtext_s = space.
          lr_fieldcat->scrtext_m = space.
          lr_fieldcat->scrtext_l = TEXT-001.
          lr_fieldcat->reptext = TEXT-001.
          lr_fieldcat->no_out = abap_true.
        WHEN zfi_000_cl02=>MC_FIELDNAME_WRBTRS.
          lr_fieldcat->just = abap_true.
          lr_fieldcat->cfieldname = zfi_000_cl02=>MC_CFIELDNAME_WAERS_BLG.
          lr_fieldcat->scrtext_l = TEXT-001.
          lr_fieldcat->reptext = TEXT-001.
          lr_fieldcat->no_out = abap_true.
        WHEN zfi_000_cl02=>MC_FIELDNAME_WRBTRH.
          lr_fieldcat->just = abap_true.
          lr_fieldcat->cfieldname = zfi_000_cl02=>MC_CFIELDNAME_WAERS_BLG.
          lr_fieldcat->scrtext_l = TEXT-001.
          lr_fieldcat->reptext = TEXT-001.
          lr_fieldcat->no_out = abap_true.
        WHEN zfi_000_cl02=>MC_FIELDNAME_DMBTRS.
          lr_fieldcat->ref_field = space.
          lr_fieldcat->ref_table = space.
          lr_fieldcat->scrtext_s = space.
          lr_fieldcat->scrtext_m = space.
          lr_fieldcat->scrtext_l = cdmbtrs.
          lr_fieldcat->reptext = cdmbtrs.
        WHEN zfi_000_cl02=>MC_FIELDNAME_DMBTRSH.
          lr_fieldcat->ref_field = space.
          lr_fieldcat->ref_table = space.
          lr_fieldcat->scrtext_s = space.
          lr_fieldcat->scrtext_m = space.
          lr_fieldcat->scrtext_l = cdmbtrh.
          lr_fieldcat->reptext = cdmbtrh.
        WHEN zfi_000_cl02=>MC_FIELDNAME_TOTALS_D.
          lr_fieldcat->ref_field = space.
          lr_fieldcat->ref_table = space.
          lr_fieldcat->scrtext_s = space.
          lr_fieldcat->scrtext_m = space.
          lr_fieldcat->scrtext_l = ctotals_d.
          lr_fieldcat->reptext = ctotals_d.
        WHEN zfi_000_cl02=>MC_FIELDNAME_TOTALH_D.
          lr_fieldcat->ref_field = space.
          lr_fieldcat->ref_table = space.
          lr_fieldcat->scrtext_s = space.
          lr_fieldcat->scrtext_m = space.
          lr_fieldcat->scrtext_l = ctotalh_d.
          lr_fieldcat->reptext = ctotalh_d.
        WHEN zfi_000_cl02=>MC_FIELDNAME_VZAHL.
          lr_fieldcat->ref_field = space.
          lr_fieldcat->ref_table = space.
          lr_fieldcat->scrtext_s = space.
          lr_fieldcat->scrtext_m = space.
          lr_fieldcat->scrtext_l = TEXT-012.
          lr_fieldcat->reptext = TEXT-012.
        WHEN zfi_000_cl02=>MC_FIELDNAME_SAKNR.
          lr_fieldcat->ref_field = space.
          lr_fieldcat->ref_table = space.
          lr_fieldcat->scrtext_s = space.
          lr_fieldcat->scrtext_m = space.
          lr_fieldcat->scrtext_l = TEXT-013.
          lr_fieldcat->reptext = TEXT-013.
        WHEN zfi_000_cl02=>MC_FIELDNAME_HKONT.
          lr_fieldcat->ref_field = space.
          lr_fieldcat->ref_table = space.
          lr_fieldcat->scrtext_s = space.
          lr_fieldcat->scrtext_m = space.
          lr_fieldcat->scrtext_l = TEXT-014.
          lr_fieldcat->reptext = TEXT-014.
        WHEN zfi_000_cl02=>MC_FIELDNAME_WAERS_BLG.
          lr_fieldcat->ref_field = space.
          lr_fieldcat->ref_table = space.
          lr_fieldcat->scrtext_s = space.
          lr_fieldcat->scrtext_m = space.
          lr_fieldcat->scrtext_l = TEXT-015.
          lr_fieldcat->reptext = TEXT-015.
        WHEN zfi_000_cl02=>MC_FIELDNAME_WAERS.
          lr_fieldcat->ref_field = space.
          lr_fieldcat->ref_table = space.
          lr_fieldcat->scrtext_s = space.
          lr_fieldcat->scrtext_m = space.
          lr_fieldcat->scrtext_l = TEXT-016.
          lr_fieldcat->reptext = TEXT-016.
        WHEN zfi_000_cl02=>MC_FIELDNAME_XREF1.
          lr_fieldcat->ref_field = space.
          lr_fieldcat->ref_table = space.
          lr_fieldcat->scrtext_s = space.
          lr_fieldcat->scrtext_m = space.
          lr_fieldcat->scrtext_l = TEXT-017.
          lr_fieldcat->reptext = TEXT-017.
        WHEN zfi_000_cl02=>MC_FIELDNAME_XREF2.
          lr_fieldcat->ref_field = space.
          lr_fieldcat->ref_table = space.
          lr_fieldcat->scrtext_s = space.
          lr_fieldcat->scrtext_m = space.
          lr_fieldcat->scrtext_l = TEXT-018.
          lr_fieldcat->reptext = TEXT-018.
        WHEN zfi_000_cl02=>MC_FIELDNAME_LINE_NUMBER.
          lr_fieldcat->ref_field = space.
          lr_fieldcat->ref_table = space.
          lr_fieldcat->scrtext_s = space.
          lr_fieldcat->scrtext_m = space.
          lr_fieldcat->scrtext_l = TEXT-025.
          lr_fieldcat->reptext = TEXT-025.
        WHEN zfi_000_cl02=>MC_FIELDNAME_JOURNAL_NUMBER.
          lr_fieldcat->ref_field = space.
          lr_fieldcat->ref_table = space.
          lr_fieldcat->scrtext_s = space.
          lr_fieldcat->scrtext_m = space.
          lr_fieldcat->scrtext_l = TEXT-026.
          lr_fieldcat->reptext = TEXT-026.
        WHEN OTHERS.
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  method LIST_OUT.
*    DATA: p_mail(1).
*    IF gs_range-p_rb2 = 'X' AND p_mail = 'X'.
*      me->send_mail( ).
*
*    ELSE.
*     me->list_fill_fieldcat(
*       EXPORTING
*         p_tabname  = 'MT_OUT'                  " Karakter alanı uzunluğu 10
*       CHANGING
*         t_fieldcat = mt_fieldcat
*     ).
*
** register the list events
*     me->list_register_events( ).
*     me->set_event_exit( ).
*
*      me->list_sortinfo_make( ).
*
*      me->list_display( ).
*
*
*  ENDIF.
*
*

  endmethod.


  method LIST_SET_ATTRIBUTE.
  endmethod.


  METHOD musteri_hasap_get_bp.
    TYPES : BEGIN OF ty_zbd,
              zbd1t TYPE bseg-zbd1t,
              zbd2t TYPE bseg-zbd2t,
              zbd3t TYPE bseg-zbd3t,
              rebzg TYPE bseg-rebzg,
              koart TYPE bseg-koart,
            END OF ty_zbd.

    DATA ls_zbd TYPE ty_zbd.


    DATA: ls_bsid TYPE bsid,
          lt_bsid TYPE TABLE OF bsid,
          lt_bsad TYPE TABLE OF bsad,
          ls_bsad TYPE bsad.
    DATA ls_itab LIKE LINE OF mt_itab.


    SELECT * FROM bsid INTO TABLE lt_bsid
                      WHERE bukrs IN gs_range-s_bukrs
                        AND  kunnr IN gs_range-s_hkont
                        AND  budat IN gs_range-s_budat
                        AND  gsber IN gs_range-s_gsber
                        AND  blart IN gs_range-s_blart
                        AND  prctr IN gs_range-s_prctr
                        AND  umskz IN gs_range-s_umskz.

    SELECT * FROM bsad APPENDING TABLE lt_bsid
                      WHERE bukrs IN gs_range-s_bukrs
                        AND  kunnr IN gs_range-s_hkont
                        AND  budat IN gs_range-s_budat
                        AND  gsber IN gs_range-s_gsber
                        AND  blart IN gs_range-s_blart
                        AND  prctr IN gs_range-s_prctr
                        AND  umskz IN gs_range-s_umskz.

    IF gs_range-s_umskz[] IS INITIAL.
      DELETE lt_bsid WHERE umskz IS NOT INITIAL.
    ENDIF.

    IF lt_bsid[] IS NOT INITIAL..
      SELECT bukrs, belnr, gjahr, buzei, xnegp, zbd1t, zbd2t, zbd3t, rebzg, koart, mwskz, paobjnr
             FROM epic_v_brs_bseg INTO TABLE @DATA(lt_bseg)
             FOR ALL ENTRIES IN @lt_bsid
             WHERE bukrs EQ @lt_bsid-bukrs
               AND belnr EQ @lt_bsid-belnr
               AND gjahr EQ @lt_bsid-gjahr.
    ENDIF.

*-> begin - AATAN / 06.12.2021
    IF NOT lt_bseg[] IS INITIAL.
      SELECT DISTINCT
        ce4c~paobjnr,
        ce4c~kmland,
        T005t~landx AS kmland_dsc,
        ce4c~ww028,
        t25b0~bezek AS ww028_dsc,
        ce4c~ww026,
        t25a8~bezek AS ww026_dsc,
        ce4c~ww027,
        t25a9~bezek AS ww027_dsc,
        ce4c~kndnr,
        kna1~name1 AS kndnr_dsc
          FROM ce4cg00_acct AS ce4c
           LEFT OUTER JOIN t005t ON t005t~spras EQ @sy-langu
                                AND t005t~land1 EQ ce4c~kmland
           LEFT OUTER JOIN t25b0 ON t25b0~spras EQ @sy-langu
                                AND t25b0~ww028 EQ ce4c~ww028
           LEFT OUTER JOIN t25a8 ON t25a8~spras EQ @sy-langu
                                AND t25a8~ww026 EQ ce4c~ww026
           LEFT OUTER JOIN t25a9 ON t25a9~spras EQ @sy-langu
                                AND t25a9~ww027 EQ ce4c~ww027
           LEFT OUTER JOIN kna1 ON kna1~kunnr = ce4c~kndnr
            FOR ALL ENTRIES IN @lt_bseg
              WHERE paobjnr EQ @lt_bseg-paobjnr
                INTO TABLE @DATA(t_ce4cdat).
      SORT t_ce4cdat BY paobjnr.
    ENDIF.
*<- end   - AATAN / 06.12.2021

*    SELECT bukrs, belnr, gjahr, vzahl FROM zfi_001_t03 INTO TABLE @DATA(lt_vzahl).
    SELECT zz1~bukrs, zz1~belnr, zz1~gjahr, zz1~vzahl
      FROM zfi_001_t03 AS zz1
           INNER JOIN @lt_bsid AS itab ON zz1~bukrs = itab~bukrs
                                      AND zz1~belnr = itab~belnr
                                      AND zz1~gjahr = itab~gjahr
      INTO TABLE @DATA(lt_vzahl).
*<- end   - ETUNC / 28.11.2020
    LOOP AT lt_bsid INTO ls_bsid.

      CLEAR ls_itab.
      MOVE-CORRESPONDING ls_bsid TO ls_itab.
      CLEAR : ls_zbd.
      READ TABLE lt_bseg INTO DATA(ls_bseg) WITH KEY bukrs = ls_bsid-bukrs
                                         belnr = ls_bsid-belnr
                                         gjahr = ls_bsid-gjahr
                                         buzei = ls_bsid-buzei.
      MOVE-CORRESPONDING ls_bseg TO ls_zbd.
*******************************************************************
*      SELECT SINGLE zbd1t zbd2t zbd3t rebzg koart xref1
*      INTO CORRESPONDING FIELDS OF ls_zbd
*      FROM bseg WHERE bukrs = ls_bsid-bukrs AND
*                      belnr = ls_bsid-belnr AND
*                      gjahr = ls_bsid-gjahr AND
*                      buzei = ls_bsid-buzei.

*-> begin - AATAN / 06.12.2021
      READ TABLE t_ce4cdat REFERENCE INTO DATA(r_ce4cdat) WITH KEY paobjnr = ls_bseg-paobjnr BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        MOVE-CORRESPONDING r_ce4cdat->* TO ls_itab.
      ENDIF.
*<- end   - AATAN / 06.12.2021

      CALL FUNCTION 'NET_DUE_DATE_GET'
        EXPORTING
          i_zfbdt = ls_bsid-zfbdt
          i_zbd1t = ls_zbd-zbd1t
          i_zbd2t = ls_zbd-zbd2t
          i_zbd3t = ls_zbd-zbd3t
          i_shkzg = ls_bsid-shkzg
          i_rebzg = ls_zbd-rebzg
          i_koart = ls_zbd-koart
        IMPORTING
          e_faedt = ls_itab-faedt.


      ls_itab-saknr = ls_bsid-hkont. "neo
      IF gs_range-c_cust = abap_true.
        SELECT SINGLE konzs FROM kna1
          INTO ls_itab-hkont
          WHERE kunnr = ls_bsid-kunnr.
        ls_itab-ref_hkont = ls_itab-hkont.
*        SELECT SINGLE konzs FROM kna1
*          INTO ls_itab-ref_hkont
*          WHERE kunnr = ls_bsid-kunnr.
      ELSE.
        MOVE: ls_bsid-kunnr TO ls_itab-hkont.
        MOVE: ls_bsid-hkont TO ls_itab-ref_hkont.
      ENDIF.

      IF ls_bsid-shkzg = zfi_000_cl02=>mc_shkzg.
        MOVE: ls_bsid-wrbtr TO ls_itab-wrbtrs.
        MOVE: ls_bsid-dmbtr TO ls_itab-dmbtrs. "HY
      ELSE.
        MOVE: ls_bsid-wrbtr TO ls_itab-wrbtrh.
        MOVE: ls_bsid-dmbtr TO ls_itab-dmbtrh. "HY
      ENDIF.
      IF gs_range-s_umskz[] IS INITIAL.
        ls_itab-umskz = space.
      ENDIF.

*np ekleme
      CLEAR ls_bseg.
      READ TABLE lt_bseg INTO ls_bseg WITH KEY gjahr = ls_bsid-gjahr
                                               bukrs = ls_bsid-bukrs
                                               belnr = ls_bsid-belnr
                                               buzei = ls_bsid-buzei.
*********************************************************************
*      SELECT SINGLE * FROM bseg INTO ls_bseg
*                WHERE gjahr = ls_bsid-gjahr
*                  AND bukrs = ls_bsid-bukrs
*                  AND belnr = ls_bsid-belnr
*                  AND buzei = ls_bsid-buzei.
      ls_itab-xnegp = ls_bseg-xnegp.
****
      me->get_material(
        EXPORTING
          bukrs = ls_bsid-bukrs                  " Şirket kodu
          belnr = ls_bsid-belnr                  " Kalem no.tayini: Malzeme belgesi - satınalma belgesi
          gjahr = ls_bsid-gjahr                " Mali yıl
          buzei = ls_bsid-buzei                 " Muhasebe belgesi içindeki kayıt satırının numarası
      ).

      CHECK sy-subrc EQ 0.
      READ TABLE lt_vzahl INTO DATA(ls_vzahl) WITH KEY bukrs = ls_itab-bukrs
                                                 belnr = ls_itab-belnr
                                                 gjahr = ls_itab-gjahr.
      ls_itab-vzahl = ls_vzahl-vzahl.
*****************************************************************************

*      SELECT SINGLE vzahl
*             FROM zfi_001_t03
*              INTO ls_itab-vzahl
*          WHERE bukrs = ls_itab-bukrs
*              AND belnr = ls_itab-belnr
*              AND gjahr = ls_itab-gjahr.

      APPEND ls_itab TO mt_itab.

    ENDLOOP.

*    SELECT * FROM bsad INTO TABLE lt_bsad
*                       WHERE bukrs IN gs_range-s_bukrs
*                        AND  kunnr IN gs_range-s_hkont
*                        AND  budat IN gs_range-s_budat
*                        AND  gsber IN gs_range-s_gsber
*                        AND  blart IN gs_range-s_blart
*                        AND  prctr IN gs_range-s_prctr
*                        AND  umskz IN gs_range-s_umskz .
*    LOOP AT lt_bsad INTO ls_bsad.
*
*      CLEAR ls_itab.
*
*      MOVE-CORRESPONDING ls_bsad TO ls_itab.
*      CLEAR : ls_zbd,ls_bseg.
*
*      READ TABLE lt_bseg INTO ls_bseg WITH KEY bukrs = ls_bsad-bukrs
*                                               belnr = ls_bsad-belnr
*                                               gjahr = ls_bsad-gjahr
*                                               buzei = ls_bsad-buzei.
*      MOVE-CORRESPONDING ls_bseg TO ls_zbd.
*********************************************************************
**      SELECT SINGLE zbd1t zbd2t zbd3t rebzg koart xref1
**      INTO CORRESPONDING FIELDS OF ls_zbd
**      FROM bseg WHERE bukrs = ls_bsad-bukrs AND
**                      belnr = ls_bsad-belnr AND
**                      gjahr = ls_bsad-gjahr AND
**                      buzei = ls_bsad-buzei.
*
*      CALL FUNCTION 'NET_DUE_DATE_GET'
*        EXPORTING
*          i_zfbdt = ls_bsad-zfbdt
*          i_zbd1t = ls_zbd-zbd1t
*          i_zbd2t = ls_zbd-zbd2t
*          i_zbd3t = ls_zbd-zbd3t
*          i_shkzg = ls_bsad-shkzg
*          i_rebzg = ls_zbd-rebzg
*          i_koart = ls_zbd-koart
*        IMPORTING
*          e_faedt = ls_itab-faedt.
*
*      ls_itab-saknr = ls_bsid-hkont. "neo
*      IF gs_range-c_cust = abap_true.
*        SELECT SINGLE konzs FROM kna1
*          INTO ls_itab-hkont
*          WHERE kunnr = ls_bsad-kunnr.
*        ls_itab-ref_hkont = ls_itab-hkont.
*      ELSE.
*        MOVE: ls_bsad-kunnr TO ls_itab-hkont.
*        MOVE: ls_bsad-hkont TO ls_itab-ref_hkont.
*      ENDIF.
*
*
*      IF ls_bsad-shkzg = c_shkzg.
*        MOVE: ls_bsad-wrbtr TO ls_itab-wrbtrs.
*        MOVE: ls_bsad-dmbtr TO ls_itab-dmbtrs.
*      ELSE.
*        MOVE: ls_bsad-wrbtr TO ls_itab-wrbtrh.
*        MOVE: ls_bsad-dmbtr TO ls_itab-dmbtrh.
*      ENDIF.
*
*      IF gs_range-s_umskz[] IS INITIAL.
*        ls_itab-umskz = space.
*      ENDIF.
*
**np ekleme
*      CLEAR ls_bseg.
*      READ TABLE lt_bseg INTO ls_bseg WITH KEY gjahr = ls_bsad-gjahr
*                                               bukrs = ls_bsad-bukrs
*                                               belnr = ls_bsad-belnr
*                                               buzei = ls_bsad-buzei.
**********************************************************************
**      SELECT SINGLE * FROM bseg INTO ls_bseg
**                WHERE gjahr = ls_bsad-gjahr
**                  AND bukrs = ls_bsad-bukrs
**                  AND belnr = ls_bsad-belnr
**                  AND buzei = ls_bsad-buzei.
*      ls_itab-xnegp = ls_bseg-xnegp.
******
*      me->get_material(
*        EXPORTING
*          bukrs = ls_bsad-bukrs                  " Şirket kodu
*          belnr = ls_bsad-belnr                  " Kalem no.tayini: Malzeme belgesi - satınalma belgesi
*          gjahr = ls_bsad-gjahr                " Mali yıl
*          buzei = ls_bsad-buzei                 " Muhasebe belgesi içindeki kayıt satırının numarası
*      ).
*      CHECK sy-subrc EQ 0.
*      CLEAR ls_vzahl.
*      READ TABLE lt_vzahl INTO ls_vzahl WITH KEY bukrs = ls_itab-bukrs
*                                                 belnr = ls_itab-belnr
*                                                 gjahr = ls_itab-gjahr.
*      ls_itab-vzahl = ls_vzahl-vzahl.
**      SELECT SINGLE vzahl
**             FROM zfi_001_t03
**              INTO ls_itab-vzahl
**          WHERE bukrs = ls_itab-bukrs
**              AND belnr = ls_itab-belnr
**              AND gjahr = ls_itab-gjahr.
*
*      APPEND ls_itab TO mt_itab.
*
*
*    ENDLOOP.


  ENDMETHOD.


  METHOD musteri_hasap_get_new.

    DATA : lt_bsid TYPE TABLE OF bsid,
           ls_bsid TYPE bsid,
           lt_bsad TYPE TABLE OF bsad,
           ls_bsad TYPE bsad.
    DATA  ls_itab LIKE LINE OF mt_itab.

    CLEAR : ls_bsid,ls_bsad.
    REFRESH :lt_bsid[],lt_bsad[].

    SELECT * INTO TABLE lt_bsid
      FROM bsid
     WHERE bukrs IN gs_range-s_bukrs
       AND kunnr IN gs_range-s_hkont
       AND budat IN gs_range-s_budat
       AND gsber IN gs_range-s_gsber
       AND blart IN gs_range-s_blart
       AND prctr IN gs_range-s_prctr
       AND umskz IN gs_range-s_umskz.

    SELECT * APPENDING TABLE lt_bsid
     FROM bsad
    WHERE bukrs IN gs_range-s_bukrs
      AND kunnr IN gs_range-s_hkont
      AND budat IN gs_range-s_budat
      AND gsber IN gs_range-s_gsber
      AND blart IN gs_range-s_blart
      AND prctr IN gs_range-s_prctr
      AND umskz IN gs_range-s_umskz.

    IF gs_range-s_umskz[] IS INITIAL.
      DELETE lt_bsid WHERE umskz IS NOT INITIAL.
    ENDIF.

*********************************************
    IF lt_bsid IS NOT INITIAL.
      SELECT bukrs, belnr, gjahr, buzei, xnegp, zbd1t, zfbdt, shkzg,
             zbd2t, zbd3t, rebzg, koart, xref1, xref2, augbl, augdt, mwskz, paobjnr
             FROM epic_v_brs_bseg INTO TABLE @DATA(lt_bseg)
             FOR ALL ENTRIES IN @lt_bsid
             WHERE bukrs EQ @lt_bsid-bukrs
               AND belnr EQ @lt_bsid-belnr
               AND gjahr EQ @lt_bsid-gjahr .
    ENDIF.
*-> begin - AATAN / 06.12.2021
    IF NOT lt_bseg[] IS INITIAL.
      SELECT DISTINCT
        ce4c~paobjnr,
        ce4c~kmland,
        T005t~landx AS kmland_dsc,
        ce4c~ww028,
        t25b0~bezek AS ww028_dsc,
        ce4c~ww026,
        t25a8~bezek AS ww026_dsc,
        ce4c~ww027,
        t25a9~bezek AS ww027_dsc,
        ce4c~kndnr,
        kna1~name1 AS kndnr_dsc
          FROM ce4cg00_acct AS ce4c
           LEFT OUTER JOIN t005t ON t005t~spras EQ @sy-langu
                                AND t005t~land1 EQ ce4c~kmland
           LEFT OUTER JOIN t25b0 ON t25b0~spras EQ @sy-langu
                                AND t25b0~ww028 EQ ce4c~ww028
           LEFT OUTER JOIN t25a8 ON t25a8~spras EQ @sy-langu
                                AND t25a8~ww026 EQ ce4c~ww026
           LEFT OUTER JOIN t25a9 ON t25a9~spras EQ @sy-langu
                                AND t25a9~ww027 EQ ce4c~ww027
           LEFT OUTER JOIN kna1 ON kna1~kunnr = ce4c~kndnr
            FOR ALL ENTRIES IN @lt_bseg
              WHERE paobjnr EQ @lt_bseg-paobjnr
                INTO TABLE @DATA(t_ce4cdat).
      SORT t_ce4cdat BY paobjnr.
    ENDIF.
*<- end   - AATAN / 06.12.2021
*    SELECT rbukrs AS bukrs, belnr, gjahr, buzei, xnegp, zbd1t, zfbdt, shkzg,
*           zbd2t, zbd3t, rebzg, koart, xref1, xref2, augbl, augdt
*           FROM acdoca INTO TABLE @DATA(lt_bseg)
*           FOR ALL ENTRIES IN @lt_bsid
*           WHERE rldnr  EQ @zfi_000_cl02=>mc_rldnr_ol
*             AND Rbukrs EQ @lt_bsid-bukrs
*             AND belnr  EQ @lt_bsid-belnr
*             AND gjahr  EQ @lt_bsid-gjahr .

*-> begin - ETUNC / 28.11.2020
*    SELECT bukrs, belnr, gjahr, vzahl FROM zfi_001_t03 INTO TABLE @DATA(lt_vzahl).
    SELECT zz1~bukrs, zz1~belnr, zz1~gjahr, zz1~vzahl
      FROM zfi_001_t03 AS zz1
           INNER JOIN @lt_bsid AS itab ON zz1~bukrs = itab~bukrs
                                      AND zz1~belnr = itab~belnr
                                      AND zz1~gjahr = itab~gjahr
      INTO TABLE @DATA(lt_vzahl).
*<- end   - ETUNC / 28.11.2020
*********************************************

    LOOP AT lt_bsid INTO ls_bsid.
      CLEAR ls_itab.
      MOVE-CORRESPONDING ls_bsid TO ls_itab.

      READ TABLE lt_bseg INTO DATA(ls_bseg) WITH KEY gjahr = ls_bsid-gjahr
                                            bukrs = ls_bsid-bukrs
                                            belnr = ls_bsid-belnr
                                            buzei = ls_bsid-buzei.
*********************************************
*      SELECT SINGLE * FROM bseg INTO ls_bseg
*                  WHERE gjahr = ls_bsid-gjahr
*                    AND bukrs = ls_bsid-bukrs
*                    AND belnr = ls_bsid-belnr
*                    AND buzei = ls_bsid-buzei.

      ls_itab-xref1 = ls_bseg-xref1.
      ls_itab-xref2 = ls_bseg-xref2.
      ls_itab-saknr = ls_bsid-hkont.

      ls_itab-xnegp = ls_bseg-xnegp.

      ls_itab-augbl = ls_bseg-augbl.
      ls_itab-augdt = ls_bseg-augdt.

*-> begin - AATAN / 06.12.2021
      ls_itab-mwskz = ls_bseg-mwskz.
      READ TABLE t_ce4cdat REFERENCE INTO DATA(r_ce4cdat) WITH KEY paobjnr = ls_bseg-paobjnr BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        MOVE-CORRESPONDING r_ce4cdat->* to ls_itab.
      ENDIF.
*<- end   - AATAN / 06.12.2021

      me->get_baslik_bilgileri(
        EXPORTING
          bukrs    =  ls_bseg-bukrs                 " Şirket kodu
          belnr    =  ls_bseg-belnr                  " Kalem no.tayini: Malzeme belgesi - satınalma belgesi
          gjahr    =  ls_bseg-gjahr                  " Mali yıl
        CHANGING
          stblg    =  ls_itab-stblg                " Ters kayıt belge numarası
          xref1_hd =  ls_itab-xref1_hd                 " Belge başlığına ilişkin referans anahtar_1 (dahili)
          bktxt    =  ls_itab-bktxt                " Belge başlığı metni
      ).


      me->net_vade_tarihi(
        EXPORTING
          zfbdt = ls_bseg-zfbdt                  " Vade hesaplamasına temel oluşturan tarih
          zbd1t = ls_bseg-zbd1t                  " Nakit indirimi günleri 1
          zbd2t = ls_bseg-zbd2t                  " Nakit indirimi günleri 2
          zbd3t = ls_bseg-zbd3t                  " Net ödeme koşullarına ilişkin süre
          shkzg = ls_bseg-shkzg                  " Borç/alacak göstergesi
          rebzg = ls_bseg-rebzg                  " İşlemin ait olduğu faturanın belge numarası
          koart = ls_bseg-koart                  " Hesap türü
          bldat = ls_bsid-bldat                  " Belge tarihi
        CHANGING
          faedt = ls_itab-faedt                 " Net ödeme vadesi
      ).



      MOVE: ls_bsid-kunnr TO ls_itab-hkont.
      MOVE: ls_bsid-hkont TO ls_itab-ref_hkont.
      IF ls_bsid-shkzg = zfi_000_cl02=>mc_shkzg.
        MOVE: ls_bsid-wrbtr TO ls_itab-wrbtrs.
        MOVE: ls_bsid-dmbtr TO ls_itab-dmbtrs. "HY
      ELSE.
        MOVE: ls_bsid-wrbtr TO ls_itab-wrbtrh.
        MOVE: ls_bsid-dmbtr TO ls_itab-dmbtrh. "HY
      ENDIF.
      IF gs_range-s_umskz[] IS INITIAL.
        ls_itab-umskz = space.
      ENDIF.

      me->get_material(
        EXPORTING
          bukrs =  ls_bsid-bukrs      " Şirket kodu
          belnr =  ls_bsid-belnr      " Kalem no.tayini: Malzeme belgesi - satınalma belgesi
          gjahr =  ls_bsid-gjahr      " Mali yıl
          buzei =  ls_bsid-buzei      " Muhasebe belgesi içindeki kayıt satırının numarası
      ).

      CHECK sy-subrc EQ 0.

      READ TABLE lt_vzahl INTO DATA(ls_vzahl) WITH KEY bukrs = ls_itab-bukrs
                                                       belnr = ls_itab-belnr
                                                       gjahr = ls_itab-gjahr.
      ls_itab-vzahl = ls_vzahl-vzahl.
*********************************************************************************
*      SELECT SINGLE vzahl
*             FROM zfi_001_t03
*              INTO ls_itab-vzahl
*          WHERE bukrs = ls_itab-bukrs
*              AND belnr = ls_itab-belnr
*              AND gjahr = ls_itab-gjahr.

      APPEND ls_itab TO mt_itab.
    ENDLOOP.

*
*    SELECT * INTO TABLE lt_bsad
*      FROM bsad
*     WHERE bukrs IN gs_range-s_bukrs
*       AND kunnr IN gs_range-s_hkont
*       AND budat IN gs_range-s_budat
*       AND gsber IN gs_range-s_gsber
*       AND blart IN gs_range-s_blart
*       AND prctr IN gs_range-s_prctr
*       AND umskz IN gs_range-s_umskz .
*
*    LOOP AT lt_bsad INTO ls_bsad.
*      CLEAR ls_itab.
*      MOVE-CORRESPONDING ls_bsad TO ls_itab.
*      CLEAR ls_bseg.
*      READ TABLE lt_bseg INTO ls_bseg WITH KEY gjahr = ls_bsad-gjahr
*                                               bukrs = ls_bsad-bukrs
*                                               belnr = ls_bsad-belnr
*                                               buzei = ls_bsad-buzei.
**************************************************************************
**      SELECT SINGLE * FROM bseg INTO ls_bseg
**                  WHERE gjahr = ls_bsad-gjahr
**                    AND bukrs = ls_bsad-bukrs
**                    AND belnr = ls_bsad-belnr
**                    AND buzei = ls_bsad-buzei.
*
*      ls_itab-xref1 = ls_bseg-xref1.
*      ls_itab-xref2 = ls_bseg-xref2.
*
*      ls_itab-xnegp = ls_bseg-xnegp.
*
*      ls_itab-augbl = ls_bseg-augbl.
*      ls_itab-augdt = ls_bseg-augdt.
*
*      me->get_baslik_bilgileri(
*       EXPORTING
*         bukrs    =  ls_bseg-bukrs                 " Şirket kodu
*         belnr    =  ls_bseg-belnr                  " Kalem no.tayini: Malzeme belgesi - satınalma belgesi
*         gjahr    =  ls_bseg-gjahr                  " Mali yıl
*       CHANGING
*         stblg    =  ls_itab-stblg                " Ters kayıt belge numarası
*         xref1_hd =  ls_itab-xref1_hd                 " Belge başlığına ilişkin referans anahtar_1 (dahili)
*         bktxt    =  ls_itab-bktxt                " Belge başlığı metni
*     ).
*
*
*      me->net_vade_tarihi(
*       EXPORTING
*         zfbdt = ls_bseg-zfbdt                  " Vade hesaplamasına temel oluşturan tarih
*         zbd1t = ls_bseg-zbd1t                  " Nakit indirimi günleri 1
*         zbd2t = ls_bseg-zbd2t                  " Nakit indirimi günleri 2
*         zbd3t = ls_bseg-zbd3t                  " Net ödeme koşullarına ilişkin süre
*         shkzg = ls_bseg-shkzg                  " Borç/alacak göstergesi
*         rebzg = ls_bseg-rebzg                  " İşlemin ait olduğu faturanın belge numarası
*         koart = ls_bseg-koart                  " Hesap türü
*         bldat = ls_bsid-bldat                  " Belge tarihi
*       CHANGING
*         faedt = ls_itab-faedt                 " Net ödeme vadesi
*     ).
*
*
*
*
*      ls_itab-saknr = ls_bsad-hkont. "neo
*
*      MOVE: ls_bsad-kunnr TO ls_itab-hkont.
*      MOVE: ls_bsad-hkont TO ls_itab-ref_hkont.
*      IF ls_bsad-shkzg = c_shkzg.
*        MOVE: ls_bsad-wrbtr TO ls_itab-wrbtrs.
*        MOVE: ls_bsad-dmbtr TO ls_itab-dmbtrs.
*      ELSE.
*        MOVE: ls_bsad-wrbtr TO ls_itab-wrbtrh.
*        MOVE: ls_bsad-dmbtr TO ls_itab-dmbtrh.
*      ENDIF.
*
*      IF gs_range-s_umskz[] IS INITIAL.
*        ls_itab-umskz = space.
*      ENDIF.
*
*      me->get_material(
*        EXPORTING
*          bukrs =  ls_bsid-bukrs      " Şirket kodu
*          belnr =  ls_bsid-belnr      " Kalem no.tayini: Malzeme belgesi - satınalma belgesi
*          gjahr =  ls_bsid-gjahr      " Mali yıl
*          buzei =  ls_bsid-buzei      " Muhasebe belgesi içindeki kayıt satırının numarası
*      ).
*
*
*      CHECK sy-subrc EQ 0.
*
*      CLEAR ls_vzahl.
*      READ TABLE lt_vzahl INTO ls_vzahl WITH KEY bukrs = ls_itab-bukrs
*                                                       belnr = ls_itab-belnr
*                                                       gjahr = ls_itab-gjahr.
*      ls_itab-vzahl = ls_vzahl-vzahl.
****************************************************************************
**      SELECT SINGLE vzahl
**             FROM zfi_001_t03
**              INTO ls_itab-vzahl
**          WHERE bukrs = ls_itab-bukrs
**              AND belnr = ls_itab-belnr
**              AND gjahr = ls_itab-gjahr.
*
*      APPEND ls_itab TO mt_itab.
*    ENDLOOP.

  ENDMETHOD.


  METHOD musteri_hesap.
    DATA ls_out LIKE LINE OF mt_out.
    me->devir_musteri_new( ).

    me->musteri_hasap_get_new( ).

    me->sort_itab( ).


    IF NOT gs_range-s_umskz[] IS INITIAL.
      me->fill_tout_with_odk( koart = zfi_000_cl02=>mC_KOART ).

    ELSE.
      me->fill_tout( ).

    ENDIF.
***np ekleme
    LOOP AT mt_out INTO ls_out WHERE   xnegp = abap_true .
*      AND
*        bukrs NOT BETWEEN '1000' AND '9000'.
      IF ls_out-dmbtrh <> 0.
        ls_out-dmbtrs =   ls_out-dmbtrh * -1.
        ls_out-dmbtrh = 0.
      ELSE.
        ls_out-dmbtrh =   ls_out-dmbtrs * -1.
        ls_out-dmbtrs = 0.
      ENDIF.
      MODIFY mt_out FROM ls_out.
    ENDLOOP.
********
*    me->list_out( ).
.

  ENDMETHOD.


  method MUSTERI_HESAP_BP.
    data ls_out like LINE OF mt_out.
    me->devir_musteri_bp( ).

    me->musteri_hasap_get_bp( ).

    me->sort_itab_bp( ).

    IF NOT gs_range-s_umskz IS INITIAL.
     me->fill_tout_with_odk( koart = zfi_000_cl02=>mc_koart ).

  ELSE.
     me->fill_tout_bp( ).

  ENDIF.

*np ekleme
  LOOP AT mt_out into ls_out WHERE   xnegp = abap_true.
*    AND
*    bukrs NOT BETWEEN '1000' AND '9000'.
    IF ls_out-wrbtrh <> 0.
      ls_out-wrbtrs =   ls_out-wrbtrh * -1.
      ls_out-wrbtrh = 0.
    ELSE.
      ls_out-wrbtrh =   ls_out-wrbtrs * -1.
      ls_out-wrbtrs = 0.
    ENDIF.
    MODIFY mt_out from ls_out.
  ENDLOOP.

  endmethod.


  method NET_VADE_TARIHI.
      DATA: s_due_inp type faede,
        s_due_ret type faede.

  s_due_inp-shkzg = shkzg.
  s_due_inp-koart = koart.
  s_due_inp-zfbdt = zfbdt.
  s_due_inp-zbd1t = zbd1t.
  s_due_inp-zbd2t = zbd2t.
  s_due_inp-zbd3t = zbd3t.
  s_due_inp-rebzg = rebzg.
  s_due_inp-bldat = bldat.


  IF koart CA ZFI_000_CL02=>mc_koart_dk. " OR NOT due_inp-zfbdt IS INITIAL.
    CALL FUNCTION 'DETERMINE_DUE_DATE'
      EXPORTING
        i_faede    = s_due_inp
        i_gl_faede = abap_true
      IMPORTING
        e_faede    = s_due_ret
      EXCEPTIONS
        OTHERS     = 1.
    IF sy-subrc = 0.
      faedt = s_due_ret-netdt.
    ELSE.
      faedt = zfbdt.
    ENDIF.
  ENDIF.






  endmethod.


  METHOD satici_hasap_get_bp.
    TYPES : BEGIN OF ty_zbd,
              zbd1t TYPE bseg-zbd1t,
              zbd2t TYPE bseg-zbd2t,
              zbd3t TYPE bseg-zbd3t,
              rebzg TYPE bseg-rebzg,
              koart TYPE bseg-koart,
            END OF ty_zbd.
    DATA ls_zbd TYPE ty_zbd.

    DATA: ls_bsik TYPE bsik,
          lt_bsik TYPE TABLE OF bsik,
          lt_bsak TYPE TABLE OF bsak,
          ls_bsak TYPE bsak.
    DATA ls_itab LIKE LINE OF mt_itab.


    SELECT * FROM bsik INTO TABLE lt_bsik
                      WHERE  bukrs IN gs_range-s_bukrs
                        AND  lifnr IN gs_range-s_hkont
                        AND  budat IN gs_range-s_budat
                        AND  gsber IN gs_range-s_gsber
                        AND  blart IN gs_range-s_blart
                        AND  prctr IN gs_range-s_prctr
                        AND  umskz IN gs_range-s_umskz.

    SELECT * FROM bsak APPENDING TABLE lt_bsik
                      WHERE  bukrs IN gs_range-s_bukrs
                        AND  lifnr IN gs_range-s_hkont
                        AND  budat IN gs_range-s_budat
                        AND  gsber IN gs_range-s_gsber
                        AND  blart IN gs_range-s_blart
                        AND  prctr IN gs_range-s_prctr
                        AND  umskz IN gs_range-s_umskz.

    IF gs_range-s_umskz[] IS INITIAL.
      DELETE lt_bsik WHERE umskz IS NOT INITIAL.
    ENDIF.

************************************************
    IF lt_bsik[] IS NOT INITIAL.
      SELECT bukrs, belnr, gjahr, buzei, xnegp, zbd1t, zbd2t, zbd3t, rebzg, koart, mwskz, paobjnr
          FROM epic_v_brs_bseg
          INTO TABLE @DATA(lt_bseg)
          FOR ALL ENTRIES IN @lt_bsik
          WHERE bukrs EQ @lt_bsik-bukrs
            AND belnr EQ @lt_bsik-belnr
            AND gjahr EQ @lt_bsik-gjahr.
    ENDIF.

*-> begin - AATAN / 06.12.2021
    IF NOT lt_bseg[] IS INITIAL.
      SELECT DISTINCT
        ce4c~paobjnr,
        ce4c~kmland,
        T005t~landx AS kmland_dsc,
        ce4c~ww028,
        t25b0~bezek AS ww028_dsc,
        ce4c~ww026,
        t25a8~bezek AS ww026_dsc,
        ce4c~ww027,
        t25a9~bezek AS ww027_dsc,
        ce4c~kndnr,
        kna1~name1 AS kndnr_dsc
          FROM ce4cg00_acct AS ce4c
           LEFT OUTER JOIN t005t ON t005t~spras EQ @sy-langu
                                AND t005t~land1 EQ ce4c~kmland
           LEFT OUTER JOIN t25b0 ON t25b0~spras EQ @sy-langu
                                AND t25b0~ww028 EQ ce4c~ww028
           LEFT OUTER JOIN t25a8 ON t25a8~spras EQ @sy-langu
                                AND t25a8~ww026 EQ ce4c~ww026
           LEFT OUTER JOIN t25a9 ON t25a9~spras EQ @sy-langu
                                AND t25a9~ww027 EQ ce4c~ww027
           LEFT OUTER JOIN kna1 ON kna1~kunnr = ce4c~kndnr
            FOR ALL ENTRIES IN @lt_bseg
              WHERE paobjnr EQ @lt_bseg-paobjnr
                INTO TABLE @DATA(t_ce4cdat).
      SORT t_ce4cdat BY paobjnr.
    ENDIF.
*<- end   - AATAN / 06.12.2021

*    SELECT bukrs, belnr, gjahr, vzahl FROM zfi_001_t03 INTO TABLE @DATA(lt_vzahl).
    SELECT zz1~bukrs, zz1~belnr, zz1~gjahr, zz1~vzahl
      FROM zfi_001_t03 AS zz1
           INNER JOIN @lt_bsik AS itab ON zz1~bukrs = itab~bukrs
                                      AND zz1~belnr = itab~belnr
                                      AND zz1~gjahr = itab~gjahr
      INTO TABLE @DATA(lt_vzahl).
*<- end   - ETUNC / 28.11.2020

    LOOP AT lt_bsik INTO ls_bsik.


      CLEAR ls_itab.
      MOVE-CORRESPONDING ls_bsik TO ls_itab.
      CLEAR : ls_zbd.
      READ TABLE lt_bseg INTO DATA(ls_bseg) WITH KEY bukrs = ls_bsik-bukrs
                                               belnr = ls_bsik-belnr
                                               gjahr = ls_bsik-gjahr
                                               buzei = ls_bsik-buzei.
      MOVE-CORRESPONDING ls_bseg TO ls_zbd.
*******************************************************************
*    SELECT SINGLE zbd1t zbd2t zbd3t rebzg koart xref1
*    INTO CORRESPONDING FIELDS OF ls_zbd
*    FROM bseg WHERE bukrs = ls_bsik-bukrs AND
*                    belnr = ls_bsik-belnr AND
*                    gjahr = ls_bsik-gjahr AND
*                    buzei = ls_bsik-buzei.

*-> begin - AATAN / 06.12.2021
      READ TABLE t_ce4cdat REFERENCE INTO DATA(r_ce4cdat) WITH KEY paobjnr = ls_bseg-paobjnr BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        MOVE-CORRESPONDING r_ce4cdat->* to ls_itab.
      ENDIF.
*<- end   - AATAN / 06.12.2021

      CALL FUNCTION 'NET_DUE_DATE_GET'
        EXPORTING
          i_zfbdt = ls_bsik-zfbdt
          i_zbd1t = ls_zbd-zbd1t
          i_zbd2t = ls_zbd-zbd2t
          i_zbd3t = ls_zbd-zbd3t
          i_shkzg = ls_bsik-shkzg
          i_rebzg = ls_zbd-rebzg
          i_koart = ls_zbd-koart
        IMPORTING
          e_faedt = ls_itab-faedt.

      ls_itab-saknr = ls_bsik-hkont. "neo
      MOVE: ls_bsik-lifnr TO ls_itab-hkont.

      MOVE: ls_bsik-hkont TO ls_itab-ref_hkont.

      IF ls_bsik-shkzg = zfi_000_cl02=>mc_shkzg.
        MOVE: ls_bsik-wrbtr TO ls_itab-wrbtrs.
        MOVE: ls_bsik-dmbtr TO ls_itab-dmbtrs. "HY

      ELSE.
        MOVE: ls_bsik-wrbtr TO ls_itab-wrbtrh.
        MOVE: ls_bsik-dmbtr TO ls_itab-dmbtrh. "HY
      ENDIF.

      IF gs_range-s_umskz IS INITIAL.
        ls_itab-umskz = space.
      ENDIF.
*np ekleme
      CLEAR ls_bseg.
      READ TABLE lt_bseg INTO ls_bseg WITH KEY gjahr = ls_bsik-gjahr
                                               bukrs = ls_bsik-bukrs
                                               belnr = ls_bsik-belnr
                                               buzei = ls_bsik-buzei.
*********************************************************************
*    SELECT SINGLE * FROM bseg INTO ls_bseg
*              WHERE gjahr = ls_bsik-gjahr
*                AND bukrs = ls_bsik-bukrs
*                AND belnr = ls_bsik-belnr
*                AND buzei = ls_bsik-buzei.
      ls_itab-xnegp = ls_bseg-xnegp.

      me->get_material(
        EXPORTING
          bukrs = ls_bsik-bukrs                 " Şirket kodu
          belnr = ls_bsik-belnr                 " Kalem no.tayini: Malzeme belgesi - satınalma belgesi
          gjahr = ls_bsik-gjahr                 " Mali yıl
          buzei = ls_bsik-buzei                " Muhasebe belgesi içindeki kayıt satırının numarası
      ).

      CHECK sy-subrc EQ 0.


      READ TABLE lt_vzahl INTO DATA(ls_vzahl) WITH KEY bukrs = ls_itab-bukrs
                                                 belnr = ls_itab-belnr
                                                 gjahr = ls_itab-gjahr.
      ls_itab-vzahl = ls_vzahl-vzahl.
*****************************************************************************
*    SELECT SINGLE vzahl
*           FROM zfi_001_t03
*            INTO ls_itab-vzahl
*        WHERE bukrs = ls_itab-bukrs
*            AND belnr = ls_itab-belnr
*            AND gjahr = ls_itab-gjahr.

      APPEND ls_itab TO mt_itab.

    ENDLOOP.
*
*  SELECT * FROM bsak INTO TABLE lt_bsak
*                     WHERE bukrs IN gs_range-s_bukrs
*                      AND  lifnr IN gs_range-s_hkont
*                      AND  budat IN gs_range-s_budat
*                      AND  gsber IN gs_range-s_gsber
*                      AND  blart IN gs_range-s_blart
*                      AND  prctr IN gs_range-s_prctr
*                      AND  umskz IN gs_range-s_umskz.
*    LOOP AT lt_bsak INTO ls_bsak.
*
*    CLEAR ls_itab.
*
*    MOVE-CORRESPONDING ls_bsak TO ls_itab.
*    CLEAR : ls_zbd,ls_bseg.
*
*    READ TABLE lt_bseg INTO ls_bseg WITH KEY bukrs = ls_bsak-bukrs
*                                             belnr = ls_bsak-belnr
*                                             gjahr = ls_bsak-gjahr
*                                             buzei = ls_bsak-buzei.
*    MOVE-CORRESPONDING ls_bseg to ls_zbd.
*********************************************************************
**    SELECT SINGLE zbd1t zbd2t zbd3t rebzg koart xref1
**    INTO CORRESPONDING FIELDS OF ls_zbd
**    FROM bseg WHERE bukrs = ls_bsak-bukrs AND
**                    belnr = ls_bsak-belnr AND
**                    gjahr = ls_bsak-gjahr AND
**                    buzei = ls_bsak-buzei.
*
*    CALL FUNCTION 'NET_DUE_DATE_GET'
*    EXPORTING
*      i_zfbdt = ls_bsak-zfbdt
*      i_zbd1t = ls_zbd-zbd1t
*      i_zbd2t = ls_zbd-zbd2t
*      i_zbd3t = ls_zbd-zbd3t
*      i_shkzg = ls_bsak-shkzg
*      i_rebzg = ls_zbd-rebzg
*      i_koart = ls_zbd-koart
*    IMPORTING
*      e_faedt = ls_itab-faedt.
*
*    ls_itab-saknr = ls_bsik-hkont. "neo burada bir bsaka bak varmı diye
*    MOVE: ls_bsak-lifnr TO ls_itab-hkont.
*
*    MOVE: ls_bsak-hkont TO ls_itab-ref_hkont.
*
*    IF ls_bsak-shkzg = c_shkzg.
*      MOVE: ls_bsak-wrbtr TO ls_itab-wrbtrs.
*      MOVE: ls_bsak-dmbtr TO ls_itab-dmbtrs. "HY
*
*    ELSE.
*      MOVE: ls_bsak-wrbtr TO ls_itab-wrbtrh.
*      MOVE: ls_bsak-dmbtr TO ls_itab-dmbtrh. "HY
*    ENDIF.
*
*    IF gs_range-s_umskz[] IS INITIAL.
*      ls_itab-umskz = space.
*    ENDIF.
*
**np ekleme
*    CLEAR ls_bseg.
*    read table lt_bseg into ls_bseg with key gjahr = ls_bsak-gjahr
*                                              bukrs = ls_bsak-bukrs
*                                              belnr = ls_bsak-belnr
*                                              buzei = ls_bsak-buzei.
*********************************************************************
**    SELECT SINGLE * FROM bseg INTO ls_bseg
**              WHERE gjahr = ls_bsak-gjahr
**                AND bukrs = ls_bsak-bukrs
**                AND belnr = ls_bsak-belnr
**                AND buzei = ls_bsak-buzei.
*    ls_itab-xnegp = ls_bseg-xnegp.
*****
*
*    me->get_material(
*      EXPORTING
*        bukrs = ls_bsak-bukrs                 " Şirket kodu
*        belnr = ls_bsak-belnr                 " Kalem no.tayini: Malzeme belgesi - satınalma belgesi
*        gjahr = ls_bsak-gjahr                 " Mali yıl
*        buzei = ls_bsak-buzei                " Muhasebe belgesi içindeki kayıt satırının numarası
*    ).
*
*
*    CHECK sy-subrc EQ 0.
*    clear ls_vzahl.
*    READ TABLE lt_vzahl INTO ls_vzahl WITH KEY bukrs = ls_itab-bukrs
*                                               belnr = ls_itab-belnr
*                                               gjahr = ls_itab-gjahr.
*    ls_itab-vzahl = ls_vzahl-vzahl.
**********************************************************************
**    SELECT SINGLE vzahl
**           FROM zfi_001_t03
**            INTO ls_itab-vzahl
**        WHERE bukrs = ls_itab-bukrs
**            AND belnr = ls_itab-belnr
**            AND gjahr = ls_itab-gjahr.
*
*    APPEND ls_itab TO mt_itab.


*ENDLOOP.




  ENDMETHOD.


  METHOD satici_hasap_get_new.


    DATA : lt_bsik TYPE TABLE OF bsik,
           ls_bsik TYPE bsik,
           lt_bsak TYPE TABLE OF bsak,
           ls_bsak TYPE bsak.
    DATA ls_itab LIKE LINE OF mt_itab.

    CLEAR : ls_bsik,ls_bsak.
    REFRESH :lt_bsik[],lt_bsak[].

    SELECT * INTO TABLE lt_bsik
      FROM bsik
     WHERE bukrs IN gs_range-s_bukrs
       AND lifnr IN gs_range-s_hkont
       AND budat IN gs_range-s_budat
       AND gsber IN gs_range-s_gsber
       AND blart IN gs_range-s_blart
       AND prctr IN gs_range-s_prctr
       AND umskz IN gs_range-s_umskz.

    SELECT * APPENDING TABLE lt_bsik
      FROM bsak
     WHERE bukrs IN gs_range-s_bukrs
       AND lifnr IN gs_range-s_hkont
       AND budat IN gs_range-s_budat
       AND gsber IN gs_range-s_gsber
       AND blart IN gs_range-s_blart
       AND prctr IN gs_range-s_prctr
       AND umskz IN gs_range-s_umskz.

    IF gs_range-s_umskz[] IS INITIAL.
      DELETE lt_bsik WHERE umskz IS NOT INITIAL.
    ENDIF.

************************************************
    IF lt_bsik[] IS NOT INITIAL.
      SELECT bukrs, belnr, gjahr, buzei, xnegp, zbd1t, zbd2t, zbd3t,
             rebzg, koart, xref1, xref2, augbl, augdt, zfbdt, shkzg, mwskz, paobjnr
         FROM epic_v_brs_bseg INTO TABLE @DATA(lt_bseg)
          FOR ALL ENTRIES IN @lt_bsik
         WHERE bukrs EQ @lt_bsik-bukrs
          AND  belnr EQ @lt_bsik-belnr
          AND  gjahr EQ @lt_bsik-gjahr .
    ENDIF.
*-> begin - AATAN / 06.12.2021
    IF NOT lt_bseg[] IS INITIAL.
      SELECT DISTINCT
        ce4c~paobjnr,
        ce4c~kmland,
        T005t~landx AS kmland_dsc,
        ce4c~ww028,
        t25b0~bezek AS ww028_dsc,
        ce4c~ww026,
        t25a8~bezek AS ww026_dsc,
        ce4c~ww027,
        t25a9~bezek AS ww027_dsc,
        ce4c~kndnr,
        kna1~name1 AS kndnr_dsc
          FROM ce4cg00_acct AS ce4c
           LEFT OUTER JOIN t005t ON t005t~spras EQ @sy-langu
                                AND t005t~land1 EQ ce4c~kmland
           LEFT OUTER JOIN t25b0 ON t25b0~spras EQ @sy-langu
                                AND t25b0~ww028 EQ ce4c~ww028
           LEFT OUTER JOIN t25a8 ON t25a8~spras EQ @sy-langu
                                AND t25a8~ww026 EQ ce4c~ww026
           LEFT OUTER JOIN t25a9 ON t25a9~spras EQ @sy-langu
                                AND t25a9~ww027 EQ ce4c~ww027
           LEFT OUTER JOIN kna1 ON kna1~kunnr = ce4c~kndnr
            FOR ALL ENTRIES IN @lt_bseg
              WHERE paobjnr EQ @lt_bseg-paobjnr
                INTO TABLE @DATA(t_ce4cdat).
      SORT t_ce4cdat BY paobjnr.
    ENDIF.
*<- end   - AATAN / 06.12.2021
*    SELECT bukrs, belnr, gjahr, vzahl FROM zfi_001_t03 INTO TABLE @DATA(lt_vzahl).
    SELECT zz1~bukrs, zz1~belnr, zz1~gjahr, zz1~vzahl
      FROM zfi_001_t03 AS zz1
           INNER JOIN @lt_bsik AS itab ON zz1~bukrs = itab~bukrs
                                      AND zz1~belnr = itab~belnr
                                      AND zz1~gjahr = itab~gjahr
      INTO TABLE @DATA(lt_vzahl).
*<- end   - ETUNC / 28.11.2020


    LOOP AT lt_bsik INTO ls_bsik.
      CLEAR ls_itab.
      MOVE-CORRESPONDING ls_bsik TO ls_itab.

      READ TABLE lt_bseg INTO DATA(ls_bseg) WITH KEY bukrs = ls_bsik-bukrs
                                               belnr = ls_bsik-belnr
                                               gjahr = ls_bsik-gjahr
                                               buzei = ls_bsik-buzei.

*******************************************************************
*    SELECT SINGLE * FROM bseg INTO ls_bseg
*                WHERE gjahr = ls_bsik-gjahr
*                  AND bukrs = ls_bsik-bukrs
*                  AND belnr = ls_bsik-belnr
*                  AND buzei = ls_bsik-buzei.

      ls_itab-xref1 = ls_bseg-xref1.
      ls_itab-xref2 = ls_bseg-xref2.
* CUT ------->
      ls_itab-augbl = ls_bseg-augbl.
      ls_itab-augdt = ls_bseg-augdt.
*np ekleme
      ls_itab-xnegp = ls_bseg-xnegp.
*   ters kayıt belge no

*-> begin - AATAN / 06.12.2021
      ls_itab-mwskz = ls_bseg-mwskz.
      READ TABLE t_ce4cdat REFERENCE INTO DATA(r_ce4cdat) WITH KEY paobjnr = ls_bseg-paobjnr BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        MOVE-CORRESPONDING r_ce4cdat->* to ls_itab.
      ENDIF.
*<- end   - AATAN / 06.12.2021

      me->get_baslik_bilgileri(
        EXPORTING
          bukrs    = ls_bseg-bukrs                 " Şirket kodu
          belnr    = ls_bseg-belnr                 " Kalem no.tayini: Malzeme belgesi - satınalma belgesi
          gjahr    = ls_bseg-gjahr                 " Mali yıl
        CHANGING
          stblg    = ls_itab-stblg                 " Ters kayıt belge numarası
          xref1_hd = ls_itab-xref1_hd                 " Belge başlığına ilişkin referans anahtar_1 (dahili)
          bktxt    = ls_itab-bktxt                 " Belge başlığı metni
      ).

*   net vade tarihi
      me->net_vade_tarihi(
        EXPORTING
          zfbdt = ls_bseg-zfbdt                 " Vade hesaplamasına temel oluşturan tarih
          zbd1t = ls_bseg-zbd1t                 " Nakit indirimi günleri 1
          zbd2t = ls_bseg-zbd2t                 " Nakit indirimi günleri 2
          zbd3t = ls_bseg-zbd3t                 " Net ödeme koşullarına ilişkin süre
          shkzg = ls_bseg-shkzg                 " Borç/alacak göstergesi
          rebzg = ls_bseg-rebzg                 " İşlemin ait olduğu faturanın belge numarası
          koart = ls_bseg-koart                 " Hesap türü
          bldat = ls_bsik-bldat                 " Belge tarihi
        CHANGING
          faedt = ls_itab-faedt               " Net ödeme vadesi
      ).

* CUT <-------


      ls_itab-saknr = ls_bsik-hkont. "neo
      MOVE: ls_bsik-lifnr TO ls_itab-hkont.

      MOVE: ls_bsik-hkont TO ls_itab-ref_hkont.

      IF ls_bsik-shkzg = zfi_000_cl02=>mc_shkzg.
        MOVE: ls_bsik-wrbtr TO ls_itab-wrbtrs.
        MOVE: ls_bsik-dmbtr TO ls_itab-dmbtrs. "HY

      ELSE.
        MOVE: ls_bsik-wrbtr TO ls_itab-wrbtrh.
        MOVE: ls_bsik-dmbtr TO ls_itab-dmbtrh. "HY
      ENDIF.

      IF gs_range-s_umskz[] IS INITIAL.
        ls_itab-umskz = space.
      ENDIF.

      me->get_material(
        EXPORTING
          bukrs = ls_bsik-bukrs                 " Şirket kodu
          belnr = ls_bsik-belnr                 " Kalem no.tayini: Malzeme belgesi - satınalma belgesi
          gjahr = ls_bsik-gjahr                 " Mali yıl
          buzei = ls_bsik-buzei                 " Muhasebe belgesi içindeki kayıt satırının numarası
      ).

      CHECK sy-subrc EQ 0.
      READ TABLE lt_vzahl INTO DATA(ls_vzahl) WITH KEY bukrs = ls_itab-bukrs
                                                 belnr = ls_itab-belnr
                                                 gjahr = ls_itab-gjahr.
      ls_itab-vzahl = ls_vzahl-vzahl.
*****************************************************************************
*    SELECT SINGLE vzahl
*           FROM ZFI_001_T03
*            INTO ls_itab-vzahl
*        WHERE bukrs = ls_itab-bukrs
*            AND belnr = ls_itab-belnr
*            AND gjahr = ls_itab-gjahr.

      APPEND ls_itab TO mt_itab.
    ENDLOOP.


*  SELECT * INTO TABLE lt_bsak
*    FROM bsak
*   WHERE bukrs IN gs_range-s_bukrs
*     AND lifnr IN gs_range-s_hkont
*     AND budat IN gs_range-s_budat
*     AND gsber IN gs_range-s_gsber
*     AND blart IN gs_range-s_blart
*     AND prctr IN gs_range-s_prctr
*     AND umskz IN gs_range-s_umskz.

*  LOOP AT lt_bsak INTO ls_bsak.
*    CLEAR ls_itab.
*
*    MOVE-CORRESPONDING ls_bsak TO ls_itab.
*
*
*    READ TABLE lt_bseg INTO ls_bseg WITH KEY bukrs = ls_bsak-bukrs
*                                             belnr = ls_bsak-belnr
*                                             gjahr = ls_bsak-gjahr
*                                             buzei = ls_bsak-buzei.
**    SELECT SINGLE * FROM bseg INTO ls_bseg
**                WHERE gjahr = ls_bsak-gjahr
**                  AND bukrs = ls_bsak-bukrs
**                  AND belnr = ls_bsak-belnr
**                  AND buzei = ls_bsak-buzei.
*
*    ls_itab-xref1 = ls_bseg-xref1.
*    ls_itab-xref2 = ls_bseg-xref2.
*
** CUT ------->
*    ls_itab-augbl = ls_bseg-augbl.
*    ls_itab-augdt = ls_bseg-augdt.
**np ekleme
*    ls_itab-xnegp = ls_bseg-xnegp.
**   ters kayıt belge no
*    me->get_baslik_bilgileri(
*      EXPORTING
*        bukrs    = ls_bseg-bukrs                 " Şirket kodu
*        belnr    = ls_bseg-belnr                 " Kalem no.tayini: Malzeme belgesi - satınalma belgesi
*        gjahr    = ls_bseg-gjahr                 " Mali yıl
*      CHANGING
*        stblg    = ls_itab-stblg                 " Ters kayıt belge numarası
*        xref1_hd = ls_itab-xref1_hd                 " Belge başlığına ilişkin referans anahtar_1 (dahili)
*        bktxt    = ls_itab-bktxt                " Belge başlığı metni
*    ).
*
**   net vade tarihi
*    me->net_vade_tarihi(
*      EXPORTING
*        zfbdt = ls_bseg-zfbdt                 " Vade hesaplamasına temel oluşturan tarih
*        zbd1t = ls_bseg-zbd1t                 " Nakit indirimi günleri 1
*        zbd2t = ls_bseg-zbd2t                 " Nakit indirimi günleri 2
*        zbd3t = ls_bseg-zbd3t                 " Net ödeme koşullarına ilişkin süre
*        shkzg = ls_bseg-shkzg                 " Borç/alacak göstergesi
*        rebzg = ls_bseg-rebzg                 " İşlemin ait olduğu faturanın belge numarası
*        koart = ls_bseg-koart                 " Hesap türü
*        bldat = ls_bsak-bldat                 " Belge tarihi
*      CHANGING
*        faedt = ls_itab-faedt                  " Net ödeme vadesi
*    ).
*
** CUT <-------
*
*
*    ls_itab-saknr = ls_bsak-hkont. "neo
*    MOVE: ls_bsak-lifnr TO ls_itab-hkont.
*
*    MOVE: ls_bsak-hkont TO ls_itab-ref_hkont.
*
*    IF ls_bsak-shkzg = c_shkzg.
*      MOVE: ls_bsak-wrbtr TO ls_itab-wrbtrs.
*      MOVE: ls_bsak-dmbtr TO ls_itab-dmbtrs. "HY
*
*    ELSE.
*      MOVE: ls_bsak-wrbtr TO ls_itab-wrbtrh.
*      MOVE: ls_bsak-dmbtr TO ls_itab-dmbtrh. "HY
*    ENDIF.
*
*    IF gs_range-s_umskz[] IS INITIAL.
*      ls_itab-umskz = space.
*    ENDIF.
*
*    me->get_material(
*      EXPORTING
*        bukrs = ls_bsik-bukrs                 " Şirket kodu
*        belnr = ls_bsik-belnr                 " Kalem no.tayini: Malzeme belgesi - satınalma belgesi
*        gjahr = ls_bsik-gjahr                 " Mali yıl
*        buzei = ls_bsik-buzei                 " Muhasebe belgesi içindeki kayıt satırının numarası
*    ).
*
*    CHECK sy-subrc EQ 0.
*    CLEAR ls_vzahl.
*    READ TABLE lt_vzahl INTO ls_vzahl WITH KEY bukrs = ls_itab-bukrs
*                                               belnr = ls_itab-belnr
*                                               gjahr = ls_itab-gjahr.
*    ls_itab-vzahl = ls_vzahl-vzahl.
**********************************************************************
**    SELECT SINGLE vzahl
**           FROM ZFI_001_T03
**            INTO ls_itab-vzahl
**        WHERE bukrs = ls_itab-bukrs
**            AND belnr = ls_itab-belnr
**            AND gjahr = ls_itab-gjahr.
*
*    APPEND ls_itab TO mt_itab.
*  ENDLOOP.

  ENDMETHOD.


  METHOD satici_hesap.
    DATA ls_out LIKE LINE OF mt_out.
    me->devir_satici_new( ).

    me->satici_hasap_get_new( ).

    me->sort_itab( ).

    me->fill_tout( ).

    IF NOT gs_range-s_umskz[] IS INITIAL.
      me->fill_tout_with_odk( koart = zfi_000_cl02=>mc_koart ).

    ELSE.
      me->fill_tout( ).

    ENDIF.
***np ekleme
    LOOP AT mt_out INTO ls_out WHERE xnegp = abap_true.
*       AND
*      bukrs NOT BETWEEN '1000' AND '9000'.
      IF ls_out-dmbtrh <> 0.
        ls_out-dmbtrs =   ls_out-dmbtrh * -1.
        ls_out-dmbtrh = 0.
      ELSE.
        ls_out-dmbtrh =   ls_out-dmbtrs * -1.
        ls_out-dmbtrs = 0.
      ENDIF.
      MODIFY mt_out FROM ls_out.
    ENDLOOP.
********
*    me->list_out( ).

  ENDMETHOD.


  method SATICI_HESAP_BP.
    data ls_out like line of mt_out.
    me->devir_satici_bp( ).

    me->satici_hasap_get_bp( ).

    me->sort_itab_bp( ).

  IF NOT gs_range-s_umskz[] IS INITIAL.
      me->fill_tout_with_odk( koart = zfi_000_cl02=>mc_koart ).
  ELSE.
     me->fill_tout_bp( ).
  ENDIF.

*np ekleme
  LOOP AT mt_out into ls_out WHERE   xnegp = abap_true .
*    AND
*    bukrs NOT BETWEEN '1000' AND '9000'.
    IF ls_out-wrbtrh <> 0.
      ls_out-wrbtrs =   ls_out-wrbtrh * -1.
      ls_out-wrbtrh = 0.
    ELSE.
      ls_out-wrbtrh =   ls_out-wrbtrs * -1.
      ls_out-wrbtrs = 0.
    ENDIF.
    MODIFY mt_out from ls_out.
  ENDLOOP.



  endmethod.


  METHOD send_mail.

    DATA: lt_mail LIKE mt_out,
          ls_mail LIKE LINE OF mt_out.
    DATA: lt_kna1 type table of kna1,
          ls_kna1 type kna1,
          ls_adr6 LIKE LINE OF lt_kna1.
    data  ls_out like line of mt_out.
    LOOP AT mt_out into ls_out.
      AT NEW hkont.
        CLEAR: ls_kna1,
               ls_adr6,
               ls_mail.
        FREE : lt_mail.
        SELECT SINGLE * FROM kna1 INTO ls_kna1
                       WHERE kunnr = ls_out-hkont.
        IF sy-subrc = 0 AND ls_kna1-adrnr NE space.
          SELECT SINGLE * FROM adr6 INTO ls_adr6
                         WHERE addrnumber = ls_kna1-adrnr.
        ENDIF.
      ENDAT.
      CLEAR ls_mail.
      MOVE-CORRESPONDING ls_out TO ls_mail.
      APPEND ls_mail to lt_mail.

      AT END OF hkont.

*        PERFORM send_mail2 TABLES t_mail
*                            USING wadr6-smtp_addr.
      ENDAT.
    ENDLOOP.


  ENDMETHOD.


  METHOD sort_itab.

    DATA: ls_hkont LIKE LINE OF mt_hkont,
          ls_waers LIKE LINE OF mt_waers.
    DATA  ls_itab   LIKE LINE OF mt_itab.
    DATA: lt_tmp LIKE mt_waers,
          ls_tmp LIKE LINE OF mt_waers.
    DATA: lt_devir  LIKE mt_devir,
          ls_devir  LIKE LINE OF mt_devir,
          Ls_devir2 LIKE LINE OF mt_devir.
    CLEAR: ls_waers.
    FREE:  mt_waers.

    LOOP AT mt_itab INTO ls_itab.
      ls_itab-waers_blg = ls_itab-waers.
      ls_itab-waers = zfi_000_cl02=>mC_WAERS.
      MODIFY mt_itab FROM ls_itab.

      MOVE-CORRESPONDING ls_itab TO ls_waers.
      MOVE-CORRESPONDING ls_itab TO ls_hkont.
      COLLECT ls_hkont INTO mt_hkont.
      COLLECT ls_waers INTO mt_waers.
    ENDLOOP.



    LOOP AT mt_devir INTO Ls_devir2.
      ls_devir = Ls_devir2.
      ls_devir-waers = zfi_000_cl02=>mC_WAERS.
      CLEAR: ls_devir-wrbtrh, ls_devir-wrbtrs.
      COLLECT ls_devir INTO lt_devir.
    ENDLOOP.

    mt_devir[] = lt_devir[].
    CLEAR ls_devir.
    LOOP AT mt_devir INTO ls_devir.
      MOVE-CORRESPONDING ls_devir TO ls_waers.
      MOVE-CORRESPONDING ls_devir TO ls_hkont.
      COLLECT ls_waers INTO mt_waers.
      COLLECT ls_hkont INTO mt_hkont.
    ENDLOOP.

    SORT mt_waers BY hkont umskz .
    SORT mt_hkont BY bukrs hkont umskz.
    SORT mt_itab BY  bukrs  hkont waers umskz  budat bldat.

  ENDMETHOD.


  method SORT_ITAB_BP.
 data :ls_itab like line of mt_itab,
       ls_hkont like line of mt_hkont,
       ls_devir like line of mt_devir,
       ls_waers like line of mt_waers.
  DATA: lt_tmp like table of mt_waers,
        ls_tmp like LINE OF mt_waers.

  CLEAR: ls_waers.
  FREE:  mt_waers.

  LOOP AT mt_itab into ls_itab.
    MOVE-CORRESPONDING ls_itab TO ls_waers.
    MOVE-CORRESPONDING ls_waers TO ls_hkont.
    COLLECT ls_hkont into mt_hkont.
    COLLECT ls_waers  into mt_waers.
  ENDLOOP.

  LOOP AT mt_devir into ls_devir.
    MOVE-CORRESPONDING ls_devir TO ls_waers.
    MOVE-CORRESPONDING ls_devir TO ls_hkont.
    COLLECT ls_waers into mt_waers.
    COLLECT ls_hkont into mt_hkont.
  ENDLOOP.

  SORT mt_waers BY hkont umskz .
  SORT mt_hkont BY bukrs hkont umskz.
  SORT mt_itab BY  bukrs  hkont waers umskz  budat bldat.
  endmethod.


  METHOD sort_waers_odk.
    DATA ls_waers LIKE LINE OF mt_waers.
    DATA ls_waers2 LIKE LINE OF mt_waers2.
    CLEAR: ls_waers2.
    FREE:  mt_waers2.

    IF gs_range-p_rb4 EQ abap_true.


      LOOP AT mt_waers INTO ls_waers WHERE bukrs = bukrs
                         AND hkont = hkont
                         AND umskz = umskz.
        CLEAR ls_waers2.
        MOVE-CORRESPONDING ls_waers TO ls_waers2.
        COLLECT ls_waers2 INTO mt_waers2.
      ENDLOOP.
    ELSEIF gs_range-p_rb5 EQ abap_true.

      CLEAR: ls_waers2.
      FREE:  mt_waers2.

      LOOP AT mt_waers INTO ls_waers WHERE bukrs = bukrs
                         AND hkont = hkont.
        CLEAR ls_waers2.
        MOVE-CORRESPONDING ls_waers TO ls_waers2.
        COLLECT ls_waers2 INTO mt_waers2.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD zbc_000_if01~at_selection_screen.

    zbc_000_cl01=>get_selections_from_program( IMPORTING es_range = me->gs_range ) .

    IF me->gs_range-p_rb1 EQ abap_true.
      CLEAR: me->gs_range-s_kunnr[],
             me->gs_range-s_lifnr[].

      IF iv_ucomm EQ zfi_000_cl02=>MC_IV_UCOMM_ONLI AND
         me->gs_range-s_hkont[] IS INITIAL.
        SET CURSOR FIELD 'S_HKONT-LOW'.
        MESSAGE TEXT-m01 TYPE ZFI_000_CL02=>mc_type_e.
      ENDIF.

    ELSEIF me->gs_range-p_rb2 EQ abap_true.
      CLEAR: me->gs_range-s_hkont[],
             me->gs_range-s_lifnr[].

      me->gs_range-p_rldnr = zfi_000_cl02=>mc_gs_range_p_rldnr_0L.

      IF iv_ucomm EQ zfi_000_cl02=>MC_IV_UCOMM_ONLI AND
         me->gs_range-s_kunnr[] IS INITIAL.
        SET CURSOR FIELD 'S_KUNNR-LOW'.
        MESSAGE TEXT-m02 TYPE ZFI_000_CL02=>mc_type_e.
      ENDIF.

    ELSEIF me->gs_range-p_rb3 EQ abap_true.
      CLEAR: me->gs_range-s_hkont[],
             me->gs_range-s_kunnr[].

      me->gs_range-p_rldnr = zfi_000_cl02=>MC_GS_RANGE_P_RLDNR.

      IF sy-ucomm EQ zfi_000_cl02=>MC_SY_UCOMM_ONLI AND
         me->gs_range-s_lifnr[] IS INITIAL.
        SET CURSOR FIELD 'S_LIFNR-LOW'.
        MESSAGE TEXT-m03 TYPE ZFI_000_CL02=>mc_type_e.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD zbc_000_if01~at_selection_screen_output.

    LOOP AT SCREEN.

      CASE screen-group1.
        WHEN zfi_000_cl02=>MC_SCREEN_GROUP1_ANA.
          IF me->gs_range-p_rb1 EQ abap_true.
            screen-active = 1.
          ELSE.
            screen-active = 0.
          ENDIF.
        WHEN zfi_000_cl02=>MC_SCREEN_GROUP1_MUS.
          IF me->gs_range-p_rb2 EQ abap_true.
            screen-active = 1.
          ELSE.
            screen-active = 0.
          ENDIF.
        WHEN zfi_000_cl02=>MC_SCREEN_GROUP1_SAT.
          IF me->gs_range-p_rb3 EQ abap_true.
            screen-active = 1.
          ELSE.
            screen-active = 0.
          ENDIF.
        WHEN zfi_000_cl02=>MC_SCREEN_GROUP1_UST.
          IF me->gs_range-p_rb5 EQ abap_true AND me->gs_range-p_rb2 EQ abap_true.
            screen-active = 1.
          ELSE.
            screen-active = 0.
          ENDIF.
        WHEN OTHERS.
      ENDCASE.
      MODIFY SCREEN.
*      CASE screen-name.
*        WHEN '%_S_HKON_%_APP_%-TEXT'
*          OR '%_S_HKON_%_APP_%-OPTI_PUSH'
*          OR 'S_HKON-LOW'
*          OR '%_S_HKON_%_APP_%-TO_TEXT'
*          OR 'S_HKON-HIGH'
*          OR '%_S_HKON_%_APP_%-VALU_PUSH'.
*          IF NOT me->gs_range-p_rb1 EQ 'X'.
*            screen-active = 0.
*            MODIFY SCREEN.
*          ELSE.
*            screen-active = 1.
*            MODIFY SCREEN.
*          ENDIF.
*
*        WHEN '%_S_KUNNR_%_APP_%-TEXT'
*          OR '%_S_KUNNR_%_APP_%-OPTI_PUSH'
*          OR 'S_KUNNR-LOW'
*          OR '%_S_KUNNR_%_APP_%-TO_TEXT'
*          OR 'S_KUNNR-HIGH'
*          OR '%_S_KUNNR_%_APP_%-VALU_PUSH'.
*          IF NOT me->gs_range-p_rb2 EQ 'X'.
*            screen-active = 0.
*            MODIFY SCREEN.
*          ELSE.
*            screen-active = 1.
*            MODIFY SCREEN.
*          ENDIF.
*        WHEN '%_S_LIFN_%_APP_%-TEXT'
*          OR '%_S_LIFN_%_APP_%-OPTI_PUSH'
*          OR 'S_LIFN-LOW'
*          OR '%_S_LIFN_%_APP_%-TO_TEXT'
*          OR 'S_LIFN-HIGH'
*          OR '%_S_LIFN_%_APP_%-VALU_PUSH'.
*          IF NOT me->gs_range-p_rb3 EQ 'X'.
*            screen-active = 0.
*            MODIFY SCREEN.
*          ELSE.
*            screen-active = 1.
*            MODIFY SCREEN.
*          ENDIF.
*        WHEN '%_P_CUST_%_APP_%-TEXT'
*         OR 'P_CUST'.
*
*          IF  me->gs_range-p_rb5 EQ 'X' and me->gs_range-p_rb2 eq 'X' .
*            screen-active = 1.
*            MODIFY SCREEN.
*          ELSE.
*            screen-active = 0.
*            MODIFY SCREEN.
*          ENDIF.
*      ENDCASE.
    ENDLOOP.



  ENDMETHOD.


  METHOD zbc_000_if01~create_alv_object.
    DATA : lo_container TYPE REF TO cl_gui_custom_container.
    DATA : ls_layout  TYPE lvc_s_layo,
           ls_variant TYPE disvariant.
    DATA : lt_exclude TYPE ui_functions,
           lt_fcat    TYPE lvc_t_fcat.
*-----

    CREATE OBJECT lo_container
      EXPORTING
        container_name = zfi_000_cl02=>mc_container_name_alvcontainer.

    CREATE OBJECT zbc_000_if01~go_alv
      EXPORTING
        i_parent = lo_container.

    lt_fcat    = me->zbc_000_if01~fill_field_catalog( ).

    lt_exclude = me->zbc_000_if01~exclude_buttons( ).
    ls_layout  = me->zbc_000_if01~fill_layout( ).

    ls_variant-report  = sy-cprog.
*    ls_variant-variant = me->gs_range-p_vari.

*    SET HANDLER me->zbc_000_if01~alv_button_click  FOR zbc_000_if01~go_alv.
*    SET HANDLER me->zbc_000_if01~alv_double_click  FOR zbc_000_if01~go_alv.
*    SET HANDLER me->zbc_000_if01~alv_menu_button   FOR zbc_000_if01~go_alv.
*    SET HANDLER me->zbc_000_if01~alv_toolbar       FOR zbc_000_if01~go_alv.
*    SET HANDLER me->zbc_000_if01~alv_user_command  FOR zbc_000_if01~go_alv.
*    SET HANDLER me->zbc_000_if01~alv_hotspot_click FOR zbc_000_if01~go_alv.

    CALL METHOD zbc_000_if01~go_alv->set_table_for_first_display
      EXPORTING
*       i_buffer_active               =                  " Buffering Active
*       i_bypassing_buffer            =                  " Switch Off Buffer
*       i_consistency_check           =                  " Starting Consistency Check for Interface Error Recognition
*       i_structure_name              = 'ZFI_001_S04 '                " Internal Output Table Structure Name
        is_variant                    = ms_variant
        i_save                        = mv_variant_save
        i_default                     = abap_true              " Default Display Variant
        is_layout                     = ls_layout                   " Layout
*       is_print                      =                  " Print Control
*       it_special_groups             =                  " Field Groups
        it_toolbar_excluding          = lt_exclude                " Excluded Toolbar Standard Functions
*       it_hyperlink                  =                  " Hyperlinks
*       it_alv_graphics               =                  " Table of Structure DTC_S_TC
*       it_except_qinfo               =                  " Table for Exception Quickinfo
*       ir_salv_adapter               =                  " Interface ALV Adapter
      CHANGING
        it_outtab                     = me->mt_out                 " Output Table
        it_fieldcatalog               = lt_fcat                " Field Catalog
*       it_sort                       =                  " Sort Criteria
*       it_filter                     =                  " Filter Criteria
      EXCEPTIONS
        invalid_parameter_combination = 1                " Wrong Parameter
        program_error                 = 2                " Program Errors
        too_many_lines                = 3                " Too many Rows in Ready for Input Grid
        OTHERS                        = 4.
    IF sy-subrc <> 0.
*      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*        WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.


  ENDMETHOD.


  method ZBC_000_IF01~EXCLUDE_BUTTONS.
  endmethod.


  METHOD zbc_000_if01~fill_field_catalog.
*    DATA: cdmbtrs(20)   TYPE c,
*        cdmbtrh(20)   TYPE c,
*        ctotals_d(25) TYPE c,
*        ctotalh_d(25) TYPE c.
    DATA ls_out LIKE LINE OF mt_out.
    DATA p_tabname TYPE dd02l-tabname.
    p_tabname = zfi_000_cl02=>mc_dd02l_tabname_mt_out.
    REFRESH rt_fcat.
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
*       i_buffer_active        = 'X'
        i_structure_name       = zfi_000_cl02=>mc_i_structure_name_zfi001s04
*       i_client_never_display = 'X'
*       I_BYPASSING_BUFFER     =
*       i_internal_tabname     = p_tabname
      CHANGING
        ct_fieldcat            = rt_fcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

*     read table mt_out into ls_out index 1.
*     SELECT SINGLE * FROM t001 into @data(ls_t001) WHERE bukrs = @ls_out-bukrs.
*
*  CONCATENATE TEXT-008 ls_t001-waers
*                                  INTO cdmbtrs
*                                  SEPARATED BY space.
*
*  CONCATENATE TEXT-009 ls_t001-waers
*                                  INTO cdmbtrh
*                                  SEPARATED BY space.
*
*  CONCATENATE TEXT-010 ls_t001-waers
*                                  INTO ctotals_d
*                                  SEPARATED BY space.
*
*  CONCATENATE TEXT-011 ls_t001-waers
*                                  INTO ctotalh_d
*                                  SEPARATED BY space.

    LOOP AT rt_fcat REFERENCE INTO DATA(lr_fieldcat).
      CASE lr_fieldcat->fieldname.
        WHEN zfi_000_cl02=>mc_fieldname_shkzg.
*          lr_fieldcat->no_out = 'X'.
        WHEN zfi_000_cl02=>mc_fieldname_wrbtr.
*          lr_fieldcat->no_out = 'X'.
*          lr_fieldcat->cfieldname = 'WAERS_BLG'.
        WHEN zfi_000_cl02=>mc_fieldname_umskz.
*          lr_fieldcat->no_out = 'X'.
        WHEN zfi_000_cl02=>mc_fieldname_gjahr.
*          lr_fieldcat->no_out = 'X'.
        WHEN zfi_000_cl02=>mc_fieldname_zuonr.
*          lr_fieldcat->no_out = 'X'.
        WHEN zfi_000_cl02=>mc_fieldname_budat.
*          lr_fieldcat->no_out = 'X'.
        WHEN zfi_000_cl02=>mc_fieldname_gsber.
*          lr_fieldcat->no_out = 'X'.
        WHEN zfi_000_cl02=>mc_fieldname_blart.
*          lr_fieldcat->no_out = 'X'.
        WHEN zfi_000_cl02=>mc_fieldname_matnr.
*          lr_fieldcat->no_out = 'X'.
        WHEN zfi_000_cl02=>mc_fieldname_waers.
*          lr_fieldcat->no_out = 'X'.
        WHEN zfi_000_cl02=>mc_fieldname_waers_blg.
*          lr_fieldcat->no_out = 'X'.
        WHEN zfi_000_cl02=>mc_fieldname_wrbtrs.
*          lr_fieldcat->just = 'X'.
          lr_fieldcat->cfieldname = zfi_000_cl02=>mc_cfieldname_waers_blg.
          lr_fieldcat->scrtext_s  = zfi_000_cl02=>mc_wrbtrs_scrtext_s_bpb_borc.
          lr_fieldcat->scrtext_m  = zfi_000_cl02=>mc_wrbtrs_scrtext_m_bpb_borc.
          lr_fieldcat->scrtext_l  = zfi_000_cl02=>mc_wrbtrs_scrtext_l_bpb_borc.
          lr_fieldcat->reptext    = zfi_000_cl02=>mc_wrbtrs_reptext_bpb_borc.
*          lr_fieldcat->no_out    = 'X'.
        WHEN zfi_000_cl02=>mc_fieldname_wrbtrh.
*          lr_fieldcat->just = 'X'.
          lr_fieldcat->cfieldname = zfi_000_cl02=>mc_cfieldname_waers_blg.
          lr_fieldcat->scrtext_s  = zfi_000_cl02=>mc_wrbtrh_scrtext_s_bpb_alacak.
          lr_fieldcat->scrtext_m  = zfi_000_cl02=>mc_wrbtrh_scrtext_m_bpb_alacak.
          lr_fieldcat->scrtext_l  = zfi_000_cl02=>mc_wrbtrh_scrtext_l_bpb_alacak.
          lr_fieldcat->reptext    = zfi_000_cl02=>mc_wrbtrh_reptext_bpb_alacak.
*          lr_fieldcat->no_out = 'X'.
        WHEN zfi_000_cl02=>mc_fieldname_dmbtrs.
*          lr_fieldcat->ref_field = space.
*          lr_fieldcat->ref_table = space.
          lr_fieldcat->scrtext_s = zfi_000_cl02=>mc_dmbtrs_scrtext_s_upb_borc.
          lr_fieldcat->scrtext_m = zfi_000_cl02=>mc_dmbtrs_scrtext_m_upb_borc.
          lr_fieldcat->scrtext_l = zfi_000_cl02=>mc_dmbtrs_scrtext_l_upb_borc.
          lr_fieldcat->reptext   = zfi_000_cl02=>mc_dmbtrs_reptext_upb_borc.
*          lr_fieldcat->reptext = cdmbtrs.
        WHEN zfi_000_cl02=>mc_fieldname_dmbtrsh.
*          lr_fieldcat->ref_field = space.
*          lr_fieldcat->ref_table = space.
          lr_fieldcat->scrtext_s = zfi_000_cl02=>mc_dmbtrsh_scrtext_s_upbalacak.
          lr_fieldcat->scrtext_m = zfi_000_cl02=>mc_dmbtrsh_scrtext_m_upbalacak.
          lr_fieldcat->scrtext_l = zfi_000_cl02=>mc_dmbtrsh_scrtext_l_upbalacak.
          lr_fieldcat->reptext   = zfi_000_cl02=>mc_dmbtrsh_reptexrt_upbalacak.
*          lr_fieldcat->reptext = cdmbtrh.
        WHEN zfi_000_cl02=>mc_fieldname_totals.
          lr_fieldcat->scrtext_s = zfi_000_cl02=>mc_totals_scrtext_s_bpb_borc_b.
          lr_fieldcat->scrtext_m = zfi_000_cl02=>mc_totals_scrtext_m_bpb_borc_b.
          lr_fieldcat->scrtext_l = zfi_000_cl02=>mc_totals_scrtext_l_bpb_borc_b.
          lr_fieldcat->reptext   = zfi_000_cl02=>mc_totals_reptext_bpb_borc_b.
        WHEN zfi_000_cl02=>mc_fieldname_totalh.
          lr_fieldcat->scrtext_s = zfi_000_cl02=>mc_totalh_scrtext_s_bpb_alacak.
          lr_fieldcat->scrtext_m = zfi_000_cl02=>mc_totalh_scrtext_m_bpb_alacak.
          lr_fieldcat->scrtext_l = zfi_000_cl02=>mc_totalh_scrtext_l_bpb_alacak.
          lr_fieldcat->reptext = zfi_000_cl02=>mc_totalh_reptext_bpb_alacak.
        WHEN zfi_000_cl02=>mc_fieldname_totals_d.
*          lr_fieldcat->ref_field = space.
*          lr_fieldcat->ref_table = space.
          lr_fieldcat->scrtext_s = zfi_000_cl02=>mc_totals_d_scrtext_s_upbborcb.
          lr_fieldcat->scrtext_m = zfi_000_cl02=>mc_totals_d_scrtext_m_upbborcb.
          lr_fieldcat->scrtext_l = zfi_000_cl02=>mc_totals_d_scrtext_l_upbborcb.
          lr_fieldcat->reptext   = zfi_000_cl02=>mc_totals_d_reptext_upbborcb.
*          lr_fieldcat->reptext = ctotals_d.
        WHEN zfi_000_cl02=>mc_fieldname_totalh_d.
*          lr_fieldcat->ref_field = space.
*          lr_fieldcat->ref_table = space.
          lr_fieldcat->scrtext_s = zfi_000_cl02=>mc_totalh_d_scrtexts_upbalacak.
          lr_fieldcat->scrtext_m = zfi_000_cl02=>mc_totalh_d_scrtextm_upbalacak.
          lr_fieldcat->scrtext_l = zfi_000_cl02=>mc_totalh_d_scrtextl_upbalacak.
          lr_fieldcat->reptext = zfi_000_cl02=>mc_totalh_d_reptext_upbalacak.
*          lr_fieldcat->reptext = ctotalh_d.
        WHEN zfi_000_cl02=>mc_fieldname_vzahl.
*          lr_fieldcat->ref_field = space.
*          lr_fieldcat->ref_table = space.
*          lr_fieldcat->scrtext_s = space.
*          lr_fieldcat->scrtext_m = space.
*          lr_fieldcat->scrtext_l = TEXT-012.
*          lr_fieldcat->reptext = TEXT-012.
        WHEN zfi_000_cl02=>mc_fieldname_saknr.
*          lr_fieldcat->ref_field = space.
*          lr_fieldcat->ref_table = space.
*          lr_fieldcat->scrtext_s = space.
*          lr_fieldcat->scrtext_m = space.
*          lr_fieldcat->scrtext_l = TEXT-013.
*          lr_fieldcat->reptext = TEXT-013.
        WHEN zfi_000_cl02=>mc_fieldname_hkont.
          IF gs_range-p_rb1 EQ abap_true.
            lr_fieldcat->scrtext_s = zfi_000_cl02=>mc_hkont_scrtext_s_anahesap.
            lr_fieldcat->scrtext_m = zfi_000_cl02=>mc_hkont_scrtext_m_anahesap.
            lr_fieldcat->scrtext_l = zfi_000_cl02=>mc_hkont_scrtext_l_anahesap.
            lr_fieldcat->reptext = zfi_000_cl02=>mc_hkont_reptext_anahesap.
          ELSEIF gs_range-p_rb2 EQ abap_true.
            lr_fieldcat->scrtext_s = zfi_000_cl02=>mc_hkont_scrtext_s_mus_hesap.
            lr_fieldcat->scrtext_m = zfi_000_cl02=>mc_hkont_scrtext_m_mus_hesap.
            lr_fieldcat->scrtext_l = zfi_000_cl02=>mc_hkont_scrtext_l_mus_hesap.
            lr_fieldcat->reptext   = zfi_000_cl02=>mc_hkont_reptext_mus_hesap.
          ELSEIF gs_range-p_rb3 EQ abap_true.
            lr_fieldcat->scrtext_s = zfi_000_cl02=>mc_hkont_scrtext_s_sat_hesap.
            lr_fieldcat->scrtext_m = zfi_000_cl02=>mc_hkont_scrtext_m_sat_hesap.
            lr_fieldcat->scrtext_l = zfi_000_cl02=>mc_hkont_scrtext_l_sat_hesap.
            lr_fieldcat->reptext = zfi_000_cl02=>mc_hkont_reptext_sat_hesap.

          ENDIF.
        WHEN zfi_000_cl02=>mc_fieldname_name1.
          IF gs_range-p_rb1 EQ abap_true.
            lr_fieldcat->scrtext_s = zfi_000_cl02=>mc_name1_scrtext_s_anahesap_a.
            lr_fieldcat->scrtext_m = zfi_000_cl02=>mc_name1_scrtext_m_anahesap_a.
            lr_fieldcat->scrtext_l = zfi_000_cl02=>mc_name1_scrtext_l_anahesap_a.
            lr_fieldcat->reptext = zfi_000_cl02=>mc_name1_reptext_anahesap_a.
          ELSEIF gs_range-p_rb2 EQ abap_true.
            lr_fieldcat->scrtext_s = zfi_000_cl02=>mc_name1_scrtext_s_mushesap_a.
            lr_fieldcat->scrtext_m = zfi_000_cl02=>mc_name1_scrtext_m_mushesap_a.
            lr_fieldcat->scrtext_l = zfi_000_cl02=>mc_name1_scrtext_l_mushesap_a.
            lr_fieldcat->reptext = zfi_000_cl02=>mc_name1_reptext_mushesap_a.
          ELSEIF gs_range-p_rb3 EQ abap_true.
            lr_fieldcat->scrtext_s = zfi_000_cl02=>mc_name1_scrtext_s_sathesap_a.
            lr_fieldcat->scrtext_m = zfi_000_cl02=>mc_name1_scrtext_m_sathesap_a.
            lr_fieldcat->scrtext_l = zfi_000_cl02=>mc_name1_scrtext_l_sathesap_a.
            lr_fieldcat->reptext = zfi_000_cl02=>mc_name1_reptext_sathesap_a.

          ENDIF.
        WHEN zfi_000_cl02=>mc_fieldname_waers_blg.
*          lr_fieldcat->ref_field = space.
*          lr_fieldcat->ref_table = space.
*          lr_fieldcat->scrtext_s = space.
*          lr_fieldcat->scrtext_m = space.
*          lr_fieldcat->scrtext_l = TEXT-015.
*          lr_fieldcat->reptext = TEXT-015.
        WHEN zfi_000_cl02=>mc_fieldname_waers.
          lr_fieldcat->ref_field = space.
          lr_fieldcat->ref_table = space.
*          lr_fieldcat->scrtext_s = space.
*          lr_fieldcat->scrtext_m = space.
*          lr_fieldcat->scrtext_l = TEXT-016.
*          lr_fieldcat->reptext = TEXT-016.
        WHEN zfi_000_cl02=>mc_fieldname_xref1.
*          lr_fieldcat->ref_field = space.
*          lr_fieldcat->ref_table = space.
*          lr_fieldcat->scrtext_s = space.
*          lr_fieldcat->scrtext_m = space.
*          lr_fieldcat->scrtext_l = TEXT-017.
*          lr_fieldcat->reptext = TEXT-017.
        WHEN zfi_000_cl02=>mc_fieldname_xref2.
*          lr_fieldcat->ref_field = space.
**          lr_fieldcat->ref_table = space.
*          lr_fieldcat->scrtext_s = space.
*          lr_fieldcat->scrtext_m = space.
*          lr_fieldcat->scrtext_l = TEXT-018.
*          lr_fieldcat->reptext = TEXT-018.
        WHEN zfi_000_cl02=>mc_fieldname_line_number.
*          lr_fieldcat->ref_field = space.
*          lr_fieldcat->ref_table = space.
*          lr_fieldcat->scrtext_s = space.
*          lr_fieldcat->scrtext_m = space.
*          lr_fieldcat->scrtext_l = TEXT-025.
*          lr_fieldcat->reptext = TEXT-025.
        WHEN zfi_000_cl02=>mc_fieldname_journal_number .
*          lr_fieldcat->ref_field = space.
*          lr_fieldcat->ref_table = space.
*          lr_fieldcat->scrtext_s = space.
*          lr_fieldcat->scrtext_m = space.
*          lr_fieldcat->scrtext_l = TEXT-026.
*          lr_fieldcat->reptext = TEXT-026.
        WHEN OTHERS.
      ENDCASE.

    ENDLOOP.
    lr_fieldcat->key = space.
    MODIFY rt_fcat FROM lr_fieldcat->* TRANSPORTING key
                              WHERE key EQ abap_true.

  ENDMETHOD.


  METHOD zbc_000_if01~fill_layout.
    RS_layout-info_fNAME = zfi_000_cl02=>MC_FNAME_COLOR.
  ENDMETHOD.


  METHOD zbc_000_if01~get_datas.
    IF gs_range-p_rb4 eq abap_true.

      me->get_data_up( ).

    ELSEIF gs_range-p_rb5 eq abap_true.

      me->get_data_bp( ).
    ENDIF.

  ENDMETHOD.


  method ZBC_000_IF01~SCREEN_PAI.
     CASE iv_ucomm.
      WHEN zfi_000_cl02=>MC_IV_UCOMM_BACK OR zfi_000_cl02=>MC_IV_UCOMM_EXIT OR zfi_000_cl02=>MC_IV_UCOMM_CANCEL.
        zbc_000_if01~go_alv->free( ).
        FREE : mt_out, zbc_000_if01~go_alv.
        SET SCREEN 0.
        LEAVE SCREEN.
    ENDCASE.
  endmethod.


  METHOD zbc_000_if01~screen_pbo.
    DATA : ls_stable   TYPE lvc_s_stbl,
           lv_title    TYPE sy-title,
           lv_count(9) TYPE c.
*---

    SET PF-STATUS 'GUI100' OF PROGRAM iv_program .

    IF zbc_000_if01~go_alv IS INITIAL.
      me->zbc_000_if01~create_alv_object( ).
      me->zbc_000_if01~gv_title = sy-title.
    ELSE.
      ls_stable-row = ls_stable-col = abap_true.
      zbc_000_if01~go_alv->refresh_table_display( EXPORTING is_stable = ls_stable ).
    ENDIF.

    DESCRIBE TABLE me->mt_out LINES lv_count.
    CONDENSE lv_count.
    CONCATENATE me->zbc_000_if01~gv_title zfi_000_cl02=>mc_parantez_ac lv_count zfi_000_cl02=>mc_parantez_kapa INTO lv_title SEPARATED BY space.
    SET TITLEBAR  'TIT100' OF PROGRAM iv_program WITH lv_title.
  ENDMETHOD.


  METHOD zbc_000_if01~start_of_selection.
    zbc_000_cl01=>get_selections_from_program( IMPORTING es_range = me->gs_range ) .

    check_authorization( ).

    me->zbc_000_if01~get_datas( ).

  ENDMETHOD.


  method ANAHESAP.
    data ls_out like line of mt_out.
    me->devir_anahesap_new( ).

    me->ana_hasap_get_new( ).

    me->sort_itab( ).

    me->fill_tout( ).


  LOOP AT mt_out into ls_out WHERE   xnegp = abap_true.
*     AND
*    bukrs NOT BETWEEN '1000' AND '9000'.
    IF ls_out-dmbtrh <> 0.
      ls_out-dmbtrs =   ls_out-dmbtrh * -1.
      ls_out-dmbtrh = 0.
    ELSE.
      ls_out-dmbtrh =   ls_out-dmbtrs * -1.
      ls_out-dmbtrs = 0.
    ENDIF.
    MODIFY mt_out from ls_out.
  ENDLOOP.



  endmethod.


  METHOD anahesap_bp.
    DATA ls_out LIKE LINE OF mt_out.
    me->devir_anahesap_new( ).

    me->ana_hasap_get_bp( ).

    me->sort_itab_bp( ).

    me->fill_tout_bp( ).


*  if not s_umskz[] is initial.
*    perform fill_tout_with_odk.
*  else.
*    perform fill_tout.
*  endif.

*np ekleme
    LOOP AT mt_out INTO ls_out WHERE   xnegp = abap_true.
*      AND
*      bukrs NOT BETWEEN '1000' AND '9000'.
      IF ls_out-wrbtrh <> 0.
        ls_out-wrbtrs =   ls_out-wrbtrh * -1.
        ls_out-wrbtrh = 0.
      ELSE.
        ls_out-wrbtrh =   ls_out-wrbtrs * -1.
        ls_out-wrbtrs = 0.
      ENDIF.
      MODIFY mt_out FROM ls_out.
    ENDLOOP.

    me->list_out( ).
  ENDMETHOD.


  METHOD ana_hasap_get_bp.
    TYPES : BEGIN OF ty_zbd ,
              zbd1t TYPE bseg-zbd1t,
              zbd2t TYPE bseg-zbd2t,
              zbd3t TYPE bseg-zbd3t,
              rebzg TYPE bseg-rebzg,
              koart TYPE bseg-koart,
            END OF ty_zbd.
    DATA ls_zbd TYPE ty_zbd.
    DATA: lt_bsis TYPE TABLE OF bsis,
          ls_bsis TYPE bsis,
          ls_itab LIKE LINE OF mt_itab,
          lt_bsas TYPE TABLE OF bsas,
          ls_bsas TYPE bsas.

*-> begin - ETUNC / 26.11.2020
    IF gs_range-p_rldnr NE zfi_000_cl02=>MC_GS_RANGE_P_RLDNR_0L.
      get_acdoca( ).
      RETURN.
    ENDIF.
*<- end   - ETUNC / 26.11.2020



    SELECT * FROM bsis INTO TABLE lt_bsis
                      WHERE bukrs IN gs_range-s_bukrs
                        AND  hkont IN gs_range-s_hkont
                        AND  budat IN gs_range-s_budat
                        AND  gsber IN gs_range-s_gsber
                        AND  blart IN gs_range-s_blart
                        AND  prctr IN gs_range-s_prctr.
************************************************
    IF lt_bsis[] IS NOT INITIAL.
      SELECT bukrs, belnr, gjahr, buzei, xnegp, zbd1t, zbd2t, zbd3t, rebzg, koart, mwskz, paobjnr
         FROM epic_v_brs_bseg
         INTO TABLE @DATA(lt_bseg)
        FOR ALL ENTRIES IN @lt_bsis
       WHERE bukrs EQ @lt_bsis-bukrs
         AND belnr EQ @lt_bsis-belnr
         AND gjahr EQ @lt_bsis-gjahr.
    ENDIF.

*-> begin - AATAN / 06.12.2021
    IF NOT lt_bseg[] IS INITIAL.
      SELECT DISTINCT
        ce4c~paobjnr,
        ce4c~kmland,
        T005t~landx AS kmland_dsc,
        ce4c~ww028,
        t25b0~bezek AS ww028_dsc,
        ce4c~ww026,
        t25a8~bezek AS ww026_dsc,
        ce4c~ww027,
        t25a9~bezek AS ww027_dsc,
        ce4c~kndnr,
        kna1~name1 AS kndnr_dsc
          FROM ce4cg00_acct AS ce4c
           LEFT OUTER JOIN t005t ON t005t~spras EQ @sy-langu
                                AND t005t~land1 EQ ce4c~kmland
           LEFT OUTER JOIN t25b0 ON t25b0~spras EQ @sy-langu
                                AND t25b0~ww028 EQ ce4c~ww028
           LEFT OUTER JOIN t25a8 ON t25a8~spras EQ @sy-langu
                                AND t25a8~ww026 EQ ce4c~ww026
           LEFT OUTER JOIN t25a9 ON t25a9~spras EQ @sy-langu
                                AND t25a9~ww027 EQ ce4c~ww027
           LEFT OUTER JOIN kna1 ON kna1~kunnr = ce4c~kndnr
            FOR ALL ENTRIES IN @lt_bseg
              WHERE paobjnr EQ @lt_bseg-paobjnr
                INTO TABLE @DATA(t_ce4cdat).
      SORT t_ce4cdat BY paobjnr.
    ENDIF.
*<- end   - AATAN / 06.12.2021

*-> begin - ETUNC / 28.11.2020
*    SELECT bukrs, belnr, gjahr, vzahl FROM zfi_001_t03 INTO TABLE @DATA(lt_vzahl).
    SELECT zz1~bukrs, zz1~belnr, zz1~gjahr, zz1~vzahl
      FROM zfi_001_t03 as zz1
           INNER JOIN @lt_bsis as itab on zz1~bukrs = itab~bukrs
                                      and zz1~belnr = itab~belnr
                                      and zz1~gjahr = itab~gjahr
      INTO TABLE @DATA(lt_vzahl).
*<- end   - ETUNC / 28.11.2020

    SELECT bukrs, belnr, gjahr, vzahl FROM zfi_001_t04 INTO TABLE @DATA(lt_vzahl2).


    LOOP AT lt_bsis INTO ls_bsis.

      CLEAR ls_itab.
      MOVE-CORRESPONDING ls_bsis TO ls_itab.
      CLEAR : ls_zbd.
      READ TABLE lt_bseg INTO DATA(ls_bseg) WITH KEY bukrs = ls_bsis-bukrs
                                               belnr = ls_bsis-belnr
                                               gjahr = ls_bsis-gjahr
                                               buzei = ls_bsis-buzei.
      MOVE-CORRESPONDING ls_bseg TO ls_zbd.
*******************************************************************
*      SELECT SINGLE zbd1t zbd2t zbd3t rebzg koart xref1
*      INTO CORRESPONDING FIELDS OF ls_zbd
*      FROM bseg WHERE bukrs = ls_bsis-bukrs AND
*                      belnr = ls_bsis-belnr AND
*                      gjahr = ls_bsis-gjahr AND
*                      buzei = ls_bsis-buzei.

*-> begin - AATAN / 06.12.2021
      READ TABLE t_ce4cdat REFERENCE INTO DATA(r_ce4cdat) WITH KEY paobjnr = ls_bseg-paobjnr BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        MOVE-CORRESPONDING r_ce4cdat->* to ls_itab.
      ENDIF.
*<- end   - AATAN / 06.12.2021

      CALL FUNCTION 'NET_DUE_DATE_GET'
        EXPORTING
          i_zfbdt = ls_bsis-zfbdt
          i_zbd1t = ls_zbd-zbd1t
          i_zbd2t = ls_zbd-zbd2t
          i_zbd3t = ls_zbd-zbd3t
          i_shkzg = ls_bsis-shkzg
          i_rebzg = ls_zbd-rebzg
          i_koart = ls_zbd-koart
        IMPORTING
          e_faedt = ls_itab-faedt.

      ls_itab-saknr = ls_bsis-hkont. "neo
      MOVE: ls_bsis-hkont TO ls_itab-hkont.
      MOVE: ls_bsis-hkont TO ls_itab-ref_hkont.

      IF ls_bsis-shkzg = zfi_000_cl02=>mc_shkzg.
        MOVE: ls_bsis-wrbtr TO ls_itab-wrbtrs.
        MOVE: ls_bsis-dmbtr TO ls_itab-dmbtrs. "HY
      ELSE.
        MOVE: ls_bsis-wrbtr TO ls_itab-wrbtrh.
        MOVE: ls_bsis-dmbtr TO ls_itab-dmbtrh. "HY
      ENDIF.
**np ekleme
      CLEAR ls_bseg.
      READ TABLE lt_bseg INTO ls_bseg WITH KEY gjahr = ls_bsis-gjahr
                                         bukrs = ls_bsis-bukrs
                                         belnr = ls_bsis-belnr
                                         buzei = ls_bsis-buzei.
*********************************************************************
*      SELECT SINGLE * FROM bseg INTO ls_bseg
*                WHERE gjahr = ls_bsis-gjahr
*                  AND bukrs = ls_bsis-bukrs
*                  AND belnr = ls_bsis-belnr
*                  AND buzei = ls_bsis-buzei.
      ls_itab-xnegp = ls_bseg-xnegp.
      me->get_material(
          EXPORTING
            bukrs = ls_bsis-bukrs                " Şirket kodu
            belnr = ls_bsis-belnr                 " Kalem no.tayini: Malzeme belgesi - satınalma belgesi
            gjahr = ls_bsis-gjahr                 " Mali yıl
            buzei = ls_bsis-buzei                 " Muhasebe belgesi içindeki kayıt satırının numarası
        ).
      CHECK sy-subrc EQ 0.
      READ TABLE lt_vzahl INTO DATA(ls_vzahl) WITH KEY bukrs = ls_itab-bukrs
                                                 belnr = ls_itab-belnr
                                                 gjahr = ls_itab-gjahr.
      ls_itab-vzahl = ls_vzahl-vzahl.
*      SELECT SINGLE vzahl
*             FROM zfi_001_t03
*              INTO ls_itab-vzahl
*          WHERE bukrs = ls_itab-bukrs
*              AND belnr = ls_itab-belnr
*              AND gjahr = ls_itab-gjahr.

      APPEND ls_itab TO mt_itab.

    ENDLOOP.

    SELECT * FROM bsas INTO TABLE lt_bsas
                       WHERE bukrs IN gs_range-s_bukrs
                        AND  hkont IN gs_range-s_hkont
                        AND  budat IN gs_range-s_budat
                        AND  gsber IN gs_range-s_gsber
                        AND  blart IN gs_range-s_blart
                        AND  prctr IN gs_range-s_prctr.
    LOOP AT lt_bsas INTO ls_bsas.


      CLEAR ls_itab.
      MOVE-CORRESPONDING ls_bsas TO ls_itab.
      CLEAR : ls_zbd,ls_bseg.

      READ TABLE lt_bseg INTO ls_bseg WITH KEY bukrs = ls_bsas-bukrs
                                               belnr = ls_bsas-belnr
                                               gjahr = ls_bsas-gjahr
                                               buzei = ls_bsas-buzei.
      MOVE-CORRESPONDING ls_bseg TO ls_zbd.
********************************************************************
*      SELECT SINGLE zbd1t zbd2t zbd3t rebzg koart xref1
*      INTO CORRESPONDING FIELDS OF ls_zbd
*      FROM bseg WHERE bukrs = ls_bsas-bukrs AND
*                      belnr = ls_bsas-belnr AND
*                      gjahr = ls_bsas-gjahr AND
*                      buzei = ls_bsas-buzei.

      CALL FUNCTION 'NET_DUE_DATE_GET'
        EXPORTING
          i_zfbdt = ls_bsas-zfbdt
          i_zbd1t = ls_zbd-zbd1t
          i_zbd2t = ls_zbd-zbd2t
          i_zbd3t = ls_zbd-zbd3t
          i_shkzg = ls_bsas-shkzg
          i_rebzg = ls_zbd-rebzg
          i_koart = ls_zbd-koart
        IMPORTING
          e_faedt = ls_itab-faedt.



      ls_itab-saknr = ls_bsis-hkont. "neo
      MOVE: ls_bsas-hkont TO ls_itab-hkont.
      MOVE: ls_bsas-hkont TO ls_itab-ref_hkont.

      IF ls_bsas-shkzg = zfi_000_cl02=>mc_shkzg.
        MOVE: ls_bsas-wrbtr TO ls_itab-wrbtrs.
        MOVE: ls_bsas-dmbtr TO ls_itab-dmbtrs. "HY
      ELSE.
        MOVE: ls_bsas-wrbtr TO ls_itab-wrbtrh.
        MOVE: ls_bsas-dmbtr TO ls_itab-dmbtrh. "HY
      ENDIF.
**np ekleme
      CLEAR ls_bseg.
      READ TABLE lt_bseg INTO ls_bseg WITH KEY gjahr = ls_bsas-gjahr
                                          bukrs = ls_bsas-bukrs
                                          belnr = ls_bsas-belnr
                                          buzei = ls_bsas-buzei.
********************************************************************
*      SELECT SINGLE * FROM bseg INTO ls_bseg
*                WHERE gjahr = ls_bsas-gjahr
*                  AND bukrs = ls_bsas-bukrs
*                  AND belnr = ls_bsas-belnr
*                  AND buzei = ls_bsas-buzei.
      ls_itab-xnegp = ls_bseg-xnegp.

      me->get_material(
             EXPORTING
               bukrs = ls_bsas-bukrs                " Şirket kodu
               belnr = ls_bsas-belnr                 " Kalem no.tayini: Malzeme belgesi - satınalma belgesi
               gjahr = ls_bsas-gjahr                 " Mali yıl
               buzei = ls_bsas-buzei                 " Muhasebe belgesi içindeki kayıt satırının numarası
           ).
      CHECK sy-subrc EQ 0.
      CLEAR ls_vzahl.
      READ TABLE lt_vzahl INTO ls_vzahl WITH KEY bukrs = ls_itab-bukrs
                                                 belnr = ls_itab-belnr
                                                 gjahr = ls_itab-gjahr.
      ls_itab-vzahl = ls_vzahl-vzahl.
*********************************************************************
*      SELECT SINGLE vzahl
*             FROM zfi_001_t03
*              INTO ls_itab-vzahl
*          WHERE bukrs = ls_itab-bukrs
*              AND belnr = ls_itab-belnr
*              AND gjahr = ls_itab-gjahr.
      IF sy-subrc NE 0.

        READ TABLE lt_vzahl2 INTO DATA(ls_vzahl2) WITH KEY bukrs = ls_itab-bukrs
                                                   belnr = ls_itab-belnr
                                                   gjahr = ls_itab-gjahr.
        ls_itab-vzahl = ls_vzahl2-vzahl.
*********************************************************************
*        SELECT SINGLE vzahl
*               FROM zfi_001_t04
*                INTO ls_itab-vzahl
*            WHERE bukrs = ls_itab-bukrs
*                AND belnr = ls_itab-belnr
*                AND gjahr = ls_itab-gjahr
*                AND BUZEI = ls_itab-buzei.

      ENDIF.

      APPEND ls_itab TO mt_itab.

    ENDLOOP.



  ENDMETHOD.


  METHOD ana_hasap_get_new.

*-> begin - ETUNC / 26.11.2020
    IF gs_range-p_rldnr NE zfi_000_cl02=>mc_gs_range_p_rldnr_0l.
      get_acdoca( ).
      RETURN.
    ENDIF.
*<- end   - ETUNC / 26.11.2020


    DATA : lt_bsis TYPE TABLE OF bsis,
           ls_bsis TYPE bsis,
           lt_bsas TYPE TABLE OF bsas,
           ls_bsas TYPE bsas.
    DATA ls_itab LIKE LINE OF mt_itab.

    CLEAR :lt_bsis,lt_bsas.
    REFRESH :lt_bsis[],lt_bsas[].

    SELECT * INTO TABLE lt_bsis
      FROM bsis
     WHERE bukrs IN gs_range-s_bukrs
       AND hkont IN gs_range-s_hkont
       AND budat IN gs_range-s_budat
       AND gsber IN gs_range-s_gsber
       AND blart IN gs_range-s_blart
       AND prctr IN gs_range-s_prctr.
************************************************
    IF lt_bsis[] IS NOT INITIAL.

      SELECT bukrs, belnr, gjahr, buzei, xnegp, zbd1t, zfbdt, shkzg,
             zbd2t, zbd3t, rebzg, koart, xref1, xref2, augbl, augdt, mwskz, paobjnr
       FROM epic_v_brs_bseg
       INTO TABLE @DATA(lt_bseg)
        FOR ALL ENTRIES IN @lt_bsis
       WHERE bukrs EQ @lt_bsis-bukrs
              AND belnr EQ @lt_bsis-belnr
              AND gjahr EQ @lt_bsis-gjahr.

    ENDIF.
*-> begin - ETUNC / 28.11.2020
*    SELECT bukrs, belnr, gjahr, vzahl FROM zfi_001_t03 INTO TABLE @DATA(lt_vzahl).
    SELECT zz1~bukrs, zz1~belnr, zz1~gjahr, zz1~vzahl
      FROM zfi_001_t03 AS zz1
           INNER JOIN @lt_bsis AS itab ON zz1~bukrs = itab~bukrs
                                      AND zz1~belnr = itab~belnr
                                      AND zz1~gjahr = itab~gjahr
      INTO TABLE @DATA(lt_vzahl).
*<- end   - ETUNC / 28.11.2020

*-> begin - AATAN / 06.12.2021
    IF NOT lt_bseg[] IS INITIAL.
      SELECT DISTINCT
        ce4c~paobjnr,
        ce4c~kmland,
        T005t~landx AS kmland_dsc,
        ce4c~ww028,
        t25b0~bezek AS ww028_dsc,
        ce4c~ww026,
        t25a8~bezek AS ww026_dsc,
        ce4c~ww027,
        t25a9~bezek AS ww027_dsc,
        ce4c~kndnr,
        kna1~name1 AS kndnr_dsc
          FROM ce4cg00_acct AS ce4c
           LEFT OUTER JOIN t005t ON t005t~spras EQ @sy-langu
                                AND t005t~land1 EQ ce4c~kmland
           LEFT OUTER JOIN t25b0 ON t25b0~spras EQ @sy-langu
                                AND t25b0~ww028 EQ ce4c~ww028
           LEFT OUTER JOIN t25a8 ON t25a8~spras EQ @sy-langu
                                AND t25a8~ww026 EQ ce4c~ww026
           LEFT OUTER JOIN t25a9 ON t25a9~spras EQ @sy-langu
                                AND t25a9~ww027 EQ ce4c~ww027
           LEFT OUTER JOIN kna1 ON kna1~kunnr = ce4c~kndnr
            FOR ALL ENTRIES IN @lt_bseg
              WHERE paobjnr EQ @lt_bseg-paobjnr
                INTO TABLE @DATA(t_ce4cdat).
      SORT t_ce4cdat BY paobjnr.
    ENDIF.
*<- end   - AATAN / 06.12.2021

    LOOP AT lt_bsis INTO ls_bsis.
      CLEAR ls_itab.
      MOVE-CORRESPONDING ls_bsis TO ls_itab.

      READ TABLE lt_bseg INTO DATA(ls_bseg) WITH KEY gjahr = ls_bsis-gjahr
                                                     bukrs = ls_bsis-bukrs
                                                     belnr = ls_bsis-belnr
                                                     buzei = ls_bsis-buzei.
*************************************************************************
*      SELECT SINGLE * FROM bseg INTO ls_bseg
*                  WHERE gjahr = ls_bsis-gjahr
*                    AND bukrs = ls_bsis-bukrs
*                    AND belnr = ls_bsis-belnr
*                    AND buzei = ls_bsis-buzei.

      ls_itab-xref1 = ls_bseg-xref1.
      ls_itab-xref2 = ls_bseg-xref2.

      ls_itab-xnegp = ls_bseg-xnegp.

      ls_itab-augbl = ls_bseg-augbl.
      ls_itab-augdt = ls_bseg-augdt.

*-> begin - AATAN / 06.12.2021
      ls_itab-mwskz = ls_bseg-mwskz.
      READ TABLE t_ce4cdat REFERENCE INTO DATA(r_ce4cdat) WITH KEY paobjnr = ls_bseg-paobjnr BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        MOVE-CORRESPONDING r_ce4cdat->* to ls_itab.
      ENDIF.
*<- end   - AATAN / 06.12.2021

      me->get_baslik_bilgileri(
        EXPORTING
          bukrs    = ls_bseg-bukrs     " Şirket kodu
          belnr    = ls_bseg-belnr     " Kalem no.tayini: Malzeme belgesi - satınalma belgesi
          gjahr    = ls_bseg-gjahr     " Mali yıl
        CHANGING
          stblg    = ls_itab-stblg     " Ters kayıt belge numarası
          xref1_hd = ls_itab-xref1_hd  " Belge başlığına ilişkin referans anahtar_1 (dahili)
          bktxt    = ls_itab-bktxt     " Belge başlığı metni
      ).

      me->net_vade_tarihi(
        EXPORTING
          zfbdt = ls_bseg-zfbdt               " Vade hesaplamasına temel oluşturan tarih
          zbd1t = ls_bseg-zbd1t                 " Nakit indirimi günleri 1
          zbd2t = ls_bseg-zbd2t                 " Nakit indirimi günleri 2
          zbd3t = ls_bseg-zbd3t                " Net ödeme koşullarına ilişkin süre
          shkzg = ls_bseg-shkzg                 " Borç/alacak göstergesi
          rebzg = ls_bseg-rebzg                 " İşlemin ait olduğu faturanın belge numarası
          koart = ls_bseg-koart                 " Hesap türü
          bldat = ls_bsis-bldat                 " Belge tarihi
        CHANGING
          faedt = ls_itab-faedt                 " Net ödeme vadesi
      ).

      ls_itab-saknr = ls_bsis-hkont. "neo
      MOVE: ls_bsis-hkont TO ls_itab-hkont.
      MOVE: ls_bsis-hkont TO ls_itab-ref_hkont.

      IF ls_bsis-shkzg = zfi_000_cl02=>mc_shkzg.
        MOVE: ls_bsis-wrbtr TO ls_itab-wrbtrs.
        MOVE: ls_bsis-dmbtr TO ls_itab-dmbtrs. "HY
      ELSE.
        MOVE: ls_bsis-wrbtr TO ls_itab-wrbtrh.
        MOVE: ls_bsis-dmbtr TO ls_itab-dmbtrh.
      ENDIF.

      me->get_material(
           EXPORTING
             bukrs = ls_bsis-bukrs   " Şirket kodu
             belnr = ls_bsis-belnr   " Kalem no.tayini: Malzeme belgesi - satınalma belgesi
             gjahr = ls_bsis-gjahr   " Mali yıl
             buzei = ls_bsis-buzei   " Muhasebe belgesi içindeki kayıt satırının numarası
         ).

      CHECK sy-subrc EQ 0.

      READ TABLE lt_vzahl INTO DATA(ls_vzahl) WITH KEY bukrs = ls_itab-bukrs
                                                       belnr = ls_itab-belnr
                                                       gjahr = ls_itab-gjahr.
      ls_itab-vzahl = ls_vzahl-vzahl.
*********************************************************************************
*      SELECT SINGLE vzahl
*             FROM zfi_001_t03
*              INTO ls_itab-vzahl
*          WHERE bukrs = ls_itab-bukrs
*              AND belnr = ls_itab-belnr
*              AND gjahr = ls_itab-gjahr.

      APPEND ls_itab TO mt_itab.
    ENDLOOP.


    SELECT * INTO TABLE lt_bsas
      FROM bsas
     WHERE bukrs IN gs_range-s_bukrs
       AND hkont IN gs_range-s_hkont
       AND budat IN gs_range-s_budat
       AND gsber IN gs_range-s_gsber
       AND blart IN gs_range-s_blart
       AND prctr IN gs_range-s_prctr.

    LOOP AT lt_bsas INTO ls_bsas.
      CLEAR ls_itab.
      MOVE-CORRESPONDING ls_bsas TO ls_itab.
      CLEAR ls_bseg.
      READ TABLE lt_bseg INTO ls_bseg WITH KEY gjahr = ls_bsas-gjahr
                                               bukrs = ls_bsas-bukrs
                                               belnr = ls_bsas-belnr
                                               buzei = ls_bsas-buzei.
*************************************************************************

*      SELECT SINGLE * FROM bseg INTO ls_bseg
*                  WHERE gjahr = ls_bsas-gjahr
*                    AND bukrs = ls_bsas-bukrs
*                    AND belnr = ls_bsas-belnr
*                    AND buzei = ls_bsas-buzei.

      ls_itab-xref1 = ls_bseg-xref1.
      ls_itab-xref2 = ls_bseg-xref2.
      ls_itab-xnegp = ls_bseg-xnegp.
      ls_itab-augbl = ls_bseg-augbl.
      ls_itab-augdt = ls_bseg-augdt.

      me->get_baslik_bilgileri(
        EXPORTING
          bukrs    = ls_bseg-bukrs     " Şirket kodu
          belnr    = ls_bseg-belnr     " Kalem no.tayini: Malzeme belgesi - satınalma belgesi
          gjahr    = ls_bseg-gjahr     " Mali yıl
        CHANGING
          stblg    = ls_itab-stblg     " Ters kayıt belge numarası
          xref1_hd = ls_itab-xref1_hd  " Belge başlığına ilişkin referans anahtar_1 (dahili)
          bktxt    = ls_itab-bktxt     " Belge başlığı metni
      ).



      me->net_vade_tarihi(
        EXPORTING
          zfbdt = ls_bseg-zfbdt               " Vade hesaplamasına temel oluşturan tarih
          zbd1t = ls_bseg-zbd1t                 " Nakit indirimi günleri 1
          zbd2t = ls_bseg-zbd2t                 " Nakit indirimi günleri 2
          zbd3t = ls_bseg-zbd3t                " Net ödeme koşullarına ilişkin süre
          shkzg = ls_bseg-shkzg                 " Borç/alacak göstergesi
          rebzg = ls_bseg-rebzg                 " İşlemin ait olduğu faturanın belge numarası
          koart = ls_bseg-koart                 " Hesap türü
          bldat = ls_bsis-bldat                 " Belge tarihi
        CHANGING
          faedt = ls_itab-faedt                 " Net ödeme vadesi
      ).


      ls_itab-saknr = ls_bsas-hkont.
      MOVE: ls_bsas-hkont TO ls_itab-hkont.
      MOVE: ls_bsas-hkont TO ls_itab-ref_hkont.

      IF ls_bsas-shkzg = zfi_000_cl02=>mc_shkzg.
        MOVE: ls_bsas-wrbtr TO ls_itab-wrbtrs.
        MOVE: ls_bsas-dmbtr TO ls_itab-dmbtrs.
      ELSE.
        MOVE: ls_bsas-wrbtr TO ls_itab-wrbtrh.
        MOVE: ls_bsas-dmbtr TO ls_itab-dmbtrh.
      ENDIF.

      me->get_material(
           EXPORTING
             bukrs = ls_bsis-bukrs                " Şirket kodu
             belnr = ls_bsis-belnr                 " Kalem no.tayini: Malzeme belgesi - satınalma belgesi
             gjahr = ls_bsis-gjahr                 " Mali yıl
             buzei = ls_bsis-buzei                 " Muhasebe belgesi içindeki kayıt satırının numarası
         ).

      CHECK sy-subrc EQ 0.

      CLEAR ls_vzahl.
      READ TABLE lt_vzahl INTO ls_vzahl WITH KEY bukrs = ls_itab-bukrs
                                                       belnr = ls_itab-belnr
                                                       gjahr = ls_itab-gjahr.
      ls_itab-vzahl = ls_vzahl-vzahl.
***************************************************************************
*      SELECT SINGLE vzahl
*             FROM zfi_001_t03
*              INTO ls_itab-vzahl
*          WHERE bukrs = ls_itab-bukrs
*              AND belnr = ls_itab-belnr
*              AND gjahr = ls_itab-gjahr.

      APPEND ls_itab TO mt_itab.
    ENDLOOP.

  ENDMETHOD.


  METHOD authority_check.
*     TYPES : BEGIN OF ty_buk ,
*         bukrs TYPE lfb1-bukrs,
*       END OF ty_buk.
*  TYPES : BEGIN OF ty_lif,
*         lifnr TYPE lfa1-lifnr,
*         begru TYPE lfa1-begru,
*       END OF ty_lif.
*  TYPES : BEGIN OF ty_hkon ,
*         saknr TYPE skb1-saknr,
*         begru TYPE skb1-begru,
*  END OF ty_hkon.
*  DATA: sy_tab LIKE sy-tabix.
*  DATA: ls_begru TYPE lfb1-begru.
*  DATA ls_bukrs LIKE LINE OF gs_range-s_bukrs.
*  DATA ls_lifnr LIKE LINE OF gs_range-s_lifnr.
*  DATA ls_shkont LIKE LINE OF gs_range-s_hkont.
*
*  DATA ls_buk TYPE ty_buk.
*  DATA ls_lif TYPE ty_lif.
*  DATA ls_hkon TYPE ty_hkon.
******* BUKRS
*  SELECT bukrs INTO TABLE mt_buk
*                FROM t001
*              WHERE bukrs IN gs_range-s_bukr.
*  LOOP AT mt_buk INTO ls_buk.
*    sy_tab = sy-tabix.
*    AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
*            ID 'BUKRS' FIELD ls_buk-bukrs.
*    IF sy-subrc NE 0.
**MESSAGE i899(mm) WITH buk-bukrs text-026.
*      MESSAGE i115(gc).
*      DELETE mt_buk INDEX sy_tab.
*    ENDIF.
*  ENDLOOP.
*  DESCRIBE TABLE mt_buk LINES sy-index.
*  IF sy-index EQ 0.
**   MESSAGE i899(mm) WITH 'Seçim ekranındaki' 'şirketlerde'
**                             'yetkiniz yok!'.
**MESSAGE i899(mm)WITH Text-005.
*    MESSAGE i007(fn).
*
*  ENDIF.
*  READ TABLE gs_range-s_bukrs INTO ls_bukrs INDEX 1.
*  LOOP AT mt_buk INTO ls_buk.
*    ls_bukrs-low = ls_buk-bukrs.
*    ls_bukrs-sign = 'I'.
*    ls_bukrs-option = 'EQ'.
*    CLEAR ls_bukrs-high.
*    APPEND ls_bukrs TO gs_range-s_bukrs.
*    CLEAR ls_bukrs.
*  ENDLOOP.
*
******* LIFNR
*  IF gs_range-p_rb3 = 'X'. "sy-tcode = 'ZFI013K' or sy-tcode = 'ZFI013'.
*    SELECT lifnr begru INTO TABLE mt_lif
*                  FROM lfa1
*                WHERE lifnr IN gs_range-s_lifn.
*    LOOP AT mt_lif into ls_lif.
*      sy_tab = sy-tabix.
*      CHECK NOT ls_lif-begru IS INITIAL.
*      AUTHORITY-CHECK OBJECT 'F_BKPF_BEK'
*         ID 'BRGRU' FIELD ls_lif-begru
*         ID 'ACTVT' FIELD '03'.
*      IF sy-subrc NE 0.
**        MESSAGE i115(gc).
**        MESSAGE '& Satıcısına yetkiniz yok' type 'I'.
*        DELETE mt_lif INDEX sy_tab.
*      ENDIF.
*    ENDLOOP.
*
*    LOOP AT mt_lif into ls_lif.
*      sy_tab = sy-tabix.
*      LOOP AT mt_buk into ls_buk.
*        CLEAR : ls_begru.
*        SELECT SINGLE begru INTO ls_begru FROM lfb1
*        WHERE bukrs = ls_buk-bukrs AND lifnr = ls_lif-lifnr.
*        CHECK NOT ls_begru IS INITIAL.
*        AUTHORITY-CHECK OBJECT 'F_BKPF_BEK'
*               ID 'BRGRU' FIELD ls_begru
*               ID 'ACTVT' FIELD '03'.
*        IF sy-subrc NE 0.
**            MESSAGE '& Satıcısına yetkiniz yok' type 'I' .
*          DELETE mt_lif INDEX sy_tab.
*          CONTINUE.
*        ENDIF.
*      ENDLOOP.
*    ENDLOOP.
*    DESCRIBE TABLE mt_lif LINES sy-index.
*    IF sy-index EQ 0.
**     MESSAGE i007(FN).
**     message 'Satıcıları görüntüleme yetkiniz yok' type 'I'.
*
*    ENDIF.
*    READ TABLE gs_range-s_lifnr into ls_lifnr index 1.
*    LOOP AT mt_lif into ls_lif.
*      ls_lifnr-low = ls_lif-lifnr.
*      ls_lifnr-sign = 'I'.
*      ls_lifnr-option = 'EQ'.
*      CLEAR ls_lifnr-high.
*      APPEND ls_lifnr to gs_range-s_lifnr.
*      CLEAR ls_lifnr.
*    ENDLOOP.
*  ENDIF.
*
*  "***********
*  IF gs_range-p_rb1 = 'X'. "sy-tcode = 'ZFI013S' or sy-tcode = 'ZFI013'.
*    SELECT saknr begru INTO TABLE mt_hkon
*      FROM skb1
*      WHERE saknr IN gs_range-s_hkon AND bukrs IN gs_range-s_bukrs.
*    LOOP AT mt_hkon into ls_hkon.
*      sy_tab = sy-tabix.
*      CHECK NOT ls_hkon-begru IS INITIAL.
*      AUTHORITY-CHECK OBJECT 'F_SKA1_BES'
*         ID 'BRGRU' FIELD ls_hkon-begru
*         ID 'ACTVT' FIELD '03'.
*      IF sy-subrc NE 0.
*        DELETE mt_hkon INDEX sy_tab.
*      ENDIF.
*    ENDLOOP.
*    read table gs_range-s_hkont into ls_shkont index 1.
*    LOOP AT mt_hkon into ls_hkon.
*      ls_shkont-low = ls_hkon-saknr.
*      ls_shkont-sign = 'I'.
*      ls_shkont-option = 'EQ'.
*      CLEAR ls_shkont-high.
*      APPEND ls_shkont to gs_range-s_hkont.
*      CLEAR ls_shkont.
*    ENDLOOP.
*  ENDIF.
  ENDMETHOD.


  method CONSTRUCTOR.
    me->zbc_000_if01~gv_struc_name = zfi_000_cl02=>MC_GV_STRUCTURE_NAME_ZFI001S04.
  endmethod.


  METHOD devir_anahesap_new.

    DATA : lt_bsis TYPE TABLE OF bsis,
           ls_bsis TYPE  bsis,
           lt_bsas TYPE TABLE OF bsas,
           ls_bsas TYPE bsas.

    DATA ls_DEVIR LIKE LINE OF mt_devir.
    DATA: lv_keydt TYPE bkpf-budat.
    DATA: r_budat TYPE RANGE OF bkpf-budat.
    DATA: rx_budat TYPE LINE OF datum_range_tab .

    CLEAR : ls_bsis,ls_bsas.
    REFRESH :lt_bsis[],lt_bsas[].

    CHECK gs_range-s_budat IS NOT INITIAL.

    r_budat[] = gs_range-s_budat .

    LOOP AT r_budat INTO rx_budat.
      lv_keydt = rx_budat-low .
      EXIT.
    ENDLOOP.

    lv_keydt = lv_keydt - 1 .

*-> begin - ETUNC / 26.11.2020
    IF GS_RANGE-p_rldnr NE zfi_000_cl02=>MC_GS_RANGE_P_RLDNR_0L.
      GET_ACDOCA_DEVIR( LV_KEYDT ).
      RETURN.
    ENDIF.
*<- end   - ETUNC / 26.11.2020


    SELECT * INTO TABLE lt_bsis
      FROM bsis
     WHERE bukrs IN gs_range-s_bukrs
       AND hkont IN gs_range-s_hkont
       AND budat LE lv_keydt
       AND gsber IN gs_range-s_gsber
       AND blart IN gs_range-s_blart
       AND prctr IN gs_range-s_prctr.

    SELECT * APPENDING TABLE lt_bsis
       FROM bsas
      WHERE bukrs IN gs_range-s_bukrs
        AND hkont IN gs_range-s_hkont
        AND budat LE lv_keydt
        AND augdt GT lv_keydt
        AND gsber IN gs_range-s_gsber
        AND blart IN gs_range-s_blart
        AND prctr IN gs_range-s_prctr.

    LOOP AT lt_bsis INTO ls_bsis.
      CLEAR: ls_DEVIR.
      MOVE: ls_bsis-bukrs TO ls_DEVIR-bukrs,
            ls_bsis-hkont TO ls_DEVIR-hkont.
       if gs_range-p_rb5 eq abap_true.
       move     ls_bsis-waers TO ls_DEVIR-waers.
       endif.

      IF ls_bsis-shkzg = zfi_000_cl02=>mc_shkzg.
        MOVE: ls_bsis-wrbtr TO ls_DEVIR-wrbtrs.
        MOVE: ls_bsis-dmbtr TO ls_DEVIR-dmbtrs.
      ELSE.
        MOVE: ls_bsis-wrbtr TO ls_DEVIR-wrbtrh.
        MOVE: ls_bsis-dmbtr TO ls_DEVIR-dmbtrh.
      ENDIF.
      me->get_material(
        EXPORTING
          bukrs = ls_bsis-bukrs                " Şirket kodu
          belnr = ls_bsis-belnr                 " Kalem no.tayini: Malzeme belgesi - satınalma belgesi
          gjahr = ls_bsis-gjahr                 " Mali yıl
          buzei = ls_bsis-buzei                 " Muhasebe belgesi içindeki kayıt satırının numarası
      ).

      CHECK sy-subrc EQ 0.

      COLLECT ls_DEVIR INTO mt_devir.
    ENDLOOP.

*    SELECT * INTO TABLE lt_bsas
*      FROM bsas
*     WHERE bukrs IN gs_range-s_bukrs
*       AND hkont IN gs_range-s_hkont
*       AND budat IN gs_range-s_budat
*       AND gsber IN gs_range-s_gsber
*       AND blart IN gs_range-s_blart
*       AND prctr IN gs_range-s_prctr.
*
*    LOOP AT lt_bsas INTO ls_bsas.
*      CLEAR: ls_devir.
*      MOVE: ls_bsas-bukrs TO ls_DEVIR-bukrs,
*            ls_bsas-hkont TO ls_DEVIR-hkont,
*            ls_bsas-waers TO ls_DEVIR-waers.
*
*      IF ls_bsas-shkzg = c_shkzg.
*        MOVE: ls_bsas-wrbtr TO ls_DEVIR-wrbtrs.
*        MOVE: ls_bsas-dmbtr TO ls_DEVIR-dmbtrs.
*
*      ELSE.
*        MOVE: ls_bsas-wrbtr TO ls_DEVIR-wrbtrh.
*        MOVE: ls_bsas-dmbtr TO ls_DEVIR-dmbtrh.
*      ENDIF.
*      me->get_material(
*         EXPORTING
*           bukrs = ls_bsis-bukrs      " Şirket kodu
*           belnr = ls_bsis-belnr      " Kalem no.tayini: Malzeme belgesi - satınalma belgesi
*           gjahr = ls_bsis-gjahr      " Mali yıl
*           buzei = ls_bsis-buzei      " Muhasebe belgesi içindeki kayıt satırının numarası
*       ).
*
*      CHECK sy-subrc EQ 0.
*
*      COLLECT ls_DEVIR INTO mt_devir.
*    ENDLOOP.

  ENDMETHOD.


  METHOD devir_musteri_bp.

        DATA : lt_bsid TYPE TABLE OF bsid,
           ls_bsid TYPE bsid,
           lt_bsad TYPE TABLE OF bsad,
           ls_bsad TYPE bsad.

    DATA ls_devir LIKE LINE OF mt_devir.
    DATA: lv_keydt TYPE bkpf-budat.
    DATA: r_budat TYPE RANGE OF bkpf-budat.
    DATA: rx_budat TYPE LINE OF datum_range_tab .
    CLEAR : ls_bsid,lt_bsad.
    REFRESH :lt_bsid[],lt_bsad[].



    CHECK gs_range-s_budat IS NOT INITIAL.


    r_budat[] = gs_range-s_budat .

    LOOP AT r_budat INTO rx_budat.
      lv_keydt = rx_budat-low .
      EXIT.
    ENDLOOP.

    lv_keydt = lv_keydt - 1 .

    SELECT * INTO TABLE lt_bsid
      FROM bsid
     WHERE bukrs IN gs_range-s_bukrs
       AND kunnr IN gs_range-s_hkont
       AND budat LE lv_keydt
       AND gsber IN gs_range-s_gsber
       AND blart IN gs_range-s_blart
       AND prctr IN gs_range-s_prctr
       AND umskz IN gs_range-s_umskz.

    SELECT * APPENDING TABLE lt_bsid
      FROM bsad
     WHERE bukrs IN gs_range-s_bukrs
       AND kunnr IN gs_range-s_hkont
       AND budat LE lv_keydt
       AND augdt GT lv_keydt
       AND gsber IN gs_range-s_gsber
       AND blart IN gs_range-s_blart
       AND prctr IN gs_range-s_prctr
       AND umskz IN gs_range-s_umskz.

    LOOP AT lt_bsid INTO ls_bsid.
      CLEAR: ls_devir.
      IF gs_range-c_cust = abap_true.
        SELECT SINGLE konzs FROM kna1
          INTO ls_devir-hkont
          WHERE kunnr = ls_bsid-kunnr.
      ELSE.
        MOVE: ls_bsid-kunnr TO ls_devir-hkont.
      ENDIF.
      MOVE: ls_bsid-bukrs TO ls_devir-bukrs,
            ls_bsid-waers TO ls_devir-waers.

      IF ls_bsid-shkzg = zfi_000_cl02=>mc_shkzg.
        MOVE: ls_bsid-wrbtr TO ls_devir-wrbtrs.
        MOVE: ls_bsid-dmbtr TO ls_devir-dmbtrs. "HY
      ELSE.
        MOVE: ls_bsid-wrbtr TO ls_devir-wrbtrh.
        MOVE: ls_bsid-dmbtr TO ls_devir-dmbtrh. "HY
      ENDIF.
      IF NOT gs_Range-s_umskz IS INITIAL.
        ls_devir-umskz = ls_bsid-umskz.
      ENDIF.
  me->get_material(
        EXPORTING
          bukrs = ls_bsid-bukrs       " Şirket kodu
          belnr = ls_bsid-belnr       " Kalem no.tayini: Malzeme belgesi - satınalma belgesi
          gjahr = ls_bsid-gjahr       " Mali yıl
          buzei = ls_bsid-buzei       " Muhasebe belgesi içindeki kayıt satırının numarası
      ).


      CHECK sy-subrc EQ 0.

      COLLECT ls_devir INTO mt_devir.
    ENDLOOP.

*
*    DATA: lt_bsid TYPE TABLE OF bsid,
*          ls_bsid TYPE bsid,
*          lt_bsad TYPE TABLE OF bsad,
*          ls_bsad TYPE bsad.
*    DATA ls_devir LIKE LINE OF mt_devir.
*
*    SELECT * FROM bsid INTO TABLE lt_bsid
*                       WHERE bukrs IN gs_range-s_bukrs
*                        AND  kunnr IN gs_range-s_hkont
*                        AND  budat IN gs_range-s_budat
*                        AND  gsber IN gs_range-s_gsber
*                        AND  blart IN gs_range-s_blart
*                        AND  prctr IN gs_range-s_prctr
*                        AND  umskz IN gs_range-s_umskz.
*    LOOP AT lt_bsid INTO ls_bsid.
*      CLEAR: ls_devir.
*      IF gs_range-c_cust = abap_true.
*        SELECT SINGLE konzs FROM kna1
*          INTO ls_devir-hkont
*          WHERE kunnr = ls_bsid-kunnr.
*      ELSE.
*        MOVE: ls_bsid-kunnr TO ls_devir-hkont.
*      ENDIF.
*      MOVE: ls_bsid-bukrs TO ls_devir-bukrs,
*            ls_bsid-waers TO ls_devir-waers.
*
*      IF ls_bsid-shkzg = c_shkzg.
*        MOVE: ls_bsid-wrbtr TO ls_devir-wrbtrs.
*        MOVE: ls_bsid-dmbtr TO ls_devir-dmbtrs. "HY
*      ELSE.
*        MOVE: ls_bsid-wrbtr TO ls_devir-wrbtrh.
*        MOVE: ls_bsid-dmbtr TO ls_devir-dmbtrh. "HY
*      ENDIF.
*      IF NOT gs_Range-s_umskz IS INITIAL.
*        ls_devir-umskz = ls_bsid-umskz.
*      ENDIF.
*  me->get_material(
*        EXPORTING
*          bukrs = ls_bsid-bukrs       " Şirket kodu
*          belnr = ls_bsid-belnr       " Kalem no.tayini: Malzeme belgesi - satınalma belgesi
*          gjahr = ls_bsid-gjahr       " Mali yıl
*          buzei = ls_bsid-buzei       " Muhasebe belgesi içindeki kayıt satırının numarası
*      ).
*
*
*      CHECK sy-subrc EQ 0.
*
*      COLLECT ls_devir INTO mt_devir.
*    ENDLOOP.
*
*    SELECT * FROM bsad INTO table lt_bsad
*                     WHERE bukrs IN gs_range-s_bukrs
*                        AND  kunnr IN gs_range-s_hkont
*                        AND  budat IN gs_range-s_budat
*                        AND  gsber IN gs_range-s_gsber
*                        AND  blart IN gs_range-s_blart
*                        AND  prctr IN gs_range-s_prctr
*                        AND  umskz IN gs_range-s_umskz.
*      LOOP AT lt_bsad into ls_bsad.
*        CLEAR: ls_devir.
*        IF gs_range-c_cust = abap_true.
*          SELECT SINGLE konzs FROM kna1
*            INTO ls_devir-hkont
*            WHERE kunnr = ls_bsad-kunnr.
*        ELSE.
*          MOVE: ls_bsad-kunnr TO ls_devir-hkont.
*        ENDIF.
*        MOVE: ls_bsad-bukrs TO ls_devir-bukrs,
*              ls_bsad-waers TO ls_devir-waers.
*
*        IF ls_bsad-shkzg = c_shkzg.
*          MOVE: ls_bsad-wrbtr TO ls_devir-wrbtrs.
*          MOVE: ls_bsad-dmbtr TO ls_devir-dmbtrs. "HY
*        ELSE.
*          MOVE: ls_bsad-wrbtr TO ls_devir-wrbtrh.
*          MOVE: ls_bsad-dmbtr TO ls_devir-dmbtrh. "HY
*        ENDIF.
*
*        IF NOT gs_range-s_umskz IS INITIAL.
*          ls_devir-umskz = ls_bsad-umskz.
*        ENDIF.
*
*  me->get_material(
*        EXPORTING
*          bukrs = ls_bsad-bukrs       " Şirket kodu
*          belnr = ls_bsad-belnr       " Kalem no.tayini: Malzeme belgesi - satınalma belgesi
*          gjahr = ls_bsad-gjahr       " Mali yıl
*          buzei = ls_bsad-buzei       " Muhasebe belgesi içindeki kayıt satırının numarası
*      ).
*
*        CHECK sy-subrc EQ 0.
*
*
*        COLLECT ls_devir into mt_devir.
*
*
*      ENDLOOP.

    ENDMETHOD.


  METHOD devir_musteri_new.

    DATA : lt_bsid TYPE TABLE OF bsid,
           ls_bsid TYPE bsid,
           lt_bsad TYPE TABLE OF bsad,
           ls_bsad TYPE bsad.

    DATA ls_devir LIKE LINE OF mt_devir.
    DATA: lv_keydt TYPE bkpf-budat.
    DATA: r_budat TYPE RANGE OF bkpf-budat.
    DATA: rx_budat TYPE LINE OF datum_range_tab .
    CLEAR : ls_bsid,lt_bsad.
    REFRESH :lt_bsid[],lt_bsad[].



    CHECK gs_range-s_budat IS NOT INITIAL.


    r_budat[] = gs_range-s_budat .

    LOOP AT r_budat INTO rx_budat.
      lv_keydt = rx_budat-low .
      EXIT.
    ENDLOOP.

    lv_keydt = lv_keydt - 1 .

    SELECT * INTO TABLE lt_bsid
      FROM bsid
     WHERE bukrs IN gs_range-s_bukrs
       AND kunnr IN gs_range-s_hkont
       AND budat LE lv_keydt
       AND gsber IN gs_range-s_gsber
       AND blart IN gs_range-s_blart
       AND prctr IN gs_range-s_prctr
       AND umskz IN gs_range-s_umskz.

    SELECT * APPENDING TABLE lt_bsid
      FROM bsad
     WHERE bukrs IN gs_range-s_bukrs
       AND kunnr IN gs_range-s_hkont
       AND budat LE lv_keydt
       AND augdt GT lv_keydt
       AND gsber IN gs_range-s_gsber
       AND blart IN gs_range-s_blart
       AND prctr IN gs_range-s_prctr
       AND umskz IN gs_range-s_umskz.

    LOOP AT lt_bsid INTO ls_bsid.
      CLEAR: ls_devir.
      MOVE: ls_bsid-bukrs TO ls_devir-bukrs,
            ls_bsid-kunnr TO ls_devir-hkont,
            ls_bsid-waers TO ls_devir-waers.

      IF ls_bsid-shkzg = zfi_000_cl02=>mc_shkzg.
        MOVE: ls_bsid-wrbtr TO ls_devir-wrbtrs.
        MOVE: ls_bsid-dmbtr TO ls_devir-dmbtrs.
      ELSE.
        MOVE: ls_bsid-wrbtr TO ls_devir-wrbtrh.
        MOVE: ls_bsid-dmbtr TO ls_devir-dmbtrh.
      ENDIF.
      IF NOT gs_range-s_umskz[] IS INITIAL.
        ls_devir-umskz = ls_bsid-umskz.
      ENDIF.
      me->get_material(
        EXPORTING
          bukrs = ls_bsid-bukrs       " Şirket kodu
          belnr = ls_bsid-belnr       " Kalem no.tayini: Malzeme belgesi - satınalma belgesi
          gjahr = ls_bsid-gjahr       " Mali yıl
          buzei = ls_bsid-buzei       " Muhasebe belgesi içindeki kayıt satırının numarası
      ).


      CHECK sy-subrc EQ 0.

      COLLECT ls_devir INTO mt_devir.
    ENDLOOP.

*
*    SELECT * INTO TABLE lt_bsad
*      FROM bsad
*     WHERE bukrs IN gs_range-s_bukrs
*       AND kunnr IN gs_range-s_hkont
*       AND budat IN gs_range-s_budat
*       AND gsber IN gs_range-s_gsber
*       AND blart IN gs_range-s_blart
*       AND prctr IN gs_range-s_prctr
*       AND umskz IN gs_range-s_umskz.
*    CLEAR ls_bsad.
*    LOOP AT lt_bsad INTO ls_bsad.
*      CLEAR: ls_devir.
*      MOVE: ls_bsad-bukrs TO ls_devir-bukrs,
*            ls_bsad-kunnr TO ls_devir-hkont,
*            ls_bsad-waers TO ls_devir-waers.
*
*      IF ls_bsad-shkzg = c_shkzg.
*        MOVE: ls_bsad-wrbtr TO ls_devir-wrbtrs.
*        MOVE: ls_bsad-dmbtr TO ls_devir-dmbtrs.
*      ELSE.
*        MOVE: ls_bsad-wrbtr TO ls_devir-wrbtrh.
*        MOVE: ls_bsad-dmbtr TO ls_devir-dmbtrh.
*      ENDIF.
*
*      IF NOT gs_range-s_umskz[] IS INITIAL.
*        ls_devir-umskz = ls_bsad-umskz.
*      ENDIF.
*      me->get_material(
*        EXPORTING
*          bukrs = ls_bsid-bukrs       " Şirket kodu
*          belnr = ls_bsid-belnr       " Kalem no.tayini: Malzeme belgesi - satınalma belgesi
*          gjahr = ls_bsid-gjahr       " Mali yıl
*          buzei = ls_bsid-buzei       " Muhasebe belgesi içindeki kayıt satırının numarası
*      ).
*
*      CHECK sy-subrc EQ 0.
*
*
*      COLLECT ls_devir INTO mt_devir.
*    ENDLOOP.


  ENDMETHOD.


  METHOD devir_satici_bp.


    DATA : lt_bsik TYPE TABLE OF bsik,
           ls_bsik TYPE bsik,
           lt_bsak TYPE TABLE OF bsak,
           ls_bsak TYPE bsak.


    DATA ls_devir LIKE LINE OF mt_devir.
    DATA: lv_keydt TYPE bkpf-budat.
    DATA: r_budat TYPE RANGE OF bkpf-budat.
    DATA: rx_budat TYPE LINE OF datum_range_tab .


    CHECK gs_range-s_budat IS NOT INITIAL.

    r_budat[] = gs_range-s_budat .

    LOOP AT r_budat INTO rx_budat.
      lv_keydt = rx_budat-low .
      EXIT.
    ENDLOOP.

    lv_keydt = lv_keydt - 1 .

    CLEAR : ls_bsik,ls_bsak.
    REFRESH :lt_bsik[],lt_bsak[].


    SELECT * FROM bsik
      INTO TABLE lt_bsik
     WHERE bukrs IN gs_range-s_bukrs
       AND lifnr IN gs_range-s_lifnr
       AND budat LE lv_keydt
       AND umskz IN gs_range-s_umskz.

    SELECT * FROM bsak
      APPENDING TABLE lt_bsik
       WHERE bukrs IN gs_range-s_bukrs
       AND lifnr IN gs_range-s_lifnr
       AND budat LE lv_keydt
       AND augdt GT lv_keydt
       AND umskz IN gs_range-s_umskz.


    IF gs_range-s_umskz[] IS INITIAL.
      DELETE lt_bsik WHERE umskz IS NOT INITIAL.
    ENDIF.


    LOOP AT lt_bsik INTO ls_bsik.

      CLEAR: ls_devir.
      MOVE: ls_bsik-bukrs TO ls_devir-bukrs,
            ls_bsik-lifnr TO ls_devir-hkont,
            ls_bsik-waers TO ls_devir-waers.

      IF ls_bsik-shkzg = zfi_000_cl02=>mc_shkzg.
        MOVE: ls_bsik-wrbtr TO ls_devir-wrbtrs.
        MOVE: ls_bsik-dmbtr TO ls_devir-dmbtrs.
      ELSE.
        MOVE: ls_bsik-wrbtr TO ls_devir-wrbtrh.
        MOVE: ls_bsik-dmbtr TO ls_devir-dmbtrh.
      ENDIF.

      IF NOT gs_range-s_umskz IS INITIAL.
        ls_devir-umskz = ls_bsik-umskz.
      ENDIF.

      me->get_material(
        EXPORTING
          bukrs =  ls_bsik-bukrs    " Şirket kodu
          belnr =  ls_bsik-belnr    " Kalem no.tayini: Malzeme belgesi - satınalma belgesi
          gjahr =  ls_bsik-gjahr    " Mali yıl
          buzei =  ls_bsik-buzei    " Muhasebe belgesi içindeki kayıt satırının numarası
      ).

      CHECK sy-subrc EQ 0.

      COLLECT ls_devir INTO mt_devir.
    ENDLOOP.

*    DATA: lt_bsik type table of bsik,
*          ls_bsik type bsik,
*          lt_bsak type table of bsak,
*          ls_bsak type bsak.
*    data ls_devir like line of mt_devir.
*  SELECT * FROM bsik INTO table lt_bsik
*                     WHERE bukrs IN gs_range-s_bukrs
*                      AND  lifnr IN gs_range-s_hkont
*                      AND  budat IN gs_range-s_budat
*                      AND  gsber IN gs_range-s_gsber
*                      AND  blart IN gs_range-s_blart
*                      AND  prctr IN gs_range-s_prctr
*                      AND  umskz IN gs_range-s_umskz.
*    LOOP AT lt_bsik into ls_bsik.
*
*
*    CLEAR: ls_devir.
*    MOVE: ls_bsik-bukrs TO ls_devir-bukrs,
*          ls_bsik-lifnr TO ls_devir-hkont,
*          ls_bsik-waers TO ls_devir-waers.
*
*    IF ls_bsik-shkzg = c_shkzg.
*      MOVE: ls_bsik-wrbtr TO ls_devir-wrbtrs.
*      MOVE: ls_bsik-dmbtr TO ls_devir-dmbtrs.
*    ELSE.
*      MOVE: ls_bsik-wrbtr TO ls_devir-wrbtrh.
*      MOVE: ls_bsik-dmbtr TO ls_devir-dmbtrh.
*    ENDIF.
*
*    IF NOT gs_range-s_umskz IS INITIAL.
*      ls_devir-umskz = ls_bsik-umskz.
*    ENDIF.
*
*    me->get_material(
*      EXPORTING
*        bukrs =  ls_bsik-bukrs    " Şirket kodu
*        belnr =  ls_bsik-belnr    " Kalem no.tayini: Malzeme belgesi - satınalma belgesi
*        gjahr =  ls_bsik-gjahr    " Mali yıl
*        buzei =  ls_bsik-buzei    " Muhasebe belgesi içindeki kayıt satırının numarası
*    ).
*
*    CHECK sy-subrc EQ 0.
*
*
*    COLLECT ls_devir into mt_devir.
*ENDLOOP.
*
*  SELECT * FROM bsak INTO table lt_bsak
*                   WHERE bukrs IN gs_range-s_bukrs
*                    AND  lifnr IN gs_range-s_hkont
*                    AND  budat IN gs_range-s_budat
*                    AND  gsber IN gs_range-s_gsber
*                    AND  blart IN gs_range-s_blart
*                    AND  prctr IN gs_range-s_prctr
*                    AND  umskz IN gs_range-s_umskz.
*    LOOP AT lt_bsak into ls_bsak .
*
*
*    CLEAR: ls_devir.
*    MOVE: ls_bsak-bukrs TO ls_devir-bukrs,
*          ls_bsak-lifnr TO ls_devir-hkont,
*          ls_bsak-waers TO ls_devir-waers.
*
*    IF ls_bsak-shkzg = c_shkzg.
*      MOVE: ls_bsak-wrbtr TO ls_devir-wrbtrs.
*      MOVE: ls_bsak-dmbtr TO ls_devir-dmbtrs. "HY
*    ELSE.
*      MOVE: ls_bsak-wrbtr TO ls_devir-wrbtrh.
*      MOVE: ls_bsak-dmbtr TO ls_devir-dmbtrh. "HY
*    ENDIF.
*
*    IF NOT gs_range-s_umskz[] IS INITIAL.
*      ls_devir-umskz = ls_bsak-umskz.
*    ENDIF.
*
*      me->get_material(
*      EXPORTING
*        bukrs =  ls_bsak-bukrs    " Şirket kodu
*        belnr =  ls_bsak-belnr    " Kalem no.tayini: Malzeme belgesi - satınalma belgesi
*        gjahr =  ls_bsak-gjahr    " Mali yıl
*        buzei =  ls_bsak-buzei    " Muhasebe belgesi içindeki kayıt satırının numarası
*    ).
*    CHECK sy-subrc EQ 0.
*
*    COLLECT ls_devir into mt_devir.
*  ENDLOOP.


  ENDMETHOD.


  METHOD devir_satici_new.

    DATA : lt_bsik TYPE TABLE OF bsik,
           ls_bsik TYPE bsik,
           lt_bsak TYPE TABLE OF bsak,
           ls_bsak TYPE bsak.


    DATA ls_devir LIKE LINE OF mt_devir.
    DATA: lv_keydt TYPE bkpf-budat.
    DATA: r_budat TYPE RANGE OF bkpf-budat.
    DATA: rx_budat TYPE LINE OF datum_range_tab .


    CHECK gs_range-s_budat IS NOT INITIAL.


    r_budat[] = gs_range-s_budat .

    LOOP AT r_budat INTO rx_budat.
      lv_keydt = rx_budat-low .
      EXIT.
    ENDLOOP.


    lv_keydt = lv_keydt - 1 .

    CLEAR : ls_bsik,ls_bsak.
    REFRESH :lt_bsik[],lt_bsak[].


    SELECT * FROM bsik
      INTO TABLE lt_bsik
     WHERE bukrs IN gs_range-s_bukrs
       AND lifnr IN gs_range-s_lifnr
       AND budat LE lv_keydt
       AND umskz IN gs_range-s_umskz.

    SELECT * FROM bsak
      APPENDING TABLE lt_bsik
       WHERE bukrs IN gs_range-s_bukrs
       AND lifnr IN gs_range-s_lifnr
       AND budat LE lv_keydt
       and augdt gt lv_keydt
       AND umskz IN gs_range-s_umskz.


if gs_range-s_umskz[] is INITIAL.
   delete lt_bsik where umskz is not INITIAL.
endif.

****

*    SELECT * INTO TABLE lt_bsik
*      FROM bsik
*     WHERE bukrs IN gs_range-s_bukrs
*       AND lifnr IN gs_range-s_hkont
*       AND budat IN gs_range-s_budat
*       AND gsber IN gs_range-s_gsber
*       AND blart IN gs_range-s_blart
*       AND prctr IN gs_range-s_prctr
*       AND umskz IN gs_range-s_umskz.



    LOOP AT lt_bsik INTO ls_bsik.
      CLEAR: ls_devir.
      MOVE: ls_bsik-bukrs TO ls_devir-bukrs,
            ls_bsik-lifnr TO ls_devir-hkont,
            ls_bsik-waers TO ls_devir-waers.

      IF ls_bsik-shkzg = zfi_000_cl02=>mc_shkzg.
        MOVE: ls_bsik-wrbtr TO ls_devir-wrbtrs.
        MOVE: ls_bsik-dmbtr TO ls_devir-dmbtrs.
      ELSE.
        MOVE: ls_bsik-wrbtr TO ls_devir-wrbtrh.
        MOVE: ls_bsik-dmbtr TO ls_devir-dmbtrh.
      ENDIF.

      IF gs_range-s_umskz[] IS not INITIAL.
        ls_devir-umskz = ls_bsik-umskz.
      ENDIF.

      me->get_material(
        EXPORTING
          bukrs = ls_bsik-bukrs                  " Şirket kodu
          belnr = ls_bsik-belnr                 " Kalem no.tayini: Malzeme belgesi - satınalma belgesi
          gjahr = ls_bsik-gjahr                 " Mali yıl
          buzei = ls_bsik-buzei                 " Muhasebe belgesi içindeki kayıt satırının numarası
      ).

      CHECK sy-subrc EQ 0.

      COLLECT ls_devir INTO mt_devir.
    ENDLOOP.

*    SELECT * INTO TABLE lt_bsak
*      FROM bsak
*     WHERE bukrs IN gs_range-s_bukrs
*       AND lifnr IN gs_range-s_hkont
*       AND budat IN gs_range-s_budat
*       AND gsber IN gs_range-s_gsber
*       AND blart IN gs_range-s_blart
*       AND prctr IN gs_range-s_prctr
*       AND umskz IN gs_range-s_umskz.
*
*    LOOP AT lt_bsak INTO ls_bsak.
*      CLEAR: ls_devir.
*      MOVE: ls_bsak-bukrs TO ls_devir-bukrs,
*            ls_bsak-lifnr TO ls_devir-hkont,
*            ls_bsak-waers TO ls_devir-waers.
*
*      IF ls_bsak-shkzg = c_shkzg.
*        MOVE: ls_bsak-wrbtr TO ls_devir-wrbtrs.
*        MOVE: ls_bsak-dmbtr TO ls_devir-dmbtrs. "HY
*      ELSE.
*        MOVE: ls_bsak-wrbtr TO ls_devir-wrbtrh.
*        MOVE: ls_bsak-dmbtr TO ls_devir-dmbtrh. "HY
*      ENDIF.
*
*      IF NOT gs_range-s_umskz[] IS INITIAL.
*        ls_devir-umskz = ls_bsak-umskz.
*      ENDIF.
*
*      me->get_material(
*        EXPORTING
*          bukrs = ls_bsik-bukrs                  " Şirket kodu
*          belnr = ls_bsik-belnr                 " Kalem no.tayini: Malzeme belgesi - satınalma belgesi
*          gjahr = ls_bsik-gjahr                 " Mali yıl
*          buzei = ls_bsik-buzei                 " Muhasebe belgesi içindeki kayıt satırının numarası
*      ).
*
*      CHECK sy-subrc EQ 0.
*
*      COLLECT ls_devir INTO mt_devir.
*    ENDLOOP.
  ENDMETHOD.


  METHOD fill_tout.
        TYPES: BEGIN OF ty_itab,
             bukrs TYPE bukrs,
             belnr TYPE zed_de_belnr,
             gjahr TYPE gjahr,
           END OF ty_itab.

    DATA: l_fark1 TYPE rf42b-saldo,
          l_fark2 TYPE rf42b-saldo,
          l_total TYPE rf42b-saldo.

    DATA: l_fark1_d TYPE rf42b-saldo,
          l_fark2_d TYPE rf42b-saldo,
          l_total_d TYPE rf42b-saldo.

    DATA: lt_itab_temp TYPE TABLE OF ty_itab.

    DATA: lv_buzei TYPE buzei,
          lv_docln TYPE zfi_001_t01-docln.

*    DATA: lt_journal TYPE SORTED TABLE OF zfi_001_t02
*              WITH NON-UNIQUE KEY bukrs belnr gjahr buzei,
*          ls_journal LIKE LINE OF lt_journal.
    DATA: ls_out     LIKE LINE OF mt_out,
          ls_bakiye  LIKE LINE OF mt_bakiye,
          ls_bakiye2 LIKE LINE OF mt_bakiye2.
    DATA  ls_hkont LIKE LINE OF mt_hkont.
    DATA  ls_waers2 LIKE LINE OF mt_waers2.
    DATA  ls_itab LIKE LINE OF mt_itab.
    DATA  ls_devir LIKE LINE OF mt_devir.

    CLEAR: ls_out, ls_bakiye, ls_bakiye2.
    FREE : mt_out, mt_bakiye, mt_bakiye2.


  FIELD-SYMBOLS <fs_itab_temp> TYPE ty_itab.
    CHECK mt_itab[] IS NOT INITIAL.

*-&Added by Fkaraguzel  29.11.2021 Begin Of |->

    LOOP AT mt_itab INTO ls_itab.
      APPEND INITIAL LINE TO lt_itab_temp ASSIGNING <fs_itab_temp>.
      <fs_itab_temp>-belnr = ls_itab-belnr.
      <fs_itab_temp>-bukrs = ls_itab-bukrs.
      <fs_itab_temp>-gjahr = ls_itab-gjahr.
    ENDLOOP.

    SELECT * FROM zed_t_journal
      INTO TABLE @DATA(lt_journal)
          FOR ALL ENTRIES IN @lt_itab_temp
         WHERE bukrs = @lt_itab_temp-bukrs
           AND belnr = @lt_itab_temp-belnr
           AND gjahr = @lt_itab_temp-gjahr.

    CLEAR ls_itab.
*    IF mt_itab IS NOT INITIAL.
*      SELECT * INTO TABLE lt_journal FROM zfi_001_t02
*             FOR ALL ENTRIES IN mt_itab
*                          WHERE bukrs = mt_itab-bukrs
*                            AND belnr = mt_itab-belnr
*                            AND gjahr = mt_itab-gjahr.
*
*      SELECT * INTO TABLE @DATA(lt_journal_2)
*         FROM zfi_001_t02
*             FOR ALL ENTRIES IN @mt_itab
*                WHERE bukrs = @mt_itab-bukrs
*                  AND belnr = @mt_itab-belnr
*                  AND gjahr = @mt_itab-gjahr.
*      SORT lt_journal_2 BY bukrs belnr gjahr.
*
*
*
*    ENDIF.
*-&Added by Fkaraguzel | 29.11.2021 End Of <-|
    LOOP AT mt_hkont INTO ls_hkont.
      me->sort_waers_odk(
        EXPORTING
          hkont   = ls_hkont-hkont   " Defteri kebir muhasebesi ana hesabı
          bukrs   = ls_hkont-bukrs   " Şirket kodu
          waers   = ls_hkont-waers    " Para birimi anahtarı
          umskz   = ls_hkont-umskz    " Özel defteri kebir göstergesi
      ).
*
      LOOP AT mt_waers2 INTO ls_waers2 WHERE bukrs = ls_hkont-bukrs
                         AND waers = ls_hkont-waers
                         AND hkont = ls_hkont-hkont.

        LOOP AT mt_itab INTO ls_itab WHERE bukrs = ls_waers2-bukrs
                       AND waers = ls_waers2-waers
                       AND hkont = ls_waers2-hkont.

          AT NEW waers.
            LOOP AT mt_devir INTO ls_devir WHERE bukrs = ls_itab-bukrs
                              AND hkont = ls_itab-hkont
                              AND waers = ls_itab-waers.
*                            and umskz = itab-umskz.
              CLEAR ls_out.
              ls_out-bukrs = ls_itab-bukrs.
              ls_out-hkont = ls_itab-hkont.
*              ls_out-sgtxt = 'DEVREDEN BAKİYE ->'.
              MOVE TEXT-019 TO ls_out-sgtxt.
              ls_out-color = zfi_000_cl02=>mc_color.
*            t_out-umskz = itab-umskz.
              ls_out-waers = ls_devir-waers.
              IF ls_devir-wrbtrs > ls_devir-wrbtrh.
                ls_out-wrbtrs = ls_devir-wrbtrs - ls_devir-wrbtrh.
                ls_out-totals = ls_devir-wrbtrs - ls_devir-wrbtrh.
              ELSEIF ls_devir-wrbtrs < ls_devir-wrbtrh.
                ls_out-wrbtrh = ls_devir-wrbtrh - ls_devir-wrbtrs.
                ls_out-totalh = ls_devir-wrbtrh - ls_devir-wrbtrs.
              ELSE.
                ls_out-wrbtrs = 0.
                ls_out-wrbtrh = 0.
              ENDIF.

              IF ls_devir-dmbtrs > ls_devir-dmbtrh.
                ls_out-dmbtrs = ls_devir-dmbtrs - ls_devir-dmbtrh.
                ls_out-totals_d = ls_devir-dmbtrs - ls_devir-dmbtrh.
              ELSEIF ls_devir-dmbtrs < ls_devir-dmbtrh.
                ls_out-dmbtrh = ls_devir-dmbtrh - ls_devir-dmbtrs.
                ls_out-totalh_d = ls_devir-dmbtrh - ls_devir-dmbtrs.
              ELSE.
                ls_out-dmbtrs = 0.
                ls_out-dmbtrh = 0.
              ENDIF.

              APPEND ls_out TO mt_out.
            ENDLOOP.
            IF sy-subrc NE 0.
              CLEAR ls_out.
              ls_out-bukrs = ls_waers2-bukrs.
              ls_out-hkont = ls_waers2-hkont.
              ls_out-waers = ls_waers2-waers.
              MOVE TEXT-019 TO ls_out-sgtxt.
              ls_out-color = zfi_000_cl02=>mc_color.
              APPEND ls_out TO mt_out.
            ENDIF.
          ENDAT.

          CLEAR: ls_out.
          "etosun
          CLEAR : ls_itab-line_number,ls_itab-journal_number,lv_buzei.
          lv_docln = ls_itab-buzei.
          CONCATENATE TEXT-021 lv_docln INTO lv_docln."text yerine '000 vardı'

*        select single line_number journal_number
*                 into (itab-line_number,itab-journal_number)
*                 from zldg_line_num
*                where bukrs = itab-bukrs
*                  and belnr = itab-belnr
*                  and gjahr = itab-gjahr
*                  and docln = lv_docln.

*-&Added by Fkaraguzel  29.11.2021 Begin Of |->

          READ TABLE lt_journal INTO DATA(ls_journal) WITH KEY bukrs = ls_itab-bukrs
                                               belnr = ls_itab-belnr
                                               gjahr = ls_itab-gjahr
                                               buzei = lv_docln.
          IF sy-subrc eq 0.
          ls_itab-line_number = ls_journal-buzei_new.
          ls_itab-journal_number = ls_journal-vzahl.
          ENDIF.

*          READ TABLE lt_journal INTO ls_journal WITH TABLE KEY bukrs = ls_itab-bukrs
*                                               belnr = ls_itab-belnr
*                                               gjahr = ls_itab-gjahr
*                                               buzei = lv_docln.
*          IF sy-subrc EQ 0.
*            ls_itab-line_number = ls_journal-vzahl_item.
*            ls_itab-journal_number = ls_journal-vzahl.
*          ELSE.
*
*            READ TABLE lt_journal_2 INTO DATA(ls_journal_2)
*               WITH KEY bukrs = ls_itab-bukrs
*                        belnr = ls_itab-belnr
*                        gjahr = ls_itab-gjahr
*                        BINARY SEARCH.
*            IF sy-subrc = 0.
*
*              ls_itab-journal_number = ls_journal_2-vzahl.
*
*            ENDIF.
*
*          ENDIF.
**        if sy-subrc is not initial.
**          lv_buzei = itab-buzei.
**          select single line_number journal_number
**                   into (itab-line_number,itab-journal_number)
**                   from zldg_line_num
**                  where bukrs = itab-bukrs
**                    and belnr = itab-belnr
**                    and gjahr = itab-gjahr
**                    and docln = lv_buzei.
**        endif.
*-&Added by Fkaraguzel | 29.11.2021 End Of <-|

**********************************************************************
*** added by gsumengen req num 7100002417 req by moz 24.06.2014.
*     CLEAR ls_line.
*     IF itab-line_number IS INITIAL AND itab-journal_number IS INITIAL.
*        call function 'ZFI_YEVMIYE_NO'
*          exporting
*            i_bukrs        = itab-bukrs
*            i_belnr        = itab-belnr
*            i_period       = itab-budat(6)
*            i_buzei        = itab-buzei
*         IMPORTING
*           E_LINE         = ls_line
*                  .
*        IF sy-subrc = 0.
*          itab-line_number = ls_line-line_number.
*          itab-journal_number = ls_line-journal_number.
*        ENDIF.
*     ENDIF.
***
          MOVE-CORRESPONDING ls_itab TO ls_out.

          CLEAR ls_bakiye.
*bakiyeler bukrs, kunnr, waers bazında collect ediliyor.
          MOVE: ls_itab-bukrs  TO ls_bakiye-bukrs,
                ls_itab-hkont  TO ls_bakiye-hkont,
                ls_itab-waers  TO ls_bakiye-waers,
                ls_itab-wrbtrs TO ls_bakiye-wrbtrs,
                ls_itab-wrbtrh TO ls_bakiye-wrbtrh,
                ls_itab-dmbtrs TO ls_bakiye-dmbtrs,
                ls_itab-dmbtrh TO ls_bakiye-dmbtrh.

*       t_bakiye-umskz = itab-umskz.

          COLLECT ls_bakiye INTO mt_bakiye.

*yukarıda collect edilen bakiyeler çekiliyor.
          CLEAR ls_bakiye.
          READ TABLE mt_bakiye INTO ls_bakiye WITH KEY bukrs = ls_itab-bukrs
                                       hkont = ls_itab-hkont
                                       waers = ls_itab-waers.
          IF sy-subrc = 0.
            l_fark1 = ls_bakiye-wrbtrs - ls_bakiye-wrbtrh.
            l_fark1_d = ls_bakiye-dmbtrs - ls_bakiye-dmbtrh.
          ELSE.
            l_fark1 = 0.
            l_fark1_d = 0.
          ENDIF.
*devir bakiyesi okunuyor.
          CLEAR ls_devir.
          READ TABLE mt_devir INTO ls_devir WITH KEY bukrs = ls_itab-bukrs
                                      hkont = ls_itab-hkont
                                      waers = ls_itab-waers.
          IF sy-subrc EQ 0.
            l_fark2 = ls_devir-wrbtrs - ls_devir-wrbtrh.
            l_fark2_d = ls_devir-dmbtrs - ls_devir-dmbtrh.
          ELSE.
            l_fark2 = 0.
            l_fark2_d = 0.
          ENDIF.
          l_total = l_fark1 + l_fark2.
          l_total_d = l_fark1_d + l_fark2_d.

          IF l_total GE 0.
            ls_out-totals = l_total.
            ls_out-totalh = abap_false.
          ELSE.
            ls_out-totalh = 0 - l_total.
            ls_out-totals  = abap_false.
          ENDIF.

          IF l_total_d GE 0.
            ls_out-totals_d = l_total_d.
            ls_out-totalh_d = abap_false.
          ELSE.
            ls_out-totalh_d = 0 - l_total_d.
            ls_out-totals_d  = abap_false.
          ENDIF.

          APPEND ls_out TO mt_out.

          AT END OF waers.
            CLEAR: ls_bakiye2.
            FREE : mt_bakiye2.
            LOOP AT mt_bakiye INTO ls_bakiye WHERE bukrs = ls_itab-bukrs
                               AND hkont = ls_itab-hkont
                               AND waers = ls_itab-waers.
*                             and umskz = itab-umskz.

              CLEAR: ls_bakiye2.
              MOVE-CORRESPONDING ls_bakiye TO ls_bakiye2.
              COLLECT ls_bakiye2 INTO mt_bakiye2.
            ENDLOOP.

            LOOP AT mt_devir INTO ls_devir WHERE bukrs = ls_itab-bukrs
                              AND hkont = ls_itab-hkont
                              AND waers = ls_itab-waers.
*                            and umskz = itab-umskz.

              MOVE-CORRESPONDING ls_devir TO ls_bakiye2.
              COLLECT ls_bakiye2 INTO mt_bakiye2.
            ENDLOOP.

            LOOP AT mt_bakiye2 INTO ls_bakiye2 WHERE bukrs = ls_itab-bukrs
                                AND hkont = ls_itab-hkont
                                AND waers = ls_itab-waers.
*                             and umskz = itab-umskz.

              CLEAR ls_out.
              ls_out-bukrs = ls_bakiye2-bukrs.
              ls_out-hkont = ls_bakiye2-hkont.
              MOVE TEXT-020 TO ls_out-sgtxt.
              ls_out-color = zfi_000_cl02=>mc_color1.
              ls_out-waers = ls_bakiye2-waers.

              IF ls_bakiye2-wrbtrs >= ls_bakiye2-wrbtrh.
                ls_out-totals = ls_bakiye2-wrbtrs - ls_bakiye2-wrbtrh.
              ELSE.
                ls_out-totalh = ls_bakiye2-wrbtrh - ls_bakiye2-wrbtrs.
              ENDIF.

              IF ls_bakiye2-dmbtrs >= ls_bakiye2-dmbtrh.
                ls_out-totals_d = ls_bakiye2-dmbtrs - ls_bakiye2-dmbtrh.
              ELSE.
                ls_out-totalh_d = ls_bakiye2-dmbtrh - ls_bakiye2-dmbtrs.
              ENDIF.

*            select single bktxt "agumus
*              from bkpf
*              into t_out-bktxt
*              where belnr = itab-belnr.
              APPEND ls_out TO mt_out.
            ENDLOOP.
          ENDAT.
        ENDLOOP.
        IF sy-subrc NE 0.
*devirden gelen ama o ay içinde hareket görmemiş para birimleri
          LOOP AT mt_devir INTO ls_devir WHERE bukrs = ls_waers2-bukrs
                            AND hkont = ls_waers2-hkont
                            AND waers = ls_waers2-waers.
*                          and umskz = itab-umskz.

            CLEAR ls_out.
            ls_out-bukrs = ls_devir-bukrs.
            ls_out-hkont = ls_devir-hkont.
            MOVE TEXT-019 TO ls_out-sgtxt.
            ls_out-color = zfi_000_cl02=>mc_color.
            IF ls_devir-wrbtrs > ls_devir-wrbtrh.
              ls_out-wrbtrs = ls_devir-wrbtrs - ls_devir-wrbtrh.
              ls_out-totals = ls_devir-wrbtrs - ls_devir-wrbtrh.
              ls_out-waers  = ls_devir-waers.
            ELSEIF ls_devir-wrbtrs < ls_devir-wrbtrh.
              ls_out-wrbtrh = ls_devir-wrbtrh - ls_devir-wrbtrs.
              ls_out-totalh = ls_devir-wrbtrh - ls_devir-wrbtrs.
              ls_out-waers  = ls_devir-waers.
            ELSE.
              ls_out-wrbtrs = 0.
              ls_out-wrbtrh = 0.
              ls_out-waers  = ls_devir-waers.
            ENDIF.

            IF ls_devir-dmbtrs > ls_devir-dmbtrh.
              ls_out-dmbtrs = ls_devir-dmbtrs - ls_devir-dmbtrh.
              ls_out-totals_d = ls_devir-dmbtrs - ls_devir-dmbtrh.
            ELSEIF ls_devir-dmbtrs < ls_devir-dmbtrh.
              ls_out-dmbtrh = ls_devir-dmbtrh - ls_devir-dmbtrs.
              ls_out-totalh_d = ls_devir-dmbtrh - ls_devir-dmbtrs.
            ELSE.
              ls_out-dmbtrs = 0.
              ls_out-dmbtrh = 0.
            ENDIF.

            IF NOT ( ls_out-totals = 0 AND
                     ls_out-totalh = 0 AND
                     ls_out-totalh_d = 0 AND
                     ls_out-totals_d = 0 ) .
              APPEND ls_out TO mt_out.
            ENDIF.
*altınada son durum ekleniyor
            CLEAR: ls_out-wrbtrs,
                   ls_out-wrbtrh,
                   ls_out-dmbtrs,
                   ls_out-dmbtrh.
            MOVE TEXT-020 TO ls_out-sgtxt.
            ls_out-color = zfi_000_cl02=>mc_color1.
            IF NOT ( ls_out-totals = 0 AND
                     ls_out-totalh = 0 AND
                     ls_out-totalh_d = 0 AND
                     ls_out-totals_d = 0 ) .
              APPEND ls_out TO mt_out.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
    me->get_name( ).

  ENDMETHOD.


  METHOD fill_tout_bp.

    TYPES: BEGIN OF ty_itab,
             bukrs TYPE bukrs,
             belnr TYPE zed_de_belnr,
             gjahr TYPE gjahr,
           END OF ty_itab.

    DATA: l_fark1 TYPE rf42b-saldo,
          l_fark2 TYPE rf42b-saldo,
          l_total TYPE rf42b-saldo.

    DATA: l_fark1_d TYPE rf42b-saldo,
          l_fark2_d TYPE rf42b-saldo,
          l_total_d TYPE rf42b-saldo.

    DATA: lv_buzei TYPE buzei,
          lv_docln TYPE zfi_001_t01-docln.

    DATA: lt_itab_temp TYPE TABLE OF ty_itab.
*              WITH NON-UNIQUE KEY bukrs belnr gjahr buzei.
*    DATA ls_journal LIKE LINE OF lt_journal.
    DATA : ls_out     LIKE LINE OF mt_out,
           ls_bakiye  LIKE LINE OF mt_bakiye,
           ls_waers2  LIKE LINE OF mt_waers2,
           ls_devir   LIKE LINE OF mt_devir,
           ls_itab    LIKE LINE OF mt_itab,
           ls_hkont   LIKE LINE OF mt_hkont,
           ls_bakiye2 LIKE LINE OF mt_bakiye2.
    CLEAR: ls_out, ls_bakiye, ls_bakiye2.
    FREE : mt_out, mt_bakiye, mt_bakiye2.

    FIELD-SYMBOLS <fs_itab_temp> TYPE ty_itab.
    CHECK mt_itab[] IS NOT INITIAL.

*-&Added by Fkaraguzel  29.11.2021 Begin Of |->

    LOOP AT mt_itab INTO ls_itab.
      APPEND INITIAL LINE TO lt_itab_temp ASSIGNING <fs_itab_temp>.
      <fs_itab_temp>-belnr = ls_itab-belnr.
      <fs_itab_temp>-bukrs = ls_itab-bukrs.
      <fs_itab_temp>-gjahr = ls_itab-gjahr.
    ENDLOOP.

    SELECT * FROM zed_t_journal
      INTO TABLE @DATA(lt_journal)
          FOR ALL ENTRIES IN @lt_itab_temp
         WHERE bukrs = @lt_itab_temp-bukrs
           AND belnr = @lt_itab_temp-belnr
           AND gjahr = @lt_itab_temp-gjahr.

    CLEAR ls_itab.

*    SELECT * INTO TABLE lt_journal FROM zfi_001_t02
*      FOR ALL ENTRIES IN mt_itab
*                           WHERE bukrs = mt_itab-bukrs
*                             AND belnr = mt_itab-belnr
*                             AND gjahr = mt_itab-gjahr.
*
*    SELECT * INTO TABLE @DATA(lt_journal_2)
*       FROM zfi_001_t04
*           FOR ALL ENTRIES IN @mt_itab
*              WHERE bukrs = @mt_itab-bukrs
*                AND belnr = @mt_itab-belnr
*                AND gjahr = @mt_itab-gjahr.
*    SORT lt_journal_2 BY bukrs belnr gjahr.

*-&Added by Fkaraguzel | 29.11.2021 End Of <-|
*  DATA: twaers3 LIKE twaers OCCURS 0 WITH HEADER LINE.

    LOOP AT mt_hkont INTO ls_hkont.
      me->sort_waers_odk(
         EXPORTING
           hkont   = ls_hkont-hkont   " Defteri kebir muhasebesi ana hesabı
           bukrs   = ls_hkont-bukrs   " Şirket kodu
           waers   = ls_hkont-waers    " Para birimi anahtarı
           umskz   = ls_hkont-umskz    " Özel defteri kebir göstergesi
       ).

*
      LOOP AT mt_waers2 INTO ls_waers2  WHERE bukrs = ls_hkont-bukrs
                         AND waers = ls_hkont-waers
                         AND hkont = ls_hkont-hkont.

        LOOP AT mt_itab INTO ls_itab WHERE bukrs = ls_waers2-bukrs
                       AND waers = ls_waers2-waers
                       AND hkont = ls_hkont-hkont.
*-------waers değiştiğinde
          AT NEW waers.
            LOOP AT mt_devir INTO ls_devir WHERE bukrs = ls_itab-bukrs
                              AND hkont = ls_itab-hkont
                              AND waers = ls_itab-waers.
*                            and umskz = itab-umskz.
              CLEAR ls_out.
              ls_out-bukrs = ls_itab-bukrs.
              ls_out-hkont = ls_itab-hkont.
              MOVE TEXT-019 TO ls_out-sgtxt.
              ls_out-color = zfi_000_cl02=>mc_color.

*            t_out-umskz = itab-umskz.

              ls_out-waers = ls_devir-waers.
              IF ls_devir-wrbtrs > ls_devir-wrbtrh.
                ls_out-wrbtrs = ls_devir-wrbtrs - ls_devir-wrbtrh.
                ls_out-totals = ls_devir-wrbtrs - ls_devir-wrbtrh.
              ELSEIF ls_devir-wrbtrs < ls_devir-wrbtrh.
                ls_out-wrbtrh = ls_devir-wrbtrh - ls_devir-wrbtrs.
                ls_out-totalh = ls_devir-wrbtrh - ls_devir-wrbtrs.
              ELSE.
                ls_out-wrbtrs = 0.
                ls_out-wrbtrh = 0.
              ENDIF.

              IF ls_devir-dmbtrs > ls_devir-dmbtrh.
                ls_out-dmbtrs = ls_devir-dmbtrs - ls_devir-dmbtrh.
                ls_out-totals_d = ls_devir-dmbtrs - ls_devir-dmbtrh.
              ELSEIF ls_devir-dmbtrs < ls_devir-dmbtrh.
                ls_out-dmbtrh = ls_devir-dmbtrh - ls_devir-dmbtrs.
                ls_out-totalh_d = ls_devir-dmbtrh - ls_devir-dmbtrs.
              ELSE.
                ls_out-dmbtrs = 0.
                ls_out-dmbtrh = 0.
              ENDIF.

              APPEND ls_out TO mt_out.
            ENDLOOP.
            IF sy-subrc NE 0.
              CLEAR ls_out.
              ls_out-bukrs = ls_waers2-bukrs.
              ls_out-hkont = ls_waers2-hkont.
              ls_out-waers = ls_waers2-waers.
              MOVE TEXT-019 TO ls_out-sgtxt.
              ls_out-color = zfi_000_cl02=>mc_color.
              APPEND ls_out TO mt_out.
            ENDIF.
          ENDAT.

          CLEAR: ls_out.

          "etosun
          CLEAR : ls_itab-line_number,ls_itab-journal_number,lv_buzei.
          lv_docln = ls_itab-buzei.
**        concatenate '000' lv_docln into lv_docln.
**
**        select single line_number journal_number
**                 into (itab-line_number,itab-journal_number)
**                 from zldg_line_num
**                where bukrs = itab-bukrs
**                  and belnr = itab-belnr
**                  and gjahr = itab-gjahr
**                  and docln = lv_docln.
**        if sy-subrc is not initial.
**          lv_buzei = itab-buzei.
**
**          select single line_number journal_number
**                   into (itab-line_number,itab-journal_number)
**                   from zldg_line_num
**                  where bukrs = itab-bukrs
**                    and belnr = itab-belnr
**                    and gjahr = itab-gjahr
**                    and docln = lv_buzei.
**        endif.

*-&Added by Fkaraguzel  29.11.2021 Begin Of |->

          READ TABLE lt_journal INTO DATA(ls_journal) WITH KEY bukrs = ls_itab-bukrs
                                               belnr = ls_itab-belnr
                                               gjahr = ls_itab-gjahr
                                               buzei = lv_docln.
          IF sy-subrc eq 0.
          ls_itab-line_number = ls_journal-buzei_new.
          ls_itab-journal_number = ls_journal-vzahl.
          ENDIF.


**          READ TABLE lt_journal INTO ls_journal WITH TABLE KEY bukrs = ls_itab-bukrs
**                                               belnr = ls_itab-belnr
**                                               gjahr = ls_itab-gjahr
**                                               buzei = lv_docln.
**          IF sy-subrc EQ 0.
**            ls_itab-line_number = ls_journal-vzahl_item.
**            ls_itab-journal_number = ls_journal-vzahl.
**          ELSE.
**
**            READ TABLE lt_journal_2 INTO DATA(ls_journal_2)
**               WITH KEY bukrs = ls_itab-bukrs
**                        belnr = ls_itab-belnr
**                        gjahr = ls_itab-gjahr
**                        BINARY SEARCH.
**            IF sy-subrc = 0.
**
**              ls_itab-journal_number = ls_journal_2-vzahl.
**
**            ENDIF.
**          ENDIF.
*-&Added by Fkaraguzel | 29.11.2021 End Of <-|
**********************************************************************
*** added by gsumengen req num 7100002417 req by moz 25.06.2015.
*     CLEAR ls_line.
*     IF itab-line_number IS INITIAL AND itab-journal_number IS INITIAL.
*        call function 'ZFI_YEVMIYE_NO'
*          exporting
*            i_bukrs        = itab-bukrs
*            i_belnr        = itab-belnr
*            i_period       = itab-budat(6)
*            i_buzei        = itab-buzei
*         IMPORTING
*           E_LINE         = ls_line
*                  .
*        IF sy-subrc = 0.
*          itab-line_number = ls_line-line_number.
*          itab-journal_number = ls_line-journal_number.
*        ENDIF.
*     ENDIF.
***

          MOVE-CORRESPONDING ls_itab TO ls_out.


          CLEAR ls_bakiye.
*---bakiyeler bukrs, kunnr, waers bazında collect ediliyor.
          MOVE: ls_itab-bukrs  TO ls_bakiye-bukrs,
                ls_itab-hkont  TO ls_bakiye-hkont,
                ls_itab-waers  TO ls_bakiye-waers,
                ls_itab-wrbtrs TO ls_bakiye-wrbtrs,
                ls_itab-wrbtrh TO ls_bakiye-wrbtrh,
                ls_itab-dmbtrs TO ls_bakiye-dmbtrs,
                ls_itab-dmbtrh TO ls_bakiye-dmbtrh.

*       t_bakiye-umskz = itab-umskz.

          COLLECT ls_bakiye INTO mt_bakiye.

*---yukarıda collect edilen bakiyeler çekiliyor.
          CLEAR ls_bakiye.
          READ TABLE mt_bakiye INTO ls_bakiye  WITH KEY bukrs = ls_itab-bukrs
                                       hkont = ls_itab-hkont
                                       waers = ls_itab-waers.
          IF sy-subrc = 0.
            l_fark1 = ls_bakiye-wrbtrs - ls_bakiye-wrbtrh.
            l_fark1_d = ls_bakiye-dmbtrs - ls_bakiye-dmbtrh.
          ELSE.
            l_fark1 = 0.
            l_fark1_d = 0.
          ENDIF.
*---devir bakiyesi okunuyor.
          CLEAR ls_devir.
          READ TABLE mt_devir INTO ls_devir WITH KEY bukrs = ls_itab-bukrs
                                      hkont = ls_itab-hkont
                                      waers = ls_itab-waers.
          IF sy-subrc EQ 0.
            l_fark2 = ls_devir-wrbtrs - ls_devir-wrbtrh.
            l_fark2_d = ls_devir-dmbtrs - ls_devir-dmbtrh.
          ELSE.
            l_fark2 = 0.
            l_fark2_d = 0.
          ENDIF.
          l_total = l_fark1 + l_fark2.
          l_total_d = l_fark1_d + l_fark2_d.

          IF l_total GE 0.
            ls_out-totals = l_total.
            ls_out-totalh = abap_false.
          ELSE.
            ls_out-totalh = 0 - l_total.
            ls_out-totals  = abap_false.
          ENDIF.

          IF l_total_d GE 0.
            ls_out-totals_d = l_total_d.
            ls_out-totalh_d = abap_false.
          ELSE.
            ls_out-totalh_d = 0 - l_total_d.
            ls_out-totals_d  = abap_false.
          ENDIF.

          APPEND ls_out TO mt_out.

          AT END OF waers.
            CLEAR: ls_bakiye2.

            FREE : mt_bakiye2.

            LOOP AT mt_bakiye INTO ls_bakiye WHERE bukrs = ls_itab-bukrs " AŞ
                               AND hkont = ls_itab-hkont
                               AND waers = ls_itab-waers.
*                             and umskz = itab-umskz.

              CLEAR: ls_bakiye2.
              MOVE-CORRESPONDING ls_bakiye TO ls_bakiye2.
              COLLECT ls_bakiye2 INTO mt_bakiye2.
            ENDLOOP.

            LOOP AT mt_devir INTO ls_devir WHERE bukrs = ls_itab-bukrs
                              AND hkont = ls_itab-hkont
                              AND waers = ls_itab-waers.
*                            and umskz = itab-umskz.

              MOVE-CORRESPONDING ls_devir TO ls_bakiye2.
              COLLECT ls_bakiye2 INTO mt_bakiye2.
            ENDLOOP.

            LOOP AT mt_bakiye2  INTO ls_bakiye2  WHERE bukrs = ls_itab-bukrs
                                AND hkont = ls_itab-hkont
                                AND waers = ls_itab-waers.
*                             and umskz = ls_itab-umskz.

              CLEAR ls_out.
              ls_out-bukrs = ls_bakiye2-bukrs.
              ls_out-hkont = ls_bakiye2-hkont.
              MOVE TEXT-020 TO ls_out-sgtxt.
              ls_out-color = zfi_000_cl02=>mc_color1.
              ls_out-waers = ls_bakiye2-waers.

              IF ls_bakiye2-wrbtrs >= ls_bakiye2-wrbtrh.
                ls_out-totals = ls_bakiye2-wrbtrs - ls_bakiye2-wrbtrh.
              ELSE.
                ls_out-totalh = ls_bakiye2-wrbtrh - ls_bakiye2-wrbtrs.
              ENDIF.

              IF ls_bakiye2-dmbtrs >= ls_bakiye2-dmbtrh.
                ls_out-totals_d = ls_bakiye2-dmbtrs - ls_bakiye2-dmbtrh.
              ELSE.
                ls_out-totalh_d = ls_bakiye2-dmbtrh - ls_bakiye2-dmbtrs.
              ENDIF.

              APPEND ls_out TO mt_out.
            ENDLOOP.
          ENDAT.
        ENDLOOP.
        IF sy-subrc NE 0.
*-----devirden gelen ama o ay içinde hareket görmemiş para birimleri
*        LOOP AT twaers3.
          LOOP AT mt_devir INTO ls_devir WHERE bukrs = ls_waers2-bukrs
                            AND hkont = ls_waers2-hkont
                            AND waers = ls_waers2-waers.
*                          and umskz = itab-umskz.

            CLEAR ls_out.
            ls_out-bukrs = ls_devir-bukrs.
            ls_out-hkont = ls_devir-hkont.
            MOVE TEXT-019 TO ls_out-sgtxt.
            ls_out-color = zfi_000_cl02=>mc_color.
            IF ls_devir-wrbtrs > ls_devir-wrbtrh.
              ls_out-wrbtrs = ls_devir-wrbtrs - ls_devir-wrbtrh.
              ls_out-totals = ls_devir-wrbtrs - ls_devir-wrbtrh.
              ls_out-waers  = ls_devir-waers.
            ELSEIF ls_devir-wrbtrs < ls_devir-wrbtrh.
              ls_out-wrbtrh = ls_devir-wrbtrh - ls_devir-wrbtrs.
              ls_out-totalh = ls_devir-wrbtrh - ls_devir-wrbtrs.
              ls_out-waers  = ls_devir-waers.
            ELSE.
              ls_out-wrbtrs = 0.
              ls_out-wrbtrh = 0.
              ls_out-waers  = ls_devir-waers.
            ENDIF.

            IF ls_devir-dmbtrs > ls_devir-dmbtrh.
              ls_out-dmbtrs = ls_devir-dmbtrs - ls_devir-dmbtrh.
              ls_out-totals_d = ls_devir-dmbtrs - ls_devir-dmbtrh.
            ELSEIF ls_devir-dmbtrs < ls_devir-dmbtrh.
              ls_out-dmbtrh = ls_devir-dmbtrh - ls_devir-dmbtrs.
              ls_out-totalh_d = ls_devir-dmbtrh - ls_devir-dmbtrs.
            ELSE.
              ls_out-dmbtrs = 0.
              ls_out-dmbtrh = 0.
            ENDIF.

            IF NOT ( ls_out-totals = 0 AND
                     ls_out-totalh = 0 AND
                     ls_out-totalh_d = 0 AND
                     ls_out-totals_d = 0 ) .
              APPEND ls_out TO mt_out.
            ENDIF.
*-----------altınada son durum ekleniyor
            CLEAR: ls_out-wrbtrs,
                   ls_out-wrbtrh,
                   ls_out-dmbtrs,
                   ls_out-dmbtrh.
            MOVE TEXT-020 TO ls_out-sgtxt.
            ls_out-color = zfi_000_cl02=>mc_color1.
            IF NOT ( ls_out-totals = 0 AND
                     ls_out-totalh = 0 AND
                     ls_out-totalh_d = 0 AND
                     ls_out-totals_d = 0 ) .
              APPEND ls_out TO mt_out.
            ENDIF.
          ENDLOOP.
*        ENDLOOP.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
    me->get_name( ).
  ENDMETHOD.


  METHOD fill_tout_with_odk.
    DATA: ls_fark1 TYPE rf42b-saldo,
          ls_fark2 TYPE rf42b-saldo,
          ls_total TYPE rf42b-saldo.

    DATA: ls_fark1_d TYPE rf42b-saldo,
          ls_fark2_d TYPE rf42b-saldo,
          ls_total_d TYPE rf42b-saldo.

    DATA: lv_buzei TYPE buzei,
          lv_docln TYPE zfi_001_t01-docln.
    DATA: ls_out     LIKE LINE OF mt_out,
          ls_bakiye  LIKE LINE OF mt_bakiye,
          ls_bakiye2 LIKE LINE OF mt_bakiye2.

    DATA ls_hkont LIKE LINE OF mt_hkont.
    DATA ls_waers2 LIKE LINE OF mt_waers2 .
    DATA ls_devir LIKE LINE OF mt_devir .

    CLEAR: ls_out, ls_bakiye, ls_bakiye2.
    FREE : mt_out, mt_bakiye, mt_bakiye2.

    DATA: ls_itab LIKE LINE OF mt_itab.
    DATA: ls_itab2 LIKE LINE OF mt_itab.
    DATA: lv_ref_hkont TYPE hkont.

    DATA: lt_journal TYPE SORTED TABLE OF zfi_001_t02
              WITH NON-UNIQUE KEY bukrs belnr gjahr buzei.
    DATA ls_journal LIKE LINE OF lt_journal.
    IF mt_itab[] IS NOT INITIAL.
      SELECT * INTO TABLE lt_journal FROM zfi_001_t02
                FOR ALL ENTRIES IN mt_itab
                             WHERE bukrs = mt_itab-bukrs
                               AND belnr = mt_itab-belnr
                               AND gjahr = mt_itab-gjahr.

      SELECT * INTO TABLE @DATA(lt_journal_2)
         FROM zfi_001_t04
             FOR ALL ENTRIES IN @mt_itab
                WHERE bukrs = @mt_itab-bukrs
                  AND belnr = @mt_itab-belnr
                  AND gjahr = @mt_itab-gjahr.
      SORT lt_journal_2 BY bukrs belnr gjahr.

    ENDIF.

    LOOP AT mt_hkont INTO ls_hkont.

* ODK için
      me->get_mutabakat_hesap_no(
        EXPORTING
          hkont   = ls_hkont-hkont   " Defteri kebir muhasebesi ana hesabı
          bukrs   = ls_hkont-bukrs   " Şirket kodu
          ktopl   = zfi_000_cl02=>mc_ktopl            " 3 baytlık alan
          koart   = koart            " Hesap türü
          umskz   = ls_hkont-umskz   " Özel defteri kebir göstergesi
        CHANGING
          c_hkont = lv_ref_hkont  " Defteri kebir muhasebesi ana hesabı
      ).

**
      me->sort_waers_odk(
        EXPORTING
          hkont   =  ls_hkont-hkont               " Defteri kebir muhasebesi ana hesabı
          bukrs   =  ls_hkont-bukrs             " Şirket kodu
          waers   =  ls_hkont-waers              " Para birimi anahtarı
          umskz   =  ls_hkont-umskz              " Özel defteri kebir göstergesi

      ).



      LOOP AT mt_waers2 INTO ls_waers2 WHERE bukrs = ls_hkont-bukrs
                       AND  waers = ls_hkont-waers
                       AND  hkont = ls_hkont-hkont
                       AND  umskz = ls_hkont-umskz.

        LOOP AT mt_itab INTO ls_itab WHERE bukrs = ls_waers2-bukrs
                       AND waers = ls_waers2-waers
                       AND hkont = ls_waers2-hkont
                       AND umskz = ls_waers2-umskz.

          ls_itab2 = ls_itab.

*-------waers değiştiğinde
*        at new waers.
          AT NEW umskz.

            ls_itab = ls_itab2.

            LOOP AT mt_devir INTO ls_devir WHERE bukrs = ls_itab-bukrs
                              AND hkont = ls_itab-hkont
                              AND waers = ls_itab-waers
                              AND umskz = ls_itab-umskz.
              CLEAR ls_out.
              ls_out-bukrs = ls_itab-bukrs.
              ls_out-hkont = ls_itab-hkont.
              MOVE TEXT-019 TO ls_out-sgtxt.
              ls_out-color = zfi_000_cl02=>mc_color.

              ls_out-belnr = lv_ref_hkont.

              ls_out-umskz = ls_itab-umskz.

              IF ls_devir-wrbtrs > ls_devir-wrbtrh.
                ls_out-wrbtrs = ls_devir-wrbtrs - ls_devir-wrbtrh.
                ls_out-totals = ls_devir-wrbtrs - ls_devir-wrbtrh.
                ls_out-waers = ls_devir-waers.
              ELSEIF ls_devir-wrbtrs < ls_devir-wrbtrh.
                ls_out-wrbtrh = ls_devir-wrbtrh - ls_devir-wrbtrs.
                ls_out-totalh = ls_devir-wrbtrh - ls_devir-wrbtrs.
                ls_out-waers = ls_devir-waers.
              ELSE.
                ls_out-wrbtrs = 0.
                ls_out-wrbtrh = 0.
                ls_out-waers = ls_devir-waers.
              ENDIF.

              IF ls_devir-dmbtrs > ls_devir-dmbtrh.
                ls_out-dmbtrs = ls_devir-dmbtrs - ls_devir-dmbtrh.
                ls_out-totals_d = ls_devir-dmbtrs - ls_devir-dmbtrh.
              ELSEIF ls_devir-dmbtrs < ls_devir-dmbtrh.
                ls_out-dmbtrh = ls_devir-dmbtrh - ls_devir-dmbtrs.
                ls_out-totalh_d = ls_devir-dmbtrh - ls_devir-dmbtrs.
              ELSE.
                ls_out-dmbtrs = 0.
                ls_out-dmbtrh = 0.
              ENDIF.

              APPEND ls_out TO mt_out.
            ENDLOOP.
            IF sy-subrc NE 0.
              CLEAR ls_out.
              ls_out-umskz = ls_itab-umskz.

              ls_out-belnr = lv_ref_hkont.

              ls_out-bukrs = ls_waers2-bukrs.
              ls_out-hkont = ls_waers2-hkont.
              ls_out-waers = ls_waers2-waers.
              MOVE TEXT-019 TO ls_out-sgtxt.
              ls_out-color = zfi_000_cl02=>mc_color.
              APPEND ls_out TO mt_out.
            ENDIF.
          ENDAT.

          CLEAR: ls_out.

          "etosun
          CLEAR : ls_itab-line_number,ls_itab-journal_number,lv_buzei.
          lv_docln = ls_itab-buzei.
          CONCATENATE TEXT-021 lv_docln INTO lv_docln."""farklımı bak?

*        select single line_number journal_number
*                 into (itab-line_number,itab-journal_number )
*                 from zldg_line_num
*                where bukrs = itab-bukrs
*                  and belnr = itab-belnr
*                  and gjahr = itab-gjahr
*                  and docln = lv_docln.
*        if sy-subrc is not initial.
*          lv_buzei = itab-buzei.
*
*          select single line_number journal_number
*                   into (itab-line_number,itab-journal_number )
*                   from zldg_line_num
*                  where bukrs = itab-bukrs
*                    and belnr = itab-belnr
*                    and gjahr = itab-gjahr
*                    and docln = lv_buzei.
*        endif.

          READ TABLE lt_journal INTO ls_journal WITH TABLE KEY bukrs = ls_itab-bukrs
                                               belnr = ls_itab-belnr
                                               gjahr = ls_itab-gjahr
                                               buzei = lv_docln.
          IF sy-subrc EQ 0.
            ls_itab-line_number = ls_journal-vzahl_item.
            ls_itab-journal_number = ls_journal-vzahl.
          ELSE.

            READ TABLE lt_journal_2 INTO DATA(ls_journal_2)
               WITH KEY bukrs = ls_itab-bukrs
                        belnr = ls_itab-belnr
                        gjahr = ls_itab-gjahr
                        BINARY SEARCH.
            IF sy-subrc = 0.

              ls_itab-journal_number = ls_journal_2-vzahl.

            ENDIF.
          ENDIF.

**********************************************************************
*** added by gsumengen req num 7100002417 req by moz.
*     CLEAR ls_line.
*     IF itab-line_number IS INITIAL AND itab-journal_number IS INITIAL.
*        call function 'ZFI_YEVMIYE_NO'
*          exporting
*            i_bukrs        = itab-bukrs
*            i_belnr        = itab-belnr
*            i_period       = itab-budat(6)
*            i_buzei        = itab-buzei
*         IMPORTING
*           E_LINE         = ls_line
*                  .
*        IF sy-subrc = 0.
*          itab-line_number = ls_line-line_number.
*          itab-journal_number = ls_line-journal_number.
*        ENDIF.
*     ENDIF.
***

          MOVE-CORRESPONDING ls_itab TO ls_out.


          CLEAR ls_bakiye.
*---bakiyeler bukrs, kunnr, waers bazında collect ediliyor.
          MOVE: ls_itab-bukrs  TO ls_bakiye-bukrs,
                ls_itab-hkont  TO ls_bakiye-hkont,
                ls_itab-waers  TO ls_bakiye-waers,
                ls_itab-wrbtrs TO ls_bakiye-wrbtrs,
                ls_itab-wrbtrh TO ls_bakiye-wrbtrh,
                ls_itab-dmbtrs TO ls_bakiye-dmbtrs,
                ls_itab-dmbtrh TO ls_bakiye-dmbtrh.

          ls_bakiye-umskz = ls_itab-umskz.


          COLLECT ls_bakiye INTO mt_bakiye.

*---yukarıda collect edilen bakiyeler çekiliyor.
          CLEAR ls_bakiye.
          READ TABLE mt_bakiye INTO ls_bakiye WITH KEY bukrs = ls_itab-bukrs
                                       hkont = ls_itab-hkont
                                       waers = ls_itab-waers
                                       umskz = ls_itab-umskz.
          IF sy-subrc = 0.
            ls_fark1 = ls_bakiye-wrbtrs - ls_bakiye-wrbtrh.
          ELSE.
            ls_fark1 = 0.
          ENDIF.
          IF sy-subrc = 0.
            ls_fark1_d = ls_bakiye-dmbtrs - ls_bakiye-dmbtrh.
          ELSE.
            ls_fark1_d = 0.
          ENDIF.
*---devir bakiyesi okunuyor.
          CLEAR ls_devir.
          READ TABLE mt_devir INTO ls_devir WITH KEY bukrs = ls_itab-bukrs
                                      hkont = ls_itab-hkont
                                      waers = ls_itab-waers
                                      umskz = ls_itab-umskz.

          IF sy-subrc EQ 0.
            ls_fark2 = ls_devir-wrbtrs - ls_devir-wrbtrh.
          ELSE.
            ls_fark2 = 0.
          ENDIF.
          IF sy-subrc EQ 0.
            ls_fark2_d = ls_devir-dmbtrs - ls_devir-dmbtrh.
          ELSE.
            ls_fark2_d = 0.
          ENDIF.

          ls_total = ls_fark1 + ls_fark2.
          IF ls_total GE 0.
            ls_out-totals = ls_total.
            ls_out-totalh = abap_false.
          ELSE.
            ls_out-totalh = 0 - ls_total.
            ls_out-totals  = abap_false.
          ENDIF.

          ls_total_d = ls_fark1_d + ls_fark2_d.
          IF ls_total_d GE 0.
            ls_out-totals_d = ls_total_d.
            ls_out-totalh_d = abap_false.
          ELSE.
            ls_out-totalh_d = 0 - ls_total_d.
            ls_out-totals_d  = abap_false.
          ENDIF.

          APPEND ls_out TO mt_out.

*        at end of waers.
          AT END OF umskz.

            ls_itab = ls_itab2.

            CLEAR: ls_bakiye2.
            FREE : mt_bakiye2.
            LOOP AT mt_bakiye INTO ls_bakiye WHERE bukrs = ls_itab-bukrs
                               AND hkont = ls_itab-hkont
                               AND waers = ls_itab-waers
                               AND umskz = ls_itab-umskz.

              CLEAR: ls_bakiye2.
              MOVE-CORRESPONDING ls_bakiye TO ls_bakiye2.
              COLLECT ls_bakiye2 INTO mt_bakiye2.
            ENDLOOP.

            LOOP AT mt_devir INTO ls_devir WHERE bukrs = ls_itab-bukrs
                              AND hkont = ls_itab-hkont
                              AND waers = ls_itab-waers
                              AND umskz = ls_itab-umskz.

              MOVE-CORRESPONDING ls_devir TO ls_bakiye2.
              COLLECT ls_bakiye2 INTO mt_bakiye2.
            ENDLOOP.

            LOOP AT mt_bakiye2  INTO ls_bakiye2 WHERE bukrs = ls_itab-bukrs
                                AND hkont = ls_itab-hkont
                                AND waers = ls_itab-waers
                                AND umskz = ls_itab-umskz.

              CLEAR ls_out.
              ls_out-bukrs = ls_bakiye2-bukrs.
              ls_out-hkont = ls_bakiye2-hkont.
              MOVE TEXT-020 TO ls_out-sgtxt.
              ls_out-color = zfi_000_cl02=>mc_color1.
              ls_out-umskz = ls_itab-umskz.
              ls_out-waers = ls_bakiye2-waers.


              IF ls_bakiye2-wrbtrs >= ls_bakiye2-wrbtrh.
                ls_out-totals = ls_bakiye2-wrbtrs - ls_bakiye2-wrbtrh.
              ELSE.
                ls_out-totalh = ls_bakiye2-wrbtrh - ls_bakiye2-wrbtrs.
              ENDIF.

              IF ls_bakiye2-dmbtrs >= ls_bakiye2-dmbtrh.
                ls_out-totals_d = ls_bakiye2-dmbtrs - ls_bakiye2-dmbtrh.
              ELSE.
                ls_out-totalh_d = ls_bakiye2-dmbtrh - ls_bakiye2-dmbtrs.
              ENDIF.

              APPEND ls_out TO mt_out.
            ENDLOOP.
          ENDAT.
        ENDLOOP.
        IF sy-subrc NE 0.
*-----devirden gelen ama o ay içinde hareket görmemiş para birimleri
*        LOOP AT twaers3.



          LOOP AT mt_devir INTO ls_devir WHERE bukrs = ls_waers2-bukrs
                            AND hkont = ls_waers2-hkont
                            AND waers = ls_waers2-waers
                            AND umskz = ls_waers2-umskz.

            CLEAR ls_out.
            ls_out-bukrs = ls_devir-bukrs.
            ls_out-hkont = ls_devir-hkont.
            MOVE TEXT-019 TO ls_out-sgtxt.
            ls_out-color = zfi_000_cl02=>mc_color.
            ls_out-umskz = ls_devir-umskz.

            ls_out-belnr = lv_ref_hkont.


            IF ls_devir-wrbtrs > ls_devir-wrbtrh.
              ls_out-wrbtrs = ls_devir-wrbtrs - ls_devir-wrbtrh.
              ls_out-totals = ls_devir-wrbtrs - ls_devir-wrbtrh.
              ls_out-waers  = ls_devir-waers.
            ELSEIF ls_devir-wrbtrs < ls_devir-wrbtrh.
              ls_out-wrbtrh = ls_devir-wrbtrh - ls_devir-wrbtrs.
              ls_out-totalh = ls_devir-wrbtrh - ls_devir-wrbtrs.
              ls_out-waers  = ls_devir-waers.
            ELSE.
              ls_out-wrbtrs = 0.
              ls_out-wrbtrh = 0.
              ls_out-waers  = ls_devir-waers.
            ENDIF.

            IF ls_devir-dmbtrs > ls_devir-dmbtrh.
              ls_out-dmbtrs = ls_devir-dmbtrs - ls_devir-dmbtrh.
              ls_out-totals_d = ls_devir-dmbtrs - ls_devir-dmbtrh.
            ELSEIF ls_devir-dmbtrs < ls_devir-dmbtrh.
              ls_out-dmbtrh = ls_devir-dmbtrh - ls_devir-dmbtrs.
              ls_out-totalh_d = ls_devir-dmbtrh - ls_devir-dmbtrs.
            ELSE.
              ls_out-dmbtrs = 0.
              ls_out-dmbtrh = 0.
            ENDIF.

            IF NOT ( ls_out-totals = 0 AND
                     ls_out-totalh = 0 AND
                     ls_out-totals_d = 0 AND
                     ls_out-totalh_d = 0 ) .
              APPEND ls_out TO mt_out.
            ENDIF.
*-----------altınada son durum ekleniyor
            CLEAR: ls_out-wrbtrs,
                   ls_out-wrbtrh,
                   ls_out-dmbtrs,
                   ls_out-dmbtrh.

            ls_out-umskz = ls_devir-umskz.
            MOVE TEXT-020 TO ls_out-sgtxt.
            ls_out-color = zfi_000_cl02=>mc_color1.
            IF NOT ( ls_out-totals = 0 AND
                     ls_out-totalh = 0 AND
                     ls_out-totals_d = 0 AND
                     ls_out-totalh_d = 0 ) .
              APPEND ls_out TO mt_out.
            ENDIF.
          ENDLOOP.
*        ENDLOOP.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
    me->get_name( ).


  ENDMETHOD.


  METHOD get_acdoca.


    SELECT acdoca~rbukrs AS bukrs,
           acdoca~belnr,
           acdoca~ryear AS gjahr,
           acdoca~buzei,
           acdoca~racct AS hkont,
           acdoca~rwcur AS waers,
           acdoca~umskz,
           bkpf~bktxt,
           acdoca~budat,
           acdoca~bldat,
           acdoca~zuonr,
           acdoca~rbusa AS gsber,
           acdoca~prctr,
           acdoca~rcntr AS kostl,
           acdoca~blart,
           acdoca~mwskz,
           acdoca~paobjnr,
           bkpf~xblnr,
           acdoca~racct AS saknr,
*           acdoca~wsl AS wrbtr,
           CASE WHEN acdoca~drcrk EQ @zfi_000_cl02=>mc_drcrk_s THEN acdoca~wsl
                                         ELSE CAST( ( acdoca~wsl * -1 ) AS DEC( 25 , 2 ) )  END AS wrbtr,
           acdoca~drcrk AS shkzg,
           acdoca~sgtxt,
           acdoca~matnr,
           acdoca~racct AS ref_hkont,
           zfi_001_t03~vzahl,
*           acdoca~WAERS_BLG,
           bseg~xref1,
           bseg~xref2,
*           bseg~faedt,
           acdoca~augbl,
           bkpf~stblg,
           bkpf~xref1_hd,
           acdoca~augdt,
           bseg~xnegp,
           CASE WHEN acdoca~drcrk EQ @zfi_000_cl02=>mc_drcrk_s THEN acdoca~wsl
                                         ELSE 0 END AS wrbtrs,
           CASE WHEN acdoca~drcrk EQ @zfi_000_cl02=>mc_drcrk_s THEN acdoca~hsl
                                         ELSE 0 END AS dmbtrs,
           CASE WHEN acdoca~drcrk EQ @zfi_000_cl02=>mc_drcrk_h THEN CAST( ( acdoca~wsl * -1 ) AS DEC( 25 , 2 ) )
                                         ELSE 0 END AS wrbtrh,
           CASE WHEN acdoca~drcrk EQ @zfi_000_cl02=>mc_drcrk_h THEN CAST( ( acdoca~hsl * -1 ) AS DEC( 25 , 2 ) )
                                         ELSE 0 END AS dmbtrh
      FROM acdoca
           INNER JOIN bseg ON acdoca~rbukrs = bseg~bukrs
                          AND acdoca~belnr  = bseg~belnr
                          AND acdoca~ryear  = bseg~gjahr
                          AND acdoca~buzei  = bseg~buzei
           INNER JOIN bkpf ON bseg~bukrs = bkpf~bukrs
                          AND bseg~belnr = bkpf~belnr
                          AND bseg~gjahr = bkpf~gjahr
           LEFT JOIN zfi_001_t03 ON acdoca~rbukrs = zfi_001_t03~bukrs
                                AND acdoca~belnr  = zfi_001_t03~belnr
                                AND acdoca~ryear  = zfi_001_t03~gjahr
      INTO CORRESPONDING FIELDS OF TABLE @mt_itab
     WHERE acdoca~rldnr  EQ @gs_range-p_rldnr
       AND acdoca~rbukrs IN @gs_range-s_bukrs
       AND acdoca~racct  IN @gs_range-s_hkont
       AND acdoca~budat  IN @gs_range-s_budat
       AND acdoca~rbusa  IN @gs_range-s_gsber
       AND acdoca~blart  IN @gs_range-s_blart
       AND acdoca~prctr  IN @gs_range-s_prctr. "#EC CI_DB_OPERATION_OK[2431747]




    SELECT acdoca~rbukrs AS bukrs,
           acdoca~belnr,
           acdoca~ryear AS gjahr,
           acdoca~buzei,
           acdoca~racct AS hkont,
           acdoca~rwcur AS waers,
           acdoca~umskz,
           bkpf~bktxt,
           acdoca~budat,
           acdoca~bldat,
           acdoca~zuonr,
           acdoca~rbusa AS gsber,
           acdoca~prctr,
           acdoca~rcntr AS kostl,
           acdoca~blart,
           acdoca~mwskz,
           acdoca~paobjnr,
           bkpf~xblnr,
           acdoca~racct AS saknr,
*           acdoca~wsl AS wrbtr,
           CASE WHEN acdoca~drcrk EQ @zfi_000_cl02=>mc_drcrk_s THEN acdoca~wsl
                                         ELSE CAST( ( acdoca~wsl * -1 ) AS DEC( 25 , 2 ) )  END AS wrbtr,
           acdoca~drcrk AS shkzg,
           acdoca~sgtxt,
           acdoca~matnr,
           acdoca~racct AS ref_hkont,
           zfi_001_t03~vzahl,
*           acdoca~WAERS_BLG,
           bseg~xref1,
           bseg~xref2,
*           bseg~faedt,
           acdoca~augbl,
           bkpf~stblg,
           bkpf~xref1_hd,
           acdoca~augdt,
           bseg~xnegp,
           CASE WHEN acdoca~drcrk EQ @zfi_000_cl02=>mc_drcrk_s THEN acdoca~wsl
                                         ELSE 0 END AS wrbtrs,
           CASE WHEN acdoca~drcrk EQ @zfi_000_cl02=>mc_drcrk_s THEN acdoca~hsl
                                         ELSE 0 END AS dmbtrs,
           CASE WHEN acdoca~drcrk EQ @zfi_000_cl02=>mc_drcrk_h THEN CAST( ( acdoca~wsl * -1 ) AS DEC( 25 , 2 ) )
                                         ELSE 0 END AS wrbtrh,
           CASE WHEN acdoca~drcrk EQ @zfi_000_cl02=>mc_drcrk_h THEN CAST( ( acdoca~hsl * -1 ) AS DEC( 25 , 2 ) )
                                         ELSE 0 END AS dmbtrh
      FROM acdoca
           INNER JOIN bseg_add AS bseg ON acdoca~rbukrs = bseg~bukrs
                          AND acdoca~belnr  = bseg~belnr
                          AND acdoca~ryear  = bseg~gjahr
                          AND acdoca~docln  = bseg~buzeı
           INNER JOIN bkpf ON bseg~bukrs = bkpf~bukrs
                          AND bseg~belnr = bkpf~belnr
                          AND bseg~gjahr = bkpf~gjahr
           LEFT JOIN zfi_001_t03 ON acdoca~rbukrs = zfi_001_t03~bukrs
                                AND acdoca~belnr  = zfi_001_t03~belnr
                                AND acdoca~ryear  = zfi_001_t03~gjahr
      APPENDING CORRESPONDING FIELDS OF TABLE @mt_itab
     WHERE acdoca~rldnr  EQ @gs_range-p_rldnr
       AND acdoca~rbukrs IN @gs_range-s_bukrs
       AND acdoca~racct  IN @gs_range-s_hkont
       AND acdoca~budat  IN @gs_range-s_budat
       AND acdoca~rbusa  IN @gs_range-s_gsber
       AND acdoca~blart  IN @gs_range-s_blart
       AND acdoca~prctr  IN @gs_range-s_prctr.

*-> begin - AATAN / 06.12.2021
    IF NOT mt_itab[] IS INITIAL.
      SELECT DISTINCT
        ce4c~paobjnr,
        ce4c~kmland,
        t005t~landx AS kmland_dsc,
        ce4c~ww028,
        t25b0~bezek AS ww028_dsc,
        ce4c~ww026,
        t25a8~bezek AS ww026_dsc,
        ce4c~ww027,
        t25a9~bezek AS ww027_dsc,
        ce4c~kndnr,
        kna1~name1 AS kndnr_dsc
          FROM ce4cg00_acct AS ce4c
           LEFT OUTER JOIN t005t ON t005t~spras EQ @sy-langu
                                AND t005t~land1 EQ ce4c~kmland
           LEFT OUTER JOIN t25b0 ON t25b0~spras EQ @sy-langu
                                AND t25b0~ww028 EQ ce4c~ww028
           LEFT OUTER JOIN t25a8 ON t25a8~spras EQ @sy-langu
                                AND t25a8~ww026 EQ ce4c~ww026
           LEFT OUTER JOIN t25a9 ON t25a9~spras EQ @sy-langu
                                AND t25a9~ww027 EQ ce4c~ww027
           LEFT OUTER JOIN kna1 ON kna1~kunnr EQ ce4c~kndnr
            FOR ALL ENTRIES IN @mt_itab
              WHERE paobjnr EQ @mt_itab-paobjnr
                INTO TABLE @DATA(t_ce4cdat).
      SORT t_ce4cdat BY paobjnr.
    ENDIF.
    LOOP AT mt_itab ASSIGNING FIELD-SYMBOL(<fs_itab>).
      READ TABLE t_ce4cdat REFERENCE INTO DATA(r_ce4cdat) WITH KEY paobjnr = <fs_itab>-paobjnr BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        MOVE-CORRESPONDING r_ce4cdat->* TO <fs_itab>.
      ENDIF.
    ENDLOOP.
*<- end   - AATAN / 06.12.2021

  ENDMETHOD.


  METHOD get_acdoca_devir.


    SELECT acdoca~rbukrs AS bukrs,
           acdoca~belnr,
           acdoca~ryear AS gjahr,
           acdoca~buzei,
           acdoca~racct AS hkont,
           acdoca~rwcur AS waers,
           acdoca~umskz,
           bkpf~bktxt,
           acdoca~budat,
           acdoca~bldat,
           acdoca~zuonr,
           acdoca~rbusa AS gsber,
           acdoca~prctr,
           acdoca~rcntr AS kostl,
           acdoca~blart,
           bkpf~xblnr,
           acdoca~racct AS saknr,
*           acdoca~wsl AS wrbtr,
           CASE WHEN acdoca~drcrk EQ @ZFI_000_CL02=>mc_drcrk_s THEN acdoca~wsl
                                         ELSE CAST( ( acdoca~wsl * -1 ) AS DEC( 25 , 2 ) )  END AS wrbtr,
           acdoca~drcrk AS shkzg,
           acdoca~sgtxt,
           acdoca~matnr,
           acdoca~racct AS ref_hkont,
           zfi_001_t03~vzahl,
*           acdoca~WAERS_BLG,
           bseg~xref1,
           bseg~xref2,
*           bseg~faedt,
           acdoca~augbl,
           bkpf~stblg,
           bkpf~xref1_hd,
           acdoca~augdt,
           bseg~xnegp,
           CASE WHEN acdoca~drcrk EQ @ZFI_000_CL02=>mc_drcrk_s THEN acdoca~wsl
                                         ELSE 0 END AS wrbtrs,
           CASE WHEN acdoca~drcrk EQ @ZFI_000_CL02=>mc_drcrk_s THEN acdoca~hsl
                                         ELSE 0 END AS dmbtrs,
           CASE WHEN acdoca~drcrk EQ @ZFI_000_CL02=>mc_drcrk_h THEN CAST( ( acdoca~wsl * -1 ) AS DEC( 25 , 2 ) )
                                         ELSE 0 END AS wrbtrh,
           CASE WHEN acdoca~drcrk EQ @ZFI_000_CL02=>mc_drcrk_h THEN CAST( ( acdoca~hsl * -1 ) AS DEC( 25 , 2 ) )
                                         ELSE 0 END AS dmbtrh
      FROM acdoca
           INNER JOIN bseg ON acdoca~rbukrs = bseg~bukrs
                          AND acdoca~belnr  = bseg~belnr
                          AND acdoca~ryear  = bseg~gjahr
                          AND acdoca~buzei  = bseg~buzei
           INNER JOIN bkpf ON bseg~bukrs = bkpf~bukrs
                          AND bseg~belnr = bkpf~belnr
                          AND bseg~gjahr = bkpf~gjahr
           LEFT JOIN zfi_001_t03 ON acdoca~rbukrs = zfi_001_t03~bukrs
                                AND acdoca~belnr  = zfi_001_t03~belnr
                                AND acdoca~ryear  = zfi_001_t03~gjahr
      INTO TABLE @DATA(lt_itab)
     WHERE acdoca~rldnr  EQ @gs_range-p_rldnr
       AND acdoca~rbukrs IN @gs_range-s_bukrs
       AND acdoca~racct  IN @gs_range-s_hkont
       AND acdoca~budat  LE @iv_keydt
       AND ( acdoca~augdt GT @iv_keydt
        OR   acdoca~augdt EQ @ZFI_000_CL02=>MC_AUGDT_00000000 )
       AND acdoca~rbusa  IN @gs_range-s_gsber
       AND acdoca~blart  IN @gs_range-s_blart
       AND acdoca~prctr  IN @gs_range-s_prctr. "#EC CI_DB_OPERATION_OK[2431747]


    SELECT acdoca~rbukrs AS bukrs,
           acdoca~belnr,
           acdoca~ryear AS gjahr,
           acdoca~buzei,
           acdoca~racct AS hkont,
           acdoca~rwcur AS waers,
           acdoca~umskz,
           bkpf~bktxt,
           acdoca~budat,
           acdoca~bldat,
           acdoca~zuonr,
           acdoca~rbusa AS gsber,
           acdoca~prctr,
           acdoca~rcntr AS kostl,
           acdoca~blart,
           bkpf~xblnr,
           acdoca~racct AS saknr,
*           acdoca~wsl AS wrbtr,
           CASE WHEN acdoca~drcrk EQ @ZFI_000_CL02=>mc_drcrk_s THEN acdoca~wsl
                                         ELSE CAST( ( acdoca~wsl * -1 ) AS DEC( 25 , 2 ) )  END AS wrbtr,
           acdoca~drcrk AS shkzg,
           acdoca~sgtxt,
           acdoca~matnr,
           acdoca~racct AS ref_hkont,
           zfi_001_t03~vzahl,
*           acdoca~WAERS_BLG,
           bseg~xref1,
           bseg~xref2,
*           bseg~faedt,
           acdoca~augbl,
           bkpf~stblg,
           bkpf~xref1_hd,
           acdoca~augdt,
           bseg~xnegp,
           CASE WHEN acdoca~drcrk EQ @ZFI_000_CL02=>mc_drcrk_s THEN acdoca~wsl
                                         ELSE 0 END AS wrbtrs,
           CASE WHEN acdoca~drcrk EQ @ZFI_000_CL02=>mc_drcrk_s THEN acdoca~hsl
                                         ELSE 0 END AS dmbtrs,
           CASE WHEN acdoca~drcrk EQ @ZFI_000_CL02=>mc_drcrk_h THEN CAST( ( acdoca~wsl * -1 ) AS DEC( 25 , 2 ) )
                                         ELSE 0 END AS wrbtrh,
           CASE WHEN acdoca~drcrk EQ @ZFI_000_CL02=>mc_drcrk_h THEN CAST( ( acdoca~hsl * -1 ) AS DEC( 25 , 2 ) )
                                         ELSE 0 END AS dmbtrh
      FROM acdoca
           INNER JOIN bseg_add AS bseg ON acdoca~rbukrs = bseg~bukrs
                          AND acdoca~belnr  = bseg~belnr
                          AND acdoca~ryear  = bseg~gjahr
                          AND acdoca~docln  = bseg~buzeı
           INNER JOIN bkpf ON bseg~bukrs = bkpf~bukrs
                          AND bseg~belnr = bkpf~belnr
                          AND bseg~gjahr = bkpf~gjahr
           LEFT JOIN zfi_001_t03 ON acdoca~rbukrs = zfi_001_t03~bukrs
                                AND acdoca~belnr  = zfi_001_t03~belnr
                                AND acdoca~ryear  = zfi_001_t03~gjahr
      APPENDING TABLE @lt_itab
     WHERE acdoca~rldnr  EQ @gs_range-p_rldnr
       AND acdoca~rbukrs IN @gs_range-s_bukrs
       AND acdoca~racct  IN @gs_range-s_hkont
       AND acdoca~budat  LE @iv_keydt
       AND ( acdoca~augdt GT @iv_keydt
        OR   acdoca~augdt EQ @ZFI_000_CL02=>MC_AUGDT_00000000 )
       AND acdoca~rbusa  IN @gs_range-s_gsber
       AND acdoca~blart  IN @gs_range-s_blart
       AND acdoca~prctr  IN @gs_range-s_prctr.



    LOOP AT lt_itab INTO DATA(ls_itab).
      IF gs_range-p_rb5 EQ abap_true.
        COLLECT CORRESPONDING zfi_001_s02( ls_itab ) INTO mt_devir.
      ELSE.
        COLLECT CORRESPONDING zfi_001_s02( ls_itab EXCEPT waers ) INTO mt_devir.
      ENDIF.
    ENDLOOP.


  ENDMETHOD.


  method GET_BASLIK_BILGILERI.
     CLEAR stblg.
  SELECT SINGLE stblg xref1_hd bktxt INTO (stblg,xref1_hd,bktxt)
    FROM bkpf
   WHERE bukrs EQ bukrs
     AND belnr EQ belnr
     AND gjahr EQ gjahr.

  endmethod.


  METHOD get_data_bp.

    TYPES : BEGIN OF ty_kunnr ,
              konzs TYPE kna1-konzs,
            END OF ty_kunnr.
     TYPES : BEGIN OF ty_kunnr2 ,
              kunnr TYPE kna1-kunnr,
            END OF ty_kunnr2.
    DATA ls_kunnr LIKE LINE OF gs_range-s_kunnr.
    DATA gt_kunnr TYPE TABLE OF ty_kunnr.
    DATA lt_kunnr2 TYPE TABLE OF ty_kunnr2.
    DATA ls_kunnr2 TYPE  ty_kunnr2.
    IF gs_range-p_rb1 = abap_true.
*       IF NOT gs_range-s_kunnr IS INITIAL.
*          gs_range-s_hkont = gs_range-s_hkont.
*        ENDIF.
      me->anahesap_bp( ).

    ELSEIF gs_range-p_rb2 = abap_true.

      IF gs_range-c_cust = abap_true.

        SELECT konzs FROM kna1
          INTO CORRESPONDING FIELDS OF TABLE gt_kunnr
          WHERE kunnr IN gs_range-s_kunnr
            AND konzs NE abap_false.

        ls_kunnr-sign = zfi_000_cl02=>MC_SIGN_I.
        ls_kunnr-option = zfi_000_cl02=>MC_OPTION_EQ.

        IF gt_kunnr[] IS NOT INITIAL.

          READ TABLE gs_range-s_kunnr INTO ls_kunnr INDEX 1.
          SELECT kunnr FROM kna1 INTO TABLE lt_kunnr2
            FOR ALL ENTRIES IN gt_kunnr
            WHERE konzs = gt_kunnr-konzs.

          LOOP AT lt_kunnr2 INTO ls_kunnr2.
            ls_kunnr-sign = zfi_000_cl02=>MC_SIGN_I.
            ls_kunnr-option = zfi_000_cl02=>MC_OPTION_EQ.
            ls_kunnr-low = ls_kunnr2-kunnr.
            APPEND ls_kunnr TO gs_range-s_kunnr.
          ENDLOOP.



        ENDIF.

        SORT gs_range-s_kunnr BY low.
        DELETE ADJACENT DUPLICATES FROM gs_range-s_kunnr COMPARING low.
        gs_range-s_hkont = gs_range-s_kunnr.

        me->musteri_hesap_bp( ).

      ELSE.

        IF NOT gs_range-s_kunnr IS INITIAL.
          gs_range-s_hkont = gs_range-s_kunnr.
        ENDIF.
        me->musteri_hesap_bp( ).
      ENDIF.

    ELSEIF gs_range-p_rb3 = abap_true.

      IF NOT gs_range-s_lifnr[] IS INITIAL.
        gs_range-s_hkont = gs_range-s_lifnr.
      ENDIF.

      me->satici_hesap_bp( ).

    ENDIF.


  ENDMETHOD.


  METHOD get_data_up.
    TYPES : BEGIN OF ty_buk ,
              bukrs TYPE lfb1-bukrs,
            END OF ty_buk.
    TYPES : BEGIN OF ty_lif ,
              lifnr TYPE lfa1-lifnr,
              begru TYPE lfa1-begru,
            END OF ty_lif.
    TYPES : BEGIN OF ty_hkon ,
              saknr TYPE skb1-saknr,
              begru TYPE skb1-begru,
            END OF ty_hkon.

    DATA lt_buk TYPE TABLE OF ty_buk.
    DATA lt_lif TYPE TABLE OF ty_lif.
    DATA lt_hkon TYPE TABLE OF ty_hkon.


*    me->authority_check( ).
*    DESCRIBE TABLE mt_buk LINES sy-index.
*    CHECK sy-index NE 0.
*    IF     gs_range-p_rb3 = abap_true .
*      DESCRIBE TABLE mt_lif LINES sy-index.
*      CHECK sy-index NE 0.
*    ENDIF.
*    IF    gs_range-p_rb1 = abap_true .
*      DESCRIBE TABLE mt_hkon LINES sy-index.
*      CHECK sy-index NE 0.
*    ENDIF.

    IF gs_range-p_rb1 = abap_true.

      me->anahesap( ).

    ELSEIF gs_range-p_rb2 = abap_true.

      IF NOT gs_range-s_kunnr[] IS INITIAL.
        gs_range-s_hkont[] = gs_range-s_kunnr[].
      ENDIF.
      me->musteri_hesap( ).


    ELSEIF gs_range-p_rb3 = abap_true.

      IF NOT gs_range-s_lifnr[] IS INITIAL.
        gs_range-s_hkont[] = gs_range-s_lifnr[].
      ENDIF.
      me->satici_hesap( ).

    ENDIF.
  ENDMETHOD.


  method GET_INSTANCE.
       IF mo_singleton IS INITIAL.
      CREATE OBJECT mo_singleton.
    ENDIF.
    ro_result = mo_singleton.
  endmethod.


  METHOD get_material.

    DATA ls_itab LIKE LINE OF mt_itab.
    IF NOT gs_range-s_matnr[] IS INITIAL.
      SELECT SINGLE bukrs, belnr, gjahr, buzei, matnr, umskz
        FROM epic_v_brs_bseg INTO @DATA(ls_bseg) WHERE bukrs EQ @bukrs
                                                   AND belnr EQ @belnr
                                                   AND gjahr EQ @gjahr
                                                   AND buzei EQ @buzei
                                                   AND matnr IN @gs_range-s_matnr
                                                   AND umskz IN @gs_range-s_umskz.
      CHECK sy-subrc EQ 0.
      ls_itab-matnr = ls_bseg-matnr.

    ENDIF.


  ENDMETHOD.


  method GET_MUTABAKAT_HESAP_NO.
    data ls_t074 type t074.
    data ls_lfb1 type lfb1.
    data ls_knb1 type knb1.
    CLEAR: ls_t074 , c_hkont.
  IF koart  EQ ZFI_000_CL02=>mc_koart_d.
    SELECT SINGLE * FROM knb1 into ls_knb1 WHERE kunnr EQ hkont
                                AND bukrs EQ bukrs.
    CHECK sy-subrc EQ 0.

    SELECT SINGLE * FROM t074 into ls_t074 WHERE ktopl = ktopl
                                AND koart = koart
                                AND umskz = umskz
                                AND hkont = ls_knb1-akont.

  ELSEIF koart  EQ ZFI_000_CL02=>mc_koart_k.
    SELECT SINGLE * FROM lfb1 into ls_lfb1 WHERE lifnr EQ hkont
                                AND bukrs EQ bukrs.
    CHECK sy-subrc EQ 0.
    SELECT SINGLE * FROM t074 into ls_t074  WHERE ktopl = ktopl
                                AND koart = koart
                                AND umskz = umskz
                                AND hkont = ls_lfb1-akont.
  ENDIF.

  CHECK sy-subrc EQ 0.

  c_hkont = ls_t074-skont.
  endmethod.


  METHOD get_name.
    TYPES : BEGIN OF ty_ska1,
              ktopl TYPE skat-ktopl,
              saknr TYPE skat-saknr,
              txt50 TYPE skat-txt50,
            END OF ty_ska1.
    TYPES : BEGIN OF ty_kna1,
              kunnr TYPE kna1-kunnr,
              name1 TYPE kna1-name1,
            END OF ty_kna1.
    TYPES : BEGIN OF ty_lfa1,
              lifnr TYPE lfa1-lifnr,
              name1 TYPE lfa1-name1,
            END OF ty_lfa1.

    DATA lt_ska1 TYPE SORTED TABLE OF ty_ska1 WITH NON-UNIQUE KEY ktopl saknr.
    DATA lt_kna1 TYPE SORTED TABLE OF ty_kna1 WITH NON-UNIQUE KEY kunnr.
    DATA lt_lfa1 TYPE SORTED TABLE OF ty_lfa1 WITH NON-UNIQUE KEY lifnr.
    DATA ls_ska1 TYPE ty_ska1.
    DATA ls_kna1 TYPE ty_kna1.
    DATA ls_lfa1 TYPE ty_lfa1.
    DATA: lt_t001 TYPE SORTED TABLE OF t001 WITH NON-UNIQUE KEY bukrs,
          ls_t001 TYPE t001.
    FIELD-SYMBOLS <fs> TYPE zfi_001_s04.


    IF mt_out IS NOT INITIAL.
      CASE abap_true.
        WHEN gs_range-p_rb1.
****************************************************************************
          SELECT bukrs
                 ktopl
            FROM t001
            INTO CORRESPONDING FIELDS OF TABLE lt_t001
            FOR ALL ENTRIES IN mt_out
           WHERE t001~bukrs = mt_out-bukrs.


          SELECT ktopl
                 saknr
                 txt50
           FROM skat
            INTO CORRESPONDING FIELDS OF TABLE lt_ska1
                       FOR ALL ENTRIES IN mt_out
                       WHERE skat~spras = sy-langu
                         AND skat~saknr = mt_out-hkont.
          LOOP AT mt_out ASSIGNING <fs>.
            CLEAR: ls_t001, ls_ska1.
            READ TABLE lt_t001 INTO ls_t001 WITH TABLE KEY bukrs = <fs>-bukrs.


            READ TABLE lt_ska1 INTO ls_ska1 WITH TABLE KEY ktopl = ls_t001-ktopl
                                                           saknr = <fs>-hkont.
            <fs>-name1 = ls_ska1-txt50.
          ENDLOOP.
****************************************************************************
        WHEN gs_range-p_rb2.

          SELECT kunnr
           name1
           FROM kna1 INTO CORRESPONDING FIELDS OF TABLE lt_kna1
                       FOR ALL ENTRIES IN mt_out
                       WHERE kunnr = mt_out-hkont.
          LOOP AT mt_out ASSIGNING <fs>.
            CLEAR: ls_kna1.
            READ TABLE lt_kna1 INTO ls_kna1 WITH TABLE KEY kunnr = <fs>-hkont.
            <fs>-name1 = ls_kna1-name1.
          ENDLOOP.
*****************************************************************************
        WHEN gs_range-p_rb3.
          SELECT lifnr
           name1
           FROM lfa1 INTO CORRESPONDING FIELDS OF TABLE lt_lfa1
                       FOR ALL ENTRIES IN mt_out
                       WHERE lifnr = mt_out-hkont.
          LOOP AT mt_out ASSIGNING <fs>.
            CLEAR ls_lfa1.
            READ TABLE lt_lfa1 INTO ls_lfa1 WITH TABLE KEY lifnr = <fs>-hkont.
            <fs>-name1 = ls_lfa1-name1.
          ENDLOOP.
****************************************************************************
      ENDCASE.


    ENDIF.
  ENDMETHOD.


  METHOD initialization.
    DATA  h_repid      LIKE sy-repid.
    DATA gs_usr02 TYPE usr02.

*    data: r_budat type line of DATUM_RANGE_TAB .
*    FIELD-SYMBOLS : <fs_budat> type ZBC_000_S03.
    h_repid = sy-repid.


    SELECT SINGLE * FROM usr02 INTO gs_usr02 WHERE bname = sy-uname.
    IF sy-subrc NE 0.
      gs_usr02-accnt = ' '.
    ENDIF.
    CLEAR: ms_variant.
    mv_repid              = sy-repid.
    ms_variant-report     = mv_repid.
    ms_variant-username   = sy-uname.
    mv_variant_save       = ZFI_000_CL02=>mc_i_save_a. "All types

    ms_xvariant = ms_variant.

    IF NOT cv_variant IS INITIAL.
      ms_xvariant-variant = cv_variant.
    ENDIF.

    CALL FUNCTION 'LVC_VARIANT_DEFAULT_GET'
      EXPORTING
        i_save        = mv_variant_save
      CHANGING
        cs_variant    = ms_xvariant
      EXCEPTIONS
        wrong_input   = 1
        not_found     = 2
        program_error = 3
        OTHERS        = 4.

    CASE sy-subrc.
      WHEN 0.
        cv_variant = ms_xvariant-variant.
      WHEN 2.
        CLEAR: cv_variant.
    ENDCASE.


*  r_budat-sign = 'I'.
*  r_budat-option = 'EQ' .
*  r_budat-high = sy-datum .
*  r_budat-low(6) = sy-datum(6).
*  r_budat-low+6(2) = '01' .
*
*  append r_budat to gs_range-s_budat .
*
*
*  loop at gs_range-s_budat ASSIGNING <fs_budat> .
*  <fs_budat>-sign = 'I'.
*  <fs_budat>-option = 'EQ' .
*  <fs_budat>-high = sy-datum .
*  <fs_budat>-low(6) = sy-datum(6).
*  <fs_budat>-low+6(2) = '01' .
*  ENDLOOP.
  data s_budat like line of gs_range-s_budat.
  s_budat-sign = zfi_000_cl02=>mc_sign_i.
  s_budat-option = zfi_000_cl02=>MC_OPTION_BT.
  s_budat-low(6) = sy-datum(6).
  s_budat-low+6(2) = zfi_000_cl02=>MC_LOW_01.
  s_budat-high = sy-datum .
  APPEND s_budat to gs_range-s_budat.

  ENDMETHOD.


  METHOD list_f4_for_variant.

    CALL FUNCTION 'LVC_VARIANT_F4'
      EXPORTING
        is_variant    = ms_variant
        i_save        = mv_variant_save
      IMPORTING
        e_exit        = mv_variant_exit
        es_variant    = ms_xvariant
      EXCEPTIONS
        not_found     = 1
        program_error = 2
        OTHERS        = 3.

    IF sy-subrc <> 0.
      MESSAGE i001(zpp) WITH TEXT-m01.
    ENDIF.

    IF mv_variant_exit IS INITIAL.
      ms_variant-variant = ms_xvariant-variant.
      cv_variant         = ms_xvariant-variant.
    ENDIF.

  ENDMETHOD.


  method LIST_FILL_FIELDCAT.


    data ls_fieldcat like line of t_fieldcat.
  "end 25.02.2009


  REFRESH t_fieldcat.

  me->list_merge_fieldcat(
    EXPORTING
      p_tabname  = p_tabname
    CHANGING
      t_fieldcat = t_fieldcat
  ).

  ls_fieldcat-key = space.
  MODIFY t_fieldcat from ls_fieldcat TRANSPORTING key
                            WHERE key EQ abap_true.

  endmethod.


  METHOD check_authorization.

    zfi_000_cl03=>check_authority_f_bkpf_buk(
      EXPORTING
        iv_actvt      = zfi_000_cl02=>mc_actvt_03          " Aktivite
*          iv_bukrs      = iv_bukrs
        iv_no_message = abap_false    " Tek basamaklı gösterge
      CHANGING
        ct_bukrs      = me->gs_range-s_bukrs[]
    ).

    CASE abap_true.
      WHEN me->gs_range-p_rb1.
        zfi_000_cl03=>check_authority_zfi_000_a3(
           EXPORTING
             iv_actvt      = zfi_000_cl02=>mc_actvt_03           " Aktivite
*                 iv_kunnr      = iv_kunnr
             iv_no_message = abap_false " Tek basamaklı gösterge
           CHANGING
             ct_saknr      = me->gs_range-s_hkont[]
             ).
      WHEN me->gs_range-p_rb2.

        zfi_000_cl03=>check_authority_zfi_000_a2(
           EXPORTING
             iv_actvt      = zfi_000_cl02=>mc_actvt_03           " Aktivite
*                 iv_kunnr      = iv_kunnr
             iv_no_message = abap_false " Tek basamaklı gösterge
           CHANGING
             ct_kunnr      = me->gs_range-s_kunnr[]

         ).

      WHEN me->gs_range-p_rb3.

        zfi_000_cl03=>check_authority_zfi_000_a1(
          EXPORTING
            iv_actvt      = zfi_000_cl02=>mc_actvt_03           " Aktivite
*                  iv_lifnr      = iv_lifnr
            iv_no_message = abap_false " Tek basamaklı gösterge
             CHANGING
            ct_lifnr      = me->gs_range-s_lifnr[]
        ).

    ENDCASE.

  ENDMETHOD.
ENDCLASS.
