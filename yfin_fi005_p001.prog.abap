*
* Documentation :
* Program name : YFIN_FI005_P_001
* Tcode:
* Description : Ana Hesap Ekstresi
* Author : Emir KARPUZ / emir.karpuz@finpro.com.tr

* Responsible person : Emrullah KAPLAN / emrullah.kaplan@finpro.com.tr

* R/3 Release : ECC 6.0
*=======================================================================
* Program type : Executable Program
* Package : YFIN_FI005
*-----------------------------------------------------------------------
* History of changes
*
* Date         by        <trans.num., CR-number, what has changed >
* ----------- ---------- -----------------------------------------------
* xx.xx.20xx  xxxxx      xxxKxxxxxx
************************************************************************

REPORT yfin_fi005_p001.

INCLUDE YFIN_FI005_P001_I01.
*INCLUDE YFIN_I_ALV_GLOBAL_P005  .

TABLES : bsid , bkpf , bsis, bsas .
TYPE-POOLS : icon .

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001 .
PARAMETERS:
    p_bukrs LIKE t001-bukrs OBLIGATORY MEMORY ID buk.
SELECT-OPTIONS:
    s_hkont FOR bsis-hkont .
SELECTION-SCREEN END OF BLOCK b1 .

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002 .
SELECT-OPTIONS:
    s_budat FOR bkpf-budat NO-EXTENSION OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b2 .

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003 .
SELECT-OPTIONS:   s_bldat FOR bkpf-bldat ,
   s_blart FOR bkpf-blart .
SELECTION-SCREEN END OF BLOCK b3 .

DATA : BEGIN OF it_report OCCURS 0 ,
         bukrs     LIKE knb1-bukrs,
         hkont     LIKE bsis-hkont,
         txt50     LIKE skat-txt50,
         gjahr     LIKE bsis-gjahr,
         belnr     LIKE bsis-belnr,
         xblnr     LIKE bsis-xblnr,
         buzei     LIKE bsis-buzei,
         blart     LIKE bsis-blart,
         ltext     LIKE t003t-ltext,
         sgtxt     LIKE bsis-sgtxt,
         budat     LIKE bsis-budat,
         bldat     LIKE bsis-bldat,
         wrbtr     LIKE bsis-wrbtr,
         waers     LIKE bsis-waers,
         dmbtrb    LIKE bsis-dmbtr,
         borc_bp   LIKE bsik-wrbtr,
         dmbtra    LIKE bsis-dmbtr,
         alacak_pb LIKE bsik-wrbtr,
         dmbtrn    LIKE bsis-dmbtr,
         wrbtrn    LIKE bsis-wrbtr,

       END OF it_report .


DATA: BEGIN OF it_detay OCCURS 0,

        bukrs LIKE t001-bukrs,

      END OF it_detay.

DATA: it_bsis LIKE bsis OCCURS 0 WITH HEADER LINE.
DATA: it_bsis2 LIKE bsis OCCURS 0 WITH HEADER LINE.
DATA: ht_skat LIKE HASHED TABLE OF skat
      WITH UNIQUE KEY saknr
      WITH HEADER LINE.

AT SELECTION-SCREEN.

  IF s_budat-high IS INITIAL.

    MESSAGE 'Lütfen ekstre bitiş tarihini boş geçmeyiniz' TYPE 'E'
    DISPLAY LIKE 'I'.


  ENDIF.


INITIALIZATION.


START-OF-SELECTION.

  PERFORM authority_check.

  PERFORM get_data.


  CLEAR : lt_t_fieldcatalog , lt_t_fieldcatalog[] .
  v_default_recname = 'IT_REPORT' .
  v_default_report_name = sy-repid .
  PERFORM set_report_fcat.
  PERFORM show_report_fcat TABLES it_report
                      USING  '' "P_VARI
                             gs_variant
                             v_default_report_name
                             v_default_recname .


  """" ALV FORMS HERE """"
*&---------------------------------------------------------------------*

*&      Form  SET_TOP_OF_PAGE
*&---------------------------------------------------------------------*

*       text
*----------------------------------------------------------------------*

FORM set_top_of_page.
  CLEAR : gt_list_top_of_page[] , gt_list_top_of_page .
  PERFORM comment_build USING gt_list_top_of_page[].
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
*     i_logo             = 'ENJOYSAP_LOGO'
      it_list_commentary = gt_list_top_of_page.
ENDFORM.                    "set_top_of_page
*---------------------------------------------------------------------*
*       FORM COMMENT_BUILD                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  LT_TOP_OF_PAGE                                                *
*---------------------------------------------------------------------*
FORM comment_build USING lt_top_of_page TYPE slis_t_listheader .
  DATA: ls_line TYPE slis_listheader.

*  clear : LS_LINE .
*  LS_LINE-TYP = 'S' .
*  LS_LINE-KEY = 'Şirket Kodu :' .
*  LS_LINE-INFO = p_bukrs .
*  APPEND LS_LINE TO LT_TOP_OF_PAGE.
*
*  clear : LS_LINE .
*  LS_LINE-TYP = 'S' .
*  LS_LINE-KEY = 'Dönem :' .
*  CONCATENATE p_spmon+4(2) '.' p_spmon(4) into LS_LINE-INFO .
*  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*  clear : LS_LINE .
*  LS_LINE-TYP = 'S' .
*  LS_LINE-KEY = 'Değerleme alanı :' .
*  LS_LINE-INFO = P_AFABE .
*  APPEND LS_LINE TO LT_TOP_OF_PAGE.

ENDFORM.                    "comment_build


*&---------------------------------------------------------------------*

*&      Form  SET_PF_STATUS_SET
*&---------------------------------------------------------------------*

*       text
*----------------------------------------------------------------------*

*      -->RT_EXTAB   text
*----------------------------------------------------------------------*

FORM set_pf_status_set USING rt_extab TYPE slis_t_extab .   "#EC CALLED
  PERFORM set_excluding_tab TABLES rt_extab.
  SET PF-STATUS 'STANDARD' EXCLUDING rt_extab[].
ENDFORM.                    "f01_set_status
*&---------------------------------------------------------------------*

*&      Form  excluding_events
*&---------------------------------------------------------------------*

*       text
*----------------------------------------------------------------------*

*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

FORM excluding_events.

  PERFORM exclude_events TABLES ex_events USING 'CALLER_EXIT' .
*  PERFORM exclude_events TABLES ex_events USING 'USER_COMMAND'.
*  PERFORM exclude_events TABLES ex_events USING 'TOP_OF_PAGE' .
  PERFORM exclude_events TABLES ex_events USING 'TOP_OF_COVERPAGE' .
  PERFORM exclude_events TABLES ex_events USING 'END_OF_COVERPAGE' .
  PERFORM exclude_events TABLES ex_events USING 'FOREIGN_TOP_OF_PAGE' .
  PERFORM exclude_events TABLES ex_events USING 'FOREIGN_END_OF_PAGE' .
*  PERFORM exclude_events TABLES ex_events USING 'PF_STATUS_SET' .
  PERFORM exclude_events TABLES ex_events USING 'LIST_MODIFY' .
  PERFORM exclude_events TABLES ex_events USING 'TOP_OF_LIST' .
  PERFORM exclude_events TABLES ex_events USING 'END_OF_PAGE' .
  PERFORM exclude_events TABLES ex_events USING 'END_OF_LIST' .
  PERFORM exclude_events TABLES ex_events USING 'AFTER_LINE_OUTPUT' .
  PERFORM exclude_events TABLES ex_events USING 'BEFORE_LINE_OUTPUT' .
  PERFORM exclude_events TABLES ex_events USING 'REPREP_SEL_MODIFY' .
  PERFORM exclude_events TABLES ex_events USING 'SUBTOTAL_TEXT' .
  PERFORM exclude_events TABLES ex_events USING 'GROUPLEVEL_CHANGE' .

*  PERFORM APPEND_EVENTS  TABLES AP_EVENTS USING 'DATA_CHANGED'.
*  PERFORM APPEND_EVENTS  TABLES AP_EVENTS USING 'ITEM_DATA_EXPAND'.
*  PERFORM APPEND_EVENTS  TABLES AP_EVENTS USING 'GROUPLEVEL_CHANGE'.
ENDFORM.                    " excluding_events

*&---------------------------------------------------------------------*

*&      Form  SET_EXCLUDING_TAB
*&---------------------------------------------------------------------*

*       text
*----------------------------------------------------------------------*

*      -->EXTAB      text
*----------------------------------------------------------------------*

FORM set_excluding_tab TABLES extab.
  REFRESH extab.
*  EXTAB = '&ABC'.      APPEND EXTAB.
*  extab = '&UMC'.      append extab.
*  extab = '%SL' .      append extab.
*  extab = '&SUM'.      append extab.
*  extab = '&OL0'.      append extab.
*  extab = '&OAD'.      append extab.
*  extab = '&AVE'.      append extab.
*  extab = '&ILT'.      append extab.
*  extab = '&ETA'.      append extab.
*  extab = '%PC' .      append extab.
*  extab = '&ALL'.      append extab.
*  extab = '&SAL'.      append extab.
*  EXTAB = '&EB9'.      APPEND EXTAB.
*  EXTAB = '&REFRESH'.  APPEND EXTAB.
*  extab = '&OUP'.      append extab.
*  extab = '&ODN'.      append extab.
*  extab = '&RNT_PREV'. append extab.
*  extab = '&VEXCEL'.   append extab.
*  extab = '&AOW'.      append extab.
*  EXTAB = '&GRAPH'.    APPEND EXTAB.
*  EXTAB = '&INFO'.     APPEND EXTAB.
*  EXTAB = '&DET'.     APPEND EXTAB.
*if c_sip is INITIAL .
*  EXTAB = '&ESLE'. APPEND EXTAB.
*endif .

ENDFORM.                    " set_excluding_tab

*&---------------------------------------------------------------------*

*&      Form  SET_REPORT_FCAT
*&---------------------------------------------------------------------*

*       text
*----------------------------------------------------------------------*

FORM set_report_fcat.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = v_default_report_name
      i_internal_tabname     = v_default_recname
      i_inclname             = v_default_report_name
      i_client_never_display = 'X'
      i_bypassing_buffer     = 'X'
    CHANGING
      ct_fieldcat            = lt_t_fieldcatalog[]
    EXCEPTIONS
      OTHERS                 = 3.

  PERFORM set_field_cat_user_exit.

ENDFORM.                    " set_report_fcat

*&---------------------------------------------------------------------*

*&      Form  SET_FIELD_CAT_USER_EXIT
*&---------------------------------------------------------------------*

*       text
*----------------------------------------------------------------------*

FORM set_field_cat_user_exit .
  DATA: recname TYPE slis_tabname.
  DATA : v_title(42) TYPE c.
  MOVE: 'SELTEXT_L/SELTEXT_M/SELTEXT_S/REPTEXT_DDIC' TO v_title.
  recname = v_default_recname .

*        ortvade_s(4) TYPE p DECIMALS 2,
*        ortvade_h(4) TYPE p DECIMALS 2,
*        fark(4) TYPE p DECIMALS 2,

  PERFORM

   set_line_field_cat TABLES lt_t_fieldcatalog USING :
          recname 'DMBTRA' v_title 'Alacak UPB' ,
          recname 'DMBTRB' v_title 'Borç UPB' ,
          recname 'ALACAK_PB ' v_title 'Alacak BPB' ,
          recname 'BORC_BP ' v_title 'Borç BPB' ,
*          recname 'DMBTRB' v_title 'Borç' ,
          recname 'DMBTRN' v_title 'UPB Bakiye' ,
          recname 'WRBTRN' v_title 'Belge PB Bakiye' ,
           recname 'TAHTUT' v_title 'Ödm. Tutarı',
          recname 'FARK' v_title 'Fark',
           recname 'ICON' v_title 'Durum' ,
           recname 'ICON' 'ICON' 'X' .
  .
*          recname 'Chbak' 'EMPHASIZE' color_light_green.


*  DELETE lt_t_fieldcatalog WHERE fieldname EQ 'ROWCOLOR' .




ENDFORM.                    " set_field_cat_user_exit

*&---------------------------------------------------------------------*

*&      Form  SET_LAYOUT_USER_EXIT
*&---------------------------------------------------------------------*

*       text
*----------------------------------------------------------------------*

*      -->P_PVARI    text
*      -->P_DRNAME   text
*----------------------------------------------------------------------*

FORM set_layout_user_exit USING    p_pvari
                                   p_drname .

*  GS_GRID_SET-EDT_CLL_CB = 'X'.

*  GS_LAYOUT-GET_SELINFOS       = 'X'.
*  GS_LAYOUT-COLTAB_FIELDNAME   = 'COLOR'.
*  gs_layout-coltab_fieldname   = 'COLOR'.
*  gs_layout-expand_fieldname  = 'BUKRS'.

  gs_layout-zebra = 'X' .
  gs_layout-colwidth_optimize = 'X' .
*  IF v_default_recname EQ 'IT_REPORT' OR
*      v_default_recname EQ 'IT_DETAY'.
*    gs_layout-info_fieldname = 'ROWCOLOR'.
*  GS_LAYOUT-BOX_FIELDNAME = 'SELKZ'.
*  ELSE .
*    CLEAR : gs_layout-edit , gs_layout-info_fieldname ,
*            gs_layout-box_fieldname .
*  ENDIF ..

ENDFORM.                    " set_layout_user_exit

*FORM SHOW_REPORT_FCAT_POP TABLES IT_REPORT
*               USING    PVARI
*                        GS_VARIANT
*                        DEFAULT_REPORT_NAME
*                        DEFAULT_RECNAME.
*  PERFORM LAYOUT_INIT USING GS_LAYOUT.
*  PERFORM EXCLUDING_EVENTS.
*  PERFORM EVENTTAB_BUILD USING GT_EVENTS[].
*  PERFORM SET_LAYOUT USING PVARI DEFAULT_REPORT_NAME.
*
*
*  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*    EXPORTING
**      I_BACKGROUND_ID    = 'ALV_BACKGROUND'
**        i_buffer_active    = 'X'
*         I_BYPASSING_BUFFER = 'X'
*         I_CALLBACK_PROGRAM = DEFAULT_REPORT_NAME
**         i_structure_name   = default_tab_name
*         I_GRID_SETTINGS    = GS_GRID_SET
*         IS_LAYOUT          = GS_LAYOUT
*         I_SAVE             = G_SAVE
*         IS_VARIANT         = GS_VARIANT
*         IT_EVENTS          = GT_EVENTS[]
*         IT_EXCLUDING = LT_EXCLUDING
*         IT_FIELDCAT        = LT_T_FIELDCATALOG[]
*         I_SCREEN_START_COLUMN = 5
*         I_SCREEN_START_LINE   = 2
*         I_SCREEN_END_COLUMN   = 150
*         I_SCREEN_END_LINE     = 18
*    IMPORTING
*         E_EXIT_CAUSED_BY_CALLER = G_EXIT_CAUSED_BY_CALLER
*         ES_EXIT_CAUSED_BY_USER  = GS_EXIT_CAUSED_BY_USER
*    TABLES
*         T_OUTTAB = IT_REPORT[]
*    EXCEPTIONS
*         PROGRAM_ERROR = 1
*         OTHERS        = 2.
*ENDFORM.                    " show_report_fcat



FORM set_user_command USING r_ucomm     LIKE sy-ucomm
                            rs_selfield TYPE slis_selfield. "#EC CALLED
  CASE r_ucomm .
    WHEN '&IC1' .
      rs_selfield-sel_tab_field = 'IT_REPORT-BELNR'.
      IF rs_selfield-sel_tab_field EQ 'IT_REPORT-BELNR'.

        CLEAR: it_report.
        READ TABLE it_report INDEX rs_selfield-tabindex .

        IF it_report-belnr IS INITIAL.
          MESSAGE 'Belge numarası alanı boş' TYPE 'E'.
        ENDIF.

        SET PARAMETER ID 'BUK' FIELD p_bukrs .
        SET PARAMETER ID 'BLN' FIELD it_report-belnr .
        SET PARAMETER ID 'GJR' FIELD it_report-bldat(4) .

        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN .

      ELSE.
        CLEAR : it_report.
        READ TABLE it_report INDEX rs_selfield-tabindex .
        IF sy-subrc IS INITIAL .

          CLEAR: it_detay, it_detay[].




          CLEAR : lt_t_fieldcatalog , lt_t_fieldcatalog[] .
          v_default_recname = 'IT_DETAY' .
          v_default_report_name = sy-repid .
          PERFORM set_report_fcat.
          PERFORM show_report_fcat TABLES it_detay
                              USING  '' "P_VARI
                                     gs_variant
                                     v_default_report_name
                                     v_default_recname .

        ENDIF .

      ENDIF.

  ENDCASE.
ENDFORM.                    "set_user_command

*&---------------------------------------------------------------------*
*&      Form  AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM authority_check .

  AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
           ID 'BUKRS' FIELD p_bukrs
           ID 'ACTVT' FIELD '03'.

  IF sy-subrc NE 0 .
    MESSAGE 'Bu şirket kodunda yetkiniz yoktur' TYPE 'E' .
  ENDIF.

ENDFORM.                    " AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data .

  DATA: BEGIN OF lt_skb1 OCCURS 0,
          saknr LIKE skb1-saknr,
          waers LIKE skb1-waers,
        END OF lt_skb1.

  DATA: lv_bakiye LIKE bsis-dmbtr.
  DATA: lv_bakiye_bp LIKE bsis-wrbtr.
  DATA: lv_devir LIKE bsis-dmbtr.
  DATA: lv_devir_bp LIKE bsis-wrbtr.
  DATA: lv_waers LIKE bsis-waers.
  DATA: keydate LIKE bkpf-budat.
  DATA: balance LIKE bapi1028_3.
  DATA: it_return LIKE bapireturn.

  keydate(4) = s_budat-low(4).
  keydate+4(4) = '0101'.

  keydate = keydate -  1 .

  SELECT * FROM bsis INTO TABLE it_bsis
    WHERE bukrs EQ p_bukrs AND
          hkont IN s_hkont AND
          budat IN s_budat AND
          blart IN s_blart AND
          bldat IN s_bldat.

  SELECT * FROM bsas APPENDING TABLE it_bsis
    WHERE bukrs EQ p_bukrs AND
          hkont IN s_hkont AND
          budat IN s_budat AND
          blart IN s_blart AND
          bldat IN s_bldat.

  SELECT * FROM bsis INTO TABLE it_bsis2
    WHERE bukrs EQ p_bukrs AND
          hkont IN s_hkont AND
         ( budat GT keydate AND
          budat LT s_budat-low ) .

  SELECT * FROM bsas APPENDING TABLE it_bsis2
    WHERE bukrs EQ p_bukrs AND
          hkont IN s_hkont AND
         ( budat GT keydate AND
          budat LT s_budat-low ) .

  SELECT a~* FROM acdoca AS a
    INNER JOIN bkpf AS b ON b~bukrs EQ a~rbukrs
                        AND b~belnr EQ a~belnr
                        AND b~gjahr EQ a~gjahr
                        AND b~tcode EQ 'FBB1'
                        AND b~bstat EQ 'U'
    INTO TABLE @DATA(lt_acdoca)
    WHERE a~rbukrs EQ @p_bukrs
      AND a~racct  IN @s_hkont
      AND a~budat  IN @s_budat
      AND a~blart  IN @s_blart
      AND a~bldat  IN @s_bldat
      AND a~rldnr  EQ '0L'.

  SELECT a~* FROM acdoca AS a
    INNER JOIN bkpf AS b ON b~bukrs EQ a~rbukrs
                        AND b~belnr EQ a~belnr
                        AND b~gjahr EQ a~gjahr
                        AND b~tcode EQ 'FBB1'
                        AND b~bstat EQ 'U'
    INTO TABLE @DATA(lt_acdoca2)
    WHERE a~rbukrs EQ @p_bukrs
      AND a~racct  IN @s_hkont
      AND a~budat  GT @keydate
      AND a~budat  LT @s_budat-low
      AND a~rldnr  EQ '0L'.

  SELECT * FROM skat INTO TABLE ht_skat
    WHERE ktopl EQ '1000' AND
          spras EQ sy-langu AND
          saknr IN s_hkont.

  SELECT saknr waers FROM skb1 INTO TABLE lt_skb1
    WHERE bukrs EQ p_bukrs AND
          saknr IN s_hkont.

  SORT it_bsis BY hkont budat belnr buzei.

  CLEAR lv_bakiye.

  LOOP AT ht_skat.

    CLEAR: lv_devir,lv_devir_bp, balance, it_report.

    IF line_exists( lt_skb1[ saknr = ht_skat-saknr ] ).
      lv_waers = lt_skb1[ saknr = ht_skat-saknr ]-waers.
    ENDIF.

    CALL FUNCTION 'BAPI_GL_GETGLACCBALANCE'
      EXPORTING
        companycode     = p_bukrs
        glacct          = ht_skat-saknr
        fiscalyear      = keydate(4)
        currencytype    = '10'
      IMPORTING
        account_balance = balance
        return          = it_return.

    lv_devir = balance-balance.

    LOOP AT it_bsis2 WHERE hkont EQ ht_skat-saknr.
      CASE it_bsis2-shkzg.
        WHEN 'S'.
          ADD it_bsis2-dmbtr TO lv_devir.
        WHEN 'H'.
          SUBTRACT it_bsis2-dmbtr FROM lv_devir.
      ENDCASE.
    ENDLOOP.

    LOOP AT lt_acdoca2 REFERENCE INTO DATA(lr_acdoca2)
                       WHERE racct EQ ht_skat-saknr.
      ADD lr_acdoca2->hsl TO lv_devir.
    ENDLOOP.

    IF lv_waers NE 'TRY'.
      CLEAR: balance.
      CALL FUNCTION 'BAPI_GL_GETGLACCBALANCE'
        EXPORTING
          companycode     = p_bukrs
          glacct          = ht_skat-saknr
          fiscalyear      = keydate(4)
          currencytype    = '00'
        IMPORTING
          account_balance = balance
          return          = it_return.

      lv_devir_bp = balance-balance.

      LOOP AT it_bsis2 WHERE hkont EQ ht_skat-saknr.
        CASE it_bsis2-shkzg.
          WHEN 'S'.
            ADD it_bsis2-wrbtr TO lv_devir_bp.
          WHEN 'H'.
            SUBTRACT it_bsis2-wrbtr FROM lv_devir_bp.
        ENDCASE.
      ENDLOOP.

      LOOP AT lt_acdoca2 REFERENCE INTO lr_acdoca2
                   WHERE racct EQ ht_skat-saknr.
        ADD lr_acdoca2->tsl TO lv_devir_bp.
      ENDLOOP.

    ENDIF.

    it_report-hkont = ht_skat-saknr.
    it_report-bukrs = p_bukrs.
    it_report-dmbtrn = lv_devir.
    it_report-wrbtrn = lv_devir_bp.
    it_report-txt50 = ht_skat-txt50.
    it_report-budat = s_budat-low - 1.
    it_report-sgtxt = 'DEVİR'.

    APPEND it_report.

    lv_bakiye = lv_devir.
    lv_bakiye_bp = lv_devir_bp.

    LOOP AT it_bsis WHERE hkont EQ ht_skat-saknr.
      CLEAR it_report.

      it_report-txt50 = ht_skat-txt50.

      MOVE-CORRESPONDING it_bsis TO it_report.

      CASE it_bsis-shkzg.

        WHEN 'S'.
          CLEAR it_report-dmbtra.
          it_report-dmbtrb = it_bsis-dmbtr.
          it_report-borc_bp = it_bsis-wrbtr.
          ADD it_report-dmbtrb TO lv_bakiye.

          IF lv_waers NE 'TRY'.
            ADD it_report-borc_bp TO lv_bakiye_bp.
          ENDIF.

        WHEN 'H'.
          CLEAR it_report-dmbtrb.
          it_report-dmbtra = it_bsis-dmbtr.
          it_report-alacak_pb = it_bsis-wrbtr.
          SUBTRACT it_report-dmbtra FROM lv_bakiye.

          it_report-wrbtr = it_report-wrbtr * -1.

          IF lv_waers NE 'TRY'.
            SUBTRACT it_report-alacak_pb FROM lv_bakiye_bp.
          ENDIF.

      ENDCASE.

      SELECT SINGLE ltext FROM t003t
         INTO it_report-ltext
         WHERE blart EQ it_report-blart
           AND spras EQ 'T'.


      it_report-dmbtrn = lv_bakiye.
      it_report-wrbtrn = lv_bakiye_bp.

      APPEND it_report.

    ENDLOOP.

    LOOP AT lt_acdoca REFERENCE INTO DATA(lr_acdoca)
                          WHERE racct EQ ht_skat-saknr.
      CLEAR it_report.

      it_report-txt50 = ht_skat-txt50.

      MOVE-CORRESPONDING lr_acdoca->* TO it_report.

      it_report-bukrs = lr_acdoca->rbukrs.
      it_report-hkont = lr_acdoca->racct.
      it_report-wrbtr = lr_acdoca->ksl.
      it_report-waers = lr_acdoca->rwcur.
      it_report-buzei = lr_acdoca->docln.

      IF lr_acdoca->hsl GT 0.
        CLEAR it_report-dmbtra.
        it_report-dmbtrb  = lr_acdoca->hsl.
        it_report-borc_bp = lr_acdoca->ksl.
        ADD it_report-dmbtrb TO lv_bakiye.

        IF lv_waers NE 'TRY'.
          ADD it_report-borc_bp TO lv_bakiye_bp.
        ENDIF.

      ELSE.
        CLEAR it_report-dmbtrb.
        it_report-dmbtra    = lr_acdoca->hsl * -1.
        it_report-alacak_pb = lr_acdoca->ksl * -1.
        SUBTRACT it_report-dmbtra FROM lv_bakiye.

        IF lv_waers NE 'TRY'.
          SUBTRACT it_report-alacak_pb FROM lv_bakiye_bp.
        ENDIF.

      ENDIF.

      SELECT SINGLE ltext FROM t003t
         INTO it_report-ltext
         WHERE blart EQ it_report-blart
           AND spras EQ 'T'.


      it_report-dmbtrn = lv_bakiye.
      it_report-wrbtrn = lv_bakiye_bp.

      APPEND it_report.
    ENDLOOP.

  ENDLOOP.


ENDFORM.
