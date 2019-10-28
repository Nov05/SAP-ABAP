```
*--------------------------------------------------------------------*
* Author          Robin Vleeschhouwer
* Company         RV SAP Consultancy
* Website         www.rvsapconsultancy.com
* E-mail          robin@rvsapconsultancy.com
* Description     Export repository object to text file
* Version         2.3
*--------------------------------------------------------------------*
*REPORT  y_export_rep_obj.

*--------------------------------------------------------------------*
* Customizable constants
*--------------------------------------------------------------------*
"Block frame title
CONSTANTS c_lbl_bsel  TYPE itex132  VALUE 'Selection'.      "#EC NOTEXT
CONSTANTS c_lbl_bflt  TYPE itex132  VALUE 'Filter'.         "#EC NOTEXT
CONSTANTS c_lbl_bexp  TYPE itex132  VALUE 'Export'.         "#EC NOTEXT
CONSTANTS c_lbl_bdir  TYPE itex132  VALUE 'Directories'.    "#EC NOTEXT
"Label selection and filter
CONSTANTS c_lbl_spack TYPE itex132  VALUE 'Package'.        "#EC NOTEXT
CONSTANTS c_lbl_stran TYPE itex132  VALUE 'Transport'.      "#EC NOTEXT
CONSTANTS c_lbl_sprog TYPE itex132  VALUE 'Program'.        "#EC NOTEXT
CONSTANTS c_lbl_swdyn TYPE itex132  VALUE 'Web Dynpro'.     "#EC NOTEXT
CONSTANTS c_lbl_sfugr TYPE itex132  VALUE 'Function group'. "#EC NOTEXT
CONSTANTS c_lbl_sfunc TYPE itex132  VALUE 'Function module'. "#EC NOTEXT
CONSTANTS c_lbl_sclif TYPE itex132  VALUE 'Class or interface'. "#EC NOTEXT
CONSTANTS c_lbl_stabl TYPE itex132  VALUE 'Table or structure'. "#EC NOTEXT
CONSTANTS c_lbl_smsag TYPE itex132  VALUE 'Message class'.  "#EC NOTEXT
CONSTANTS c_lbl_susrn TYPE itex132  VALUE 'Username'.       "#EC NOTEXT
"Label export
CONSTANTS c_lbl_xcust TYPE itex132  VALUE 'Only customer objects'. "#EC NOTEXT
CONSTANTS c_lbl_xnams TYPE itex132  VALUE 'Namespace'.      "#EC NOTEXT
CONSTANTS c_lbl_xsimu TYPE itex132  VALUE 'Simulation, don''t create files'. "#EC NOTEXT
"Label directories
CONSTANTS c_lbl_droot TYPE itex132  VALUE 'Root directory'. "#EC NOTEXT
CONSTANTS c_lbl_dprog TYPE itex132  VALUE 'Programs'.       "#EC NOTEXT
CONSTANTS c_lbl_dwdyn TYPE itex132  VALUE 'Web Dynpro'.     "#EC NOTEXT
CONSTANTS c_lbl_dwdyc TYPE itex132  VALUE 'WD Controllers'. "#EC NOTEXT
CONSTANTS c_lbl_dwdyw TYPE itex132  VALUE 'WD Windows'.     "#EC NOTEXT
CONSTANTS c_lbl_dwdyv TYPE itex132  VALUE 'WD Views'.       "#EC NOTEXT
CONSTANTS c_lbl_dfugr TYPE itex132  VALUE 'Function groups'. "#EC NOTEXT
CONSTANTS c_lbl_dfunc TYPE itex132  VALUE 'Function modules'. "#EC NOTEXT
CONSTANTS c_lbl_dscrn TYPE itex132  VALUE 'Screens'.        "#EC NOTEXT
CONSTANTS c_lbl_dclas TYPE itex132  VALUE 'Classes'.        "#EC NOTEXT
CONSTANTS c_lbl_dintf TYPE itex132  VALUE 'Interfaces'.     "#EC NOTEXT
CONSTANTS c_lbl_dtabl TYPE itex132  VALUE 'Tables'.         "#EC NOTEXT
CONSTANTS c_lbl_dstru TYPE itex132  VALUE 'Structures'.     "#EC NOTEXT
CONSTANTS c_lbl_dmsag TYPE itex132  VALUE 'Message classes'. "#EC NOTEXT
"Value directories
CONSTANTS c_dir_droot TYPE itex132  VALUE 'C:\TEMP'.        "#EC NOTEXT
CONSTANTS c_dir_dprog TYPE itex132  VALUE 'Programs'.       "#EC NOTEXT
CONSTANTS c_dir_dwdyn TYPE itex132  VALUE 'Web Dynpro'.     "#EC NOTEXT
CONSTANTS c_dir_dwdyc TYPE itex132  VALUE 'Controllers'.    "#EC NOTEXT
CONSTANTS c_dir_dwdyw TYPE itex132  VALUE 'Windows'.        "#EC NOTEXT
CONSTANTS c_dir_dwdyv TYPE itex132  VALUE 'Views'.          "#EC NOTEXT
CONSTANTS c_dir_dfugr TYPE itex132  VALUE 'Function groups'. "#EC NOTEXT
CONSTANTS c_dir_dfunc TYPE itex132  VALUE 'Function modules'. "#EC NOTEXT
CONSTANTS c_dir_dscrn TYPE itex132  VALUE 'Screens'.        "#EC NOTEXT
CONSTANTS c_dir_dclas TYPE itex132  VALUE 'Classes'.        "#EC NOTEXT
CONSTANTS c_dir_dintf TYPE itex132  VALUE 'Interfaces'.     "#EC NOTEXT
CONSTANTS c_dir_dtabl TYPE itex132  VALUE 'Tables'.         "#EC NOTEXT
CONSTANTS c_dir_dstru TYPE itex132  VALUE 'Structures'.     "#EC NOTEXT
CONSTANTS c_dir_dmsag TYPE itex132  VALUE 'Message classes'. "#EC NOTEXT
"Value filename
CONSTANTS c_file_txtp TYPE itex132  VALUE '_TEXTPOOL'.      "#EC NOTEXT
CONSTANTS c_file_scne TYPE itex132  VALUE '_ELEMENTS'.      "#EC NOTEXT
CONSTANTS c_file_ext  TYPE itex132  VALUE '.txt'.           "#EC NOTEXT
"Value header textpool
CONSTANTS c_txtp_sel  TYPE itex132  VALUE 'Selection-text'. "#EC NOTEXT
CONSTANTS c_txtp_text TYPE itex132  VALUE 'Text-symbols'.   "#EC NOTEXT
CONSTANTS c_txtp_excp TYPE itex132  VALUE 'Exception-text'. "#EC NOTEXT
"Value header table structure
CONSTANTS c_tbl_field TYPE itex132  VALUE 'Field'.          "#EC NOTEXT
CONSTANTS c_tbl_key   TYPE itex132  VALUE 'Key'.            "#EC NOTEXT
CONSTANTS c_tbl_delem TYPE itex132  VALUE 'Data-element'.   "#EC NOTEXT
CONSTANTS c_tbl_dtype TYPE itex132  VALUE 'Data-type'.      "#EC NOTEXT
CONSTANTS c_tbl_len   TYPE itex132  VALUE 'Length'.         "#EC NOTEXT
CONSTANTS c_tbl_dec   TYPE itex132  VALUE 'Decimals'.       "#EC NOTEXT
CONSTANTS c_tbl_dom   TYPE itex132  VALUE 'Domain'.         "#EC NOTEXT
CONSTANTS c_tbl_check TYPE itex132  VALUE 'Check-table'.    "#EC NOTEXT
CONSTANTS c_tbl_incl  TYPE itex132  VALUE 'Include-table'.  "#EC NOTEXT
"Value header message class
CONSTANTS c_msg_descr TYPE itex132  VALUE 'Description'.    "#EC NOTEXT
CONSTANTS c_msg_langu TYPE itex132  VALUE 'Language'.       "#EC NOTEXT
CONSTANTS c_msg_numb  TYPE itex132  VALUE 'Number'.         "#EC NOTEXT
CONSTANTS c_msg_msag  TYPE itex132  VALUE 'Message'.        "#EC NOTEXT
"ALV header
CONSTANTS c_alv_stat  TYPE scrtext_s  VALUE 'Status'.       "#EC NOTEXT
CONSTANTS c_alv_type  TYPE scrtext_s  VALUE 'Type'.         "#EC NOTEXT
CONSTANTS c_alv_path  TYPE scrtext_s  VALUE 'Path'.         "#EC NOTEXT
CONSTANTS c_alv_name  TYPE scrtext_s  VALUE 'Name'.         "#EC NOTEXT
CONSTANTS c_alv_msg   TYPE scrtext_s  VALUE 'Message'.      "#EC NOTEXT

*--------------------------------------------------------------------*
* Non-customizable constants
*--------------------------------------------------------------------*
"R3 reference to object type
CONSTANTS c_r3_prog   TYPE trobjtype  VALUE 'PROG'.         "#EC NOTEXT
CONSTANTS c_r3_wdyn   TYPE trobjtype  VALUE 'WDYN'.         "#EC NOTEXT
CONSTANTS c_r3_wdyc   TYPE trobjtype  VALUE 'WDYC'.         "#EC NOTEXT
CONSTANTS c_r3_wdyw   TYPE trobjtype  VALUE 'WDYW'.         "#EC NOTEXT
CONSTANTS c_r3_wdyv   TYPE trobjtype  VALUE 'WDYV'.         "#EC NOTEXT
CONSTANTS c_r3_fugr   TYPE trobjtype  VALUE 'FUGR'.         "#EC NOTEXT
CONSTANTS c_r3_func   TYPE trobjtype  VALUE 'FUNC'.         "#EC NOTEXT
CONSTANTS c_r3_incl   TYPE trobjtype  VALUE 'INCL'.         "#EC NOTEXT
CONSTANTS c_r3_scrn   TYPE trobjtype  VALUE 'SCRN'.         "#EC NOTEXT
CONSTANTS c_r3_clas   TYPE trobjtype  VALUE 'CLAS'.         "#EC NOTEXT
CONSTANTS c_r3_intf   TYPE trobjtype  VALUE 'INTF'.         "#EC NOTEXT
CONSTANTS c_r3_tabl   TYPE trobjtype  VALUE 'TABL'.         "#EC NOTEXT
CONSTANTS c_r3_stru   TYPE trobjtype  VALUE 'STRU'.         "#EC NOTEXT
CONSTANTS c_r3_msag   TYPE trobjtype  VALUE 'MSAG'.         "#EC NOTEXT
CONSTANTS c_r3_txtp   TYPE trobjtype  VALUE 'TXTP'.         "#EC NOTEXT

*--------------------------------------------------------------------*
* Type-pools
*--------------------------------------------------------------------*
TYPE-POOLS abap.
TYPE-POOLS wdyn.
TYPE-POOLS icon.
TYPE-POOLS col.

*--------------------------------------------------------------------*
* Types
*--------------------------------------------------------------------*
"Object
TYPES:  BEGIN OF ty_s_object,
          name  TYPE sobj_name,
          type  TYPE trobjtype,
          pack  TYPE devclass,
        END OF ty_s_object.
TYPES ty_t_object TYPE STANDARD TABLE OF ty_s_object.

"Selection
TYPES:  BEGIN OF ty_s_selection,
          name  TYPE string,
          path  TYPE string,
          type  TYPE trobjtype,
        END OF ty_s_selection.
TYPES ty_t_selection TYPE STANDARD TABLE OF ty_s_selection.

"Source
TYPES   BEGIN OF ty_s_source.
        INCLUDE TYPE ty_s_selection.
TYPES:    filename  TYPE string,
          source    TYPE string_table,
        END OF ty_s_source.
TYPES ty_t_source   TYPE STANDARD TABLE OF ty_s_source.

"Export status
TYPES:  BEGIN OF ty_s_export_status,
          status    TYPE icon_4,
          type      TYPE trobjtype,
          path      TYPE string,
          name      TYPE sobj_name,
          message   TYPE string,
          colors    TYPE lvc_t_scol,
        END OF ty_s_export_status.
TYPES ty_t_export_status TYPE STANDARD TABLE OF ty_s_export_status.

*--------------------------------------------------------------------*
* Selection-screen variables for select-options
*--------------------------------------------------------------------*
DATA gv_pack          TYPE devclass.
DATA gv_tran          TYPE tr_trkorr.
DATA gv_prog          TYPE program.
DATA gv_wdyn          TYPE wdy_component_name.
DATA gv_fugr          TYPE rs38l_area.
DATA gv_func          TYPE rs38l_fnam.
DATA gv_clif          TYPE seoclsname.
DATA gv_tabl          TYPE typename.
DATA gv_msag          TYPE msgid.
DATA gv_usrn          TYPE xubname.

*--------------------------------------------------------------------*
* Selection screen
*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK blocks WITH FRAME TITLE lbl_bsel.
SELECT-OPTIONS s_spack FOR gv_pack.                      "#EC SEL_WRONG
SELECT-OPTIONS s_stran FOR gv_tran.                      "#EC SEL_WRONG
SELECT-OPTIONS s_sprog FOR gv_prog.                      "#EC SEL_WRONG
SELECT-OPTIONS s_swdyn FOR gv_wdyn.                      "#EC SEL_WRONG
SELECT-OPTIONS s_sfugr FOR gv_fugr.                      "#EC SEL_WRONG
SELECT-OPTIONS s_sfunc FOR gv_func  MATCHCODE OBJECT recafuncname. "#EC SEL_WRONG
SELECT-OPTIONS s_sclif FOR gv_clif  MATCHCODE OBJECT dd_class. "#EC SEL_WRONG
SELECT-OPTIONS s_stabl FOR gv_tabl.                      "#EC SEL_WRONG
SELECT-OPTIONS s_smsag FOR gv_msag.                      "#EC SEL_WRONG
SELECT-OPTIONS s_susrn FOR gv_usrn  MATCHCODE OBJECT sc_user. "#EC SEL_WRONG
SELECTION-SCREEN END OF BLOCK blocks.

SELECTION-SCREEN BEGIN OF BLOCK blockfe WITH FRAME TITLE lbl_bflt.
SELECT-OPTIONS s_fpack FOR gv_pack.                      "#EC SEL_WRONG
SELECT-OPTIONS s_fusrn FOR gv_usrn  MATCHCODE OBJECT sc_user. "#EC SEL_WRONG
SELECT-OPTIONS s_fprog FOR gv_prog.                      "#EC SEL_WRONG
SELECT-OPTIONS s_fwdyn FOR gv_wdyn.                      "#EC SEL_WRONG
SELECT-OPTIONS s_ffugr FOR gv_fugr.                      "#EC SEL_WRONG
SELECTION-SCREEN END OF BLOCK blockfe.

SELECTION-SCREEN BEGIN OF BLOCK blockx WITH FRAME TITLE lbl_bexp.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(32) lbl_xcus FOR FIELD p_xcust.
PARAMETERS p_xcust AS CHECKBOX DEFAULT abap_true.        "#EC SEL_WRONG
SELECTION-SCREEN END OF LINE.
PARAMETERS p_xnams TYPE namespace.                       "#EC SEL_WRONG
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(32) lbl_xsim FOR FIELD p_xsimu.
PARAMETERS p_xsimu AS CHECKBOX.                          "#EC SEL_WRONG
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK blockx.

SELECTION-SCREEN BEGIN OF BLOCK blockd WITH FRAME TITLE lbl_bdir.
PARAMETERS p_droot  TYPE string OBLIGATORY LOWER CASE.   "#EC SEL_WRONG
PARAMETERS p_dprog  TYPE string OBLIGATORY LOWER CASE.   "#EC SEL_WRONG
PARAMETERS p_dwdyn  TYPE string OBLIGATORY LOWER CASE.   "#EC SEL_WRONG
PARAMETERS p_dwdyc  TYPE string OBLIGATORY LOWER CASE.   "#EC SEL_WRONG
PARAMETERS p_dwdyw  TYPE string OBLIGATORY LOWER CASE.   "#EC SEL_WRONG
PARAMETERS p_dwdyv  TYPE string OBLIGATORY LOWER CASE.   "#EC SEL_WRONG
PARAMETERS p_dfugr  TYPE string OBLIGATORY LOWER CASE.   "#EC SEL_WRONG
PARAMETERS p_dfunc  TYPE string OBLIGATORY LOWER CASE.   "#EC SEL_WRONG
PARAMETERS p_dscrn  TYPE string OBLIGATORY LOWER CASE.   "#EC SEL_WRONG
PARAMETERS p_dclas  TYPE string OBLIGATORY LOWER CASE.   "#EC SEL_WRONG
PARAMETERS p_dintf  TYPE string OBLIGATORY LOWER CASE.   "#EC SEL_WRONG
PARAMETERS p_dtabl  TYPE string OBLIGATORY LOWER CASE.   "#EC SEL_WRONG
PARAMETERS p_dstru  TYPE string OBLIGATORY LOWER CASE.   "#EC SEL_WRONG
PARAMETERS p_dmsag  TYPE string OBLIGATORY LOWER CASE.   "#EC SEL_WRONG
SELECTION-SCREEN END OF BLOCK blockd.

*--------------------------------------------------------------------*
* Initialization
*--------------------------------------------------------------------*
INITIALIZATION.
  "Block frame title
  lbl_bsel = c_lbl_bsel.
  lbl_bflt = c_lbl_bflt.
  lbl_bexp = c_lbl_bexp.
  lbl_bdir = c_lbl_bdir.
  "Label selection
  %_s_spack_%_app_%-text  = c_lbl_spack.
  %_s_stran_%_app_%-text  = c_lbl_stran.
  %_s_sprog_%_app_%-text  = c_lbl_sprog.
  %_s_swdyn_%_app_%-text  = c_lbl_swdyn.
  %_s_sfugr_%_app_%-text  = c_lbl_sfugr.
  %_s_sfunc_%_app_%-text  = c_lbl_sfunc.
  %_s_sclif_%_app_%-text  = c_lbl_sclif.
  %_s_stabl_%_app_%-text  = c_lbl_stabl.
  %_s_smsag_%_app_%-text  = c_lbl_smsag.
  %_s_susrn_%_app_%-text  = c_lbl_susrn.
  "Label filter
  %_s_fpack_%_app_%-text  = c_lbl_spack.
  %_s_fusrn_%_app_%-text  = c_lbl_susrn.
  %_s_fprog_%_app_%-text  = c_lbl_sprog.
  %_s_fwdyn_%_app_%-text  = c_lbl_swdyn.
  %_s_ffugr_%_app_%-text  = c_lbl_sfugr.
  "Label simulation
  lbl_xcus                = c_lbl_xcust.
  %_p_xnams_%_app_%-text  = c_lbl_xnams.
  lbl_xsim                = c_lbl_xsimu.
  "Label directories
  %_p_droot_%_app_%-text  = c_lbl_droot.
  %_p_dprog_%_app_%-text  = c_lbl_dprog.
  %_p_dwdyn_%_app_%-text  = c_lbl_dwdyn.
  %_p_dwdyc_%_app_%-text  = c_lbl_dwdyc.
  %_p_dwdyw_%_app_%-text  = c_lbl_dwdyw.
  %_p_dwdyv_%_app_%-text  = c_lbl_dwdyv.
  %_p_dfugr_%_app_%-text  = c_lbl_dfugr.
  %_p_dfunc_%_app_%-text  = c_lbl_dfunc.
  %_p_dscrn_%_app_%-text  = c_lbl_dscrn.
  %_p_dclas_%_app_%-text  = c_lbl_dclas.
  %_p_dintf_%_app_%-text  = c_lbl_dintf.
  %_p_dtabl_%_app_%-text  = c_lbl_dtabl.
  %_p_dstru_%_app_%-text  = c_lbl_dstru.
  %_p_dmsag_%_app_%-text  = c_lbl_dmsag.
  "Value directories
  p_droot                 = c_dir_droot.
  p_dprog                 = c_dir_dprog.
  p_dwdyn                 = c_dir_dwdyn.
  p_dwdyc                 = c_dir_dwdyc.
  p_dwdyw                 = c_dir_dwdyw.
  p_dwdyv                 = c_dir_dwdyv.
  p_dfugr                 = c_dir_dfugr.
  p_dfunc                 = c_dir_dfunc.
  p_dscrn                 = c_dir_dscrn.
  p_dclas                 = c_dir_dclas.
  p_dintf                 = c_dir_dintf.
  p_dtabl                 = c_dir_dtabl.
  p_dstru                 = c_dir_dstru.
  p_dmsag                 = c_dir_dmsag.

*----------------------------------------------------------------------*
* CLASS lcx_export_to_text DEFINITION ABSTRACT
* Author          Robin Vleeschhouwer
* Company         RV SAP Consultancy
* Website         www.rvsapconsultancy.com
* E-mail          robin@rvsapconsultancy.com
*----------------------------------------------------------------------*
CLASS lcx_export_to_text DEFINITION INHERITING FROM cx_static_check FINAL.
ENDCLASS.                    "lcx_export_to_text DEFINITION

*----------------------------------------------------------------------*
* CLASS lcl_export_to_text DEFINITION ABSTRACT
* Author          Robin Vleeschhouwer
* Company         RV SAP Consultancy
* Website         www.rvsapconsultancy.com
* E-mail          robin@rvsapconsultancy.com
*----------------------------------------------------------------------*
CLASS lcl_export_to_text DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS constructor       IMPORTING iv_name             TYPE string
                                        iv_path             TYPE string
                                        iv_type             TYPE trobjtype.
    METHODS export_to_text    CHANGING  ct_export_status    TYPE ty_t_export_status.

  PROTECTED SECTION.
    DATA mv_name              TYPE string.
    DATA mv_path              TYPE string.
    DATA mv_type              TYPE trobjtype.
    DATA mt_source            TYPE ty_t_source.

    METHODS get_source        CHANGING  ct_export_status    TYPE ty_t_export_status
                              RAISING   lcx_export_to_text.
    METHODS write_to_file     CHANGING  ct_export_status    TYPE ty_t_export_status.
ENDCLASS.                    "lcl_export_to_text DEFINITION

*----------------------------------------------------------------------*
* CLASS lcl_export_to_text IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_export_to_text IMPLEMENTATION.
  METHOD constructor.
    "Set member variables
    mv_name = iv_name.
    mv_path = iv_path.
    mv_type = iv_type.
  ENDMETHOD.                    "constructor

  METHOD export_to_text.
    TRY.
        "Get source
        get_source( CHANGING ct_export_status = ct_export_status ).

        "Write to text file
        write_to_file( CHANGING ct_export_status = ct_export_status ).
      CATCH lcx_export_to_text.                         "#EC NO_HANDLER
    ENDTRY.
  ENDMETHOD.                    "export_to_text

  METHOD get_source.                                        "#EC NEEDED
  ENDMETHOD.                    "get_source

  METHOD write_to_file.
    DATA ls_source        TYPE ty_s_source.
    DATA lv_filename      TYPE string.
    DATA ls_export_status TYPE ty_s_export_status.

    LOOP AT mt_source INTO ls_source.
      CONCATENATE ls_source-path '\' ls_source-filename c_file_ext INTO lv_filename.

      "Only save when not sumulating
      IF p_xsimu = abap_false.
        cl_gui_frontend_services=>gui_download( EXPORTING   filename  = lv_filename
                                                CHANGING    data_tab  = ls_source-source
                                                EXCEPTIONS  OTHERS    = 1 ).
      ENDIF.
      "Set export status
      IF sy-subrc = 0.
        ls_export_status-status = icon_led_green.
        MESSAGE s339(sbds) INTO ls_export_status-message.
      ELSE.
        ls_export_status-status = icon_led_red.
        MESSAGE s039(trust) INTO ls_export_status-message.
      ENDIF.
      ls_export_status-type     = ls_source-type.
      ls_export_status-name     = ls_source-filename.
      ls_export_status-path     = ls_source-path.
      APPEND ls_export_status TO ct_export_status.
    ENDLOOP.
  ENDMETHOD.                    "write_to_file
ENDCLASS.                    "lcl_export_to_text IMPLEMENTATION

*----------------------------------------------------------------------*
* CLASS lcl_export_prog DEFINITION
* Author          Robin Vleeschhouwer
* Company         RV SAP Consultancy
* Website         www.rvsapconsultancy.com
* E-mail          robin@rvsapconsultancy.com
*----------------------------------------------------------------------*
CLASS lcl_export_prog DEFINITION INHERITING FROM lcl_export_to_text FINAL.
  PROTECTED SECTION.
    METHODS get_source REDEFINITION.
ENDCLASS.                    "lcl_export_prog DEFINITION

*----------------------------------------------------------------------*
* CLASS lcl_export_prog IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_export_prog IMPLEMENTATION.
  METHOD get_source.
    DATA lt_program       TYPE scr_programs.
    DATA lv_program       TYPE program.
    DATA lt_r_program     TYPE RANGE OF program.
    DATA ls_r_program     LIKE LINE OF lt_r_program.
    DATA lv_funcgroup     TYPE rs38l_area.
    DATA lt_funcmod       TYPE re_t_funcincl.
    DATA ls_funcmod       TYPE rs38l_incl.
    DATA lt_r_funcmod     TYPE RANGE OF rs38l_fnam.
    DATA ls_r_funcmod     LIKE LINE OF lt_r_funcmod.
    DATA ls_source        TYPE ty_s_source.
    DATA lt_dynpro_nr     TYPE swytdynpro.
    DATA lv_dynpro_nr     TYPE dynnr.
    DATA ls_dynpro_header TYPE d020s.
    DATA lt_dynpro_elem   TYPE tdt_d021s.
    DATA ls_dynpro_elem   TYPE d021s.
    DATA lt_dynpro_source TYPE dyn_flowlist.
    DATA ls_dynpro_source TYPE d022s.
    DATA lt_textpool      TYPE textpool_table.
    DATA ls_textpool      TYPE textpool.
    DATA lv_line          TYPE string.
    DATA lv_title         TYPE sap_bool.
    DATA ls_export_status TYPE ty_s_export_status.

    CLEAR mt_source.

    "Get includes
    lv_program = mv_name.
    CALL FUNCTION 'RS_GET_ALL_INCLUDES'
      EXPORTING
        program    = lv_program
      TABLES
        includetab = lt_program
      EXCEPTIONS
        OTHERS     = 1.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_export_to_text.
    ENDIF.

    IF p_xcust = abap_true.
      "Set only customer objects
      ls_r_program-sign   = 'I'.
      ls_r_program-option = 'CP'.
      IF mv_type = c_r3_prog.
        ls_r_program-low  = 'Z*'.
        APPEND ls_r_program TO lt_r_program.
        ls_r_program-low  = 'Y*'.
        APPEND ls_r_program TO lt_r_program.
      ELSE.
        ls_r_program-low  = 'LZ*'.
        APPEND ls_r_program TO lt_r_program.
        ls_r_program-low  = 'LY*'.
        APPEND ls_r_program TO lt_r_program.
      ENDIF.
      "Include objects in namespace
      IF p_xnams IS NOT INITIAL.
        CONCATENATE p_xnams '*' INTO ls_r_program-low.
        APPEND ls_r_program TO lt_r_program.
      ENDIF.
      "Delete non customer programs
      DELETE lt_program WHERE table_line NOT IN lt_r_program.
    ENDIF.

    "Insert main program
    INSERT mv_name INTO lt_program INDEX 1.
    "Set program and includes in the source table
    LOOP AT lt_program INTO lv_program.
      ls_source-name        = lv_program.
      ls_source-path        = mv_path.
      IF sy-tabix = 1.
        ls_source-type      = mv_type.
      ELSE.
        ls_source-type      = c_r3_incl.
      ENDIF.
      "Remove SAPL from name for function group
      IF  sy-tabix  = 1
      AND mv_type   = c_r3_fugr.
        ls_source-filename  = lv_program+4.
      ELSE.
        ls_source-filename  = lv_program.
      ENDIF.
      APPEND ls_source TO mt_source.
    ENDLOOP.

    "Get function modules
    IF mv_type = c_r3_fugr.
      lv_funcgroup = mv_name+4.
      CALL FUNCTION 'RS_FUNCTION_POOL_CONTENTS'
        EXPORTING
          function_pool = lv_funcgroup
        TABLES
          functab       = lt_funcmod
        EXCEPTIONS
          OTHERS        = 1.
      IF sy-subrc <> 0.
        "Set export status
        ls_export_status-status = icon_led_red.
        ls_export_status-type   = mv_type.
        "Remove SAPL from name for function group
        ls_export_status-name   = mv_name+4.
        ls_export_status-path   = mv_path.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                INTO ls_export_status-message.
        APPEND ls_export_status TO ct_export_status.
        RAISE EXCEPTION TYPE lcx_export_to_text.
      ENDIF.

      IF p_xcust = abap_true.
        "Set only customer objects
        ls_r_funcmod-sign   = 'I'.
        ls_r_funcmod-option = 'CP'.
        ls_r_funcmod-low    = 'Z*'.
        APPEND ls_r_funcmod TO lt_r_funcmod.
        ls_r_funcmod-low    = 'Y*'.
        APPEND ls_r_funcmod TO lt_r_funcmod.
        "Include objects in namespace
        IF p_xnams IS NOT INITIAL.
          CONCATENATE p_xnams '*' INTO ls_r_funcmod-low.
          APPEND ls_r_funcmod TO lt_r_funcmod.
        ENDIF.
        "Delete non customer function modules
        DELETE lt_funcmod WHERE funcname NOT IN lt_r_funcmod.
      ENDIF.

      LOOP AT lt_funcmod INTO ls_funcmod.
        ls_source-name      = ls_funcmod-include.
        CONCATENATE mv_path '\' c_dir_dfunc INTO ls_source-path.
        ls_source-type      = c_r3_func.
        ls_source-filename  = ls_funcmod-funcname.
        READ TABLE mt_source  WITH KEY name = ls_funcmod-include
                              TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          MODIFY mt_source FROM ls_source INDEX sy-tabix.
        ENDIF.
      ENDLOOP.
    ENDIF.

    "Read source
    LOOP AT mt_source INTO ls_source.
      lv_program = ls_source-name.
      READ REPORT lv_program INTO ls_source-source.
      IF sy-subrc <> 0.
        "Set export status
        ls_export_status-status = icon_led_red.
        ls_export_status-type   = mv_type.
        "Remove SAPL from name for function group
        IF mv_type = c_r3_fugr.
          ls_export_status-name = mv_name+4.
        ELSE.
          ls_export_status-name = mv_name.
        ENDIF.
        ls_export_status-path   = mv_path.
        MESSAGE e077(ds) WITH '' INTO ls_export_status-message.
        APPEND ls_export_status TO ct_export_status.
        RAISE EXCEPTION TYPE lcx_export_to_text.
      ENDIF.
      MODIFY mt_source FROM ls_source.
    ENDLOOP.

    "Read screen
    lv_program = mv_name.
    CALL FUNCTION 'SWY_READ_DYNPROS_OF_PROGRAM'
      EXPORTING
        program_name   = lv_program
      IMPORTING
        dynpro_numbers = lt_dynpro_nr.

    LOOP AT lt_dynpro_nr INTO lv_dynpro_nr.
      CALL FUNCTION 'RS_IMPORT_DYNPRO'
        EXPORTING
          dyname = lv_program
          dynumb = lv_dynpro_nr
        IMPORTING
          header = ls_dynpro_header
        TABLES
          ftab   = lt_dynpro_elem
          pltab  = lt_dynpro_source
        EXCEPTIONS
          OTHERS = 1.
      IF sy-subrc = 0.
        "Don't export selection screen
        IF ls_dynpro_header-type = 'S'.
          CONTINUE.
        ENDIF.

        "Set dynpro source
        CLEAR ls_source.
        LOOP AT lt_dynpro_source INTO ls_dynpro_source.
          lv_line = ls_dynpro_source-line.
          APPEND lv_line TO ls_source-source.
        ENDLOOP.
        IF ls_source-source IS NOT INITIAL.
          ls_source-name      = mv_name.
          CONCATENATE mv_path '\' p_dscrn INTO ls_source-path.
          ls_source-type      = c_r3_scrn.
          ls_source-filename  = lv_dynpro_nr.
          APPEND ls_source TO mt_source.
        ENDIF.

        "Set screen elements
        CLEAR ls_source.
        LOOP AT lt_dynpro_elem  INTO  ls_dynpro_elem
                                WHERE fnam IS NOT INITIAL.
          CONCATENATE ls_dynpro_elem-fnam cl_abap_char_utilities=>horizontal_tab
                      ls_dynpro_elem-stxt INTO lv_line.
          APPEND lv_line TO ls_source-source.
        ENDLOOP.
        IF ls_source-source IS NOT INITIAL.
          ls_source-name      = mv_name.
          CONCATENATE mv_path '\' p_dscrn INTO ls_source-path.
          ls_source-type      = c_r3_scrn.
          CONCATENATE lv_dynpro_nr c_file_scne INTO ls_source-filename.
          APPEND ls_source TO mt_source.
        ENDIF.
      ENDIF.
    ENDLOOP.

    "Read textpool
    CLEAR ls_source.
    lv_program = mv_name.
    READ TEXTPOOL lv_program INTO lt_textpool LANGUAGE sy-langu.
    LOOP AT lt_textpool INTO  ls_textpool
                        WHERE id  = 'I'.  "Text
      IF lv_title IS INITIAL.
        APPEND c_txtp_text TO ls_source-source.
        lv_title = abap_true.
      ENDIF.
      CONCATENATE ls_textpool-key   cl_abap_char_utilities=>horizontal_tab
                  ls_textpool-entry INTO lv_line.
      APPEND lv_line TO ls_source-source.
    ENDLOOP.
    "Set seperator
    IF lv_title = abap_true.
      APPEND INITIAL LINE TO ls_source-source.
      CLEAR lv_title.
    ENDIF.
    LOOP AT lt_textpool INTO  ls_textpool
                        WHERE id  = 'S'.  "Selection
      IF lv_title IS INITIAL.
        APPEND c_txtp_sel  TO ls_source-source.
        lv_title = abap_true.
      ENDIF.
      SHIFT ls_textpool-entry LEFT DELETING LEADING space.
      CONCATENATE ls_textpool-key   cl_abap_char_utilities=>horizontal_tab
                  ls_textpool-entry INTO lv_line.
      APPEND lv_line TO ls_source-source.
    ENDLOOP.
    IF ls_source-source IS NOT INITIAL.
      ls_source-name      = mv_name.
      ls_source-path      = mv_path.
      ls_source-type      = c_r3_txtp.
      "Remove SAPL from name for function group
      IF mv_type = c_r3_fugr.
        CONCATENATE mv_name+4 c_file_txtp INTO ls_source-filename.
      ELSE.
        CONCATENATE mv_name c_file_txtp INTO ls_source-filename.
      ENDIF.
      APPEND ls_source TO mt_source.
    ENDIF.
  ENDMETHOD.                    "get_source
ENDCLASS.                    "lcl_export_prog IMPLEMENTATION

*----------------------------------------------------------------------*
* CLASS lcl_export_wdyn DEFINITION
* Author          Robin Vleeschhouwer
* Company         RV SAP Consultancy
* Website         www.rvsapconsultancy.com
* E-mail          robin@rvsapconsultancy.com
*----------------------------------------------------------------------*
CLASS lcl_export_wdyn DEFINITION INHERITING FROM lcl_export_to_text FINAL.
  PROTECTED SECTION.
    METHODS get_source REDEFINITION.
ENDCLASS.                    "lcl_export_wdyn DEFINITION

*----------------------------------------------------------------------*
* CLASS lcl_export_clif IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_export_wdyn IMPLEMENTATION.
  METHOD get_source.
    DATA ls_source            TYPE ty_s_source.
    DATA lv_line              TYPE char255.
    DATA lo_component         TYPE REF TO if_wdy_md_component.
    DATA lv_component_name    TYPE wdy_component_name.
    DATA ls_definition        TYPE wdy_component.
    DATA ls_definition_ctr    TYPE wdy_controller.
    DATA lo_controller        TYPE REF TO if_wdy_md_controller.
    DATA lv_controller_type   TYPE wdy_md_controller_type.
    DATA lo_map_ctr           TYPE REF TO if_object_map.
    DATA lo_iter_ctr          TYPE REF TO if_object_collection_iterator.
    DATA lt_wdy_ctlr_compo    TYPE STANDARD TABLE OF wdy_ctlr_compo.
    DATA lt_wdy_ctlr_param    TYPE STANDARD TABLE OF wdy_ctlr_param.
    FIELD-SYMBOLS <ls_wdy_ctlr_compo> TYPE wdy_ctlr_compo.
    FIELD-SYMBOLS <ls_wdy_ctlr_param> TYPE wdy_ctlr_param.

    "Set web dynpro component name
    lv_component_name = mv_name.

    TRY.
        "Get web dynpro component
        lo_component = cl_wdy_md_component=>get_object_by_key(  name    = lv_component_name
                                                                version = wdywb_version_active ).
      CATCH cx_wdy_md_not_existing
            cx_wdy_md_permission_failure.
        RAISE EXCEPTION TYPE lcx_export_to_text.
    ENDTRY.

    "Get component definition
    lo_component->if_wdy_md_object~get_definition( IMPORTING definition = ls_definition ).

    "Component controllers
    lo_map_ctr  = lo_component->get_controllers( ).
    lo_iter_ctr = lo_map_ctr->get_values_iterator( ).
    WHILE lo_iter_ctr->has_next( ) = abap_true.
      CLEAR ls_source.
      lo_controller ?= lo_iter_ctr->get_next( ).
      lv_controller_type = lo_controller->get_type( ).

      "Get controller definition
      lo_controller->if_wdy_md_object~get_definition( IMPORTING definition = ls_definition_ctr ).

      "Only save source of controllers that are interesting
      IF lo_controller->get_type( ) = wdyn_ctlr_type_cmp_config.
        CONTINUE.
      ENDIF.
      IF  ls_definition-version = if_wdy_md_object=>co_version_active
      AND lo_controller->if_wdy_md_object~get_state( ) = if_wdy_md_object=>co_state_pseudo_active.
        CONTINUE.
      ENDIF.
      IF ls_definition_ctr-component_name = wdyn_empty_view_name.
        CONTINUE.
      ENDIF.

      "Get controller source
      SELECT *  INTO TABLE lt_wdy_ctlr_compo
                FROM wdy_ctlr_compo
                WHERE component_name  = ls_definition_ctr-component_name
                AND   controller_name = ls_definition_ctr-controller_name
                AND   version         = if_wdy_md_object=>co_version_active.
      IF sy-subrc = 0.
        LOOP AT lt_wdy_ctlr_compo ASSIGNING <ls_wdy_ctlr_compo>.
          IF <ls_wdy_ctlr_compo>-code_body IS NOT INITIAL.
            "Read paramaters
            SELECT *  INTO TABLE lt_wdy_ctlr_param
                      FROM wdy_ctlr_param
                      WHERE component_name  = ls_definition_ctr-component_name
                      AND   controller_name = ls_definition_ctr-controller_name
                      AND   cmpname         = <ls_wdy_ctlr_compo>-cmpname
                      AND   version         = if_wdy_md_object=>co_version_active.
            APPEND '*----------------------------------------------------------------------------------------*'
            TO ls_source-source.
            IF sy-subrc = 0.
              LOOP AT lt_wdy_ctlr_param ASSIGNING <ls_wdy_ctlr_param>.
                CASE <ls_wdy_ctlr_param>-declaration_type.
                  WHEN 0.
                    CONCATENATE '*' 'IMPORTING' INTO lv_line SEPARATED BY space.
                  WHEN 1.
                    CONCATENATE '*' 'EXPORTING' INTO lv_line SEPARATED BY space.
                  WHEN 2.
                    CONCATENATE '*' 'CHANGING' INTO lv_line SEPARATED BY space.
                  WHEN 3.
                    CONCATENATE '*' 'RETURNING' INTO lv_line SEPARATED BY space.
                ENDCASE.
                lv_line+12 = <ls_wdy_ctlr_param>-parameter_name.
                IF <ls_wdy_ctlr_param>-optional = abap_true.
                  lv_line+43 = '[OPTIONAL]'.
                ENDIF.
                IF <ls_wdy_ctlr_param>-abap_typing = 0.
                  lv_line+54 = 'TYPE'.
                ELSE.
                  lv_line+54 = 'TYPE REF TO'.
                ENDIF.
                lv_line+66 = <ls_wdy_ctlr_param>-abap_type.
                APPEND lv_line TO ls_source-source.
              ENDLOOP.
            ELSE.
              APPEND '* NO PARAMETERS' TO ls_source-source.
            ENDIF.
            APPEND '*----------------------------------------------------------------------------------------*'
            TO ls_source-source.

            APPEND <ls_wdy_ctlr_compo>-code_body TO ls_source-source.
          ENDIF.
        ENDLOOP.
      ENDIF.

      "Only save file if it has source
      IF ls_source-source IS INITIAL.
        CONTINUE.
      ENDIF.

      CASE lv_controller_type.
        WHEN wdyn_ctlr_type_component.
          ls_source-type = mv_type.
          CONCATENATE mv_path '\' p_dwdyc INTO ls_source-path.

        WHEN wdyn_ctlr_type_window.
          ls_source-type = c_r3_wdyw.
          CONCATENATE mv_path '\' p_dwdyw INTO ls_source-path.

        WHEN wdyn_ctlr_type_view.
          ls_source-type = c_r3_wdyv.
          CONCATENATE mv_path '\' p_dwdyv INTO ls_source-path.

        WHEN OTHERS.
          ls_source-type = c_r3_wdyc.
          CONCATENATE mv_path '\' p_dwdyc INTO ls_source-path.
      ENDCASE.
      ls_source-name      = mv_name.
      ls_source-filename  = ls_definition_ctr-controller_name.
      APPEND ls_source TO mt_source.
    ENDWHILE.
  ENDMETHOD.                    "get_source
ENDCLASS.                    "lcl_export_wdyn IMPLEMENTATION

*----------------------------------------------------------------------*
* CLASS lcl_export_clif DEFINITION
* Author          Robin Vleeschhouwer
* Company         RV SAP Consultancy
* Website         www.rvsapconsultancy.com
* E-mail          robin@rvsapconsultancy.com
*----------------------------------------------------------------------*
CLASS lcl_export_clif DEFINITION INHERITING FROM lcl_export_to_text FINAL.
  PROTECTED SECTION.
    METHODS get_source REDEFINITION.
ENDCLASS.                    "lcl_export_clif DEFINITION

*----------------------------------------------------------------------*
* CLASS lcl_export_clif IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_export_clif IMPLEMENTATION.
  METHOD get_source.
    DATA lo_source        TYPE REF TO cl_oo_source.
    DATA lo_class         TYPE REF TO cl_oo_class.
    DATA lv_clskey        TYPE seoclskey.
    DATA ls_methkey       TYPE seocmpkey.
    DATA lv_not_active    TYPE sap_bool.
    DATA lv_deleted       TYPE sap_bool.
    DATA lt_attribute     TYPE seo_attributes.
    DATA ls_attribute     TYPE vseoattrib.
    DATA ls_source        TYPE ty_s_source.
    DATA lt_source_orig   TYPE string_table.
    DATA lv_source_line   TYPE string.
    DATA lv_empty         TYPE sap_bool.
    DATA lv_program       TYPE program.
    DATA lt_textpool      TYPE textpool_table.
    DATA ls_textpool      TYPE textpool.
    DATA lv_line          TYPE string.
    DATA lv_otr_concept   TYPE sotr_conc.
    DATA lv_otr_text      TYPE sotr_txt.
    DATA lv_title         TYPE sap_bool.
    DATA ls_export_status TYPE ty_s_export_status.

    CLEAR mt_source.

    "Set the key
    lv_clskey = mv_name.

    "Check if class or interface
    IF mv_type = c_r3_clas.
      "Check if class exists
      CALL FUNCTION 'SEO_CLASS_EXISTENCE_CHECK'
        EXPORTING
          clskey     = lv_clskey
        IMPORTING
          not_active = lv_not_active
        EXCEPTIONS
          OTHERS     = 1.
      IF sy-subrc <> 0
      OR lv_not_active = abap_true.
        RAISE EXCEPTION TYPE lcx_export_to_text.
      ENDIF.

      "Get class source
      CREATE OBJECT lo_source
        EXPORTING
          clskey = lv_clskey
        EXCEPTIONS
          others = 1.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE lcx_export_to_text.
      ENDIF.

      "Check if method exists for netweaver 7.02+ compatibility
      ls_methkey-clsname  = 'CL_OO_SOURCE'.
      ls_methkey-cmpname  = 'READ'.
      CALL FUNCTION 'SEO_METHOD_EXISTENCE_CHECK'
        EXPORTING
          mtdkey  = ls_methkey
        IMPORTING
          deleted = lv_deleted
        EXCEPTIONS
          OTHERS  = 1.
      IF  sy-subrc    = 0
      AND lv_deleted  = abap_false.
        CALL METHOD lo_source->('READ')
          EXPORTING
            version = 'A'. "Active
      ENDIF.
      lt_source_orig = lo_source->source.
    ELSE.
      "Get interface source
      CALL FUNCTION 'SEO_INTERFACE_GET_SOURCE'
        EXPORTING
          cifkey          = lv_clskey
        IMPORTING
          source_expanded = lt_source_orig
        EXCEPTIONS
          OTHERS          = 1.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE lcx_export_to_text.
      ENDIF.
    ENDIF.

    "Clean the code
    LOOP AT lt_source_orig INTO lv_source_line.
      IF lv_source_line IS INITIAL.
        lv_empty = abap_true.
        CONTINUE.
      ENDIF.
      IF  strlen( lv_source_line ) > 2
      AND lv_source_line(3) = '*"*'.
        CONTINUE.
      ENDIF.

      "Set initial line before source
      IF lv_empty = abap_true.
        APPEND INITIAL LINE TO ls_source-source.
        CLEAR lv_empty.
      ELSE.
        IF lv_source_line CS 'PUBLIC SECTION'
        OR lv_source_line CS 'PRIVATE SECTION'
        OR lv_source_line CS 'PROTECTED SECTION'.
          APPEND INITIAL LINE TO ls_source-source.
        ENDIF.
      ENDIF.

      APPEND lv_source_line TO ls_source-source.

      "Set initial line after source
      IF lv_source_line CS 'PUBLIC SECTION'
      OR lv_source_line CS 'PRIVATE SECTION'
      OR lv_source_line CS 'PROTECTED SECTION'
      OR lv_source_line CS 'ENDMETHOD'
      OR lv_source_line CS 'ENDCLASS'.
        lv_empty = abap_true.
      ENDIF.
    ENDLOOP.

    "Pretty printer source
    CALL FUNCTION 'PRETTY_PRINTER'
      EXPORTING
        inctoo = abap_false
      TABLES
        ntext  = ls_source-source
        otext  = ls_source-source
      EXCEPTIONS
        OTHERS = 1.
    IF sy-subrc <> 0.
      "Set export status
      ls_export_status-status = icon_led_red.
      ls_export_status-type   = mv_type.
      ls_export_status-name   = mv_name.
      ls_export_status-path   = mv_path.
      APPEND ls_export_status TO ct_export_status.
      RAISE EXCEPTION TYPE lcx_export_to_text.
    ENDIF.

    CALL FUNCTION 'CREATE_PRETTY_PRINT_FORMAT'
      EXPORTING
        mode   = 'HIKEY'
      TABLES
        source = ls_source-source
      EXCEPTIONS
        OTHERS = 1.
    IF sy-subrc <> 0.
      "Set export status
      ls_export_status-status = icon_led_red.
      ls_export_status-type   = mv_type.
      ls_export_status-name   = mv_name.
      ls_export_status-path   = mv_path.
      APPEND ls_export_status TO ct_export_status.
      RAISE EXCEPTION TYPE lcx_export_to_text.
    ENDIF.

    ls_source-name      = mv_name.
    ls_source-path      = mv_path.
    ls_source-type      = mv_type.
    ls_source-filename  = mv_name.
    APPEND ls_source TO mt_source.

    "Read exception-text into textpool
    CLEAR ls_source.
    "Check if the class is an exception class
    IF cl_oo_classname_service=>get_class_category( lv_clskey-clsname ) = '40'.
      TRY.
          CREATE OBJECT lo_class
            EXPORTING
              clsname = lv_clskey-clsname.

          lt_attribute = lo_class->get_attributes( ).
          LOOP AT lt_attribute  INTO  ls_attribute
                                WHERE type = 'SOTR_CONC'.
            "Get text from OTR
            REPLACE ALL OCCURRENCES OF '''' IN ls_attribute-attvalue WITH ''.
            lv_otr_concept = ls_attribute-attvalue.
            CALL FUNCTION 'SOTR_GET_TEXT_KEY'
              EXPORTING
                concept = lv_otr_concept
                langu   = sy-langu
              IMPORTING
                e_text  = lv_otr_text
              EXCEPTIONS
                OTHERS  = 1.
            IF sy-subrc = 0.
              IF lv_title IS INITIAL.
                APPEND c_txtp_excp TO ls_source-source.
                lv_title = abap_true.
              ENDIF.
              CONCATENATE ls_attribute-cmpname cl_abap_char_utilities=>horizontal_tab
                          lv_otr_text INTO lv_line.
              APPEND lv_line TO ls_source-source.
            ENDIF.
          ENDLOOP.
        CATCH cx_class_not_existent.                    "#EC NO_HANDLER
      ENDTRY.
      "Set seperator
      IF lv_title = abap_true.
        APPEND INITIAL LINE TO ls_source-source.
        CLEAR lv_title.
      ENDIF.
    ENDIF.

    "Read textpool
    lv_program = cl_oo_classname_service=>get_classpool_name( lv_clskey-clsname ).
    READ TEXTPOOL lv_program INTO lt_textpool LANGUAGE sy-langu.
    LOOP AT lt_textpool INTO  ls_textpool
                        WHERE id  = 'I'.  "Text
      IF lv_title IS INITIAL.
        APPEND c_txtp_text TO ls_source-source.
        lv_title = abap_true.
      ENDIF.
      CONCATENATE ls_textpool-key   cl_abap_char_utilities=>horizontal_tab
                  ls_textpool-entry INTO lv_line.
      APPEND lv_line TO ls_source-source.
    ENDLOOP.
    IF ls_source-source IS NOT INITIAL.
      ls_source-name      = mv_name.
      ls_source-path      = mv_path.
      ls_source-type      = c_r3_txtp.
      CONCATENATE mv_name c_file_txtp INTO ls_source-filename.
      APPEND ls_source TO mt_source.
    ENDIF.
  ENDMETHOD.                    "get_source
ENDCLASS.                    "lcl_export_clif IMPLEMENTATION

*----------------------------------------------------------------------*
* CLASS lcl_export_tabl DEFINITION
* Author          Robin Vleeschhouwer
* Company         RV SAP Consultancy
* Website         www.rvsapconsultancy.com
* E-mail          robin@rvsapconsultancy.com
*----------------------------------------------------------------------*
CLASS lcl_export_tabl DEFINITION INHERITING FROM lcl_export_to_text FINAL.
  PROTECTED SECTION.
    METHODS get_source REDEFINITION.
ENDCLASS.                    "lcl_export_tabl DEFINITION

*----------------------------------------------------------------------*
* CLASS lcl_export_tabl IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_export_tabl IMPLEMENTATION.
  METHOD get_source.
    DATA lt_dd02l         TYPE STANDARD TABLE OF dd02l.
    DATA ls_dd02l         TYPE dd02l.
    DATA lt_dd03l         TYPE STANDARD TABLE OF dd03l.
    DATA ls_dd03l         TYPE dd03l.
    DATA lv_version       TYPE as4vers.
    DATA lv_type          TYPE trobjtype.
    DATA lv_line          TYPE string.
    DATA ls_source        TYPE ty_s_source.
    DATA ls_export_status TYPE ty_s_export_status.

    CLEAR mt_source.

    "Get properties
    SELECT *  INTO  TABLE lt_dd02l
              FROM  dd02l
              WHERE tabname   = mv_name
              AND   as4local  = 'A'.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_export_to_text.
    ENDIF.

    "Determine version
    SORT lt_dd02l BY as4vers DESCENDING.
    READ TABLE lt_dd02l INTO ls_dd02l INDEX 1.

    "Determine type
    CASE ls_dd02l-tabclass.
      WHEN 'TRANSP'.
        lv_type = c_r3_tabl.
      WHEN 'INTTAB'.
        lv_type = c_r3_stru.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE lcx_export_to_text.
    ENDCASE.

    "Get fields
    SELECT *  INTO  TABLE lt_dd03l
              FROM  dd03l
              WHERE tabname   = mv_name
              AND   as4local  = 'A'
              AND   as4vers   = lv_version.
    IF sy-subrc <> 0.
      "Set export status
      ls_export_status-status = icon_led_red.
      ls_export_status-type   = mv_type.
      ls_export_status-name   = mv_name.
      ls_export_status-path   = mv_path.
      APPEND ls_export_status TO ct_export_status.
      RAISE EXCEPTION TYPE lcx_export_to_text.
    ENDIF.
    SORT lt_dd03l BY position ASCENDING.

    "Set header line
    IF lines( lt_dd03l ) > 0.
      CONCATENATE c_tbl_field cl_abap_char_utilities=>horizontal_tab
                  c_tbl_key   cl_abap_char_utilities=>horizontal_tab
                  c_tbl_delem cl_abap_char_utilities=>horizontal_tab
                  c_tbl_dtype cl_abap_char_utilities=>horizontal_tab
                  c_tbl_len   cl_abap_char_utilities=>horizontal_tab
                  c_tbl_dec   cl_abap_char_utilities=>horizontal_tab
                  c_tbl_dom   cl_abap_char_utilities=>horizontal_tab
                  c_tbl_check cl_abap_char_utilities=>horizontal_tab
                  c_tbl_incl
                  INTO lv_line.
      APPEND lv_line TO ls_source-source.
    ENDIF.

    "Set source
    LOOP AT lt_dd03l INTO ls_dd03l.
      CONCATENATE ls_dd03l-fieldname  cl_abap_char_utilities=>horizontal_tab
                  ls_dd03l-keyflag    cl_abap_char_utilities=>horizontal_tab
                  ls_dd03l-rollname   cl_abap_char_utilities=>horizontal_tab
                  ls_dd03l-datatype   cl_abap_char_utilities=>horizontal_tab
                  ls_dd03l-leng       cl_abap_char_utilities=>horizontal_tab
                  ls_dd03l-decimals   cl_abap_char_utilities=>horizontal_tab
                  ls_dd03l-domname    cl_abap_char_utilities=>horizontal_tab
                  ls_dd03l-checktable cl_abap_char_utilities=>horizontal_tab
                  ls_dd03l-precfield
                  INTO lv_line.
      APPEND lv_line TO ls_source-source.
    ENDLOOP.
    IF ls_source-source IS NOT INITIAL.
      IF lv_type = c_r3_tabl.
        CONCATENATE mv_path '\' p_dtabl INTO ls_source-path.
      ELSE.
        CONCATENATE mv_path '\' p_dstru INTO ls_source-path.
      ENDIF.
      ls_source-name      = mv_name.
      ls_source-type      = lv_type.
      ls_source-filename  = mv_name.
      APPEND ls_source TO mt_source.
    ENDIF.
  ENDMETHOD.                    "get_source
ENDCLASS.                    "lcl_export_tabl IMPLEMENTATION

*----------------------------------------------------------------------*
* CLASS lcl_export_msag DEFINITION
* Author          Robin Vleeschhouwer
* Company         RV SAP Consultancy
* Website         www.rvsapconsultancy.com
* E-mail          robin@rvsapconsultancy.com
*----------------------------------------------------------------------*
CLASS lcl_export_msag DEFINITION INHERITING FROM lcl_export_to_text FINAL.
  PROTECTED SECTION.
    METHODS get_source REDEFINITION.
ENDCLASS.                    "lcl_export_msag DEFINITION

*----------------------------------------------------------------------*
* CLASS lcl_export_msag IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_export_msag IMPLEMENTATION.
  METHOD get_source.
    DATA ls_t100t   TYPE t100t.
    DATA lt_t100    TYPE STANDARD TABLE OF t100.
    DATA ls_t100    TYPE t100.
    DATA lv_line    TYPE string.
    DATA ls_source  TYPE ty_s_source.

    CLEAR mt_source.

    "Get definition
    SELECT SINGLE * INTO  ls_t100t
                    FROM  t100t
                    WHERE sprsl = sy-langu
                    AND   arbgb = mv_name.

    "Get texts
    SELECT *  INTO  TABLE lt_t100                       "#EC CI_GENBUFF
              FROM  t100
              WHERE sprsl = sy-langu
              AND   arbgb = mv_name.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_export_to_text.
    ENDIF.
    SORT lt_t100 BY msgnr.

    IF lines( lt_t100 ) > 0.
      "Set definition line
      CONCATENATE c_msg_descr   cl_abap_char_utilities=>horizontal_tab
                  ls_t100t-stext INTO lv_line.
      APPEND lv_line TO ls_source-source.
      CONCATENATE c_msg_langu   cl_abap_char_utilities=>horizontal_tab
                  ls_t100t-sprsl INTO lv_line.
      APPEND lv_line TO ls_source-source.
      APPEND INITIAL LINE TO ls_source-source.

      "Set header line
      CONCATENATE c_msg_numb    cl_abap_char_utilities=>horizontal_tab
                  c_msg_msag
                  INTO lv_line.
      APPEND lv_line TO ls_source-source.
    ENDIF.

    "Set source
    LOOP AT lt_t100 INTO ls_t100.
      CONCATENATE ls_t100-msgnr cl_abap_char_utilities=>horizontal_tab
                  ls_t100-text
                  INTO lv_line.
      APPEND lv_line TO ls_source-source.
    ENDLOOP.
    IF ls_source-source IS NOT INITIAL.
      ls_source-type      = mv_type.
      ls_source-name      = mv_name.
      ls_source-path      = mv_path.
      ls_source-filename  = mv_name.
      APPEND ls_source TO mt_source.
    ENDIF.
  ENDMETHOD.                    "get_source
ENDCLASS.                    "lcl_export_msag IMPLEMENTATION

*----------------------------------------------------------------------*
* CLASS lcl_main_ctr DEFINITION
* Author          Robin Vleeschhouwer
* Company         RV SAP Consultancy
* Website         www.rvsapconsultancy.com
* E-mail          robin@rvsapconsultancy.com
*----------------------------------------------------------------------*
CLASS lcl_main_ctr DEFINITION CREATE PRIVATE FINAL.
  PUBLIC SECTION.
    CLASS-DATA so_main_ctr TYPE REF TO lcl_main_ctr.

    CLASS-METHODS class_constructor.
    METHODS process.

  PRIVATE SECTION.
    METHODS get_selection             EXPORTING et_selection      TYPE ty_t_selection.
    METHODS get_object_from_tran      CHANGING  ct_object         TYPE ty_t_object.
    METHODS get_object_from_func      IMPORTING it_r_funcname     LIKE s_sfunc[]  OPTIONAL
                                                it_funcname       TYPE fbname     OPTIONAL
                                      CHANGING  ct_object         TYPE ty_t_object.
    METHODS set_selection_from_object IMPORTING it_object         TYPE ty_t_object
                                                iv_package        TYPE sap_bool
                                      CHANGING  ct_selection      TYPE ty_t_selection.
    METHODS display                   CHANGING  ct_export_status  TYPE ty_t_export_status.
ENDCLASS.                    "lcl_main_ctr DEFINITION

*----------------------------------------------------------------------*
* CLASS lcl_main_ctr IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_main_ctr IMPLEMENTATION.
  METHOD class_constructor.
    CREATE OBJECT so_main_ctr.
  ENDMETHOD.                    "class_constructor

  METHOD process.
    DATA lo_export_to_text  TYPE REF TO lcl_export_to_text.
    DATA lt_selection       TYPE ty_t_selection.
    DATA ls_selection       TYPE ty_s_selection.
    DATA lt_export_status   TYPE ty_t_export_status.
    DATA lv_text            TYPE char255.
    DATA lv_percentage      TYPE n LENGTH 3.

    "Get the selection
    get_selection( IMPORTING et_selection = lt_selection ).

    "Export objects to text files
    LOOP AT lt_selection INTO ls_selection.
      "Display progression
      lv_percentage = 100 * sy-tabix / lines( lt_selection ).
      WRITE lv_percentage TO lv_percentage USING EDIT MASK '==ALPHA'.
      WRITE lv_percentage TO lv_percentage RIGHT-JUSTIFIED.
      CONCATENATE lv_percentage '% - ' ls_selection-name INTO lv_text RESPECTING BLANKS.
      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          text = lv_text.

      "Export the objects
      CASE ls_selection-type.
        WHEN c_r3_prog OR c_r3_fugr.
          CREATE OBJECT lo_export_to_text
            TYPE
            lcl_export_prog
            EXPORTING
              iv_name = ls_selection-name
              iv_path = ls_selection-path
              iv_type = ls_selection-type.
        WHEN c_r3_wdyn.
          CREATE OBJECT lo_export_to_text
            TYPE
            lcl_export_wdyn
            EXPORTING
              iv_name = ls_selection-name
              iv_path = ls_selection-path
              iv_type = ls_selection-type.
        WHEN c_r3_clas OR c_r3_intf.
          CREATE OBJECT lo_export_to_text
            TYPE
            lcl_export_clif
            EXPORTING
              iv_name = ls_selection-name
              iv_path = ls_selection-path
              iv_type = ls_selection-type.
        WHEN c_r3_tabl.
          CREATE OBJECT lo_export_to_text
            TYPE
            lcl_export_tabl
            EXPORTING
              iv_name = ls_selection-name
              iv_path = ls_selection-path
              iv_type = ls_selection-type.
        WHEN c_r3_msag.
          CREATE OBJECT lo_export_to_text
            TYPE
            lcl_export_msag
            EXPORTING
              iv_name = ls_selection-name
              iv_path = ls_selection-path
              iv_type = ls_selection-type.
      ENDCASE.
      lo_export_to_text->export_to_text( CHANGING ct_export_status = lt_export_status ).
      CLEAR lo_export_to_text.
    ENDLOOP.

    "Display export status
    IF lt_export_status IS NOT INITIAL.
      display( CHANGING ct_export_status = lt_export_status ).
    ENDIF.
  ENDMETHOD.                    "process

  METHOD get_selection.
    DATA lt_object      TYPE ty_t_object.
    DATA lt_r_obj_name  TYPE RANGE OF sobj_name.
    DATA ls_r_obj_name  LIKE LINE OF lt_r_obj_name.

    IF p_xcust = abap_true.
      "Set only customer objects
      ls_r_obj_name-sign    = 'I'.
      ls_r_obj_name-option  = 'CP'.
      ls_r_obj_name-low     = 'Z*'.
      APPEND ls_r_obj_name TO lt_r_obj_name.
      ls_r_obj_name-low     = 'Y*'.
      APPEND ls_r_obj_name TO lt_r_obj_name.
      "Include objects in namespace
      IF p_xnams IS NOT INITIAL.
        CONCATENATE p_xnams '*' INTO ls_r_obj_name-low.
        APPEND ls_r_obj_name TO lt_r_obj_name.
      ENDIF.
    ENDIF.

    "Package
    IF s_spack IS NOT INITIAL.
      SELECT obj_name
             object
             devclass INTO  TABLE lt_object           "#EC CI_SGLSELECT
                      FROM  tadir
                      WHERE pgmid       = 'R3TR'
                      AND ( object      = c_r3_prog
                      OR    object      = c_r3_wdyn
                      OR    object      = c_r3_fugr
                      OR    object      = c_r3_clas
                      OR    object      = c_r3_intf
                      OR    object      = c_r3_tabl
                      OR    object      = c_r3_msag )
                      AND   obj_name   IN lt_r_obj_name   "Customer objects
                      AND   devclass   IN s_spack
                      AND   devclass   IN s_fpack         "Filter
                      AND   author     IN s_fusrn         "Filter
                      AND   delflag     = abap_false.
      "Translate object to selection table
      IF lt_object IS NOT INITIAL.
        set_selection_from_object(  EXPORTING it_object     = lt_object
                                              iv_package    = abap_true
                                    CHANGING  ct_selection  = et_selection ).
        CLEAR lt_object.
      ENDIF.
    ENDIF.
    "Username
    IF s_susrn IS NOT INITIAL.
      SELECT obj_name
             object
             devclass INTO  TABLE lt_object           "#EC CI_SGLSELECT
                      FROM  tadir
                      WHERE pgmid       = 'R3TR'
                      AND ( object      = c_r3_prog
                      OR    object      = c_r3_wdyn
                      OR    object      = c_r3_fugr
                      OR    object      = c_r3_clas
                      OR    object      = c_r3_intf
                      OR    object      = c_r3_tabl
                      OR    object      = c_r3_msag )
                      AND   obj_name   IN lt_r_obj_name   "Customer objects
                      AND   author     IN s_susrn
                      AND   devclass   IN s_fpack         "Filter
                      AND   author     IN s_fusrn         "Filter
                      AND   delflag     = abap_false.
      "Translate object to selection table
      IF lt_object IS NOT INITIAL.
        set_selection_from_object(  EXPORTING it_object     = lt_object
                                              iv_package    = abap_true
                                    CHANGING  ct_selection  = et_selection ).
        CLEAR lt_object.
      ENDIF.
    ENDIF.
    "Transport
    IF s_stran IS NOT INITIAL.
      get_object_from_tran( CHANGING ct_object = lt_object ).
      "Translate object to selection table
      IF lt_object IS NOT INITIAL.
        set_selection_from_object(  EXPORTING it_object     = lt_object
                                              iv_package    = abap_true
                                    CHANGING  ct_selection  = et_selection ).
        CLEAR lt_object.
      ENDIF.
    ENDIF.
    "Program
    IF s_sprog IS NOT INITIAL.
      SELECT obj_name
             object
             devclass APPENDING TABLE lt_object       "#EC CI_SGLSELECT
                      FROM  tadir
                      WHERE pgmid       = 'R3TR'
                      AND   object      = c_r3_prog
                      AND   obj_name   IN s_sprog
                      AND   obj_name   IN lt_r_obj_name   "Customer objects
                      AND   devclass   IN s_fpack         "Filter
                      AND   author     IN s_fusrn         "Filter
                      AND   delflag     = abap_false.
    ENDIF.
    "Web Dynpro
    IF s_swdyn IS NOT INITIAL.
      SELECT obj_name
             object
             devclass APPENDING TABLE lt_object       "#EC CI_SGLSELECT
                      FROM  tadir
                      WHERE pgmid       = 'R3TR'
                      AND   object      = c_r3_wdyn
                      AND   obj_name   IN s_swdyn
                      AND   obj_name   IN lt_r_obj_name   "Customer objects
                      AND   devclass   IN s_fpack         "Filter
                      AND   author     IN s_fusrn         "Filter
                      AND   delflag     = abap_false.
    ENDIF.
    "Function group
    IF s_sfugr IS NOT INITIAL.
      SELECT obj_name
             object
             devclass APPENDING TABLE lt_object       "#EC CI_SGLSELECT
                      FROM  tadir
                      WHERE pgmid       = 'R3TR'
                      AND   object      = c_r3_fugr
                      AND   obj_name   IN s_sfugr
                      AND   obj_name   IN lt_r_obj_name   "Customer objects
                      AND   devclass   IN s_fpack         "Filter
                      AND   author     IN s_fusrn         "Filter
                      AND   delflag     = abap_false.
    ENDIF.
    "Function modules
    IF s_sfunc IS NOT INITIAL.
      get_object_from_func( EXPORTING it_r_funcname = s_sfunc[]
                            CHANGING  ct_object     = lt_object ).
    ENDIF.
    "Class or interface
    IF s_sclif IS NOT INITIAL.
      SELECT obj_name
             object
             devclass APPENDING TABLE lt_object       "#EC CI_SGLSELECT
                      FROM  tadir
                      WHERE pgmid       = 'R3TR'
                      AND ( object      = c_r3_clas
                      OR    object      = c_r3_intf )
                      AND   obj_name   IN s_sclif
                      AND   obj_name   IN lt_r_obj_name   "Customer objects
                      AND   devclass   IN s_fpack         "Filter
                      AND   author     IN s_fusrn         "Filter
                      AND   delflag     = abap_false.
    ENDIF.
    "Table or structure
    IF s_stabl IS NOT INITIAL.
      SELECT obj_name
             object
             devclass APPENDING TABLE lt_object       "#EC CI_SGLSELECT
                      FROM  tadir
                      WHERE pgmid       = 'R3TR'
                      AND   object      = c_r3_tabl
                      AND   obj_name   IN s_stabl
                      AND   obj_name   IN lt_r_obj_name   "Customer objects
                      AND   devclass   IN s_fpack         "Filter
                      AND   author     IN s_fusrn         "Filter
                      AND   delflag     = abap_false.
    ENDIF.
    "Message class
    IF s_smsag IS NOT INITIAL.
      SELECT obj_name
             object
             devclass APPENDING TABLE lt_object       "#EC CI_SGLSELECT
                      FROM  tadir
                      WHERE pgmid       = 'R3TR'
                      AND   object      = c_r3_msag
                      AND   obj_name   IN s_smsag
                      AND   obj_name   IN lt_r_obj_name   "Customer objects
                      AND   devclass   IN s_fpack         "Filter
                      AND   author     IN s_fusrn         "Filter
                      AND   delflag     = abap_false.
    ENDIF.

    "Translate object to selection table
    IF lt_object IS NOT INITIAL.
      set_selection_from_object(  EXPORTING it_object     = lt_object
                                            iv_package    = abap_false
                                  CHANGING  ct_selection  = et_selection ).
    ENDIF.

    "Delete duplicates
    SORT et_selection BY type name path.
    DELETE ADJACENT DUPLICATES FROM et_selection COMPARING ALL FIELDS.
  ENDMETHOD.                    "get_selection

  METHOD get_object_from_tran.
    DATA lt_r_trkorr    TYPE RANGE OF tr_trkorr.
    DATA ls_r_trkorr    LIKE LINE OF lt_r_trkorr.
    DATA lt_trkorr      TYPE trkorrs.
    DATA lv_trkorr      TYPE tr_trkorr.
    DATA lt_object      TYPE ty_t_object.
    DATA lt_funcname    TYPE fbname.
    DATA lt_r_obj_name  TYPE RANGE OF sobj_name.
    DATA ls_r_obj_name  LIKE LINE OF lt_r_obj_name.

    FIELD-SYMBOLS <ls_object> TYPE ty_s_object.

    "Add selection to range
    lt_r_trkorr = s_stran[].

    "Add subtasks from transport to range
    SELECT trkorr INTO TABLE lt_trkorr
                  FROM e070
                  WHERE strkorr IN s_stran.

    "Set range
    ls_r_trkorr-sign   = 'I'.
    ls_r_trkorr-option = 'EQ'.
    LOOP AT lt_trkorr INTO lv_trkorr.
      ls_r_trkorr-low = lv_trkorr.
      APPEND ls_r_trkorr TO lt_r_trkorr.
    ENDLOOP.

    "Get objects from transport
    SELECT obj_name
           object   INTO  TABLE lt_object
                    FROM  e071
                    WHERE trkorr IN lt_r_trkorr
                    AND ( pgmid     = 'R3TR'
                    AND ( object    = c_r3_prog
                    OR    object    = c_r3_wdyn
                    OR    object    = c_r3_fugr
                    OR    object    = c_r3_clas
                    OR    object    = c_r3_intf
                    OR    object    = c_r3_tabl
                    OR    object    = c_r3_msag )
                    OR    pgmid     = 'LIMU'
                    AND   object    = c_r3_func
                    OR    pgmid     = 'LIMU'
                    AND   object    = 'METH'
                    OR    pgmid     = 'LIMU'
                    AND   object    = c_r3_wdyc
                    OR    pgmid     = 'LIMU'
                    AND   object    = 'REPS' )  ##too_many_itab_fields.

    "Determine programs and classes
    LOOP AT lt_object ASSIGNING <ls_object>.
      IF <ls_object>-type = 'METH'.
        <ls_object>-name  = <ls_object>-name(30).
        <ls_object>-type  = c_r3_clas.
      ELSEIF <ls_object>-type = c_r3_wdyc.
        <ls_object>-name  = <ls_object>-name(30).
        <ls_object>-type  = c_r3_wdyn.
      ELSEIF <ls_object>-type = 'REPS'.
        <ls_object>-type  = c_r3_prog.
      ENDIF.
    ENDLOOP.

    "Delete duplicates
    SORT lt_object BY type name.
    DELETE ADJACENT DUPLICATES FROM lt_object COMPARING ALL FIELDS.

    "Determine function modules
    LOOP AT lt_object ASSIGNING <ls_object>
                      WHERE type = c_r3_func.
      APPEND <ls_object>-name TO lt_funcname.
    ENDLOOP.
    DELETE lt_object WHERE type = c_r3_func.

    IF lt_object IS NOT INITIAL.
      IF p_xcust = abap_true.
        "Set only customer objects
        ls_r_obj_name-sign    = 'I'.
        ls_r_obj_name-option  = 'CP'.
        ls_r_obj_name-low     = 'Z*'.
        APPEND ls_r_obj_name TO lt_r_obj_name.
        ls_r_obj_name-low     = 'Y*'.
        APPEND ls_r_obj_name TO lt_r_obj_name.
        "Include objects in namespace
        IF p_xnams IS NOT INITIAL.
          CONCATENATE p_xnams '*' INTO ls_r_obj_name-low.
          APPEND ls_r_obj_name TO lt_r_obj_name.
        ENDIF.
      ENDIF.

      "Get objects from transport
      SELECT obj_name                                 "#EC CI_SGLSELECT
             object
             devclass APPENDING TABLE ct_object
                      FROM  tadir
                      FOR ALL ENTRIES IN lt_object
                      WHERE pgmid       = 'R3TR'
                      AND   object      = lt_object-type
                      AND   obj_name    = lt_object-name
                      AND   obj_name   IN lt_r_obj_name   "Customer objects
                      AND   devclass   IN s_fpack         "Filter
                      AND   author     IN s_fusrn         "Filter
                      AND   delflag     = abap_false.
    ENDIF.

    IF lt_funcname IS NOT INITIAL.
      get_object_from_func( EXPORTING it_funcname = lt_funcname
                            CHANGING  ct_object   = ct_object ).
    ENDIF.

    "Delete duplicates
    SORT ct_object BY type name.
    DELETE ADJACENT DUPLICATES FROM ct_object COMPARING ALL FIELDS.
  ENDMETHOD.                    "get_object_from_tran

  METHOD get_object_from_func.
    DATA lt_object      TYPE ty_t_object.
    DATA lt_r_obj_name  TYPE RANGE OF sobj_name.
    DATA ls_r_obj_name  LIKE LINE OF lt_r_obj_name.

    FIELD-SYMBOLS <ls_object> TYPE ty_s_object.

    "Get function group
    IF it_r_funcname IS NOT INITIAL.
      SELECT pname  INTO  TABLE lt_object             "#EC CI_SGLSELECT
                    FROM  tfdir
                    WHERE funcname IN it_r_funcname   ##too_many_itab_fields.
    ENDIF.
    IF it_funcname IS NOT INITIAL.
      SELECT pname  INTO  TABLE lt_object             "#EC CI_SGLSELECT
                    FROM  tfdir
                    FOR   ALL ENTRIES IN it_funcname
                    WHERE funcname = it_funcname-table_line ##too_many_itab_fields.
    ENDIF.

    "Delete duplicates
    SORT lt_object BY name.
    DELETE ADJACENT DUPLICATES FROM lt_object COMPARING name.

    LOOP AT lt_object ASSIGNING <ls_object>.
      "Remove SAPL
      <ls_object>-name = <ls_object>-name+4.
      <ls_object>-type = c_r3_fugr.
    ENDLOOP.

    IF lt_object IS NOT INITIAL.
      IF p_xcust = abap_true.
        "Set only customer objects
        ls_r_obj_name-sign    = 'I'.
        ls_r_obj_name-option  = 'CP'.
        ls_r_obj_name-low     = 'Z*'.
        APPEND ls_r_obj_name TO lt_r_obj_name.
        ls_r_obj_name-low     = 'Y*'.
        APPEND ls_r_obj_name TO lt_r_obj_name.
        "Include objects in namespace
        IF p_xnams IS NOT INITIAL.
          CONCATENATE p_xnams '*' INTO ls_r_obj_name-low.
          APPEND ls_r_obj_name TO lt_r_obj_name.
        ENDIF.
      ENDIF.

      "Get objects from function
      SELECT obj_name                                 "#EC CI_SGLSELECT
             object
             devclass APPENDING TABLE ct_object
                      FROM  tadir
                      FOR ALL ENTRIES IN lt_object
                      WHERE pgmid       = 'R3TR'
                      AND   object      = lt_object-type
                      AND   obj_name    = lt_object-name
                      AND   obj_name   IN lt_r_obj_name   "Customer objects
                      AND   devclass   IN s_fpack         "Filter
                      AND   author     IN s_fusrn         "Filter
                      AND   delflag     = abap_false.
    ENDIF.
  ENDMETHOD.                    "get_object_from_func

  METHOD set_selection_from_object.
    DATA ls_object    TYPE ty_s_object.
    DATA ls_selection TYPE ty_s_selection.
    DATA lv_droot     TYPE string.

    LOOP AT it_object INTO ls_object.
      "Filter
      IF  ( ls_object-type = c_r3_prog
      AND   ls_object-name NOT IN s_fprog )
      OR  ( ls_object-type = c_r3_wdyn
      AND   ls_object-name NOT IN s_fwdyn )
      OR  ( ls_object-type = c_r3_fugr
      AND   ls_object-name NOT IN s_ffugr ).
        CONTINUE.
      ENDIF.
      "Set root directory
      IF iv_package = abap_true.
        CONCATENATE p_droot '\' ls_object-pack INTO lv_droot.
      ELSE.
        lv_droot = p_droot.
      ENDIF.
      "Add to selection
      CASE ls_object-type.
        WHEN c_r3_prog.
          ls_selection-type = ls_object-type.
          ls_selection-name = ls_object-name.
          CONCATENATE lv_droot '\' p_dprog '\' ls_object-name INTO ls_selection-path.
          APPEND ls_selection TO ct_selection.
        WHEN c_r3_wdyn.
          ls_selection-type = ls_object-type.
          ls_selection-name = ls_object-name.
          CONCATENATE lv_droot '\' p_dwdyn '\' ls_object-name INTO ls_selection-path.
          APPEND ls_selection TO ct_selection.
        WHEN c_r3_fugr.
          ls_selection-type = ls_object-type.
          CONCATENATE 'SAPL' ls_object-name INTO ls_selection-name.
          CONCATENATE lv_droot '\' p_dfugr '\' ls_object-name INTO ls_selection-path.
          APPEND ls_selection TO ct_selection.
        WHEN c_r3_clas.
          ls_selection-type = ls_object-type.
          ls_selection-name = ls_object-name.
          CONCATENATE lv_droot '\' p_dclas INTO ls_selection-path.
          APPEND ls_selection TO ct_selection.
        WHEN c_r3_intf.
          ls_selection-type = ls_object-type.
          ls_selection-name = ls_object-name.
          CONCATENATE lv_droot '\' p_dintf INTO ls_selection-path.
          APPEND ls_selection TO ct_selection.
        WHEN c_r3_tabl.
          ls_selection-type = ls_object-type.
          ls_selection-name = ls_object-name.
          "Path for structure or table will be determined in object
          ls_selection-path = lv_droot.
          APPEND ls_selection TO ct_selection.
        WHEN c_r3_msag.
          ls_selection-type = ls_object-type.
          ls_selection-name = ls_object-name.
          CONCATENATE lv_droot '\' p_dmsag INTO ls_selection-path.
          APPEND ls_selection TO ct_selection.
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.                    "get_selection_from_object

  METHOD display.
    DATA lo_alv         TYPE REF TO cl_salv_table.
    DATA lx_error       TYPE REF TO cx_root.
    DATA lo_functions   TYPE REF TO cl_salv_functions.
    DATA lo_columns     TYPE REF TO cl_salv_columns_table.
    DATA lo_column      TYPE REF TO cl_salv_column_table.
    DATA lt_columns     TYPE salv_t_column_ref.
    DATA ls_column      TYPE salv_s_column_ref.
    DATA ls_colors      TYPE lvc_s_scol.
    DATA lv_stext       TYPE scrtext_s.
    DATA lv_mtext       TYPE scrtext_m.
    DATA lv_ltext       TYPE scrtext_l.

    FIELD-SYMBOLS <ls_export_status> TYPE ty_s_export_status.

    LOOP AT ct_export_status ASSIGNING <ls_export_status>.
      "Set colors
      IF  <ls_export_status>-type <> c_r3_prog
      AND <ls_export_status>-type <> c_r3_wdyn
      AND <ls_export_status>-type <> c_r3_fugr
      AND <ls_export_status>-type <> c_r3_clas
      AND <ls_export_status>-type <> c_r3_intf
      AND <ls_export_status>-type <> c_r3_tabl
      AND <ls_export_status>-type <> c_r3_stru
      AND <ls_export_status>-type <> c_r3_msag.
        ls_colors-color-col = col_background.
        ls_colors-color-int = 1.
        APPEND ls_colors TO <ls_export_status>-colors.
      ENDIF.
      "Set path
      CONCATENATE <ls_export_status>-path '\' INTO <ls_export_status>-path.
    ENDLOOP.

    TRY.
        "Fullscreen ALV
        CALL METHOD cl_salv_table=>factory
          EXPORTING
            list_display = abap_false
          IMPORTING
            r_salv_table = lo_alv
          CHANGING
            t_table      = ct_export_status.

        "Set toolbar
        lo_functions = lo_alv->get_functions_base( ).
        lo_functions->set_all( value = abap_true ).

        "Optimize all columns
        lo_columns = lo_alv->get_columns( ).
        lo_columns->set_optimize( value = abap_true ).
        lo_columns->set_color_column( value = 'COLORS' ).
        lt_columns = lo_columns->get( ).
        "Set attributes of the columns
        LOOP AT lt_columns INTO ls_column.
          lo_column ?= ls_column-r_column.
          CASE lo_column->get_columnname( ).
            WHEN 'STATUS'.
              lv_ltext = lv_mtext = lv_stext = c_alv_stat.
            WHEN 'TYPE'.
              lv_ltext = lv_mtext = lv_stext = c_alv_type.
            WHEN 'PATH'.
              lv_ltext = lv_mtext = lv_stext = c_alv_path.
            WHEN 'NAME'.
              lv_ltext = lv_mtext = lv_stext = c_alv_name.
            WHEN 'MESSAGE'.
              lv_ltext = lv_mtext = lv_stext = c_alv_msg.
            WHEN OTHERS.
              CONTINUE.
          ENDCASE.
          "Set header text
          lo_column->set_short_text(  lv_stext ).
          lo_column->set_medium_text( lv_mtext ).
          lo_column->set_long_text(   lv_ltext ).
        ENDLOOP.

        lo_alv->display( ).
      CATCH cx_salv_msg cx_salv_data_error INTO lx_error.
        MESSAGE lx_error TYPE 'E'.
    ENDTRY.
  ENDMETHOD.                    "display
ENDCLASS.                    "lcl_main_ctr IMPLEMENTATION

*----------------------------------------------------------------------*
* CLASS lcl_value_help DEFINITION
* Author          Robin Vleeschhouwer
* Company         RV SAP Consultancy
* Website         www.rvsapconsultancy.com
* E-mail          robin@rvsapconsultancy.com
*----------------------------------------------------------------------*
CLASS lcl_value_help DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS get_value_help_prog IMPORTING iv_field_name       TYPE string
                                      CHANGING  cv_field            TYPE program.
    CLASS-METHODS get_value_help_wdyn IMPORTING iv_field_name       TYPE string
                                      CHANGING  cv_field            TYPE wdy_component_name.
    CLASS-METHODS get_value_help_fugr IMPORTING iv_field_name       TYPE string
                                      CHANGING  cv_field            TYPE rs38l_area.
    CLASS-METHODS get_value_help_msag IMPORTING iv_field_name       TYPE string
                                      CHANGING  cv_field            TYPE msgid.
    CLASS-METHODS get_value_help_dir  IMPORTING iv_field_name       TYPE string
                                      CHANGING  cv_field            TYPE string.
  PRIVATE SECTION.
    CLASS-METHODS get_rep_info_sys_f4 IMPORTING iv_type             TYPE seu_obj
                                                iv_value            TYPE string
                                      RETURNING value(rv_selection) TYPE string.
    CLASS-METHODS get_field_value     IMPORTING iv_field_name       TYPE string
                                      RETURNING value(rv_value)     TYPE string.

ENDCLASS.                    "lcl_value_help DEFINITION

*----------------------------------------------------------------------*
* CLASS lcl_value_help IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_value_help IMPLEMENTATION.
  METHOD get_value_help_prog.
    DATA lv_selection TYPE string.
    DATA lv_value     TYPE string.

    lv_value = get_field_value( iv_field_name ).
    "Select program
    lv_selection = get_rep_info_sys_f4( iv_type   = 'PROG'
                                        iv_value  = lv_value ).
    IF lv_selection IS NOT INITIAL.
      cv_field = lv_selection.
    ELSE.
      cv_field = lv_value.
    ENDIF.
  ENDMETHOD.                    "get_value_help_prog

  METHOD get_value_help_wdyn.
    DATA lv_selection TYPE string.
    DATA lv_value     TYPE string.

    lv_value = get_field_value( iv_field_name ).
    "Select web dynpro
    lv_selection = get_rep_info_sys_f4( iv_type   = 'WDYN'
                                        iv_value  = lv_value ).
    IF lv_selection IS NOT INITIAL.
      cv_field = lv_selection.
    ELSE.
      cv_field = lv_value.
    ENDIF.
  ENDMETHOD.                    "get_value_help_wdyn

  METHOD get_value_help_fugr.
    DATA lv_selection TYPE string.
    DATA lv_value     TYPE string.

    lv_value = get_field_value( iv_field_name ).
    "Select function group
    lv_selection = get_rep_info_sys_f4( iv_type   = 'F'
                                        iv_value  = lv_value ).
    IF lv_selection IS NOT INITIAL.
      cv_field = lv_selection.
    ELSE.
      cv_field = lv_value.
    ENDIF.
  ENDMETHOD.                    "get_value_help_fugr

  METHOD get_value_help_msag.
    DATA lv_selection TYPE string.
    DATA lv_value     TYPE string.

    lv_value = get_field_value( iv_field_name ).
    "Select function group
    lv_selection = get_rep_info_sys_f4( iv_type   = 'MSAG'
                                        iv_value  = lv_value ).
    IF lv_selection IS NOT INITIAL.
      cv_field = lv_selection.
    ELSE.
      cv_field = lv_value.
    ENDIF.
  ENDMETHOD.                    "get_value_help_msag

  METHOD get_value_help_dir.
    DATA lv_window_title  TYPE string VALUE c_lbl_droot.
    DATA lv_selection     TYPE string.
    DATA lv_value         TYPE string.

    lv_value = get_field_value( iv_field_name ).
    "Select root directory
    cl_gui_frontend_services=>directory_browse( EXPORTING   window_title    = lv_window_title
                                                            initial_folder  = lv_value
                                                CHANGING    selected_folder = lv_selection
                                                EXCEPTIONS  OTHERS          = 1 ).
    IF  sy-subrc = 0
    AND lv_selection IS NOT INITIAL.
      cv_field = lv_selection.
    ELSE.
      cv_field = lv_value.
    ENDIF.
  ENDMETHOD.                    "get_value_help_dir

  METHOD get_rep_info_sys_f4.
    "Select function group
    CALL FUNCTION 'REPOSITORY_INFO_SYSTEM_F4'
      EXPORTING
        object_type          = iv_type
        object_name          = iv_value
        suppress_selection   = abap_true
      IMPORTING
        object_name_selected = rv_selection
      EXCEPTIONS
        OTHERS               = 0.
  ENDMETHOD.                    "get_rep_info_sys_f4

  METHOD get_field_value.
    DATA lt_dynpfields  TYPE dynpread_tabtype.
    DATA ls_dynpfields  TYPE dynpread.
    ls_dynpfields-fieldname = iv_field_name.
    APPEND ls_dynpfields TO lt_dynpfields.

    "Get field value
    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        dyname     = sy-repid
        dynumb     = sy-dynnr
      TABLES
        dynpfields = lt_dynpfields
      EXCEPTIONS
        OTHERS     = 0.
    READ TABLE lt_dynpfields INTO ls_dynpfields INDEX 1.
    rv_value = ls_dynpfields-fieldvalue.
  ENDMETHOD.                    "get_field_value
ENDCLASS.                    "lcl_value_help IMPLEMENTATION

*--------------------------------------------------------------------*
* Value-request s_stran
*--------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_stran-low.
  CALL FUNCTION 'TR_F4_REQUESTS'
    IMPORTING
      ev_selected_request = s_stran-low.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_stran-high.
  CALL FUNCTION 'TR_F4_REQUESTS'
    IMPORTING
      ev_selected_request = s_stran-high.

*--------------------------------------------------------------------*
* Value-request s_sprog
*--------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_sprog-low.
  lcl_value_help=>get_value_help_prog(  EXPORTING iv_field_name = 'S_SPROG-LOW'
                                        CHANGING  cv_field      = s_sprog-low ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_sprog-high.
  lcl_value_help=>get_value_help_prog(  EXPORTING iv_field_name = 'S_SPROG-HIGH'
                                        CHANGING  cv_field      = s_sprog-high ).

*--------------------------------------------------------------------*
* Value-request s_swdyn
*--------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_swdyn-low.
  lcl_value_help=>get_value_help_wdyn(  EXPORTING iv_field_name = 'S_SWDYN-LOW'
                                        CHANGING  cv_field      = s_swdyn-low ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_swdyn-high.
  lcl_value_help=>get_value_help_wdyn(  EXPORTING iv_field_name = 'S_SWDYN-HIGH'
                                        CHANGING  cv_field      = s_swdyn-high ).

*--------------------------------------------------------------------*
* Value-request s_sfugr
*--------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_sfugr-low.
  lcl_value_help=>get_value_help_fugr(  EXPORTING iv_field_name = 'S_SFUGR-LOW'
                                        CHANGING  cv_field      = s_sfugr-low ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_sfugr-high.
  lcl_value_help=>get_value_help_fugr(  EXPORTING iv_field_name = 'S_SFUGR-HIGH'
                                        CHANGING  cv_field      = s_sfugr-high ).

*--------------------------------------------------------------------*
* Value-request s_smsag
*--------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_smsag-low.
  lcl_value_help=>get_value_help_msag(  EXPORTING iv_field_name = 'S_SMSAG-LOW'
                                        CHANGING  cv_field      = s_smsag-low ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_smsag-high.
  lcl_value_help=>get_value_help_msag(  EXPORTING iv_field_name = 'S_SMSAG-HIGH'
                                        CHANGING  cv_field      = s_smsag-high ).

*--------------------------------------------------------------------*
* Value-request s_fprog
*--------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_fprog-low.
  lcl_value_help=>get_value_help_prog(  EXPORTING iv_field_name = 'S_FPROG-LOW'
                                        CHANGING  cv_field      = s_fprog-low ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_fprog-high.
  lcl_value_help=>get_value_help_prog(  EXPORTING iv_field_name = 'S_FPROG-HIGH'
                                        CHANGING  cv_field      = s_fprog-high ).

*--------------------------------------------------------------------*
* Value-request s_fwdyn
*--------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_fwdyn-low.
  lcl_value_help=>get_value_help_wdyn(  EXPORTING iv_field_name = 'S_FWDYN-LOW'
                                        CHANGING  cv_field      = s_fwdyn-low ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_fwdyn-high.
  lcl_value_help=>get_value_help_wdyn(  EXPORTING iv_field_name = 'S_FWDYN-HIGH'
                                        CHANGING  cv_field      = s_fwdyn-high ).

*--------------------------------------------------------------------*
* Value-request s_ffugr
*--------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_ffugr-low.
  lcl_value_help=>get_value_help_fugr(  EXPORTING iv_field_name = 'S_FFUGR-LOW'
                                        CHANGING  cv_field      = s_ffugr-low ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_ffugr-high.
  lcl_value_help=>get_value_help_fugr(  EXPORTING iv_field_name = 'S_FFUGR-HIGH'
                                        CHANGING  cv_field      = s_ffugr-high ).

*--------------------------------------------------------------------*
* Value-request p_droot
*--------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_droot.
  lcl_value_help=>get_value_help_dir( EXPORTING iv_field_name = 'P_DROOT'
                                      CHANGING  cv_field      = p_droot ).

*--------------------------------------------------------------------*
* Start of selection
*--------------------------------------------------------------------*
START-OF-SELECTION.
  lcl_main_ctr=>so_main_ctr->process( ).
```