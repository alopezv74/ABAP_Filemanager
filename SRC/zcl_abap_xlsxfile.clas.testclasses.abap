*"* use this source file for your ABAP unit test classes
CLASS ltc_upload_excel DEFINITION FOR TESTING
    RISK LEVEL HARMLESS
    DURATION SHORT.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_aux_folio,
             rcdty TYPE t9cesat_auxfol-rcdty,  "Record type AccPay or HR
             bukrs TYPE bkpf-bukrs,            "Company
             belnr TYPE bkpf-belnr,            "Document Number
             budat TYPE bkpf-budat,            "Posting date
             xblnr TYPE bkpf-xblnr,            "Vendor invoice number
             uuid  TYPE t9cesat_auxfol-uuid,   "Universal Unique Identifier
             taxid TYPE t9cesat_auxfol-taxid,  "Tax Id (RFC)
             zwels TYPE t9cesat_auxfol-zwels,  "Payment method
             waers TYPE bkpf-waers,            "Currency
             wrbtr TYPE bseg-wrbtr,            "Amount
             kursf TYPE bkpf-kursf,            "Exchange rate
             cnoex TYPE char01,                "Nat.Other and Foreign flag
           END OF ty_aux_folio,
           tyt_aux_folio TYPE STANDARD TABLE OF ty_aux_folio WITH DEFAULT KEY.
*    TYPES: ty_uuid  TYPE zif_abap_file_types=>ty_add_uuid,
*           tyt_uuid TYPE STANDARD TABLE OF ty_uuid WITH DEFAULT KEY.

  PRIVATE SECTION.

    DATA: m_cut  TYPE REF TO zif_abap_filemanager,
          m_comp TYPE abap_component_tab.
*          mv_dir type D:\alopezve\Documentos ABAP\FILEMANAGER\test_files.


    METHODS: setup,
      file_3x3 FOR TESTING,
      file_3x3_1 FOR TESTING,
      file_6x2 FOR TESTING,
      file_2x6 FOR TESTING,
      file_uuid FOR TESTING,
      check_component FOR TESTING.
ENDCLASS.

CLASS ltc_upload_excel IMPLEMENTATION.

  METHOD setup.

    DATA(lo_vbeln) = CAST cl_abap_datadescr( cl_abap_typedescr=>describe_by_name( 'VBELN' ) ).
    m_comp = VALUE abap_component_tab(
                                        ( name = 'VBELN' type = CAST cl_abap_datadescr( cl_abap_typedescr=>describe_by_name( 'VBELN' ) ) )
                                        ( name = 'UUID' type = CAST cl_abap_datadescr( cl_abap_typedescr=>describe_by_name( 'CHAR36' ) ) )
                                        ( name = 'SELLOSAT' type = CAST cl_abap_datadescr( cl_abap_typedescr=>describe_by_name( 'STRING' ) ) )
                                        ( name = 'RFCPAC' type = CAST cl_abap_datadescr( cl_abap_typedescr=>describe_by_name( 'CHAR13' ) ) )
                                      ).
  ENDMETHOD.
  METHOD check_component.
*    " Check componet table
*
*    cl_abap_typedescr=>describe_by_name( EXPORTING p_name = |TY_UUID|
*                                         RECEIVING p_descr_ref = DATA(lo_type)
*                                         EXCEPTIONS type_not_found = 1 ).
*    DATA(lo_struct) = CAST cl_abap_structdescr( lo_type ).
*
*    IF sy-subrc = 0.
*
*      cl_abap_unit_assert=>assert_equals( act = lo_struct->get_components( )
*                                          exp = m_comp
*                                          msg = |check component table| ).
*    ELSE.
*      cl_abap_unit_assert=>fail( msg = |type not found| ).
*    ENDIF.
  ENDMETHOD.
  METHOD file_3x3.

    TYPES: BEGIN OF ty_file_3,
             a TYPE string,
             b TYPE string,
             c TYPE string,
           END OF ty_file_3,

           tyt_file_3 TYPE STANDARD TABLE OF ty_file_3 WITH DEFAULT KEY.

    FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE.
    DATA: lt_table TYPE tyt_file_3.

*   given
    DATA(lt_archive) = VALUE zif_abap_filemanager=>tyt_data_values(
                                                                  ( row = '1' col = '1' name = 'A1' value = 'header1' )
                                                                  ( row = '1' col = '2' name = 'B1' value = 'header2' )
                                                                  ( row = '1' col = '3' name = 'C1' value = 'header3' )
                                                                  ( row = '2' col = '1' name = 'A2' value = '1' )
                                                                  ( row = '2' col = '2' name = 'B2' value = '2' )
                                                                  ( row = '2' col = '3' name = 'C2' value = '3' )
                                                                  ( row = '3' col = '1' name = 'A3' value = '2548' )
                                                                  ( row = '3' col = '2' name = 'B3' value = '32145-321568-5454' )
                                                                  ( row = '3' col = '3' name = 'C3' value = 'kEm34545' )
                                                                  ).
    DATA(lt_data) = VALUE tyt_file_3(
*                            ( a = '1' b = '2' c = '3' )
                            ( a = '2548' b = '32145-321568-5454' c = 'kEm34545' )
                            ).

*    when
    m_cut = NEW zcl_abap_xlsxfile( iv_sheet = 1 ).
    m_cut->upload_file( iv_filename = 'D:\alopezve\Documentos ABAP\FILEMANAGER\test_files\prueba1.xlsx'
                        iv_header   = 2 ).

*    ASSIGN m_cut->mo_data->* TO  FIELD-SYMBOL(<lt_data>).
    m_cut->get_table(  IMPORTING et_table = lt_table ).

*    then
    cl_abap_unit_assert=>assert_equals( exp = lt_data
                                        act = lt_table
                                        msg = |tabla 2x3 con cabecera| ).

  ENDMETHOD.
  METHOD file_3x3_1.

    TYPES: BEGIN OF ty_file_3,
             a TYPE string,
             b TYPE string,
             c TYPE string,
           END OF ty_file_3,

           tyt_file_3 TYPE STANDARD TABLE OF ty_file_3 WITH DEFAULT KEY.

    FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE.
    DATA: lt_table TYPE tyt_file_3.

*   given
    DATA(lt_archive) = VALUE zif_abap_filemanager=>tyt_data_values(
                                                                  ( row = '1' col = '1' name = 'A1' value = 'header1' )
                                                                  ( row = '1' col = '2' name = 'B1' value = 'header2' )
                                                                  ( row = '1' col = '3' name = 'C1' value = 'header3' )
                                                                  ( row = '2' col = '1' name = 'A2' value = '1' )
                                                                  ( row = '2' col = '2' name = 'B2' value = '2' )
                                                                  ( row = '2' col = '3' name = 'C2' value = '3' )
                                                                  ( row = '3' col = '1' name = 'A3' value = '2548' )
                                                                  ( row = '3' col = '2' name = 'B3' value = '32145-321568-5454' )
                                                                  ( row = '3' col = '3' name = 'C3' value = 'kEm34545' )
                                                                  ).
    DATA(lt_data) = VALUE tyt_file_3(
*                            ( a = 'header1' b = 'header2' c = 'header3' )
                            ( a = '1' b = '2' ) " c = '3' )
                            ( a = '2548' b = '32145-321568-5454' c = 'kEm34545' )
                            ).

*    when
    m_cut = NEW zcl_abap_xlsxfile( iv_sheet = 5 ).
    m_cut->upload_file( iv_filename = 'D:\alopezve\Documentos ABAP\FILEMANAGER\test_files\prueba1.xlsx'
                        iv_header   = 1 ).

*    ASSIGN m_cut->mo_data->* TO  FIELD-SYMBOL(<lt_data>).
    m_cut->get_table(  IMPORTING et_table = lt_table ).

*    then
    cl_abap_unit_assert=>assert_equals( exp = lt_data
                                        act = lt_table
                                        msg = |tabla 2x3 con cabecera| ).

  ENDMETHOD.
  METHOD file_6x2.
    TYPES: BEGIN OF ty_file_2,
             a TYPE string,
             b TYPE string,
           END OF ty_file_2,

           tyt_file_2 TYPE STANDARD TABLE OF ty_file_2 WITH DEFAULT KEY.

    DATA: lt_table TYPE tyt_file_2.
*    given
    DATA(lt_archive) = VALUE zif_abap_filemanager=>tyt_data_values(
                                                                  ( row = '1' col = '1' name = 'A1' value = 'header1' )
                                                                  ( row = '1' col = '2' name = 'B1' value = 'header2' )
                                                                  ( row = '2' col = '1' name = 'A2' value = '1' )
                                                                  ( row = '2' col = '2' name = 'B2' value = '2' )
                                                                  ( row = '3' col = '1' name = 'A3' value = 'ALGO' )
                                                                  ( row = '3' col = '2' name = 'B3' value = 'ROSAS marfil' )
                                                                  ( row = '4' col = '1' name = 'A4' value = '15.02.2018' )
                                                                  ( row = '4' col = '2' name = 'B4' value = '15/02/2018' )
                                                                  ( row = '5' col = '1' name = 'A5' value = '3/6/2018' )
                                                                  ( row = '5' col = '2' name = 'B5' value = '15.25' )
                                                                  ( row = '6' col = '1' name = 'A6' value = 'en otra pestaña' )
                                                                  ( row = '6' col = '2' name = 'B6' value = '0.33' )
                                                                  ).
    DATA(lt_data) = VALUE tyt_file_2(
                            ( a = 'header1' b = 'header2' )
                            ( a = '1' b = '2' )
                            ( a = 'ALGO' b = 'ROSAS marfil' )
                            ( a = '15.02.2018' b = '15/02/2018' )
                            ( a = '3/6/2018' b = '15.25' )
                            ( a = 'en otra pestaña' b = '0.33' )
                            ).


*    when
    m_cut = NEW zcl_abap_xlsxfile( iv_sheet = 2 ).
    m_cut->upload_file( iv_filename = 'D:\alopezve\Documentos ABAP\FILEMANAGER\test_files\prueba1.xlsx' ).
*                        iv_header   = 0 ).

    m_cut->get_table(  IMPORTING et_table = lt_table ).

*    then
    cl_abap_unit_assert=>assert_equals( exp = lt_data
                                        act = lt_table
                                        msg = |tabla 6x2 sin cabecera| ).


  ENDMETHOD.
  METHOD file_2x6.
    TYPES: BEGIN OF ty_file_6,
             a TYPE string,
             b TYPE string,
             c TYPE string,
             d TYPE string,
             e TYPE string,
             f TYPE string,
           END OF ty_file_6,

           tyt_file_6 TYPE STANDARD TABLE OF ty_file_6 WITH DEFAULT KEY.

    DATA: lt_table TYPE tyt_file_6.
*    when
    DATA(lt_archive) = VALUE zif_abap_filemanager=>tyt_data_values(
                                                                  ( row = '1' col = '1' name = 'A1' value = |col1 | )
                                                                  ( row = '1' col = '2' name = 'B1' value = 'COLUMNA2' )
                                                                  ( row = '1' col = '3' name = 'C1' value = 'heADER3' )
                                                                  ( row = '1' col = '4' name = 'D1' value = 'una cuarta' )
                                                                  ( row = '1' col = '5' name = 'E1' value = 'y la quita' )
                                                                  ( row = '1' col = '6' name = 'F1' value = 'colum6' )
                                                                  ( row = '2' col = '1' name = 'A2' value = '2' )
                                                                  ( row = '2' col = '2' name = 'B2' value = '4' )
                                                                  ( row = '2' col = '3' name = 'C2' value = '1' )
                                                                  ( row = '2' col = '4' name = 'D2' value = '6' )
                                                                  ( row = '2' col = '5' name = 'E2' value = '3' )
                                                                  ( row = '2' col = '6' name = 'F2' value = '0.25' )
                                                                  ).
    DATA(lt_data) = VALUE tyt_file_6(
                            ( a = '2' b = '4' c = '1' d = '6' e = '3' f = '0.25' )
                            ).

*    when
    m_cut = NEW zcl_abap_xlsxfile( iv_sheet = 3 ).
    m_cut->upload_file( iv_filename = 'D:\alopezve\Documentos ABAP\FILEMANAGER\test_files\prueba1.xlsx'
                        iv_header   = 1 ).

    m_cut->get_table(  IMPORTING et_table = lt_table ).

*    then
    cl_abap_unit_assert=>assert_equals( exp = lt_data
                                        act = lt_table
                                        msg = |tabla 2x6 con cabecera| ).

  ENDMETHOD.
  METHOD file_uuid.

    DATA: lt_table TYPE zif_example=>tyt_aux_folio.
    CLEAR m_cut.
    m_cut = NEW zcl_abap_xlsxfile( iv_struct = |ZIF_EXAMPLE=>TY_AUX_FOLIO| ).

    m_cut->upload_file( iv_filename = zcl_abap_filemanager=>get_filename( )
                        iv_header   = 1 ).

    m_cut->get_table( IMPORTING et_table = lt_table ).
    cl_salv_table=>factory( IMPORTING r_salv_table = DATA(lo_alv)
                            CHANGING t_table = lt_table
                           ).
    lo_alv->get_columns( )->set_optimize( abap_true ).
    lo_alv->get_display_settings( )->set_striped_pattern( abap_true ).
    lo_alv->get_functions( )->set_all( abap_true ).
    lo_alv->display( ).
*    m_cut->mo_structure
*    DATA(lt_archive) = VALUE tyt_uuid(
*                                      ( vbeln = '1' uuid = '1234-1234-12345678-13456789012' sellosat = 'sellostachalalalallalalalalalal' rfcpac = 'PFE140312IW8' )
*                                      ( vbeln = '2' uuid = '1234-1234-12345678-13456789012' sellosat = |estoe s una cadena del sat que puedes ser super larguísima bueno ni tanto, pero si debe de tener más de 72| &&
*                                         | caracteres para hacer lel split del save data| rfcpac = 'PFE140312IW8' )
*                                      ( vbeln = '3' uuid = '1234-1234-12345678-13456789012' sellosat = '' rfcpac = 'PFE140312IW8' )
*                                     ).
*
*    cl_abap_typedescr=>describe_by_name( EXPORTING p_name = |TY_UUID|
*                                         RECEIVING p_descr_ref = DATA(lo_type)
*                                         EXCEPTIONS type_not_found = 1 ).
*    DATA(lo_struct) = CAST cl_abap_structdescr( lo_type ).
*
*    IF sy-subrc = 0.
*      DATA(lo_data) = m_cut->upload_file( EXPORTING iv_filename  = 'E:\Users\XTSYSTEMS2\Desktop\subir_facturas.xlsx'
*                                                    iv_header    = abap_true
*                                                    io_structure = lo_struct ).
*      ASSIGN lo_data->* TO FIELD-SYMBOL(<lt_table>).
*      cl_abap_unit_assert=>assert_equals( exp = lt_archive
*                                          act = <lt_table>
*                                          msg = |Archivo uuid con cabecera| ).
*    ELSE.
*      cl_abap_unit_assert=>fail( msg = |Error when creating the type| ).
*    ENDIF.
  ENDMETHOD.
ENDCLASS.
