*"* use this source file for your ABAP unit test classes
CLASS ltc_upload_txt DEFINITION FOR TESTING
    RISK LEVEL HARMLESS
    DURATION SHORT.

  PRIVATE SECTION.
    DATA: m_cut TYPE REF TO zif_abap_filemanager.

    METHODS: altas FOR TESTING,
      banamex FOR TESTING,
      scotia FOR TESTING,
      teardown.
ENDCLASS.
CLASS ltc_upload_txt IMPLEMENTATION.
  METHOD altas.

    m_cut = NEW zcl_abap_txtfile( iv_struct = |ZIF_HRMX_OLDMUTUAL=>TY_HIRE| ).
    m_cut->upload_file( iv_filename = 'D:\alopezve\Documentos ABAP\FILEMANAGER\test_files\Altas.txt' ).
    " then
    cl_abap_unit_assert=>assert_bound( act = m_cut->mo_table ).

  ENDMETHOD.

  METHOD banamex.

    m_cut = NEW zcl_abap_txtfile( iv_struct = |ZIF_AUXILIAR=>TY_REG_BANMEX| ).
    m_cut->upload_file( iv_filename = 'D:\alopezve\Documentos ABAP\FILEMANAGER\test_files\BANMEX.txt' ).
    cl_abap_unit_assert=>assert_bound( act = m_cut->mo_table ).
  ENDMETHOD.

  METHOD scotia.

    m_cut = NEW zcl_abap_txtfile( iv_struct = |ZIF_AUXILIAR=>TY_SCOTIA_TEST| ).
    m_cut->upload_file( iv_filename = 'D:\alopezve\Documentos ABAP\FILEMANAGER\test_files\SCOTIA.txt' ).

    cl_abap_unit_assert=>assert_bound( act = m_cut->mo_table ).
  ENDMETHOD.

  METHOD teardown.
    FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE.

    ASSIGN m_cut->mo_table->* TO <lt_table>.
    IF <LT_TABLE> IS ASSIGNED.
    cl_salv_table=>factory( IMPORTING r_salv_table = DATA(lo_alv)
                            CHANGING t_table = <lt_table>
                           ).
    lo_alv->get_columns( )->set_optimize( abap_true ).
    lo_alv->get_display_settings( )->set_striped_pattern( abap_true ).
    lo_alv->get_functions( )->set_all( abap_true ).
    lo_alv->display( ).
    ELSE.
     cl_demo_output=>display_text( |No table| ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
