*"* use this source file for your ABAP unit test classes
CLASS ltc_upload_csv DEFINITION FOR TESTING
    RISK LEVEL HARMLESS
    DURATION SHORT.

  PRIVATE SECTION.
    DATA: m_cut TYPE REF TO zif_abap_filemanager.

    METHODS: cambios_comma FOR TESTING,
      cambios_tab FOR TESTING,
      cambios_struc FOR TESTING,
      cambios_pipe FOR TESTING,
      teardown.
ENDCLASS.

CLASS ltc_upload_csv IMPLEMENTATION.
  METHOD cambios_comma.
    m_cut = NEW zcl_abap_csvfile( iv_separator = ',' ).
    m_cut->upload_file( iv_filename = 'D:\alopezve\Documentos ABAP\FILEMANAGER\test_files\cambios_comma.csv' ).
    " then
    cl_abap_unit_assert=>assert_bound( act = m_cut->mo_table ).
  ENDMETHOD.

  METHOD cambios_tab.
    m_cut = NEW zcl_abap_csvfile( iv_separator = |\t| ).
    m_cut->upload_file( iv_filename = 'D:\alopezve\Documentos ABAP\FILEMANAGER\test_files\cambios_tab.txt' ).
    " then
    cl_abap_unit_assert=>assert_bound( act = m_cut->mo_table ).
  ENDMETHOD.

  METHOD cambios_pipe.
    m_cut = NEW zcl_abap_csvfile( iv_separator = '|' ).
    m_cut->upload_file( iv_filename = 'D:\alopezve\Documentos ABAP\FILEMANAGER\test_files\cambios_pipe.txt' ).
    " then
    cl_abap_unit_assert=>assert_bound( act = m_cut->mo_table ).
  ENDMETHOD.
    METHOD cambios_struc.
    m_cut = NEW zcl_abap_csvfile( iv_struct = |ZIF_HRMX_OLDMUTUAL=>TY_CHANGE|
                                  iv_separator = |\t| ).
    m_cut->upload_file( iv_filename = 'D:\alopezve\Documentos ABAP\FILEMANAGER\test_files\cambios_tab_struct.txt' ).
    " then
    cl_abap_unit_assert=>assert_bound( act = m_cut->mo_table ).
  ENDMETHOD.

  METHOD teardown.
    FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE.

    ASSIGN m_cut->mo_table->* TO <lt_table>.
    IF <lt_table> IS ASSIGNED.
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
