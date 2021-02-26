*&---------------------------------------------------------------------*
*&  Include  zfilemanager_example_cl1
*&---------------------------------------------------------------------*
CLASS lcl_filemanager DEFINITION.
  PUBLIC SECTION.
    DATA: mo_filemanager TYPE REF TO zif_abap_filemanager READ-ONLY.

    METHODS: set_txtfile IMPORTING io_struct TYPE REF TO cl_abap_structdescr OPTIONAL
                                   iv_struct TYPE string OPTIONAL,
      set_csvfile IMPORTING io_struct    TYPE REF TO cl_abap_structdescr OPTIONAL
                            iv_struct    TYPE string OPTIONAL
                            iv_separator TYPE char01
                            iv_bin       TYPE xsdboolean DEFAULT abap_false,
      set_xlsfile IMPORTING io_struct TYPE REF TO cl_abap_structdescr OPTIONAL
                            iv_struct TYPE string OPTIONAL,
      download_file.
ENDCLASS.
CLASS lcl_filemanager IMPLEMENTATION.
  METHOD set_txtfile.
    CLEAR mo_filemanager.
    FREE mo_filemanager.
    mo_filemanager = NEW zcl_abap_txtfile( io_struct = io_struct
                                           iv_struct = iv_struct ).
  ENDMETHOD.

  METHOD set_csvfile.
    CLEAR mo_filemanager.
    FREE mo_filemanager.
    mo_filemanager = NEW zcl_abap_csvfile( io_struct    = io_struct
                                           iv_struct    = iv_struct
                                           iv_bin       = iv_bin
                                           iv_separator = iv_separator ).
  ENDMETHOD.

  METHOD set_xlsfile.
    CLEAR mo_filemanager.
    FREE mo_filemanager.
    mo_filemanager = NEW zcl_abap_xlsxfile( io_struct = io_struct
                                            iv_struct = iv_struct ).
  ENDMETHOD.

  METHOD download_file.

    FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE.
    DATA: lv_name     TYPE string,
          lv_path     TYPE string,
          lv_filename TYPE string,
          lo_table    TYPE REF TO data.

    DATA(lo_table_str) = cl_abap_tabledescr=>create( p_line_type = mo_filemanager->mo_structure ).
    CREATE DATA lo_table TYPE HANDLE lo_table_str .
    ASSIGN lo_table->* TO <lt_table>.
    " set file
    mo_filemanager->get_table( IMPORTING et_table = <lt_table> ).
    mo_filemanager->create_file( it_data = <lt_table> ).

    " get filename
    cl_gui_frontend_services=>file_save_dialog( CHANGING filename = lv_name
                                                         path     = lv_path
                                                         fullpath = lv_filename ).

    " download
    DATA(lx_data) = cl_bcs_convert=>xstring_to_solix( o_filemanager->mo_filemanager->mv_file_bin ).
    cl_gui_frontend_services=>gui_download( EXPORTING bin_filesize = xstrlen( o_filemanager->mo_filemanager->mv_file_bin )
                                                      filename     = lv_filename
                                                      filetype     = 'BIN'
                                            CHANGING  data_tab     = lx_data
                                           ).
  ENDMETHOD.

ENDCLASS.
