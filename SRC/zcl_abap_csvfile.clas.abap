CLASS zcl_abap_csvfile DEFINITION
  PUBLIC
  INHERITING FROM zcl_abap_filemanager
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS zif_abap_filemanager~create_file REDEFINITION.
    METHODS: zif_abap_filemanager~upload_file REDEFINITION.
    METHODS: constructor IMPORTING io_struct    TYPE REF TO cl_abap_structdescr OPTIONAL
                                   iv_struct    TYPE string OPTIONAL
                                   iv_bin       TYPE xsdboolean DEFAULT abap_false
                                   iv_separator TYPE char01.
  PROTECTED SECTION.
    METHODS: create_structure REDEFINITION.
  PRIVATE SECTION.
    DATA: mv_separator TYPE char01.

    METHODS: get_table_comp IMPORTING it_table            TYPE STANDARD TABLE
                            RETURNING VALUE(rt_component) TYPE abap_compdescr_tab,
      read_file IMPORTING !iv_filename TYPE string
                          !iv_header   TYPE i OPTIONAL.
ENDCLASS.



CLASS zcl_abap_csvfile IMPLEMENTATION.
  METHOD constructor.
    super->constructor( io_struct = io_struct
                        iv_struct = iv_struct
                        iv_bin    = iv_bin  ).
    mv_separator = iv_separator.
  ENDMETHOD.
  METHOD zif_abap_filemanager~create_file.
    FIELD-SYMBOLS: <lv_value> TYPE any.
    DATA: lv_first TYPE xsdboolean.

    DATA(lt_component) = get_table_comp( it_data ).

    CLEAR: mv_file,
           mv_file_bin.

    LOOP AT it_data ASSIGNING FIELD-SYMBOL(<ls_file>).
      lv_first = abap_true.
      LOOP AT lt_component ASSIGNING FIELD-SYMBOL(<ls_comp>).
        ASSIGN COMPONENT sy-tabix OF STRUCTURE <ls_file> TO <lv_value>.
        IF sy-subrc = 0.
          IF lv_first = abap_true.
            lv_first = abap_false.
            mv_file = |{ mv_file }{ <lv_value> }|.
          ELSE.
            mv_file = |{ mv_file }{ mv_separator }{ <lv_value> }|.
          ENDIF.
        ENDIF.
      ENDLOOP.
      mv_file = |{ mv_file }\r\n|.
    ENDLOOP.
*     mv_file     = rv_file.
    mv_file_bin = cl_bcs_convert=>string_to_xstring( iv_string   = mv_file
                                                     iv_codepage = '4110' ).
  ENDMETHOD.
  METHOD zif_abap_filemanager~upload_file.

    FIELD-SYMBOLS: <lt_csvtable> TYPE STANDARD TABLE.

    read_file( iv_filename = iv_filename
               iv_header   = iv_header ).

    DATA(lo_csv_table) = create_structure( ).
    ASSIGN lo_csv_table->* TO <lt_csvtable>.
    fill_table( <lt_csvtable> ).

  ENDMETHOD.
  METHOD create_structure.

    FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE.
    DATA: lt_comp TYPE abap_component_tab,
          lt_data TYPE zif_abap_filemanager=>tyt_data_values.

    DATA(lv_row) = 0.
    LOOP AT mt_file ASSIGNING FIELD-SYMBOL(<ls_file>).
      SPLIT <ls_file> AT mv_separator INTO TABLE DATA(lt_filedata).
      lv_row = lv_row + 1.
      LOOP AT lt_filedata ASSIGNING FIELD-SYMBOL(<ls_filedata>).
        DATA(lv_index) = sy-tabix.
        APPEND INITIAL LINE TO lt_data ASSIGNING FIELD-SYMBOL(<ls_data>).
        <ls_data> = VALUE zif_abap_filemanager=>ty_data_values( row   = lv_row
                                                                col   = lv_index
                                                                name  = set_column_name( lv_index )
                                                                value = <ls_filedata>
                                                             ).
        IF NOT line_exists( lt_comp[ name = <ls_data>-name ] ).
          APPEND INITIAL LINE TO lt_comp ASSIGNING FIELD-SYMBOL(<ls_comp>).
          <ls_comp>-name = <ls_data>-name.
          <ls_comp>-type = CAST cl_abap_datadescr( cl_abap_typedescr=>describe_by_data( <ls_filedata> ) ).
        ENDIF.
      ENDLOOP.
    ENDLOOP.


    DATA(lo_struct) = cl_abap_structdescr=>create( lt_comp ).
    IF mo_structure IS INITIAL.
      mo_structure = lo_struct.
    ENDIF.
    " create table with the string components
    DATA(lo_table) = cl_abap_tabledescr=>create( p_line_type = lo_struct ).
    CREATE DATA ro_table TYPE HANDLE lo_table.
    ASSIGN ro_table->* TO <lt_table>.

    LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<ls_line>)
      GROUP BY ( row = <ls_line>-row ) ASSIGNING FIELD-SYMBOL(<ls_row_data>).

      APPEND INITIAL LINE TO <lt_table> ASSIGNING FIELD-SYMBOL(<ls_table>).
      LOOP AT GROUP <ls_row_data> ASSIGNING FIELD-SYMBOL(<ls_cell_data>).
        ASSIGN COMPONENT <ls_cell_data>-col OF STRUCTURE <ls_table> TO FIELD-SYMBOL(<lv_value>).
        <lv_value> = <ls_cell_data>-value.
      ENDLOOP.
    ENDLOOP.

    " delete header
    IF mv_header IS NOT INITIAL.
      DELETE <lt_table>
       FROM 1 TO mv_header.
    ENDIF.

  ENDMETHOD.

  METHOD get_table_comp.
    DATA(lo_table) = CAST cl_abap_tabledescr( cl_abap_tabledescr=>describe_by_data( p_data = it_table ) ).
    DATA(lo_struct) = CAST cl_abap_structdescr( lo_table->get_table_line_type( ) ).
    rt_component[] = lo_struct->components[].
  ENDMETHOD.


  METHOD read_file.

    cl_gui_frontend_services=>gui_upload( EXPORTING filename       = iv_filename
                                                    read_by_line   = abap_true
                                           CHANGING data_tab       = mt_file
                                           EXCEPTIONS no_batch      = 1
                                                      file_read_error = 3
                                                      invalid_type = 2
                                                      OTHERS       = 4 ).
    CASE sy-subrc.
      WHEN '0'.
        " nothig to do
      WHEN '1'.
        " read from appserver
        upload_appserver( iv_filename ).
      WHEN '2' OR '3'. " not ASC
        cl_gui_frontend_services=>gui_upload( EXPORTING filename   = iv_filename
                                                    read_by_line   = abap_true
                                                    codepage       = '4110'
                                           CHANGING data_tab       = mt_file
                                           EXCEPTIONS no_batch     = 1
                                                      invalid_type = 2
                                                      OTHERS       = 3 ).
      WHEN OTHERS.
        IF sy-batch IS INITIAL.
          cl_demo_output=>display_text( |Error al subir el archivo| ).
        ELSE.
          WRITE: |Error al subir el archivo|.
        ENDIF.
        EXIT. " raise exception
    ENDCASE.
    mv_header = iv_header.
  ENDMETHOD.

ENDCLASS.
