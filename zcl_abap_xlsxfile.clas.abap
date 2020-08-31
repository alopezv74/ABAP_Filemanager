CLASS zcl_abap_xlsxfile DEFINITION
  PUBLIC
  INHERITING FROM zcl_abap_filemanager
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS zif_abap_filemanager~create_file REDEFINITION.
    METHODS: zif_abap_filemanager~upload_file REDEFINITION.

    METHODS constructor IMPORTING io_struct TYPE REF TO cl_abap_structdescr OPTIONAL
                                  iv_struct TYPE string OPTIONAL
                                  iv_sheet  TYPE i OPTIONAL. " sheet to be read
  PROTECTED SECTION.
    METHODS: create_structure REDEFINITION.
  PRIVATE SECTION.

    TYPES: BEGIN OF ty_cols,
             min TYPE i,
             max TYPE i,
           END OF ty_cols,

           BEGIN OF ty_cells,
             position TYPE string,
             type     TYPE string,
             value    TYPE string,
           END OF ty_cells,
           BEGIN OF ty_rows,
             rowid   TYPE string,
             t_cells TYPE STANDARD TABLE OF ty_cells WITH DEFAULT KEY,
           END OF ty_rows,
           BEGIN OF ty_sheet,
             dim    TYPE string,
             t_cols TYPE STANDARD TABLE OF ty_cols WITH DEFAULT KEY,
             t_rows TYPE STANDARD TABLE OF ty_rows WITH DEFAULT KEY,
           END OF ty_sheet,
           BEGIN OF ty_strings,
             value TYPE string,
           END OF ty_strings,
           BEGIN OF ty_shared,
             string_count  TYPE string,
             string_ucount TYPE string,
             t_strings     TYPE STANDARD TABLE OF ty_strings WITH DEFAULT KEY,
           END OF ty_shared ,
           tyt_sheet  TYPE STANDARD TABLE OF ty_sheet,
           tyt_shared TYPE STANDARD TABLE OF ty_shared.

    DATA: mo_salv       TYPE REF TO cl_salv_table,
          mv_sheet      TYPE i,
          ms_sheet      TYPE ty_sheet,
          ms_shared     TYPE ty_shared.
    METHODS read_excel.

ENDCLASS.


CLASS zcl_abap_xlsxfile IMPLEMENTATION.

  METHOD zif_abap_filemanager~create_file.

    DATA: lo_data TYPE REF TO data.

    DATA(lo_table) = CAST cl_abap_tabledescr( cl_abap_tabledescr=>describe_by_data( p_data = it_data ) ).
    CREATE DATA lo_data TYPE HANDLE lo_table.
    ASSIGN lo_data->* TO FIELD-SYMBOL(<lt_table>).
    <lt_table> = it_data[].

    TRY .
        cl_salv_table=>factory(
        IMPORTING
          r_salv_table = mo_salv
        CHANGING
          t_table      = <lt_table> ).
      CATCH cx_salv_msg.
    ENDTRY.

    CLEAR: mv_file,
           mv_file_bin.

    mv_file_bin = mo_salv->to_xml( if_salv_bs_xml=>c_type_xlsx ). " convert to xlsx file
    " convert to string if needed
    mv_file     = cl_bcs_convert=>xstring_to_string( iv_xstr = mv_file_bin
                                                     iv_cp = '4110' ).
  ENDMETHOD.

  METHOD zif_abap_filemanager~upload_file.
    TRY . " get managed classes for excel file
        " upload file
        mx_file   = cl_openxml_helper=>load_local_file( iv_filename ).
      CATCH cx_openxml_not_found INTO DATA(lo_error_notfound). " file not in the local pc
        upload_appserver( iv_filename ).
    ENDTRY.
    mv_header = iv_header.
    read_excel( ).

  ENDMETHOD.

  METHOD create_structure.

    FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE.

    DATA: lt_comps      TYPE abap_component_tab,
          lt_table_data TYPE zif_abap_filemanager=>tyt_data_values.

    LOOP AT ms_sheet-t_rows ASSIGNING FIELD-SYMBOL(<ls_row>).
      DATA(lv_index) = 0.
*      DELETE <ls_row>-t_cells
*        WHERE value IS INITIAL.
      LOOP AT <ls_row>-t_cells ASSIGNING FIELD-SYMBOL(<ls_cell>).

        lv_index = lv_index + 1.
        APPEND INITIAL LINE TO lt_table_data ASSIGNING FIELD-SYMBOL(<ls_data>).
        <ls_data> = VALUE zif_abap_filemanager=>ty_data_values( row   = <ls_row>-rowid
                                                                 col   = lv_index
                                                                 name  = <ls_cell>-position
                                                                 value = COND #( WHEN <ls_cell>-type = 's'
                                                                                 THEN  |{ ms_shared-t_strings[ <ls_cell>-value + 1 ]-value }|
                                                                                 ELSE <ls_cell>-value )
                                                                ).
        " create components names
        IF NOT line_exists( lt_comps[ name = set_column_name( <ls_data>-col ) ] ).
          APPEND INITIAL LINE TO lt_comps ASSIGNING FIELD-SYMBOL(<ls_comp>).
          <ls_comp>-name = set_column_name( <ls_data>-col ).
          <ls_comp>-type ?= cl_abap_typedescr=>describe_by_name( 'STRING' ).
        ENDIF.
      ENDLOOP. " columns
    ENDLOOP. " rows


    DATA(lo_structure) = cl_abap_structdescr=>create( lt_comps ).
    IF mo_structure IS INITIAL.
      mo_structure = lo_structure.
    ENDIF.
    DATA(lo_table) = cl_abap_tabledescr=>create( p_line_type = lo_structure ).
    CREATE DATA ro_table TYPE HANDLE lo_table.
    ASSIGN ro_table->* TO <lt_table>.

    LOOP AT lt_table_data ASSIGNING FIELD-SYMBOL(<ls_line>)
      GROUP BY ( row = <ls_line>-row ) ASSIGNING FIELD-SYMBOL(<ls_row_data>).

      APPEND INITIAL LINE TO <lt_table> ASSIGNING FIELD-SYMBOL(<ls_table>).
      LOOP AT GROUP <ls_row_data> ASSIGNING FIELD-SYMBOL(<ls_cell_data>).
        ASSIGN COMPONENT <ls_cell_data>-col OF STRUCTURE <ls_table> TO FIELD-SYMBOL(<lv_value>).
        <lv_value> = <ls_cell_data>-value.
      ENDLOOP.
    ENDLOOP.
    " delete header lines
    IF  mv_header IS NOT INITIAL.
      DELETE <lt_table>
          FROM 1 TO mv_header.
    ENDIF.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( io_struct = io_struct
                        iv_struct = iv_struct
                        iv_bin    = abap_true ).
    mv_sheet = COND i( WHEN iv_sheet IS SUPPLIED
                       THEN iv_sheet
                       ELSE 1 ).
  ENDMETHOD.


  METHOD read_excel.

    FIELD-SYMBOLS: <lt_excel> TYPE STANDARD TABLE.

    TRY.
        "shared data
        DATA(lv_shared) = cl_xlsx_document=>load_document( mx_file )->get_workbookpart( )->get_sharedstringspart( )->get_data( ).
        " get data for sheet selected
        DATA(lv_sheet) = cl_xlsx_document=>load_document( mx_file )->get_workbookpart( )->get_part_by_id( |rId{ mv_sheet }| )->get_data( ).

        " call the transformation to get the data into local tables
        CALL TRANSFORMATION zabap_xml_off2007_sheet
          SOURCE XML lv_sheet
          RESULT param = ms_sheet.

        CALL TRANSFORMATION zabap_xml_off2007_shared
          SOURCE XML lv_shared
          RESULT param = ms_shared.

        DATA(lo_excel) = create_structure( ).
        ASSIGN lo_excel->* TO <lt_excel>.
        fill_table( <lt_excel> ).

      CATCH cx_openxml_not_found.
      CATCH cx_openxml_format. " not excel
      CATCH cx_st_match_element INTO DATA(lo_error_trans).
        MESSAGE |error in transformation name = { lo_error_trans->actual_name } value = { lo_error_trans->actual_value }| &&
                | in path = { lo_error_trans->xml_path }| TYPE 'E'.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
