CLASS zcl_abap_txtfile DEFINITION
  PUBLIC
  INHERITING FROM zcl_abap_filemanager
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS zif_abap_filemanager~create_file REDEFINITION.
    METHODS: zif_abap_filemanager~upload_file REDEFINITION.
    METHODS: constructor IMPORTING io_struct TYPE REF TO cl_abap_structdescr OPTIONAL
                                   iv_struct TYPE string OPTIONAL.
  PROTECTED SECTION.
    METHODS: create_structure REDEFINITION.
  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_abap_txtfile IMPLEMENTATION.
  METHOD zif_abap_filemanager~create_file.
    CLEAR: mv_file,
           mv_file_bin.

    LOOP AT it_data ASSIGNING FIELD-SYMBOL(<ls_data>).
      IF sy-tabix = 1.
        CONCATENATE mv_file
                    <ls_data>
                INTO mv_file RESPECTING BLANKS.
      ELSE.
        CONCATENATE mv_file
                    cl_abap_char_utilities=>cr_lf
                    <ls_data>
              INTO mv_file RESPECTING BLANKS.
      ENDIF.
    ENDLOOP.
    TRY.
        mv_file_bin = cl_bcs_convert=>string_to_xstring( iv_string   = mv_file
                                                         iv_codepage = '4110' ).
      CATCH cx_bcs INTO DATA(lo_error).
        IF sy-batch IS INITIAL.
          cl_demo_output=>display_text( lo_error->get_longtext( ) ).
        ELSE.
          WRITE lo_error->get_longtext( ).
        ENDIF.
    ENDTRY.
  ENDMETHOD.
  METHOD zif_abap_filemanager~upload_file.

    " for txt with position we must need a structure to pass the data
    " so if it is not provided, then raise exception
    IF mo_structure IS INITIAL.
      " raise exception
      IF sy-batch IS INITIAL.
        cl_demo_output=>display_text( |No hay estructura para llenar la tabla| ).
      ELSE.
        WRITE: |No hay estructura para llenar la tabla|.
      ENDIF.
      EXIT. " raise exception
    ENDIF.
    cl_gui_frontend_services=>gui_upload( EXPORTING filename     = iv_filename
                                                    read_by_line = abap_true
                                           CHANGING data_tab     = mt_file
                                           EXCEPTIONS no_batch   = 1
                                                      OTHERS     = 2 ).
    CASE sy-subrc.
      WHEN '0'.
        " nothig to do
      WHEN '1'.
        " read from appserver
        upload_appserver( iv_filename ).
      WHEN OTHERS.
        IF sy-batch IS INITIAL.
          cl_demo_output=>display_text( |Error al subir el archivo| ).
        ELSE.
          WRITE: |Error al subir el archivo|.
        ENDIF.
        EXIT. " raise exception
    ENDCASE.

    mv_header = iv_header.
    mo_table = create_structure( ).
  ENDMETHOD.
  METHOD create_structure.

    FIELD-SYMBOLS: <lt_table>     TYPE STANDARD TABLE.

    DATA(lo_table) = cl_abap_tabledescr=>create( p_line_type = mo_structure ).
    CREATE DATA ro_table TYPE HANDLE lo_table.
    ASSIGN ro_table->* TO <lt_table>.

    LOOP AT mt_file ASSIGNING FIELD-SYMBOL(<ls_line>).
      DATA(lv_pos) = 0.
      APPEND INITIAL LINE TO <lt_table> ASSIGNING FIELD-SYMBOL(<ls_table>).
      LOOP AT mo_structure->components ASSIGNING FIELD-SYMBOL(<ls_comp>).
        DATA(lv_length) = <ls_comp>-length / 2.  " length is the internal length that is double than the output length
        CASE <ls_comp>-type_kind.
*          WHEN cl_abap_typedescr=>typekind_struct1. " flat structure
*            DATA(lv_data)   = <ls_line>(lv_length).
          WHEN cl_abap_typedescr=>typekind_table OR cl_abap_typedescr=>typekind_struct2. " table or deep structure
            " raise error No table inside table is allowed
          WHEN OTHERS.
            DATA(lv_data)   = <ls_line>+lv_pos(lv_length).
        ENDCASE.
        ASSIGN COMPONENT <ls_comp>-name OF STRUCTURE <ls_table> TO FIELD-SYMBOL(<lv_value>).
        <lv_value> = lv_data.
        lv_pos     = lv_pos + lv_length.
      ENDLOOP.
    ENDLOOP.
    " delete rows for header
    IF mv_header IS NOT INITIAL.
      DELETE <lt_table>
        FROM 1 TO mv_header.
    ENDIF.

  ENDMETHOD.

  METHOD constructor.
    super->constructor( io_struct = io_struct
                        iv_struct = iv_struct
                        iv_bin    = abap_false ).
  ENDMETHOD.

ENDCLASS.
