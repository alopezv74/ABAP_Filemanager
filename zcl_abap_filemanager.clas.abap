CLASS zcl_abap_filemanager DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_abap_filemanager ABSTRACT METHODS upload_file
      create_file.

    ALIASES: mv_file      FOR zif_abap_filemanager~mv_file,
             mv_file_bin  FOR zif_abap_filemanager~mv_file_bin,
             mo_table     FOR zif_abap_filemanager~mo_table,
             mo_structure FOR zif_abap_filemanager~mo_structure,
             upload_file  FOR zif_abap_filemanager~upload_file,
             create_file  FOR zif_abap_filemanager~create_file,
             display_data FOR zif_abap_filemanager~display_data.
    CLASS-DATA: mv_decformat TYPE xudcpfm.
    CLASS-METHODS: class_constructor,
      get_filename RETURNING VALUE(rv_filename) TYPE string.
    METHODS: constructor IMPORTING io_struct TYPE REF TO cl_abap_structdescr OPTIONAL
                                   iv_struct TYPE string OPTIONAL
                                   iv_bin    TYPE xsdboolean OPTIONAL.

  PROTECTED SECTION.

    DATA: mv_bin    TYPE xsdboolean,
          mt_file   TYPE TABLE OF string WITH EMPTY KEY,
          mx_file   TYPE xstring,
          mv_header TYPE i.
    METHODS: fill_table IMPORTING !it_data TYPE STANDARD TABLE,
      fill_components IMPORTING !io_struct TYPE REF TO cl_abap_structdescr
                                !is_data   TYPE any
                      CHANGING  !cs_line   TYPE any
                                !cv_index  TYPE i,
      set_column_name IMPORTING !iv_count             TYPE i
                      RETURNING VALUE(rv_column_name) TYPE fieldname ,
      create_structure ABSTRACT
        RETURNING VALUE(ro_table) TYPE REF TO data, " cl_abap_structdescr,
      upload_appserver
        IMPORTING
          iv_filename TYPE string." zif_abap_filemanager=>tyt_data_values.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abap_filemanager IMPLEMENTATION.

  METHOD fill_table.

    FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE.
    DATA: lo_line TYPE REF TO data.

    DATA(lo_table) = cl_abap_tabledescr=>get( EXPORTING p_line_type = mo_structure ).
    CREATE DATA lo_line TYPE HANDLE mo_structure.
    CREATE DATA mo_table TYPE HANDLE lo_table.
    ASSIGN mo_table->* TO <lt_table>.

    LOOP AT it_data ASSIGNING FIELD-SYMBOL(<ls_data>).
      APPEND INITIAL LINE TO <lt_table> ASSIGNING FIELD-SYMBOL(<ls_line>).
      DATA(lv_tabix) = 1. " Initial component, means start with 1
      fill_components( EXPORTING io_struct = mo_structure
                                 is_data   = <ls_data>
                       CHANGING  cs_line   = <ls_line>
                                 cv_index  = lv_tabix ).

    ENDLOOP.


  ENDMETHOD.
  METHOD fill_components.

    LOOP AT io_struct->components ASSIGNING FIELD-SYMBOL(<ls_component>).
      ASSIGN COMPONENT cv_index OF STRUCTURE is_data TO FIELD-SYMBOL(<lv_data>).
      IF sy-subrc = 0.
        ASSIGN COMPONENT <ls_component>-name OF STRUCTURE cs_line TO FIELD-SYMBOL(<lv_value>).
        IF sy-subrc = 0.
          " this is for the kind of data
          CASE <ls_component>-type_kind.
            WHEN 'D'. " date
              TRY.

                  cl_abap_datfm=>conv_date_ext_to_int( EXPORTING im_datext = <lv_data>
                                                                 im_datfmdes = cl_abap_datfm=>get_datfm( )
                                                       IMPORTING ex_datint = <lv_value> ).
                  " in any error just send the actual value
                CATCH cx_abap_datfm_invalid_date
                      cx_abap_datfm_no_date
                      cx_abap_datfm_ambiguous
                      cx_abap_datfm_format_unknown.

                  <lv_value> = <lv_data>.
              ENDTRY.
              " Numerical with decimals
            WHEN cl_abap_structdescr=>typekind_packed " P probably curr
                OR cl_abap_structdescr=>typekind_float. " F float
              TRY.
                  <lv_value> = <lv_data>.

                CATCH cx_sy_conversion_no_number.

                  DATA(lv_tmp) = CONV string( <lv_data> ).
                  CASE mv_decformat.
                    WHEN 'X'.
*                      lv_tmp = replace( val = lv_tmp sub = |,| with = |.| ).
*                      TRANSLATE lv_tmp USING ',.'.
                    WHEN 'Y'.
                      CONDENSE lv_tmp.
*                      lv_tmp = replace( val = lv_tmp sub = |,| with = |.| ).
                    WHEN OTHERS.
*                      lv_tmp = replace( val = lv_tmp sub = |,| with = |.| ).
*                      TRANSLATE lv_tmp USING '.,'.
                  ENDCASE.
                  lv_tmp = replace( val = lv_tmp sub = |,| with = |.| ).
                  <lv_value> = lv_tmp.

              ENDTRY.
            WHEN cl_abap_structdescr=>typekind_struct1 OR cl_abap_structdescr=>typekind_struct2. " 'u'. " structure
              " get component structure
              cl_abap_structdescr=>describe_by_data( EXPORTING p_data = <lv_value>
                                                     RECEIVING p_descr_ref = DATA(lo_type) ).

              DATA(lo_struct_int) = CAST cl_abap_structdescr( lo_type ). " ?= lo_type.
              DATA(lv_line) = 1.
              fill_components( EXPORTING io_struct = lo_struct_int
                                         is_data   = is_data
                               CHANGING  cs_line  = <lv_value>
                                         cv_index  = lv_line ).

            WHEN cl_abap_elemdescr=>typekind_char. " character type, try to convert to internal value
              cl_abap_structdescr=>describe_by_data( EXPORTING p_data      = <lv_value>
                                                     RECEIVING p_descr_ref = lo_type ).
*                                                     EXCEPTIONS type_not_found = 1 ).
              IF lo_type->is_ddic_type( ).
                DATA(lv_fieldname) = lo_type->get_relative_name( ).
                " first get the meta field data
                cl_rsan_ut_conversion_exit=>try_get_fieldinfo( EXPORTING i_typename  = lv_fieldname
                                                               IMPORTING e_fieldinfo = DATA(ls_fieldinfo) ).
                " now try to set the internal value
                cl_rsan_ut_conversion_exit=>convert_to_intern( EXPORTING i_fieldinfo = ls_fieldinfo
                                                                         i_external_value = <lv_data>
                                                               IMPORTING e_internal_value = <lv_value> ).
              ELSE.
                <lv_value> = <lv_data>.
              ENDIF.
            WHEN OTHERS.
              <lv_value> = <lv_data>.
          ENDCASE.
          cv_index = cv_index + 1.
        ENDIF.
      ENDIF.
    ENDLOOP.
    cv_index = cv_index - 1.
  ENDMETHOD.

* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAP_FILE_MANAGER->SET_COLUMN_NAME
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_COUNT                       TYPE        I
* | [<-()] RV_COLUMN_NAME                 TYPE        FIELDNAME
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_column_name.
    CONSTANTS : lc_letters TYPE i VALUE 26. " this is the number of letters

    DATA: lv_res    TYPE i,
          lv_coc    TYPE i,
          lv_letter TYPE char1.

    DATA(lv_remain) = iv_count - 1. " because A = 0 ; Z = 25 ;
    " concatenate the number of letters

    WHILE lv_remain >= lc_letters.
      "calculate coz and residue
      lv_coc = lv_remain DIV lc_letters.
      lv_res = lv_remain MOD lc_letters.
      rv_column_name = |{ sy-abcde+lv_res(1) }{ rv_column_name }|.
      lv_remain      = lv_coc - 1. " continues in block of 26 characters
    ENDWHILE.
    " if less than 26, and the last remain
    rv_column_name = |{ sy-abcde+lv_remain(1) }{ rv_column_name }|.
  ENDMETHOD.

  METHOD constructor.

    mo_structure = COND #( WHEN iv_struct IS NOT INITIAL
                           THEN CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_name( EXPORTING p_name = iv_struct ) )
                           ELSE io_struct
                         ).

    mv_bin = COND xsdboolean( WHEN iv_bin IS NOT INITIAL
                              THEN iv_bin
                              ELSE abap_false ).

  ENDMETHOD.

  METHOD upload_appserver.
    DATA(lv_cp) = cl_abap_file_utilities=>get_current_nonuni_codepage( ).
    " open file
    IF mv_bin IS INITIAL.
      OPEN DATASET iv_filename FOR INPUT CODE PAGE lv_cp IN LEGACY TEXT MODE.
      IF sy-subrc = 0.
        " move data to table
        WHILE sy-subrc = 0.
          APPEND INITIAL LINE TO mt_file ASSIGNING FIELD-SYMBOL(<ls_line>).
          READ DATASET iv_filename INTO <ls_line>.
        ENDWHILE.

      ENDIF.
    ELSE.
      OPEN DATASET iv_filename FOR INPUT IN BINARY MODE.
      IF sy-subrc = 0.
        " move data to table
        READ DATASET iv_filename INTO mx_file.

      ENDIF.
    ENDIF.
*      " always close
    CLOSE DATASET iv_filename.
  ENDMETHOD.

  METHOD zif_abap_filemanager~get_table.

    FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE.
*    CREATE DATA mo_table TYPE HANDLE lo_table.
    IF mo_table IS NOT INITIAL.
      ASSIGN mo_table->* TO <lt_table>.
      et_table = <lt_table>.
    ENDIF.
  ENDMETHOD.

  METHOD get_filename.
    DATA: lt_files  TYPE filetable,
          lv_rc     TYPE i,
          lv_action TYPE i.
    cl_gui_frontend_services=>file_open_dialog( EXPORTING multiselection = abap_false
                                                CHANGING  file_table     = lt_files
                                                          rc             = lv_rc
                                                          user_action    = lv_action ).
    IF lv_rc <> -1
      AND lv_action = cl_gui_frontend_services=>action_ok.
      rv_filename = lt_files[ 1 ].
    ENDIF.

  ENDMETHOD.

  METHOD class_constructor.
    SELECT SINGLE dcpfm
    FROM usr01
    INTO @mv_decformat
    WHERE bname = @sy-uname.
  ENDMETHOD.

  METHOD display_data.

    FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE.

    ASSIGN mo_table->* TO <lt_table>.
    TRY.
        cl_salv_table=>factory( EXPORTING list_display = sy-batch
                                IMPORTING r_salv_table = DATA(lo_alv)
                                CHANGING t_table       = <lt_table>
                               ).
        lo_alv->get_columns( )->set_optimize( abap_true ).
        lo_alv->get_display_settings( )->set_striped_pattern( abap_true ).
        lo_alv->get_functions( )->set_all( abap_true ).
        lo_alv->display( ).

      CATCH cx_salv_msg
            cx_root INTO DATA(lo_salv_error).
        IF sy-batch IS INITIAL.
          cl_demo_output=>display_text( lo_salv_error->get_longtext( ) ).
        ELSE.
          WRITE lo_salv_error->get_longtext( ).
        ENDIF.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
