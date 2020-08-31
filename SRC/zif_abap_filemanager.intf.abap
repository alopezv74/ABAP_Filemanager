INTERFACE zif_abap_filemanager
  PUBLIC .

  TYPES:
    BEGIN OF ty_data_values,
      row   TYPE i,
      col   TYPE i,
      name  TYPE fieldname,
      value TYPE string,
    END OF ty_data_values .
  TYPES:
    tyt_data_values TYPE STANDARD TABLE OF ty_data_values WITH DEFAULT key. "NON-UNIQUE KEY name .

  DATA: mo_table     TYPE REF TO data READ-ONLY,
        mv_file      TYPE string READ-ONLY,
        mv_file_bin  TYPE xstring READ-ONLY,
        mo_structure TYPE REF TO cl_abap_structdescr READ-ONLY.

  METHODS: upload_file IMPORTING !iv_filename TYPE string
                                 !iv_header   TYPE i optional,
    create_file IMPORTING !it_data TYPE STANDARD TABLE,
    get_table EXPORTING et_table TYPE STANDARD TABLE,
    display_data.

*      !io_structure   TYPE REF TO cl_abap_structdescr OPTIONAL
*    RETURNING
*      VALUE(ro_table) TYPE REF TO data,


ENDINTERFACE.
