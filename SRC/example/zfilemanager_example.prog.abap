*&---------------------------------------------------------------------*
*& Report  zfilemanager_example
*&
*&---------------------------------------------------------------------*
*& Develop by Adolfo López Vega
*&Example on how to use the package zabap_filemanger
*&---------------------------------------------------------------------*
REPORT zfilemanager_example.

INCLUDE: zfilemanager_example_top,
         zfilemanager_example_cl1.


INITIALIZATION.
* Create the instance of the local class
  o_filemanager = NEW #( ).

START-OF-SELECTION.

*&---------------------------------------------------------------------*
*& Para TXT - It is needed pass an structure
*&---------------------------------------------------------------------*
* the method set_txtfile just set create an instance for txt files
  o_filemanager->set_txtfile( iv_struct = |ZIF_EXAMPLE=>TY_TEST_TXT| ).
  " upload the file
  o_filemanager->mo_filemanager->upload_file( iv_filename = zcl_abap_filemanager=>get_filename( ) ).
  " show the file uploaded
  o_filemanager->mo_filemanager->display_data( ).
  " To download the file
  o_filemanager->download_file( ).


*&---------------------------------------------------------------------*
*& Para CSV - comma separated
*&---------------------------------------------------------------------*
* the method set_txtfile just set create an instance for txt files
  o_filemanager->set_csvfile( iv_separator = ',' ).
  " upload the file
  o_filemanager->mo_filemanager->upload_file( iv_filename = zcl_abap_filemanager=>get_filename( ) ).
  " show the file uploaded
  o_filemanager->mo_filemanager->display_data( ).
  " To download the file
  o_filemanager->download_file( ).

*&---------------------------------------------------------------------*
*& Para CSV - tabulador
*&---------------------------------------------------------------------*
* the method set_txtfile just set create an instance for txt files
  o_filemanager->set_csvfile( iv_separator = |\t| ).
  " upload the file
  o_filemanager->mo_filemanager->upload_file( iv_filename = zcl_abap_filemanager=>get_filename( ) ).
  " show the file uploaded
  o_filemanager->mo_filemanager->display_data( ).
  " To download the file
  o_filemanager->download_file( ).

*&---------------------------------------------------------------------*
*& Para excel 2007
*&---------------------------------------------------------------------*
* the method set_txtfile just set create an instance for txt files
  o_filemanager->set_xlsfile( ).
  " upload the file
  o_filemanager->mo_filemanager->upload_file( iv_filename = zcl_abap_filemanager=>get_filename( ) ).
  " show the file uploaded
  o_filemanager->mo_filemanager->display_data( ).
  " To download the file
  o_filemanager->download_file( ).
