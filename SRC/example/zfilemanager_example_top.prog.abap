*&---------------------------------------------------------------------*
*&  Include  zfilemanager_example_top
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&  TYPES
*&---------------------------------------------------------------------*
 TYPES: BEGIN OF ty_test_txt,
          name   TYPE char20,
          num_id TYPE char10,
          text   TYPE char40,
        END OF ty_test_txt.

* In order to be able to declare the object, need the class definition
 CLASS lcl_filemanager DEFINITION DEFERRED.
 DATA: o_filemanager TYPE REF TO lcl_filemanager.
