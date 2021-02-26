# ABAP_Filemanager
Manage upload and download file from presentation server
<p>This projetc will let you upload any file (excel -xlsx ; txt - length delimited ; csv - default comma delimited, but custom separator can be used ) and store in an internal table to process.</p>
<p>Also, this will let you download an internal table into a file (excel -xlsx ; txt - length delimited ; csv - tab or point delimited).</p>
## Contet

- Interface ZIF_ABAP_FILEMANAGER
- Abstrac Class ZCL_ABAP_FILEMANAGER
- Clases for each file type supported 
  - ZCL_ABAP_TXTFILE
  - ZCL_CSVFILE
  - ZCL_XLSXFILE
- Transformation
  - ZABAP_XML_OFF2007_SHARED
  - ZABAP_XML_OFF2007_SHEET

# EXAMPLE
## content

- interfaz ZIF_EXAMPLE - with auxiliary TYPES used
- Main program - ZFILEMANAGER_EXAMPLE
- Include TOP  - ZFILEMANAGER_EXAMPLE_TOP for global data definitions and selection screen
- Include CL1  - ZFILEMANAGER_EXAMPLE_CL1 Local clases  

## How to use
The local class *lcl_filemanager* has a public atribute **mo_filemanager** of type zif_abap_filemanager 
that is the one that will be used to access the functionallity

The local class, has additional methods, to set the differents file formarts 
- txt --> set_txtfile, 
- csv with custom separator --> set_csvfile 
- excel file --> set_xlsfile

The example logic to download a file, can be found in the method of the local class *download_file*

This method will download, with the same format uploaded (as the class created is for the format type), 
the file obtained with the data uploaded from the file uploaded.

First fill a internal tabla with the contents uploaded 

then use that internal table to create the file (method create_file ) 

   To use this method, you just need to have an instance for the format you want the file, and pass any internal table (not empoty) to generate the file
    
 For testing propouses we are getting the value of the table filled with the upload method and use those values to create the file

Finally, use the standard class to select the filename to be save and download the file (gui_download) with the **read-only** atribute MV_FILE_BIN filled within the method create_file

