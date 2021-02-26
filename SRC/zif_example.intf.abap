INTERFACE zif_example
  PUBLIC .
  TYPES: BEGIN OF ty_aux_folio,
           rcdty TYPE char03,  "Record type AccPay or HR
           bukrs TYPE bkpf-bukrs,            "Company
           belnr TYPE bkpf-belnr,            "Document Number
           budat TYPE bkpf-budat,            "Posting date
           xblnr TYPE bkpf-xblnr,            "Vendor invoice number
           uuid  TYPE char36,   "Universal Unique Identifier
           taxid TYPE pmxp_rfcnm,  "Tax Id (RFC)
           zwels TYPE dzwels,  "Payment method
           waers TYPE bkpf-waers,            "Currency
           wrbtr TYPE bseg-wrbtr,            "Amount
           kursf TYPE bkpf-kursf,            "Exchange rate
           cnoex TYPE char01,                "Nat.Other and Foreign flag
         END OF ty_aux_folio,

         BEGIN OF ty_test_txt,
           name   TYPE char20,
           num_id TYPE char10,
           text   TYPE char40,
         END OF ty_test_txt,

         tyt_aux_folio TYPE STANDARD TABLE OF ty_aux_folio WITH DEFAULT KEY.
ENDINTERFACE.
