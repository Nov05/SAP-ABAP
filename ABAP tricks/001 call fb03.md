
# source code

```
*&---------------------------------------------------------------------*
*& Report ZNOV05_003_FB03
*&---------------------------------------------------------------------*
*& Call Transaction FB03 to Display FI Documents
*& 2016-02-03 Created by Nov05
*&---------------------------------------------------------------------*
REPORT znov05_003_fb03.

DATA: lt_rspar_belnr TYPE TABLE OF rsparams WITH HEADER LINE.

lt_rspar_belnr-selname = 'BR_BELNR'.
lt_rspar_belnr-kind = 'S'.
lt_rspar_belnr-sign = 'I'.
lt_rspar_belnr-option = 'BT'.
lt_rspar_belnr-low = '1700000451'.
lt_rspar_belnr-high = '2000000102'.
APPEND lt_rspar_belnr.
lt_rspar_belnr-low = '2000000118'.
APPEND lt_rspar_belnr.

SUBMIT rfbueb00
      USING SELECTION-SCREEN 1000
      WITH br_bukrs-low = '0001'
      WITH SELECTION-TABLE lt_rspar_belnr
      WITH br_gjahr-low = '2013'
      AND RETURN.
```