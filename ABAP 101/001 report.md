
# Report Program

This is a simple report program with pushbutton etc. on the selection screen.

#### Execution Result 

<img src="https://github.com/Nov05/pictures/blob/master/ABAP%20101/2019-10-13%20selection%20screen.gif?raw=true">   

# Source code

```
*&---------------------------------------------------------------------*
*& Report  ZNOV05_001_PUSHBUTTON
*&
*&---------------------------------------------------------------------*
*& 2019-10-11 Initial version
*&
*&---------------------------------------------------------------------*
REPORT ZNOV05_001_PUSHBUTTON.

TABLES: ZNOV05_TABLE01,
        SSCRFIELDS.
DATA: CLICK   TYPE I VALUE 0,
      OK_CODE TYPE SY-UCOMM.

SELECTION-SCREEN BEGIN OF BLOCK BLK_01 WITH FRAME TITLE TEXT-S01.
PARAMETERS PARAM_01 TYPE I.
SELECT-OPTIONS SEL_01 FOR ZNOV05_TABLE01-BNAME MATCHCODE OBJECT USER_ADDR.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN COMMENT /05(70) COMM_01.
SELECTION-SCREEN COMMENT /05(70) TEXT-S02.
SELECTION-SCREEN SKIP 2.
SELECTION-SCREEN PUSHBUTTON 2(21) TEXT-S03 USER-COMMAND Z001.
SELECTION-SCREEN END OF BLOCK BLK_01.

INITIALIZATION.
  COMM_01 = 'This text is dynamically defined.'.

AT SELECTION-SCREEN.
  OK_CODE = SY-UCOMM.
  CASE SSCRFIELDS-UCOMM.
    WHEN 'Z001'.
      CLICK = CLICK + 1.
      PARAM_01 = CLICK.
  ENDCASE.
```