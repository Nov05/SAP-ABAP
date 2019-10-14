# Exception Handling

This is a simple program with exception handling.

#### Excution Result 

<img src="https://github.com/Nov05/pictures/blob/master/ABAP%20101/2019-10-13%2021_44_54-exception%20handling.png?raw=true" width=500>

# Source Code

```
*&---------------------------------------------------------------------*
*& Report ZNOV05_009_EXCEPTION
*&---------------------------------------------------------------------*
*& 2019-10-13 Created by Nov05
*&---------------------------------------------------------------------*
REPORT ZNOV05_009_EXCEPTION.

DATA: N1         TYPE I VALUE 1,
      N2         TYPE I VALUE 0,
      STR        TYPE STRING VALUE 's',
      RESULT     TYPE P LENGTH 8 DECIMALS 2,
      GR_CX_ROOT TYPE REF TO CX_ROOT,
      TEXT       TYPE STRING.

TRY.
    RESULT = N1 / N2.
  CATCH CX_SY_ZERODIVIDE INTO GR_CX_ROOT.
    TEXT = GR_CX_ROOT->GET_TEXT( ).
    WRITE: 'Error 1:', TEXT, /.
ENDTRY.

TRY.
    RESULT = N1 / STR.
  CATCH CX_ROOT INTO GR_CX_ROOT.
    CLEAR TEXT.
    TEXT = GR_CX_ROOT->GET_TEXT( ).
    WRITE: 'Error 2:', TEXT, /.
ENDTRY.

* DATA() - Inline Declaration
TRY.
    CL_DEMO_OUTPUT=>DISPLAY( 1 / 0 ).
  CATCH CX_SY_ARITHMETIC_ERROR INTO DATA(OREF).
    CLEAR TEXT.
    TEXT = OREF->GET_TEXT( ).
    WRITE: 'Error 3:', 'Short text:', TEXT, /.
    CLEAR TEXT.
    TEXT = OREF->GET_LONGTEXT( ).
    WRITE: '        ', 'Long text:', TEXT, /.
    CLEAR TEXT.
    OREF->GET_SOURCE_POSITION(
    IMPORTING
      PROGRAM_NAME = DATA(PROGRAM_NAME)
      INCLUDE_NAME = DATA(INCLUDE_NAME)
      SOURCE_LINE = DATA(SOURCE_LINE)
    ).
    TEXT = |{ CONV STRING( SOURCE_LINE ) WIDTH = 10 ALPHA = IN }|.
    SHIFT TEXT LEFT DELETING LEADING '0'.
    CONCATENATE PROGRAM_NAME INCLUDE_NAME TEXT INTO TEXT SEPARATED BY', '.
    WRITE: '        ', 'Source position:', TEXT, /.
ENDTRY.
```

Reference:  
https://wiki.scn.sap.com/wiki/display/ABAP/Exception+Handling+in+ABAP+object+with+the+help+of+Exception+Class  

<img src="https://wiki.scn.sap.com/wiki/download/attachments/64259785/exception_handling_3.GIF?version=1&modificationDate=1228299550000&api=v2" width=400>