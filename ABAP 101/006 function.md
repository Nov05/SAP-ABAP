# Function Module

This is a simple function module example.

1. T-Code `SE37`   
... create a function module "ZNOV05_FG01"   
... create a function module "ZNOV05_FM001"    
Or create through `SE80`.

<img src="https://github.com/Nov05/pictures/blob/master/ABAP%20101/2019-10-13%2018_11_08-server.sapides.online_12299%20-%20Remote%20Desktop%20Connection.png?raw=true">

<img src="https://github.com/Nov05/pictures/blob/master/ABAP%20101/2019-10-13%2018_22_00-server.sapides.online_12299%20-%20Remote%20Desktop%20Connection.png?raw=true" width=500>

2. T-Code `SE38` (or`SE80`), create a program to call the function module.

```
CALL FUNCTION 'ZNOV05_FM001'
  EXPORTING
    I_BNAME   = GV_BNAME
  IMPORTING
    E_NAME1   = GV_NAME1
  EXCEPTIONS
    NOT_FOUND = 1
    OTHERS    = 2.
```

#### Execution Result 
 
<img src="https://github.com/Nov05/pictures/blob/master/ABAP%20101/2019-10-13%2018_28_18-server.sapides.online_12299%20-%20Remote%20Desktop%20Connection.png?raw=true" width=300>

# Source Code

Function Module   
```
FUNCTION ZNOV05_FM001.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_BNAME) TYPE  USR03-BNAME OPTIONAL
*"  EXPORTING
*"     VALUE(E_NAME1) TYPE  USR03-NAME1
*"  EXCEPTIONS
*"      NOT_FOUND
*"----------------------------------------------------------------------

  DATA: OKCODE   TYPE  C,
        ADDR_USR TYPE  V_ADDR_USR,
        LW_USR03 LIKE  USR03.

  CHECK I_BNAME IS NOT INITIAL.

  SELECT SINGLE * FROM USR03 INTO CORRESPONDING FIELDS OF LW_USR03
    WHERE BNAME = I_BNAME.

  IF SY-SUBRC <> 0.
    RAISE NOT_FOUND.
  ENDIF.

  E_NAME1 = LW_USR03-NAME1.

ENDFUNCTION.
```

Program that calls the function module     
```
*&---------------------------------------------------------------------*
*& Report ZNOV05_008_FUNCTION
*&---------------------------------------------------------------------*
*& 2019-10-13 Created by Nov05
*&---------------------------------------------------------------------*
REPORT ZNOV05_008_FUNCTION.

DATA: GV_BNAME TYPE USR03-BNAME,
      GV_NAME1 TYPE USR03-NAME1.

GV_BNAME ='C1156102'.

CALL FUNCTION 'ZNOV05_FM001'
  EXPORTING
    I_BNAME   = GV_BNAME
  IMPORTING
    E_NAME1   = GV_NAME1
  EXCEPTIONS
    NOT_FOUND = 1
    OTHERS    = 2.
IF SY-SUBRC <> 0.
  WRITE 'Oops. Nothing is found.'.
ELSE.
  WRITE: GV_BNAME, GV_NAME1.
ENDIF.
```