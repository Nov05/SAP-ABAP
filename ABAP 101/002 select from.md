
# Select From and Modify

This is a simple report program with select from and modify DDIC tables.

1. Go to Transaction `SE38`(ABAP Editor), aka T-code SE38, create a report program.

2. Define an internal table and work area, field symbol accordingly by refering to a DDIC table. e.g.  
```
DATA: GT_TABLE01 TYPE STANDARD TABLE OF ZNOV05_TABLE01,
      GW_TABLE01 TYPE ZNOV05_TABLE01.
FIELD-SYMBOLS <FS_TABLE01> TYPE ZNOV05_TABLE01.
```

3. Define an internal table by refer to a type. e.g.
```
TYPES: BEGIN OF TY_USR21,
         BNAME TYPE USR21-BNAME,
       END OF TY_USR21.
DATA: GT_USR21 TYPE STANDARD TABLE OF TY_USR21,
      GW_USR21 TYPE TY_USR21.
FIELD-SYMBOLS <FS_USR21> TYPE TY_USR21.
```

4. Select from and modify DDIC tables. e.g. 
```
SELECT BNAME FROM USR21 INTO TABLE GT_USR21 UP TO 10 ROWS.
MODIFY ZNOV05_TABLE01 FROM TABLE GT_TABLE01.
```

5. Convert number to string. e.g.
```
DATA: COUNT TYPE I VALUE 0,
      STR   TYPE STRING.
STR = |{ CONV STRING( COUNT ) WIDTH = 10 ALPHA = IN }|.
SHIFT STR LEFT DELETING LEADING '0'.
CONCATENATE 'This is No. ' STR ' user.' INTO GW_TABLE01-LTEXT.
```

6. Set a `BREAK-POINT`, press `F8` to execute the program.  
<img src='https://github.com/Nov05/pictures/blob/master/ABAP%20101/2019-10-11%2023_19_54-server.sapides.online_12299%20-%20Remote%20Desktop%20Connection.png?raw=true'>  

7. T-code `SE16`(Data Browser) to check table data.   
<img src="https://github.com/Nov05/pictures/blob/master/ABAP%20101/2019-10-11%2023_49_32-server.sapides.online_12299%20-%20Remote%20Desktop%20Connection.png?raw=true" width=400>

# Source Code

```
*&---------------------------------------------------------------------*
*& Report ZNOV05_004_SQL
*&---------------------------------------------------------------------*
*& 2019-10-11 Created by Nov05
*&---------------------------------------------------------------------*
REPORT ZNOV05_004_SQL.

DATA: GT_TABLE01 TYPE STANDARD TABLE OF ZNOV05_TABLE01,
      GW_TABLE01 TYPE ZNOV05_TABLE01.
FIELD-SYMBOLS <FS_TABLE01> TYPE ZNOV05_TABLE01.

TYPES: BEGIN OF TY_USR21,
         BNAME TYPE USR21-BNAME,
       END OF TY_USR21.
DATA: GT_USR21 TYPE STANDARD TABLE OF TY_USR21,
      GW_USR21 TYPE TY_USR21.
FIELD-SYMBOLS <FS_USR21> TYPE TY_USR21.

DATA: COUNT TYPE I VALUE 0,
      STR   TYPE STRING.

SELECT BNAME FROM USR21 INTO TABLE GT_USR21 UP TO 10 ROWS.

CLEAR GT_TABLE01.
LOOP AT GT_USR21 ASSIGNING <FS_USR21>.
  COUNT = COUNT + 1.
  WRITE: COUNT, <FS_USR21>, /.
  CLEAR GW_TABLE01.
  GW_TABLE01-BNAME = <FS_USR21>-BNAME.
  STR = |{ CONV STRING( COUNT ) WIDTH = 10 ALPHA = IN }|.
  SHIFT STR LEFT DELETING LEADING '0'.
  CONCATENATE 'This is No. ' STR ' user.' INTO GW_TABLE01-LTEXT.
  APPEND GW_TABLE01 TO GT_TABLE01.
ENDLOOP.

MODIFY ZNOV05_TABLE01 FROM TABLE GT_TABLE01.
```