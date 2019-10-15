﻿# Open SQL

This is a simple SQL code example.

1. Select all fields from DDIC table `spfli` into inner table `gt_spfli`. In this case, old Open SQL syntax still works.
```
SELECT *
  APPENDING CORRESPONDING FIELDS OF TABLE gt_spfli
  UP TO 1 ROWS
  FROM spfli
  WHERE carrid = c1.
```

2. However if commas is used in field list, you will have to use @ to escape ABAP variables.
```
SELECT SINGLE connid, cityfrom, cityto
  INTO @gw_spfli
  FROM spfli
  WHERE carrid = @c1.
```

# Source Code

```
*&---------------------------------------------------------------------*
*& Report ZNOV05_012_SQL
*&---------------------------------------------------------------------*
*& 2019-10-15 Created by Nov05
*&---------------------------------------------------------------------*
REPORT znov05_012_new_open_sql.

TYPES: BEGIN OF ty_spfli,
         connid   TYPE spfli-connid,
         cityfrom TYPE spfli-cityfrom,
         cityto   TYPE spfli-cityto,
       END OF ty_spfli.

DATA: gw_spfli TYPE ty_spfli,
      gt_spfli TYPE STANDARD TABLE OF ty_spfli.
FIELD-SYMBOLS: <fs_spfli> TYPE ty_spfli.

DATA: c1 TYPE spfli-carrid VALUE 'LH'.

* if commas is used in field list, you will
* have to escape ABAP variables with @
SELECT SINGLE connid, cityfrom, cityto
  INTO @gw_spfli
  FROM spfli
  WHERE carrid = @c1
  .
WRITE: gw_spfli, /.

SELECT connid, cityfrom, cityto
  INTO CORRESPONDING FIELDS OF TABLE @gt_spfli
  FROM spfli
  WHERE carrid = @c1
  .
SELECT *
  APPENDING CORRESPONDING FIELDS OF TABLE gt_spfli
  UP TO 1 ROWS
  FROM spfli
  WHERE carrid = c1
  .
LOOP AT gt_spfli ASSIGNING <fs_spfli>.
  WRITE / <fs_spfli>.

  AT LAST.
    WRITE: /, <fs_spfli>.
  ENDAT.
ENDLOOP.
```

Reference:   
https://blogs.sap.com/2017/02/17/why-the-new-open-sql-syntax-is-better/    
https://help.sap.com/doc/abapdocu_750_index_htm/7.50/en-US/abennews-750-open_sql.htm    
https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/abennews-751-open_sql.htm   