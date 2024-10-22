﻿# Native SQL

This is a simple example of native SQL.

1. In this case, database table name `SPFLI` and field names `CONNID, CITYFROM, CITYTO` have to be in uppercase.
```
EXEC SQL PERFORMING loop_output.
  select CONNID, CITYFROM, CITYTO
  into :wa
  from SPFLI
  where CARRID = :c1
ENDEXEC.
```
 
2. Native SQL can query database tables defined or not defined in ABAP Dictionary (DDIC). This is an example of native SQL querying a database table defined in DDIC (the red line in the figure).

<img src="https://github.com/Nov05/pictures/blob/master/ABAP%20101/database_access_through_native_sql_statement_170145_v2.png?raw=true" width=500>

#### Execution Result

<img src="https://github.com/Nov05/pictures/blob/master/ABAP%20101/2019-10-15%2015_24_40-server.sapides.online_12299%20-%20Remote%20Desktop%20Connection.png?raw=true" width=400>  



# Source Code

```
*&---------------------------------------------------------------------*
*& Report ZNOV05_011_SQL
*&---------------------------------------------------------------------*
*& 2019-10-15 Created by Nov05
*&---------------------------------------------------------------------*
REPORT znov05_011_native_sql.

DATA: BEGIN OF wa,
        connid   TYPE spfli-connid,
        cityfrom TYPE spfli-cityfrom,
        cityto   TYPE spfli-cityto,
      END OF wa.

DATA: c1   TYPE spfli-carrid VALUE 'LH',
      text TYPE string.

* table and column names have to be in uppercase
TRY.
    EXEC SQL PERFORMING loop_output.
      select CONNID, CITYFROM, CITYTO
      into :wa
      from SPFLI
      where CARRID = :c1
    ENDEXEC.
  CATCH cx_root INTO DATA(gr_cx_root).
    text = gr_cx_root->get_text( ).
    WRITE: / text.
ENDTRY.

FORM loop_output.
  WRITE: / wa-connid, wa-cityfrom, wa-cityto.
ENDFORM.
```

Reference:    
https://help.sap.com/doc/saphelp_nw70/7.0.31/en-US/fc/eb3b8b358411d1829f0000e829fbfe/content.htm     
https://blogs.sap.com/2012/12/28/native-sql-its-use-with-database-connection-in-sap/    