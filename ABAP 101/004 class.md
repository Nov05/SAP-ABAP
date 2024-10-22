# ABAP Class

This is a simple ABAP class.     

#### Execution Result 

<img src="https://github.com/Nov05/pictures/blob/master/ABAP%20101/2019-10-12%2022_03_04-server.sapides.online_12299%20-%20Remote%20Desktop%20Connection.png?raw=true" width=500>   

# Source code

```
*&---------------------------------------------------------------------*
*& Report ZNOV05_006_CLASS
*&---------------------------------------------------------------------*
*& 2019-10-12 Modified by Nov05
*&---------------------------------------------------------------------*
REPORT ZNOV05_006_CLASS.

*---------------------------------------------------------------------*
*       CLASS main DEFINITION
*---------------------------------------------------------------------*
CLASS MAIN DEFINITION.

  PUBLIC SECTION.
    "// Instance Methods ( Note we use the statement 'METHODS'
    "// to define an instance method )
    METHODS SET_DATA IMPORTING I_DATA TYPE STRING.
    METHODS GET_DATA RETURNING VALUE(R_DATA) TYPE STRING.
    METHODS PRINT_ATTRIBUTE IMPORTING I_DATA TYPE STRING.
    "// Instance Methods ( Note we use the statement 'CLASS-METHODS'
    "// to define a static method )
    CLASS-METHODS SET_CLASSDATA IMPORTING I_DATA TYPE STRING.
    CLASS-METHODS GET_CLASSDATA RETURNING VALUE(R_DATA) TYPE STRING.
    CLASS-METHODS PRINT_CLASSATTRIBUTE IMPORTING I_DATA TYPE STRING.

  PROTECTED SECTION.
    "// Instance Attribute ( Note we use the statement 'DATA'
    "// to define an instance attribute )
    DATA ATTRIBUTE TYPE STRING.
    "// Static Attribute ( Note we use the statement 'CLASS-DATA'
    "// to define a static attribute )
    CLASS-DATA CLASSATTRIBUTE TYPE STRING.

  PRIVATE SECTION.
    "// Instace event ( Note we use the statement 'EVENTS'
    "// to define an instance event )
    EVENTS EVENT EXPORTING VALUE(E_DATA) TYPE STRING.
    "// Instace event ( Note we use the statement 'CLASS-EVENTS'
    "// to define a static event )
    CLASS-EVENTS CLASSEVENT EXPORTING VALUE(E_DATA) TYPE STRING.
    "// For more informations about events see the following example:
    "// ABAP Objects - Creating your First Local Class - Using Events

ENDCLASS.                    "main DEFINITION
*---------------------------------------------------------------------*
*       CLASS main IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS MAIN IMPLEMENTATION.

  METHOD SET_DATA.
    CONCATENATE 'Instance Attribute value' I_DATA
    INTO ATTRIBUTE SEPARATED BY SPACE.
  ENDMETHOD.                    "set_data

  METHOD GET_DATA.
    MOVE ATTRIBUTE TO R_DATA.
  ENDMETHOD.                    "get_data

  METHOD SET_CLASSDATA.
    CONCATENATE 'Static Attribute value' I_DATA
    INTO CLASSATTRIBUTE SEPARATED BY SPACE.
  ENDMETHOD.                    "set_classdata

  METHOD GET_CLASSDATA.
    MOVE MAIN=>CLASSATTRIBUTE TO R_DATA.
  ENDMETHOD.                    "get_classdata

  METHOD PRINT_ATTRIBUTE.
    WRITE: I_DATA, /.
  ENDMETHOD.                    "print_attribute

  METHOD PRINT_CLASSATTRIBUTE.
    WRITE: I_DATA, /.
  ENDMETHOD.                    "print_classattribute

ENDCLASS.                    "main IMPLEMENTATION

DATA: VAR              TYPE STRING,
      OBJECT_REFERENCE TYPE REF TO MAIN.

START-OF-SELECTION.

  "// Calling a Static method (note we don't have a object )
  "// instead we use the <class name>=><method name>.
  MAIN=>SET_CLASSDATA( 'SDN' ).
  VAR = MAIN=>GET_CLASSDATA( ).
  "// Print the var value
  MAIN=>PRINT_CLASSATTRIBUTE( VAR ).

  CREATE OBJECT OBJECT_REFERENCE.
  "// - Calling a Instance Method( Note we have to use a object to
  "// access the insntace components of class main )
  "// - Note we're using the statment "CALL METHOD", see looking for
  "// functional & General methods for more informations
  CALL METHOD OBJECT_REFERENCE->SET_DATA( 'BPX' ).
  VAR = OBJECT_REFERENCE->GET_DATA(  ).
  OBJECT_REFERENCE->PRINT_ATTRIBUTE( VAR ).
```

Reference:   
https://wiki.scn.sap.com/wiki/display/Snippets/ABAP+Objects+-+Creating+your+First+Local+Class+-+Defining+Components 