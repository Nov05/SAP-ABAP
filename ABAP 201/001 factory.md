# ABAP Object Factory Implementation  

This is simple program to implement factory design. 

1. Factory design is used to abstract the process of creating objects, so the type of object created can be specified at runtime.  
#### Object-Oriented Factory Method Design Pattern UML   
<p align="center">
<img src="https://www.abaptutorial.com/wp-content/uploads/2011/05/factorymethod.gif" width=400 align=">
</p>

2. ABAP interface object is in use in this case.    
#### ABAP Factory Method Design Pattern UML Diagram   
<p align="center"> 
<img src="https://github.com/Nov05/pictures/blob/master/ABAP%20201/2019-10-14%20abap%20factory%20uml.png?raw=true" width=400>    
</p>

3. This program creats either a `Sales Order` object or a `Sales Quotation` at runtime.

#### Execution Result   

<img src="https://github.com/Nov05/pictures/blob/master/ABAP%20201/20191014_023743%20ABAP%20Object%20Factory.gif?raw=true">  

# Source Code 

```
*&---------------------------------------------------------------------*
*& Report ZNOV05_010_FACTORY
*&---------------------------------------------------------------------*
*& 2019-10-14 Created by Nov05
*&---------------------------------------------------------------------*
REPORT znov05_010_factory.

*----------------------------------------------------------------------*
*       CLASS cl_sales_document DEFINITION
*----------------------------------------------------------------------*
* Sale document
*----------------------------------------------------------------------*
CLASS cl_sales_document DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS: write ABSTRACT.
ENDCLASS.                    "cl_sales_document DEFINITION
*----------------------------------------------------------------------*
*       CLASS cl_quotation  DEFINITIO
*----------------------------------------------------------------------*
* Quotation
*----------------------------------------------------------------------*
CLASS cl_quotation DEFINITION
      INHERITING FROM cl_sales_document.
  PUBLIC SECTION.
    METHODS: write REDEFINITION.
ENDCLASS.                    "cl_quotation  DEFINITIO
*----------------------------------------------------------------------*
*       CLASS cl_quotation IMPLEMENTATION
*----------------------------------------------------------------------*
* Quotation implementation
*----------------------------------------------------------------------*
CLASS cl_quotation IMPLEMENTATION.
  METHOD write.
    WRITE: 'This is a Sales Quotation'.
  ENDMETHOD.                    "write
ENDCLASS.                    "cl_quotation IMPLEMENTATION
*----------------------------------------------------------------------*
*       CLASS cl_order  DEFINITIO
*----------------------------------------------------------------------*
* Sale order
*----------------------------------------------------------------------*
CLASS cl_order DEFINITION
      INHERITING FROM cl_sales_document.
  PUBLIC SECTION.
    METHODS: write REDEFINITION.
ENDCLASS.                    "cl_order  DEFINITIO
*----------------------------------------------------------------------*
*       CLASS cl_order IMPLEMENTATION
*----------------------------------------------------------------------*
* Sale Order implementation
*----------------------------------------------------------------------*
CLASS cl_order IMPLEMENTATION.
  METHOD write.
    WRITE: 'This is a Sales Order'.
  ENDMETHOD.                    "write
ENDCLASS.                    "cl_order IMPLEMENTATION
*----------------------------------------------------------------------*
*       INTERFACE if_sales_document_factory IMPLEMENTATION
*----------------------------------------------------------------------*
* Sale document factor interface
*----------------------------------------------------------------------*
INTERFACE if_sales_document_factory.
  METHODS:
    create IMPORTING i_vbtyp                  TYPE vbtyp
           RETURNING VALUE(rr_sales_document) TYPE REF TO cl_sales_document.
ENDINTERFACE.                    "lif_sales_document_factory IMPLEMENTATION
*----------------------------------------------------------------------*
*       CLASS cl_sales_document_factory DEFINITION
*----------------------------------------------------------------------*
* Sale document factory
*----------------------------------------------------------------------*
CLASS cl_sales_document_factory DEFINITION.
  PUBLIC SECTION.
    INTERFACES:
      if_sales_document_factory.
    ALIASES:
      create FOR if_sales_document_factory~create.
ENDCLASS.                    "cl_sales_document_factory DEFINITION
*----------------------------------------------------------------------*
*       CLASS cl_sales_document_factory IMPLEMENTATION
*----------------------------------------------------------------------*
* Sale document factory implementation
*----------------------------------------------------------------------*
CLASS cl_sales_document_factory IMPLEMENTATION.

  METHOD create.
    DATA: lr_sales_document TYPE REF TO cl_sales_document.

    CASE i_vbtyp.
      WHEN 'B'.
        CREATE OBJECT lr_sales_document TYPE cl_quotation.
      WHEN 'C'.
        CREATE OBJECT lr_sales_document TYPE cl_order.
      WHEN OTHERS.
        " default, create order
        CREATE OBJECT lr_sales_document TYPE cl_order.
    ENDCASE.
    rr_sales_document = lr_sales_document.
  ENDMETHOD.                    "create

ENDCLASS.                    "cl_sales_document_factory IMPLEMENTATION

DATA:
  gr_doc                    TYPE REF TO cl_sales_document,
  gr_sales_document_factory TYPE REF TO if_sales_document_factory.

*----------------------------------------------------------------------*
*       Selection Screen
*----------------------------------------------------------------------*
PARAMETERS: p_vbtyp TYPE vbtyp. " B - quotation, C- order

*----------------------------------------------------------------------*
*      START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  " create factory
  CREATE OBJECT gr_sales_document_factory
    TYPE cl_sales_document_factory.

  " create sale document at runtime
  gr_doc = gr_sales_document_factory->create( p_vbtyp ).
  gr_doc->write( ).
```

Reference:     
https://www.abaptutorial.com/abap-factory-method-design-pattern/  
https://answers.sap.com/questions/4237018/returning-value-and-exporting.html  
```
To get some values from a method, one can use the EXPORTING, CHANGING or RETURNING parameters.
If one uses RETURNING parameters, the following restrictions apply:
(1) No EXPORTING/CHANGING parameters can be used for the method.
(2) Only one RETURNING parameter can be used.
(3) RETURNING parameters are only passed by value.
```  