# Class Event   

This is a simple function with event and event handler.

1. Create two classes `main` and `second`. 

2. Define an event in class `main`. 
```
EVENTS evt EXPORTING VALUE(e_data) TYPE char01.
``` 

3. Define an event trigger in class `main`.   
```
RAISE EVENT evt EXPORTING e_data = me->attribute.
```

4. Define an event handler in class `second`.  
```
METHODS event_handler FOR EVENT evt OF main
      IMPORTING e_data sender.
```

#### Execution Result 
 
<img src="https://github.com/Nov05/pictures/blob/master/ABAP%20101/2019-10-13%2023_23_13-abap%20event.png?raw=true" width=500>  

# Source Code

```
*&---------------------------------------------------------------------*
*& Report ZNOV05_007_EVENT
*&---------------------------------------------------------------------*
*& 2019-10-13 Created by Nov05
*&---------------------------------------------------------------------*
REPORT znov05_007_event.

*---------------------------------------------------------------------*
*       CLASS main DEFINITION
*---------------------------------------------------------------------*
CLASS main DEFINITION.

  PUBLIC SECTION.
*   Events Definitions (Note all instance events export its own objects
*   through the implicit export parameter sender )
    EVENTS evt EXPORTING VALUE(e_data) TYPE char01.
    METHODS event_trigger.
    METHODS set_data IMPORTING i_data TYPE char01.

  PRIVATE SECTION.
    DATA attribute TYPE char01.

ENDCLASS.
*---------------------------------------------------------------------*
*       CLASS main IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS main IMPLEMENTATION.

  METHOD event_trigger.
    WRITE: 'main->event_trigger() is called.', /.
*   The event is triggered (In this moment event export its own objects
*   through the implicit export parameter sender )
    RAISE EVENT evt EXPORTING e_data = me->attribute.
  ENDMETHOD.

  METHOD set_data.
    MOVE i_data TO me->attribute.
    WRITE: 'main->set_data() is called.'.
    WRITE: / 'main->attribute:', me->attribute, /.
  ENDMETHOD.

ENDCLASS.
*---------------------------------------------------------------------*
*       CLASS second DEFINITION
*---------------------------------------------------------------------*
CLASS second DEFINITION.

  PUBLIC SECTION.
*   Event Handler Definition ( We can import the parameter sender
*   containig object of event's class )
    METHODS event_handler FOR EVENT evt OF main
      IMPORTING e_data sender.

ENDCLASS.
*---------------------------------------------------------------------*
*       CLASS second IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS second IMPLEMENTATION.

  METHOD event_handler.
*   Here we are calling the instance and public method of object sender
*   that contains the instance of class main.
    WRITE: 'second->event_handler() is called by',
           cl_abap_classdescr=>get_class_name( sender ), /.
    CALL METHOD sender->set_data( i_data = 'Y' ).
  ENDMETHOD.

ENDCLASS.

*---------------------------------------------------------------------*
*       START-OF-SELECTION.
*---------------------------------------------------------------------*
DATA: gr_main   TYPE REF TO main,
      gr_second TYPE REF TO second.

START-OF-SELECTION.

  CREATE OBJECT: gr_main, gr_second.

  SET HANDLER gr_second->event_handler FOR gr_main.
  CALL METHOD gr_main->event_trigger( ).
```

Reference:    
https://wiki.scn.sap.com/wiki/display/Snippets/ABAP+Objects+-+Creating+your+First+Local+Class+-+Using+Events   
