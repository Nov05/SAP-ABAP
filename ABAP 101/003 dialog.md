
# Dialog Program

This is a simple dialog program with progress function.

1. T-code `SE80`, right click on package "ZNOV05_PACKAGE01" -> Create -> Program:      
program name "ZNOV05_005_PROGRESS"   
program type "Module Pool"  
   
2. Create screen 0100.
```
PROCESS BEFORE OUTPUT.
  MODULE STATUS_0100.

PROCESS AFTER INPUT.
  MODULE USER_COMMAND_0100.
```

3. T-code `SE41`, copy Program `RSSYSTDB` Status, aka GUI Status or PF-Status `%_00` to Program "ZNOV05_005_PROGRESS" to Status "Z001".  

4. T-code `SE41`, create GUI Titles for Program "ZNOV05_005_PROGRESS":  
Title Code "Z001"   
Title "Simple Progress Function"   

4. T-code `SE93`, create Transaction "ZNOV05_005_PROGRESS":   
Start object "Program and dynpro (dialog transaction)       
Program "ZNOV05_005_PROGRESS"       
Screen number "100"         
GUI support check all options (SAP GUI for HTML, SAP GUI for Java, SAP GUI for Windows).   

#### Execution Result 

<img src="https://github.com/Nov05/pictures/blob/master/ABAP%20101/2019-10-13%20progress%20indicator.gif?raw=true">   

# Source code 

```
*&---------------------------------------------------------------------*
*& Program ZNOV05_005_PROGRESS
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
PROGRAM ZNOV05_005_PROGRESS.

*&---------------------------------------------------------------------*
*&      Form  FRM_SAPGUI_PROGRESS_INDICATOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_SAPGUI_PROGRESS_INDICATOR .

  DATA: LV_PERCENTAGE TYPE I, "Progress percentage
        LV_TEXT       TYPE TEXT40. "Any text

* Progress indicator
  DO 5 TIMES.
    LV_PERCENTAGE = LV_PERCENTAGE + 20.
    IF LV_PERCENTAGE = 100.
      LV_PERCENTAGE = 99.
    ENDIF.
    LV_TEXT = 'We are almost there.'.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        PERCENTAGE = LV_PERCENTAGE
        TEXT       = LV_TEXT.

    WAIT UP TO 1 SECONDS.
  ENDDO.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.

  SET PF-STATUS 'Z001'.
  SET TITLEBAR 'Z001'.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

  CASE SY-UCOMM.
    WHEN 'EXECUTE'.
* Progress indicator
      PERFORM FRM_SAPGUI_PROGRESS_INDICATOR.
      MESSAGE I003(ZNOV05). " 100% progress is achieved.

    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE PROGRAM.
  ENDCASE.

ENDMODULE.
```