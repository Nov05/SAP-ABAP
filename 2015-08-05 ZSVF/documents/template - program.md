
# SVF program template

```
***********************************************************************
* Project Name : D**kin DCC Upgrading - SVF
* Program Name : <Program>
* Description  : XXXX
* Date/Autohr  : 2014-09-XX / XXXX
***********************************************************************
* M O D I F I C A T I O N  L O G
***********************************************************************
* ChangeDate  Programmer  Request     Description
* ==========  ==========  ==========  =================================
*
***********************************************************************
REPORT  <program>
*   NO STANDARD PAGE HEADING
*   LINE-SIZE 120
*   LINE-COUNT 90
MESSAGE-ID zsvf.
*======================================================================*
*  Selection Screen
*======================================================================*
SELECTION-SCREEN BEGIN OF BLOCK blk01 WITH FRAME TITLE text-b01.
SELECTION-SCREEN END OF BLOCK blk01.
*======================================================================*
*  Includes
*======================================================================*
*** Global Data Declear
*INCLUDE <program>_c01.  "Class Definition
*INCLUDE <program>_top.   "Global Variable
*INCLUDE <program>_o01.  "PBO
*INCLUDE <program>_i01.  "PAI
*INCLUDE <program>_f01.   "Subroutine(FORMS)
*======================================================================*
*  Selection Screen Events
*======================================================================*
*** Maintain Selection Screen Output
*AT SELECTION-SCREEN OUTPUT.

*** F4 Value Help
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
*  PERFORM frm_f4_path CHANGING p_path.

*** Check Input Data
*AT SELECTION-SCREEN.

*AT SELECTION-SCREEN ON <f>.
*AT SELECTION-SCREEN ON BLOCK <>.
****CHECK ON SELECT SCREEN INPUT

*======================================================================*
*  Report events
*======================================================================*
*** initial data
*INITIALIZATION.

*** Prepare Report Data
*START-OF-SELECTION.

*** Output Report
*END-OF-SELECTION.

*======================================================================*
*  List events
*======================================================================*
*** Page header
*TOP-OF-PAGE.

*** Page Header After First List
*TOP-OF-PAGE DURING LINE-SELECTION.

*** Page Footer
*END-OF-PAGE.

*** When Double Click
*AT LINE-SELECTION.

*** When Click Some Icon (function code)
*AT USER-COMMAND.
```