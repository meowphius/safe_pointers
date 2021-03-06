-- ---------------------------------------------------------------------
-- This software is provided as is in the hope that it might be found
-- useful.
-- You may use and modify it freely provided you keep this copyright
-- notice unchanged and mark modifications appropriately.
--
-- Bug reports and proposals for improvements are welcome. Please send
-- them to the eMail address below.
--
-- Christoph Karl Walter Grein
-- Hauptstr. 42
-- D-86926 Greifenberg
-- Germany
--
-- eMail:    Christ-Usch.Grein@T-Online.de
-- Internet: http://www.christ-usch-grein.homepage.t-online.de/
--
-- Copyright (c) 1998, 2005, 2009 Christoph Karl Walter Grein
-- ---------------------------------------------------------------------

generic

  type Object is limited private;

package Safe_Pointers.On_Limited_Types is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   2.1
  -- Date      3 December 2009
  --====================================================================
  --
  --====================================================================
  -- History:
  -- Author  Version   Date   Reason for change
  --  C.G.   1.0  21.03.1997
  --  C.G.   2.0  18.03.2005  Separated common part into parent
  --  C.G.   2.1  03.12.2009  Removed unneeded with-clause
  --====================================================================

  type Safe_Pointer is private;

  function Null_Pointer return Safe_Pointer;

  function "=" (Left, Right: Safe_Pointer) return Boolean;

  procedure Allocate   (Pointer: in out Safe_Pointer);
  procedure Deallocate (Pointer: in out Safe_Pointer);
  procedure Alias      (Pointer: in out Safe_Pointer; Value: access Object);
  function  Value      (Pointer:        Safe_Pointer)        return Object;

private

  type Object_Pointer is access all Object;  pragma Controlled (Object_Pointer);
  procedure Free is new Ada.Unchecked_Deallocation (Object, Object_Pointer);

  package Tracker_Instance is new Generic_Tracker (Object_Pointer);

  type Safe_Pointer is new Tracker_Instance.Safe_Pointer with null record;

end Safe_Pointers.On_Limited_Types;
