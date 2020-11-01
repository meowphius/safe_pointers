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
-- Copyright (c) 2009 Christoph Karl Walter Grein
-- ---------------------------------------------------------------------

with Ada.Exceptions;
with Ada.Text_IO;
use  Ada.Text_IO;

with My_Limited_Pointers;  -- testee
use  My_Limited_Pointers;

procedure Test_My_Limited_Pointers is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   1.0
  -- Date      11 February 2009
  --====================================================================
  -- Test program for package Safe_Pointers.On_Limited_Types.
  -- Ada 95: The objects are of a return-by-reference type and Value
  --         returns a constant view of the object.
  --====================================================================
  -- History:
  -- Author  Version   Date   Reason for change
  --  C.G.   1.0  11.02.2009
  --====================================================================

  use Safe_Limited_Pointers;

  P: Safe_Pointer;

begin

  Allocate (P);
  Value (P).E (123);  -- Ada95: T is a return-by-reference type

  declare
    C: T renames Value (P);  -- this is a constant view
  begin
    C.E (-10);
    Deallocate (P);
    C.E (0);  -- what happens here?
  end;

end Test_My_Limited_Pointers;
