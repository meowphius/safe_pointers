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

with Safe_Pointers.On_Limited_Types;  -- testee

package My_Limited_Pointers is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   1.0
  -- Date      11 February 2009
  --====================================================================
  -- Test instantiation for package Safe_Pointers.On_Limited_Types.
  -- Ada 95: Must be instantiated on library level.
  --====================================================================
  -- History:
  -- Author  Version   Date   Reason for change
  --  C.G.   1.0  11.02.2009
  --====================================================================

  task type T is
    entry E (I: in Integer);
  end T;

  package Safe_Limited_Pointers is new Safe_Pointers.On_Limited_Types (T);

end My_Limited_Pointers;
