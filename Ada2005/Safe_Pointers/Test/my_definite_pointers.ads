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
-- Copyright (c) 1997, 2011 Christoph Karl Walter Grein
-- ---------------------------------------------------------------------

with Safe_Pointers.On_Definite_Types;  -- testee

package My_Definite_Pointers is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   2.0
  -- Date      27 June 2011
  --====================================================================
  -- Test program for package Safe_Pointers.On_Definite_Types.
  -- Ada 95  : Must be instantiated on library level.
  -- Ada 2005: Can be instantiated on any level.
  --====================================================================
  -- History:
  -- Author  Version   Date   Reason for change
  --  C.G.   1.0  15.03.1997
  --  C.G.   1.1  22.03.1997  New design of Safe_Pointers
  --  C.G.   2.0  27.06.2011  Added Free
  --====================================================================

  procedure Free (X: in out Integer);  -- just to document the call

  package Instantiation is new Safe_Pointers.On_Definite_Types (Integer, Free);

end My_Definite_Pointers;
