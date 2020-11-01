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
-- Copyright (c) 2011 Christoph Karl Walter Grein
-- ---------------------------------------------------------------------

with Ada.Text_IO;
use  Ada.Text_IO;

with Safe_Pointers.On_Definite_Types;  -- testee

package body My_Definite_Pointers is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   1.0
  -- Date      27 June 2011
  --====================================================================
  --
  --====================================================================
  -- History:
  -- Author  Version   Date   Reason for change
  --  C.G.   1.0  27.06.2011  Created
  --====================================================================

  procedure Free (X: in out Integer) is
  begin
    Put_Line ("Free called for " & Integer'Image (X));
  end Free;

end My_Definite_Pointers;
