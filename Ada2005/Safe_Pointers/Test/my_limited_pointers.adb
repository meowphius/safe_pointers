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

with Ada.Text_IO;
use  Ada.Text_IO;

package body My_Limited_Pointers is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   2.0
  -- Date      29 December 2009
  --====================================================================
  --
  --====================================================================
  -- History:
  -- Author  Version   Date   Reason for change
  --  C.G.   2.0  29.12.2009  entry Stop
  --  C.G.   1.0  11.02.2009
  --====================================================================

  task body T is
  begin
    loop
      select
        accept E (I: in Integer) do
          Put_Line (Integer'Image (I));
        end E;
      or
        accept Stop;
        exit;
      end select;
    end loop;
  end T;

end My_Limited_Pointers;
