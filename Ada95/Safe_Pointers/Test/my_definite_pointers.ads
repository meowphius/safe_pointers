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
-- Copyright (c) 1997 Christoph Karl Walter Grein
-- ---------------------------------------------------------------------

with Safe_Pointers.On_Definite_Types;  -- testee

package My_Definite_Pointers is new Safe_Pointers.On_Definite_Types (Integer);

--====================================================================
-- Author    Christoph Grein
-- Version   1.1
-- Date      22 March 1997
--====================================================================
-- Test program for package Safe_Pointers.On_Definite_Types.
-- Ada 95: Must be instantiated on library level.
--====================================================================
-- History:
-- Author  Version   Date   Reason for change
--  C.G.   1.0  15.03.1997
--  C.G.   1.1  22.03.1997  New design of Safe_Pointers
--====================================================================
