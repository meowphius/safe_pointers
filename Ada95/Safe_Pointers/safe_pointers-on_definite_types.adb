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
-- Copyright (c) 1998-2000, 2005 Christoph Karl Walter Grein
-- ---------------------------------------------------------------------

package body Safe_Pointers.On_Definite_Types is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   4.0
  -- Date      18 March 2005
  --====================================================================
  -- Be careful not to free aliased objects!
  --====================================================================
  -- History:
  -- Author  Version   Date   Reason for change
  --  C.G.   1.0  11.04.1991
  --  C.G.   2.0  14.03.1997  Ada 95
  --  C.G.   2.1  16.03.1997  Add procedure Alias
  --  C.G.   2.2  18.03.1997  Bug fix (type conversion needed in Alias)
  --  C.G.   3.0  22.03.1997  Make child unit
  --  C.G.   3.1  27.07.2000  Work-around for problem in Finalize
  --  C.G.   4.0  18.03.2005  Separated common part into parent
  --====================================================================

  use Tracker_Instance;

  function Null_Pointer return Safe_Pointer is
  begin
    return (Ada.Finalization.Controlled with
            Track => Null_Track);
  end Null_Pointer;

  function "=" (Left, Right: Safe_Pointer) return Boolean is
  begin
    return Left.Track.Object = Right.Track.Object;
  end "=";

  procedure Allocate (Pointer: in out Safe_Pointer) is
  begin
    Finalize (Pointer);
    Pointer.Track := new Track'(new Object, Count => 1, Pool_Element => True);
  end Allocate;

  procedure Allocate (Pointer: in out Safe_Pointer; Value: in Object) is
  begin
    Finalize (Pointer);
    Pointer.Track := new Track'(new Object'(Value), Count => 1, Pool_Element => True);
  end Allocate;

  procedure Deallocate (Pointer: in out Safe_Pointer) is
  begin
    if Pointer.Track = Null_Track then
      return;
    end if;
    if Pointer.Track.Pool_Element then
      Free (Pointer.Track.Object);
    end if;
    Pointer.Track.Count := Pointer.Track.Count - 1;
    if Pointer.Track.Count = 0 then  -- last pointer
      Free (Pointer.Track);
    end if;
    Pointer.Track := Null_Track;
  end Deallocate;

  procedure Assign (Pointer: in Safe_Pointer; Value: in Object) is
  begin
    Pointer.Track.Object.all := Value;
  end Assign;

  function Value (Pointer: Safe_Pointer) return Object is
  begin
    return Pointer.Track.Object.all;
  end Value;

  procedure Alias (Pointer: in out Safe_Pointer; Value: access Object) is
  begin
    Finalize (Pointer);
    Pointer.Track := new Track'(Object_Pointer (Value), Count => 1, Pool_Element => False);
  end Alias;

end Safe_Pointers.On_Definite_Types;
