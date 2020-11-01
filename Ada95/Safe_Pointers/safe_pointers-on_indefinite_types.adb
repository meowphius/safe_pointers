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

package body Safe_Pointers.On_Indefinite_Types is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   2.1
  -- Date      18 January 2009
  --====================================================================
  --
  --====================================================================
  -- History:
  -- Author  Version   Date   Reason for change
  --  C.G.   1.0  22.03.1997
  --  C.G.   2.0  18.03.2005  Separated common part into parent
  --  C.G.   2.1  18.01.2009  Assign
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

  function Value (Pointer: Safe_Pointer) return Object is
  begin
    return Pointer.Track.Object.all;
  end Value;

  procedure Alias (Pointer: in out Safe_Pointer; Value: access Object) is
  begin
    Finalize (Pointer);
    Pointer.Track := new Track'(Object_Pointer (Value), Count => 1, Pool_Element => False);
  end Alias;

  procedure Assign (Pointer: in Safe_Pointer; Value: in Object) is
  begin
    Pointer.Track.Object.all := Value;
  end Assign;

end Safe_Pointers.On_Indefinite_Types;
