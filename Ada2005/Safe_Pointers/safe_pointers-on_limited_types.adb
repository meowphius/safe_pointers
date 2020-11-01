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

package body Safe_Pointers.On_Limited_Types is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   1.1
  -- Date      1 July 2011
  --====================================================================
  --
  --====================================================================
  -- History:
  -- Author  Version   Date   Reason for change
  --  C.G.   1.0  22.03.1997
  --  C.G.   2.0  18.03.2005  Separated common part into parent
  --  C.G.   3.0  11.02.2009  Ada 2005: Removed illegal function Value;
  --                          new function Reference
  --  C.G.   4.0  29.12.2009  Reference via Accessor; removed Alias
  --  C.G.   4.1  01.07.2011  Added generic formal Free
  --====================================================================

  procedure Free (X: in out Unsafe_Pointer) is
    procedure Free is new Ada.Unchecked_Deallocation (Object, Unsafe_Pointer);
  begin
    if X /= null then
      Free (X.all);
      Free (X);
    end if;
  end Free;

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
    Pointer.Track := new Track'(new Object, References => 1, Raw_Count => 0);
  end Allocate;

  procedure Deallocate (Pointer: in out Safe_Pointer) is
  begin
    if Pointer.Track = Null_Track then
      return;
    end if;
    if Pointer.Track.Raw_Count > 0 then  -- tampering event
      raise Program_Error;  -- as do Ada.Containers in such a case
    end if;
    Free (Pointer.Track.Object);
    Pointer.Track.References := Pointer.Track.References - 1;
    if Pointer.Track.References = 0 then  -- last pointer
      Free (Pointer.Track);
    end if;
    Pointer.Track := Null_Track;
  end Deallocate;

  function Reference (Pointer: Safe_Pointer) return Accessor is
  begin
    Pointer.Track.Raw_Count := Pointer.Track.Raw_Count + 1;
    return This: Accessor (Value => Pointer.Track.Object) do
      This.Safe := Pointer;
    end return;
  end Reference;

  overriding procedure Finalize (Object: in out Limiter) is
  begin
    Object.Outer.Safe.Track.Raw_Count := Object.Outer.Safe.Track.Raw_Count - 1;
  end Finalize;

end Safe_Pointers.On_Limited_Types;
