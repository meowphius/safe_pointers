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
-- Copyright (c) 2005, 2009 Christoph Karl Walter Grein
-- ---------------------------------------------------------------------

package body Safe_Pointers is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   2.0
  -- Date      9 May 2011
  --====================================================================
  -- The idea behind the implementation is to allocate each object
  -- designated by a Safe_Pointer exactly once and to count all pointers
  -- designating the same object.
  --
  -- Now let P and Q be two safe pointers. After their declaration
  -- (without an initial value), they both have the component value
  -- Null_Track.
  --
  --   Allocate (P);    -- Decrease counter of object designated by P,
  --                    -- create new object and tracker, 1 pointer to
  --                    -- new object.
  --   Q := P;          -- Decrease counter of (object designated by) Q,
  --                    -- increase counter of P, 2 pointers to object.
  --   declare
  --     R: Safe_Pointer;  -- component value Null_Track
  --   begin            -- 2 pointers to object
  --     R := P;        -- 3 pointers to object
  --   end;             -- R leaves its scope, so it is finialized;
  --                    -- 2 pointers to object remain.
  --   Deallocate (Q);  -- Deallocate the object designated by P and Q,
  --                    -- leave counter unchanged.
  --====================================================================
  -- History:
  -- Author  Version   Date   Reason for change
  --  C.G.   1.0  18.03.2005
  --  C.G.   1.1  27.12.2009  Count renamed to References
  --  C.G.   2.0  09.05.2011  Bug fix in Finalize - must be idempotent:
  --                          Pointer.Track := Null_Track;
  --====================================================================

  package body Generic_Tracker is

    procedure Adjust (Pointer: in out Safe_Pointer) is
    begin
      if Pointer.Track /= Null_Track then
        Pointer.Track.References := Pointer.Track.References + 1;
      end if;
    end Adjust;

    procedure Finalize (Pointer: in out Safe_Pointer) is
      -- might be called several times for the same object.
    begin
      if Pointer.Track /= Null_Track then
        Pointer.Track.References := Pointer.Track.References - 1;
        if Pointer.Track.References = 0 then  -- last pointer
          Free (Pointer.Track.Object);
          Free (Pointer.Track);
        end if;
        Pointer.Track := Null_Track;  -- idempotence
      end if;
    end Finalize;

  end Generic_Tracker;

end Safe_Pointers;
