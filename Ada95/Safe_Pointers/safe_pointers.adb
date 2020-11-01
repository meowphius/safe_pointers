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
-- Copyright (c) 2005 Christoph Karl Walter Grein
-- ---------------------------------------------------------------------

package body Safe_Pointers is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   1.0
  -- Date      18 March 2005
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
  --
  -- Be careful not to free aliased objects!
  --====================================================================
  -- History:
  -- Author  Version   Date   Reason for change
  --  C.G.   1.0  18.03.2005
  --====================================================================

  package body Generic_Tracker is

    procedure Adjust (Pointer: in out Safe_Pointer) is
    begin
      if Pointer.Track /= Null_Track then
        Pointer.Track.Count := Pointer.Track.Count + 1;
      end if;
    end Adjust;

    procedure Finalize (Pointer: in out Safe_Pointer) is
    begin
      if Pointer.Track /= Null_Track then
        -- I do not see a flaw in my reasoning, but experience with a big
        -- application shows that after main program termination, when
        -- everything gets finalized, we arrive more than once (observed
        -- was six times for a given object) within this if-statement after
        -- Pointer.Track.Count has been set to zero, although this seems
        -- impossible (Pointer.Track is freed in this case and thus equals
        -- Null_Track afterwards).
        -- This raises an exception which prevents good program termination.
        -- As a work-around (until the reason for this queer behaviour is
        -- found), we prevent the counter from going below zero.
        Pointer.Track.Count := Integer'Max (Pointer.Track.Count - 1, 0);
        if Pointer.Track.Count = 0 then  -- last pointer
          if Pointer.Track.Pool_Element then
            Free (Pointer.Track.Object);
          end if;
          Free (Pointer.Track);
        end if;
      end if;
    end Finalize;

  end Generic_Tracker;

end Safe_Pointers;