let () =
  Round_trip.register ();
  Refusals.register ();
  Rebuild.register ();
  Run.register ();
  Regressions.register ();
  Tezt.Test.run ()
