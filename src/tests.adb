---------------------------------------------------------------------------
-- FILE    : do_tests.adb
-- SUBJECT : Top level test fixture for the Media Tools library
-- AUTHOR  : (C) Copyright 2010 by Peter C. Chapin
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------

with Media_Tools.Images.PNG.Test;

procedure Do_Tests is
begin
   Media_Tools.Images.PNG.Test.Execute_Tests;
end Do_Tests;
