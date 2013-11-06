---------------------------------------------------------------------------
-- FILE    : tests.adb
-- SUBJECT : Top level test fixture for the Media library
-- AUTHOR  : (C) Copyright 2010 by Peter C. Chapin
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------

with Media.Images.PNG.Test;

procedure Tests is
begin
   Media.Images.PNG.Test.Execute_Tests;
end Tests;
