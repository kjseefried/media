---------------------------------------------------------------------------
-- FILE    : media-images-png-test.adb
-- SUBJECT : Implementation of test code for the PNG image package.
-- AUTHOR  : (C) Copyright 2010 by Peter C. Chapin
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------

package body Media.Images.PNG.Test is

   procedure Execute_Tests is
      Image : PNG_Image;
   begin
      Read(Image, "../../../pchapin.png");
   end Execute_Tests;

end Media.Images.PNG.Test;
