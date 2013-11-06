---------------------------------------------------------------------------
-- FILE    : media-images-png.adb
-- SUBJECT : Package for manipulating PNG image files.
-- AUTHOR  : (C) Copyright 2010 by Peter C. Chapin
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
with Ada.Sequential_IO;

package body Media.Images.PNG is

   package Byte_IO is new Ada.Sequential_IO(Element_Type => Media.Images.Image_Byte);

   -- This procedure checks the PNG signature on the given file. It raises Corrupt_Image if the
   -- signature is invalid. It will raise other exceptions from the library if bytes can't be
   -- read from the file when needed.
   --
   procedure Verify_Signature(File : Byte_IO.File_Type) is
      type Signature_Array is array(0 .. 7) of Media.Images.Image_Byte;

      Expected_Signature : constant Signature_Array := (137, 80, 78, 71, 13, 10, 26, 10);
      Actual_Signature   :          Signature_Array;
   begin
      for I in Signature_Array'Range loop
         Byte_IO.Read(File, Actual_Signature(I));
      end loop;
      if Actual_Signature /= Expected_Signature then
         raise Corrupt_Image with "Bad signature";
      end if;
   end Verify_Signature;


   function Read_Chunk_Size(File : Byte_IO.File_Type) return Natural is
      Raw_Size : array(0 .. 3) of Media.Images.Image_Byte;
   begin
      for I in Raw_Size'Range loop
         Byte_IO.Read(File, Raw_Size(I));
      end loop;
      -- TODO: Finish Me!
      return 0;
   end Read_Chunk_Size;


   procedure Read(Image : out PNG_Image; File_Name : in String) is
      use Byte_IO;
      File : File_Type;
   begin
      Open(File, In_File, File_Name);
      Verify_Signature(File);
      Close(File);
   exception
      when others =>
         if Byte_IO.Is_Open(File) then
            Close(File);
         end if;
         raise;
   end Read;


   procedure Convert_To_Raw(Image : in PNG_Image; Raw_Image : out Media.Images.Image) is
   begin
      raise Media.Not_Implemented;
   end Convert_To_Raw;


   procedure Convert_To_PNG(Raw_Image : in Media.Images.Image; Image : out PNG_Image) is
   begin
      raise Media.Not_Implemented;
   end Convert_To_PNG;


   procedure Write(Image: in PNG_Image; File_Name : in String) is
   begin
      raise Media.Not_Implemented;
   end Write;


   function Chunk_Count(Image : PNG_Image) return Natural is
   begin
      return Natural(Image.Chunks.Length);
   end Chunk_Count;

end Media.Images.PNG;
