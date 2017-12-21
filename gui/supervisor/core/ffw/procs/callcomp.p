def var tvc_HTMLFile     as char no-undo.
def var tvc_OutputErrors as char no-undo.

input from src/esshtml.d.
repeat:
   import tvc_HTMLFile.

   run src/htmlcomp.p
      (input "",                 /* Directory */
       input tvc_HTMLFile,       /* HTML File to compile */
       input "",    /* XREF File */
       output tvc_OutputErrors). /* Compile errors */

   output to "compile.log" append.
   put unformatted
   tvc_OutputErrors skip.
   output close.

end.
