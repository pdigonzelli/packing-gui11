/************************************************************************/
/*  PROGRAMA    : s_pritxt.p                                            */
/************************************************************************/
/*  Genera TXT del archivo de Spool y lo manda a PRINTER                */
/************************************************************************/
/*  PROGRAMADOR: Juan Carlos R¡os - Grupo Sƒuken S.A.                   */
/*  FECHA      : 06/03/2005                                             */
/*  VERSION    : 1.0                                                    */
/************************************************************************/
  
define input parameter vpi_spool   like par_reportes.arc_spool.
define input parameter vpi_puntero as recid no-undo.
DEFINE INPUT PARAMETER vpi_copias AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER vpi_landscape AS LOGICAL NO-UNDO.

define variable cadena  as character no-undo.
define variable pos     as integer   no-undo.
define variable tam     as integer   no-undo.
define variable ban     as logical   no-undo.
DEFINE VAR land AS INTEGER NO-UNDO.
DEFINE VARIABLE result AS LOGICAL NO-UNDO.



if search(vpi_spool) = ? then
  do:
    bell.
    message " No se puede encontrar el archivo solicitado ! " view-as alert-box error.
    return.
  end. 
  IF vpi_landscape THEN land = 3. ELSE land = 1.

RUN adecomm/_osprint.p (INPUT ?, INPUT vpi_spool , INPUT ?, INPUT land,
                     INPUT 0, INPUT vpi_copias, OUTPUT result).
return.

